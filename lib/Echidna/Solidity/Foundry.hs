-- | What Foundry records about its own builds: which compiler built each
-- compilation unit, and which id that compiler gave each of the unit's files.
--
-- A source map names a file by its compiler id. solc numbers a unit's files in
-- path order, which is the order crytic-compile lists them in, so the position
-- of a file in its export is its id. Solar numbers them in another order, so a
-- unit Foundry built with Solar takes its ids from Foundry's build-info.
module Echidna.Solidity.Foundry
  ( Compiler(..)
  , FoundryBuild(..)
  , compilerFromVersion
  , readFoundryBuilds
  , readFoundryCache
  , compilerIdsFor
  , foundryPath
  ) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (forM, guard, when)
import Data.Aeson
  ( FromJSON(..), Value(..), eitherDecode, eitherDecodeFileStrict, withObject
  , (.:), (.:?), (.!=) )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist, makeAbsolute)
import System.Exit (ExitCode(..))
import System.FilePath (joinPath, normalise, splitDirectories, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (cwd, proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)

data Compiler = Solc | Solar
  deriving (Eq, Show)

-- | Foundry records Solar under versions such as
-- @0.8.38+commit.a2b44354.solar.0.3.0-dev@ and solc under plain ones.
compilerFromVersion :: Text -> Compiler
compilerFromVersion version
  | "solar" `T.isInfixOf` T.toLower version = Solar
  | otherwise = Solc

-- | One build listed in Foundry's cache.
data FoundryBuild = FoundryBuild
  { buildId :: Text
  , compiler :: Compiler
  , sourceIds :: Map FilePath Int
    -- ^ the compiler's file ids, keyed by path as Foundry wrote it: relative
    -- to the project root, or absolute for a file outside it
  }
  deriving (Eq, Show)

-- | The builds of the Foundry project at the given root, or none when the
-- root is not a Foundry project or its cache is missing.
readFoundryBuilds :: FilePath -> IO [FoundryBuild]
readFoundryBuilds root = do
  isFoundry <- doesFileExist (root </> "foundry.toml")
  if not isFoundry then pure [] else do
    config <- forgeConfig root
    let cacheFile = root </> config.cachePath </> "solidity-files-cache.json"
    hasCache <- doesFileExist cacheFile
    if hasCache then readFoundryCache root cacheFile else do
      when (fmap compilerFromVersion config.solc == Just Solar) $
        hPutStrLn stderr $ "WARNING: Foundry builds " <> root <> " with Solar but keeps no cache at "
          <> cacheFile <> ", so Echidna can't read Solar's file ids and coverage may be reported on the wrong files"
      pure []

-- | The settings of the active Foundry profile that tell where the build
-- went and which compiler ran.
data ForgeConfig = ForgeConfig
  { cachePath :: FilePath
  , solc :: Maybe Text
  }

instance FromJSON ForgeConfig where
  parseJSON = withObject "forge config" $ \o ->
    ForgeConfig <$> o .:? "cache_path" .!= "cache" <*> o .:? "solc"

-- | The active profile as @forge config@ resolves it, or Foundry's defaults
-- when forge can't tell.
forgeConfig :: FilePath -> IO ForgeConfig
forgeConfig root = do
  result <- try (readCreateProcessWithExitCode (proc "forge" ["config", "--json"]) { cwd = Just root } "")
  pure $ case result :: Either IOException (ExitCode, String, String) of
    Right (ExitSuccess, out, _) | Right config <- eitherDecode (BSL.pack out) -> config
    _ -> ForgeConfig { cachePath = "cache", solc = Nothing }

-- | The builds a Foundry cache file lists, with the compiler and ids from their
-- build-info files. Only builds the cache lists count, so a build-info left
-- behind by an earlier build is never used. A build whose build-info can't be
-- read keeps no ids, so it matches no unit.
readFoundryCache :: FilePath -> FilePath -> IO [FoundryBuild]
readFoundryCache root cacheFile = do
  exists <- doesFileExist cacheFile
  if not exists then pure [] else
    eitherDecodeFileStrict cacheFile >>= \case
      Left _ -> pure []
      Right (cache :: FilesCache) -> forM cache.builds $ \bid -> do
        info <- readBuildInfo (root </> cache.buildInfoDir </> T.unpack bid <> ".json")
        absRoot <- makeAbsolute root
        let sourceIds = Map.mapKeys (foundryPath absRoot) (maybe mempty (.ids) info)
        let version = (info >>= (.version)) <|> Map.lookup bid cache.buildVersions
        pure FoundryBuild
          { buildId = bid
          , compiler = maybe Solc compilerFromVersion version
          , sourceIds
          }

data FilesCache = FilesCache
  { buildInfoDir :: FilePath
  , builds :: [Text]
  , buildVersions :: Map Text Text
    -- ^ the compiler version of each build, from the artifacts it produced
  }

instance FromJSON FilesCache where
  parseJSON = withObject "solidity-files-cache" $ \o -> do
    paths <- o .: "paths"
    buildInfoDir <- paths .: "build_infos"
    builds <- o .:? "builds" .!= []
    files <- o .:? "files" .!= mempty :: Parser (Map Text Value)
    let buildVersions = Map.fromList (concatMap artifactBuilds (Map.elems files))
    pure FilesCache { buildInfoDir, builds, buildVersions }
    where
    -- A file's artifacts are keyed by contract, then by compiler version;
    -- each version holds the build ids that produced it.
    artifactBuilds (Object file)
      | Just (Object artifacts) <- KeyMap.lookup "artifacts" file =
          [ (bid, Key.toText version)
          | Object versions <- KeyMap.elems artifacts
          , (version, built) <- KeyMap.toList versions
          , bid <- buildIdsIn built
          ]
    artifactBuilds _ = []
    buildIdsIn = \case
      Object o -> case KeyMap.lookup "build_id" o of
        Just (String bid) -> [bid]
        _ -> concatMap buildIdsIn (KeyMap.elems o)
      _ -> []

-- | A build-info file's compiler version and file ids, keyed by path relative
-- to the project root. Newer Foundry versions write the ids as
-- @source_id_to_path@, older ones only in the compiler output.
data BuildInfo = BuildInfo
  { version :: Maybe Text
  , ids :: Map FilePath Int
  }

instance FromJSON BuildInfo where
  parseJSON = withObject "build-info" $ \o -> do
    version <- o .:? "solcLongVersion"
    ids <- o .:? "source_id_to_path" >>= \case
      Just byId -> Map.fromList <$> mapM swap (Map.toList (byId :: Map Text FilePath))
      Nothing -> do
        output <- o .: "output"
        sources <- output .: "sources" :: Parser (Map FilePath Value)
        traverse (withObject "source" (.: "id")) sources
    pure BuildInfo { version, ids }
    where
    swap (i, path) = maybe (fail $ "source id is not a number: " <> T.unpack i)
                           (pure . (path,)) (readMaybe (T.unpack i))

-- | A file outside the project root is written relative to it (@../x.sol@),
-- so it is turned into an absolute path, without resolving links: the case of
-- a path matters, since two imports of one file that differ only in case are
-- two files to the compiler.
foundryPath :: FilePath -> FilePath -> FilePath
foundryPath absRoot path
  | ".." `elem` splitDirectories path = collapse (absRoot </> path)
  | otherwise = normalise path
  where
  collapse = joinPath . reverse . foldl step [] . splitDirectories
  step (dir : dirs) ".." | dir /= "/" = dirs
  step dirs "." = dirs
  step dirs part = part : dirs

readBuildInfo :: FilePath -> IO (Maybe BuildInfo)
readBuildInfo path = do
  exists <- doesFileExist path
  if not exists then pure Nothing else
    either (const Nothing) Just <$> eitherDecodeFileStrict path

-- | The compiler ids of a unit's files, keyed by the ids hevm gave them, when
-- Foundry built the unit with Solar. The unit is the build that lists exactly
-- its files: crytic-compile's paths may be absolute, so each one matches the
-- longest trailing part of it that the build lists. 'Left' explains why a Solar unit could not be matched; 'Right
-- Nothing' means the unit's position ids are already the compiler's.
compilerIdsFor :: [FoundryBuild] -> Map Int FilePath -> Either String (Maybe (Map Int Int))
compilerIdsFor builds files =
  case nub [ (b.compiler, ids) | b <- builds, Just ids <- [idsIn b] ] of
    [] | any ((== Solar) . (.compiler)) builds ->
           Left "no Solar build in Foundry's cache lists exactly this unit's files"
       | otherwise -> Right Nothing
    [(Solc, _)] -> Right Nothing
    [(Solar, ids)] -> Right (Just ids)
    matches
      | all ((== Solc) . fst) matches -> Right Nothing
      | otherwise ->
          Left "several builds in Foundry's cache list this unit's files with different ids"
  where
  idsIn build = do
    guard (Map.size build.sourceIds == Map.size files)
    ids <- traverse (lookupPathSuffix build.sourceIds) files
    guard (length (nub (Map.elems ids)) == Map.size ids)
    pure ids

-- | The entry for the longest trailing part of a path that the map lists.
lookupPathSuffix :: Map FilePath a -> FilePath -> Maybe a
lookupPathSuffix m path = listToMaybe $ mapMaybe (`Map.lookup` m) suffixes
  where
  parts = splitDirectories (normalise path)
  suffixes = [joinPath (drop n parts) | n <- [0 .. length parts - 1]]
