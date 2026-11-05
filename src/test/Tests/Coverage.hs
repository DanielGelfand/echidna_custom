module Tests.Coverage (coverageTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import EVM.Solidity (BuildOutput(..), SolcContract(..), SourceCache(..), SrcMap(..), getContractsMap)

import Common (testContract, passed, countCorpus, checkCoverageUsesCorpusDir)
import Echidna.Solidity (readSolcBatch)
import Echidna.Solidity.Foundry
  ( Compiler(..), FoundryBuild(..), compilerFromVersion, compilerIdsFor, foundryOutArg
  , foundryPath, readFoundryBuilds )

coverageTests :: TestTree
coverageTests = testGroup "Coverage tests"
  [
      -- single.sol is really slow and kind of unstable. it also messes up travis.
     -- testContract "coverage/single.sol"    (Just "coverage/test.yaml")
     -- [ ("echidna_state failed",                   solved      "echidna_state") ]
     -- testContract' "coverage/multi.sol" Nothing Nothing (Just "coverage/test.yaml") False False
     -- [ ("echidna_state3 failed",                  solved      "echidna_state3") ]
      testContract "coverage/boolean.sol"       (Just "coverage/boolean.yaml")
      [ ("echidna_true failed",                    passed     "echidna_true")
      , ("unexpected corpus count ",               countCorpus 1)]

  -- Test corpus and coverage directory functionality
  , testContract "basic/revert.sol"              (Just "basic/coverage-test.yaml")
      [ ("corpus count",                           countCorpus 1)]

  -- Test coverage fallback to corpus directory
  , testContract "basic/revert.sol"              (Just "basic/corpus-fallback-test.yaml")
      [ ("uses corpusDir for coverage",           checkCoverageUsesCorpusDir "test-corpus")]

  -- Solar numbers the files of solar-ids in reverse (Zlib 0, Alpha 1, Main 2),
  -- and crytic-compile lists them in that order. The second unit's B.sol is
  -- also file 0 of its own unit.
  , testCase "coverage/solar-ids" $ do
      buildOutput <- mconcat <$> readSolcBatch [] "coverage/solar-ids/export"
      let main = sourceSnippets buildOutput "coverage/solar-ids/src/Main.sol:Main"
          b = sourceSnippets buildOutput "coverage/solar-ids/b/src/B.sol:B"
      assertBool "Zlib's x + 1 is not mapped to Zlib.sol" $
        ("coverage/solar-ids/src/z/Zlib.sol", "x + 1") `elem` main
      assertBool "Main's x < 5 is not mapped to Main.sol" $
        ("coverage/solar-ids/src/Main.sol", "x < 5") `elem` main
      assertBool "B's v * 2 is not mapped to B.sol" $
        ("coverage/solar-ids/b/src/B.sol", "v * 2") `elem` b

  -- foundry-ids/solar is the same project built by Foundry with Solar, and
  -- exported by crytic-compile 0.3.11, which lists the files in path order
  -- (Main, Alpha, Zlib).
  , testCase "coverage/foundry-ids: a Solar build takes Foundry's ids" $ do
      builds <- readFoundryBuilds [] "coverage/foundry-ids/solar"
      map (\b -> (b.buildId, b.compiler)) builds @?= [("8732b5cdf368240e", Solar)]
      byPosition <- mconcat <$> readSolcBatch [] "coverage/foundry-ids/solar/export"
      assertBool "position ids already map Zlib's x + 1 to Zlib.sol, so the fixture shows nothing" $
        (solarPath "src/z/Zlib.sol", "x + 1") `notElem` sourceSnippets byPosition (solarPath "src/Main.sol:Main")
      buildOutput <- mconcat <$> readSolcBatch builds "coverage/foundry-ids/solar/export"
      let main = sourceSnippets buildOutput (solarPath "src/Main.sol:Main")
      assertBool "Zlib's x + 1 is not mapped to Zlib.sol" $
        (solarPath "src/z/Zlib.sol", "x + 1") `elem` main
      assertBool "Main's x < 5 is not mapped to Main.sol" $
        (solarPath "src/Main.sol", "x < 5") `elem` main
      assertBool "Alpha's Zlib.inc(x) is not mapped to Alpha.sol" $
        (solarPath "src/a/Alpha.sol", "Zlib.inc(x)") `elem` main

  -- out-old holds a solc build of the same files. crytic-compile reads the
  -- build-info of the out directory it is given, and so does Echidna.
  , testCase "coverage/foundry-ids: --foundry-out-directory picks the build-info" $ do
      let outOld = [("e291cf7debf86b84", Solc)]
          compilers args = map (\b -> (b.buildId, b.compiler)) <$> readFoundryBuilds args "coverage/foundry-ids/solar"
      compilers ["--foundry-out-directory=out-old"] >>= (@?= outOld)
      compilers ["--foundry-ignore-compile", "--foundry-out-directory", "out-old"] >>= (@?= outOld)
      compilers ["--foundry-compile-all"] >>= (@?= [("8732b5cdf368240e", Solar)])
      readFoundryBuilds [] "coverage/foundry-ids" >>= (@?= [])
      foundryOutArg ["--foundry-out-directory"] @?= Nothing

  , testCase "coverage/foundry-ids: a solc build keeps its position ids" $ do
      builds <- readFoundryBuilds [] "coverage/foundry-ids/solc"
      map (\b -> (b.buildId, b.compiler)) builds @?= [("e291cf7debf86b84", Solc)]
      byPosition <- readSolcBatch [] "coverage/foundry-ids/solc/export"
      withBuilds <- readSolcBatch builds "coverage/foundry-ids/solc/export"
      withBuilds @?= byPosition
      assertBool "Zlib's x + 1 is not mapped to Zlib.sol" $
        ("coverage/foundry-ids/solc/src/z/Zlib.sol", "x + 1")
          `elem` sourceSnippets (mconcat withBuilds) "coverage/foundry-ids/solc/src/Main.sol:Main"

  , testCase "Foundry compiler versions" $ do
      compilerFromVersion "0.8.38+commit.a2b44354.solar.0.3.0-dev" @?= Solar
      compilerFromVersion "0.8.35+commit.47b9dedd" @?= Solc
      compilerFromVersion "0.8.35" @?= Solc

  , testCase "units matched to Foundry builds" $ do
      let unit = Map.fromList [(0, "/p/src/Main.sol"), (1, "/p/src/Zlib.sol")]
          build bid c ids = FoundryBuild { buildId = bid, compiler = c, sourceIds = Map.fromList ids }
          solar = build "a" Solar [("/p/src/Main.sol", 1), ("/p/src/Zlib.sol", 0)]
          solc = build "b" Solc [("/p/src/Main.sol", 0), ("/p/src/Zlib.sol", 1)]
          otherSolar = build "c" Solar [("/p/src/Other.sol", 0)]
      compilerIdsFor [solar] unit @?= Right (Just (Map.fromList [(0, 1), (1, 0)]))
      compilerIdsFor [solc] unit @?= Right Nothing
      compilerIdsFor [] unit @?= Right Nothing
      -- a build listing more files than the unit is another unit
      compilerIdsFor [build "d" Solar [("/p/src/Main.sol", 2), ("/p/src/Zlib.sol", 0), ("/p/src/X.sol", 1)]] unit
        @?= Left "no Solar build-info lists exactly this unit's files"
      compilerIdsFor [solc, otherSolar] unit @?= Right Nothing
      compilerIdsFor [otherSolar] unit
        @?= Left "no Solar build-info lists exactly this unit's files"
      compilerIdsFor [solar, solc] unit
        @?= Left "several build-infos list this unit's files with different ids"
      compilerIdsFor [solar, solar { buildId = "e" }] unit
        @?= Right (Just (Map.fromList [(0, 1), (1, 0)]))
      -- Imports that differ only in case are two files to the compiler, and a
      -- path matches the longest trailing part of it the build lists.
      let caseUnit = Map.fromList
            [(0, "/r/src/pre/A.sol"), (1, "/r/src/Pre/A.sol"), (2, "/r/lib/x/src/A.sol"), (3, "/r/src/A.sol")]
          caseBuild = build "f" Solar
            [("src/Pre/A.sol", 0), ("src/pre/A.sol", 1), ("src/A.sol", 2), ("lib/x/src/A.sol", 3)]
      compilerIdsFor [caseBuild] caseUnit @?= Right (Just (Map.fromList [(0, 1), (1, 0), (2, 3), (3, 2)]))

  , testCase "Foundry paths" $ do
      foundryPath "/r/p" "src/A.sol" @?= "src/A.sol"
      foundryPath "/r/p" "./src/A.sol" @?= "src/A.sol"
      foundryPath "/r/p" "../shared/src/A.sol" @?= "/r/shared/src/A.sol"
      foundryPath "/r/p" "/abs/A.sol" @?= "/abs/A.sol"
  ]
  where
  solarPath p = "coverage/foundry-ids/solar/" <> p

-- | The (file, source text) each source map entry of a contract's runtime
-- code points at.
sourceSnippets :: BuildOutput -> Text -> [(FilePath, ByteString)]
sourceSnippets buildOutput name =
  [ (path, BS.take len (BS.drop offset contents))
  | Just contract <- [Map.lookup name (getContractsMap buildOutput.contracts)]
  , SM offset len file _ _ <- toList contract.runtimeSrcmap
  , Just (path, contents) <- [Map.lookup file buildOutput.sources.files]
  ]
