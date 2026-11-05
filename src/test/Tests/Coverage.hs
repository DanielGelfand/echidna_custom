module Tests.Coverage (coverageTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import EVM.Solidity (BuildOutput(..), SolcContract(..), SourceCache(..), SrcMap(..), getContractsMap)

import Common (testContract, testContract', passed, countCorpus, checkCoverageUsesCorpusDir, codeUnits, uniqueCodehashes, hitCountInvariants, lineExecutionsReported, uniqueEdgesAtLeast)
import Echidna.Solidity (readSolcBatch)
import Echidna.Types.Worker (WorkerType(..))

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

  -- A and B share runtime code but not creation code: three creation units
  -- (Main, A, B), two runtime units (Main, A/B), two distinct contracts.
  , testContract' "coverage/shared_runtime.sol" (Just "Main") Nothing (Just "coverage/boolean.yaml") True FuzzWorker
      [ ("creation units keyed by creation code", codeUnits 3 2)
      , ("unique codehashes count owners",        uniqueCodehashes 2)]

  , testContract "coverage/hitcounts.sol"     (Just "coverage/boolean.yaml")
      [ ("hit counts agree with coverage",        hitCountInvariants)
      , ("line executions reported",             lineExecutionsReported)]

  -- Edge coverage: taken jumps are recorded and reported
  , testContract "coverage/boolean.sol"       (Just "coverage/edges.yaml")
      [ ("echidna_true failed",                    passed     "echidna_true")
      , ("jump edges recorded",                    uniqueEdgesAtLeast 4)]

  -- A new jump into covered code keeps its sequence only with edge coverage:
  -- the same campaign keeps two more sequences with edges on
  , testContract "coverage/edge_only.sol"     (Just "coverage/edge_only.yaml")
      [ ("edge-only novelty kept",                countCorpus 7)]
  , testContract "coverage/edge_only.sol"     (Just "coverage/edge_only_off.yaml")
      [ ("instruction novelty only",              countCorpus 5)]

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
      buildOutput <- mconcat <$> readSolcBatch "coverage/solar-ids/export"
      let main = sourceSnippets buildOutput "coverage/solar-ids/src/Main.sol:Main"
          b = sourceSnippets buildOutput "coverage/solar-ids/b/src/B.sol:B"
      assertBool "Zlib's x + 1 is not mapped to Zlib.sol" $
        ("coverage/solar-ids/src/z/Zlib.sol", "x + 1") `elem` main
      assertBool "Main's x < 5 is not mapped to Main.sol" $
        ("coverage/solar-ids/src/Main.sol", "x < 5") `elem` main
      assertBool "B's v * 2 is not mapped to B.sol" $
        ("coverage/solar-ids/b/src/B.sol", "v * 2") `elem` b
  ]

-- | The (file, source text) each source map entry of a contract's runtime
-- code points at.
sourceSnippets :: BuildOutput -> Text -> [(FilePath, ByteString)]
sourceSnippets buildOutput name =
  [ (path, BS.take len (BS.drop offset contents))
  | Just contract <- [Map.lookup name (getContractsMap buildOutput.contracts)]
  , SM offset len file _ _ <- toList contract.runtimeSrcmap
  , Just (path, contents) <- [Map.lookup file buildOutput.sources.files]
  ]
