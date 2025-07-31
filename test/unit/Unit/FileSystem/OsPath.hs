{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.FileSystem.OsPath (tests) where

import Control.Monad (void)
import Data.Either (isRight)
import FileSystem.OsPath
  ( OsPath,
    OsPathOrEmpty (OsPathEmpty, OsPathNonEmpty),
    TildePrefixState
      ( TildePrefixStateNone,
        TildePrefixStateStripped
      ),
    osp,
    ospPathSep,
  )
import FileSystem.OsPath qualified as FS.OsP
import Hedgehog
  ( Gen,
    PropertyT,
    annotate,
    annotateShow,
    assert,
    evalNF,
    forAll,
    (===),
  )
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath qualified as OsP
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "OsPath"
    [ encodingTests,
      ospPathSepTests,
      tildeTests
    ]

encodingTests :: TestTree
encodingTests =
  testGroup
    "Encoding"
    [ testEncodeLenientTotal,
      testEncodeLenientEqualsEncode,
      testEncodeValidLenientTotal,
      testEncodeValidLenientEqualsEncode,
      testDecodeLenientTotal,
      testDecodeLenientEqualsDecode
    ]

testEncodeLenientTotal :: TestTree
testEncodeLenientTotal = testPropertyNamed desc "testEncodeLenientTotal" $ do
  H.property $ do
    fp <- forAll genSketchyFilePath

    let encoded = FS.OsP.encodeLenient fp

    void $ evalNF encoded
  where
    desc = "encodeLenient is total"

testEncodeLenientEqualsEncode :: TestTree
testEncodeLenientEqualsEncode = testPropertyNamed desc "testEncodeLenientEqualsEncode" $ do
  H.property $ do
    filePath <- forAll genFilePath

    let eEncoded = FS.OsP.encode filePath
        encodedLenient = FS.OsP.encodeLenient filePath
        encodeSuccess = isRight eEncoded

    annotateShow encodedLenient

    compareWithCoverage encodeSuccess eEncoded encodedLenient
  where
    desc = "encode == encodeLenient for good paths"

testEncodeValidLenientTotal :: TestTree
testEncodeValidLenientTotal = testPropertyNamed desc "testEncodeValidLenientTotal" $ do
  H.property $ do
    fp <- forAll genSketchyFilePath

    let encoded = FS.OsP.encodeValidLenient fp

    void $ evalNF encoded
  where
    desc = "encodeValidLenient is total"

testEncodeValidLenientEqualsEncode :: TestTree
testEncodeValidLenientEqualsEncode = testPropertyNamed desc "testEncodeValidLenientEqualsEncode" $ do
  H.property $ do
    filePath <- forAll genFilePath

    let eEncoded = FS.OsP.encodeValid filePath
        encodedLenient = FS.OsP.encodeValidLenient filePath
        encodeSuccess = isRight eEncoded

    annotateShow encodedLenient

    compareWithCoverage encodeSuccess eEncoded encodedLenient
  where
    desc = "encodeValid == encodeValidLenient for good paths"

testDecodeLenientTotal :: TestTree
testDecodeLenientTotal = testPropertyNamed desc "testDecodeLenientTotal" $ do
  H.property $ do
    osPath <- forAll genSketchyOsPath

    let decoded = FS.OsP.decodeLenient osPath

    void $ evalNF decoded
  where
    desc = "decodeLenient is total"

testDecodeLenientEqualsDecode :: TestTree
testDecodeLenientEqualsDecode = testPropertyNamed desc "testDecodeLenientEqualsDecode" $ do
  H.property $ do
    osPath <- forAll genValidOsPath

    let eDecoded = FS.OsP.decode osPath
        decodedLenient = FS.OsP.decodeLenient osPath
        decodeSuccess = isRight eDecoded

    annotate decodedLenient

    compareWithCoverage decodeSuccess eDecoded decodedLenient
  where
    desc = "decode == decodeLenient for good paths"

-- NOTE: [Generating good paths]
--
-- We want to test "when paths are good ==> encode <=> encodeLenient",
-- thus we want encoding to succeed. This is hard to gurantee, however,
-- so in the case of an encode failure, we simply skip the test.
--
-- We want to ensure we are actually testing the right path most of
-- the time, hence the coverage check.
--
-- The CoverPercentage is very high because in general there are very few
-- generated paths that do not encode successfully. It isn't zero, though,
-- so we cannot assume it always succeeds. We include the coverage check to
-- ensure we are actually testing what we want most of the time.
--
-- We have tested that this actually make sense i.e. in a run with 100,000
-- tests, only 3 failed, so there are in fact __some__ failures, but very few.
--
-- Note that hedghog's coverage checker performs rounding. In particular,
-- 99.5+ will round to 100.
compareWithCoverage ::
  ( Eq a,
    Show a
  ) =>
  Bool ->
  Either e a ->
  a ->
  PropertyT IO ()
compareWithCoverage isSuccess eResult resultLenient = do
  H.cover 99 "Generation succeeded" isSuccess
  case eResult of
    Left _ -> H.label "Generated bad path"
    Right result -> do
      H.label "Generated good path"
      result === resultLenient

-- | The idea is, generate a possibly invalid OsPath s.t. generation is total
-- but decoding might fail. Turns out this works i.e. if we replace
-- decodeLenient with unsafeDecode, then the test fails
-- (which is what we want, since we want to test that lenient is actually
-- doing something).
genSketchyOsPath :: Gen OsPath
genSketchyOsPath = do
  str <- genSketchyFilePath
  let osCharList = OsP.unsafeFromChar <$> str
  pure $ OsP.pack osCharList

genSketchyFilePath :: Gen FilePath
genSketchyFilePath = genSomeFilePath 0 Gen.unicodeAll

genValidOsPath :: Gen OsPath
-- Kind of curious how this is valid...surely arbitrary unicode (removing only
-- null bytes) can produce invalid paths and cause unsafeEncodeValid to throw.
--
-- Update: Yeah, this fails on windows (strangely, only for tilde tests,
-- apparently). Let's use encodeValidLenient which should be total.
genValidOsPath = FS.OsP.encodeValidLenient <$> genFilePath

genFilePath :: Gen FilePath
genFilePath = genSomeFilePath 1 g
  where
    g = Gen.filter (/= '\NUL') Gen.unicode

genSomeFilePath :: Int -> Gen Char -> Gen FilePath
genSomeFilePath start = Gen.string (Range.linearFrom start start 50)

ospPathSepTests :: TestTree
ospPathSepTests =
  testGroup
    "ospPathSep"
    [ testReplacesSlashes
    ]

testReplacesSlashes :: TestTree
testReplacesSlashes = testCase "Slashes are replaced" $ do

#if WINDOWS

  [osp|\path\to\foo|] @=? [ospPathSep|/path/to/foo|]
  [osp|\path\to\foo\|] @=? [ospPathSep|/path/to/foo/|]
  [osp|.\path\to\foo\|] @=? [ospPathSep|./path/to/foo/|]
  [osp|.\|] @=? [ospPathSep|./|]
  [osp|.|] @=? [ospPathSep|.|]

#else

  [osp|/path/to/foo|] @=? [ospPathSep|\path\to\foo|]
  [osp|/path/to/foo/|] @=? [ospPathSep|\path\to\foo\|]
  [osp|./path/to/foo|] @=? [ospPathSep|.\path\to\foo|]
  [osp|./|] @=? [ospPathSep|.\|]
  [osp|.|] @=? [ospPathSep|.|]

  -- no escaping in quasiquote
  [osp|//path//to//foo|] @=? [ospPathSep|\\path\\to\\foo|]

#endif

tildeTests :: TestTree
tildeTests =
  testGroup
    "toTildePrefixState"
    [ testTildeCases,
      testTildePreservesValid
    ]

testTildeCases :: TestTree
testTildeCases = testCase "Cases" $ do
  -- Both unix and windows consider '~/' a tilde prefix.
  TildePrefixStateNone [osp|foo/bar|] @=? FS.OsP.toTildePrefixState [osp|foo/bar|]
  TildePrefixStateStripped OsPathEmpty @=? FS.OsP.toTildePrefixState [osp|~/|]
  TildePrefixStateStripped OsPathEmpty @=? FS.OsP.toTildePrefixState [osp|~|]
  -- Multiple tildes is not a prefix
  TildePrefixStateNone [osp|~~~|] @=? FS.OsP.toTildePrefixState [osp|~~~|]
  TildePrefixStateStripped (OsPathNonEmpty [osp|foo/bar|]) @=? FS.OsP.toTildePrefixState [osp|~/foo/bar|]
  TildePrefixStateNone [osp|foo/b~ar|] @=? FS.OsP.toTildePrefixState [osp|foo/b~ar|]
  TildePrefixStateStripped (OsPathNonEmpty [osp|foo/b~ar|]) @=? FS.OsP.toTildePrefixState [osp|~/foo/b~ar|]
  -- Use ospPathSep so that we test the "canonical" path separator i.e.
  -- '~\' on unix (redundant for unix, since should be same as above).
  TildePrefixStateNone [ospPathSep|foo/bar|] @=? FS.OsP.toTildePrefixState [ospPathSep|foo/bar|]
  TildePrefixStateStripped (OsPathNonEmpty [ospPathSep|foo/bar|]) @=? FS.OsP.toTildePrefixState [ospPathSep|~/foo/bar|]
  TildePrefixStateNone [ospPathSep|foo/b~ar|] @=? FS.OsP.toTildePrefixState [ospPathSep|foo/b~ar|]
  TildePrefixStateStripped (OsPathNonEmpty [ospPathSep|foo/b~ar|]) @=? FS.OsP.toTildePrefixState [ospPathSep|~/foo/b~ar|]
  -- Both unix and windows should handle this the same.
  TildePrefixStateStripped (OsPathNonEmpty [osp|foo/~\bar|]) @=? FS.OsP.toTildePrefixState [osp|~/foo/~\bar|]
  -- Consecutive prefixes are stripped
  TildePrefixStateStripped (OsPathNonEmpty [osp|foo~bar|]) @=? FS.OsP.toTildePrefixState [osp|~/~/~/foo~bar|]
#if WINDOWS
  -- Windows should recognize the tilde prefix '~\'.
  TildePrefixStateStripped OsPathEmpty @=? FS.OsP.toTildePrefixState [osp|~\|]
  TildePrefixStateStripped (OsPathNonEmpty [osp|foo\bar|]) @=? FS.OsP.toTildePrefixState [osp|~\foo\bar|]
  -- Windows should strip the first prefix then die on the second, since we
  -- only allow one prefix.
  TildePrefixStateStripped (OsPathNonEmpty [osp|foo~/bar|]) @=? FS.OsP.toTildePrefixState [osp|~\foo~/bar|]
  -- Consecutive prefixes are stripped
  TildePrefixStateStripped (OsPathNonEmpty [osp|foo~bar|]) @=? FS.OsP.toTildePrefixState [osp|~/~\~/foo~bar|]
#else
  -- Unix does not recognize '~\' as a tilde prefix.
  TildePrefixStateNone [osp|~\foo\bar|] @=? FS.OsP.toTildePrefixState [osp|~\foo\bar|]
  -- Unix should die on the first prefix, since it's windows only.
  TildePrefixStateNone [osp|~\foo~/bar|] @=? FS.OsP.toTildePrefixState [osp|~\foo~/bar|]
  -- Stops after first posix prefix.
  TildePrefixStateStripped (OsPathNonEmpty [osp|~\~/foo~bar|]) @=? FS.OsP.toTildePrefixState [osp|~/~\~/foo~bar|]
#endif

testTildePreservesValid :: TestTree
testTildePreservesValid = testPropertyNamed desc name $ do
  H.property $ do
    osPath <- forAll genValidOsPath

    let ts = FS.OsP.toTildePrefixState osPath

    H.annotateShow ts
    case ts of
      TildePrefixStateStripped (OsPathNonEmpty p) ->
        assert $ OsP.isValid p
      _ -> pure ()
  where
    desc = "Preserves validity"
    name = "testTildePreservesValid"
