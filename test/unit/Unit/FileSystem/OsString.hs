{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.FileSystem.OsString (tests) where

import Control.Monad (void)
import Data.Either (isRight)
import FileSystem.OsString (OsString, osstrPathSep)
import FileSystem.OsString qualified as FS.OsString
import Hedgehog
  ( Gen,
    PropertyT,
    annotate,
    annotateShow,
    evalNF,
    forAll,
    (===),
  )
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsString (osstr)
import System.OsString qualified as OsString
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "OsString"
    [ encodingTests,
      osstrPathSepTests
    ]

encodingTests :: TestTree
encodingTests =
  testGroup
    "Encoding"
    [ testEncodeLenientTotal,
      testEncodeLenientEqualsEncode,
      testDecodeLenientTotal,
      testDecodeLenientEqualsDecode
    ]

testEncodeLenientTotal :: TestTree
testEncodeLenientTotal = testPropertyNamed desc "testEncodeLenientTotal" $ do
  H.property $ do
    fp <- forAll genSketchyString

    let encoded = FS.OsString.encodeLenient fp

    void $ evalNF encoded
  where
    desc = "encodeLenient is total"

testEncodeLenientEqualsEncode :: TestTree
testEncodeLenientEqualsEncode = testPropertyNamed desc "testEncodeLenientEqualsEncode" $ do
  H.property $ do
    string <- forAll genString

    let eEncoded = FS.OsString.encode string
        encodedLenient = FS.OsString.encodeLenient string
        encodeSuccess = isRight eEncoded

    annotateShow encodedLenient

    compareWithCoverage encodeSuccess eEncoded encodedLenient
  where
    desc = "encode == encodeLenient for good paths"

testDecodeLenientTotal :: TestTree
testDecodeLenientTotal = testPropertyNamed desc "testDecodeLenientTotal" $ do
  H.property $ do
    osString <- forAll genSketchyOsString

    let decoded = FS.OsString.decodeLenient osString

    void $ evalNF decoded
  where
    desc = "decodeLenient is total"

testDecodeLenientEqualsDecode :: TestTree
testDecodeLenientEqualsDecode = testPropertyNamed desc "testDecodeLenientEqualsDecode" $ do
  H.property $ do
    osString <- forAll genValidOsString

    let eDecoded = FS.OsString.decode osString
        decodedLenient = FS.OsString.decodeLenient osString
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

-- | The idea is, generate a possibly invalid OsString s.t. generation is total
-- but decoding might fail. Turns out this works i.e. if we replace
-- decodeLenient with unsafeDecode, then the test fails
-- (which is what we want, since we want to test that lenient is actually
-- doing something).
genSketchyOsString :: Gen OsString
genSketchyOsString = do
  str <- genSketchyString
  let osCharList = OsString.unsafeFromChar <$> str
  pure $ OsString.pack osCharList

genSketchyString :: Gen String
genSketchyString = genSomeString 0 Gen.unicodeAll

genValidOsString :: Gen OsString
genValidOsString = FS.OsString.unsafeEncode <$> genString

genString :: Gen String
genString = genSomeString 1 g
  where
    g = Gen.filter (/= '\NUL') Gen.unicode

genSomeString :: Int -> Gen Char -> Gen String
genSomeString start = Gen.string (Range.linearFrom start start 50)

osstrPathSepTests :: TestTree
osstrPathSepTests =
  testGroup
    "osstrPathSep"
    [ testReplacesSlashes
    ]

testReplacesSlashes :: TestTree
testReplacesSlashes = testCase "Slashes are replaced" $ do

#if WINDOWS

  [osstr|\path\to\foo|] @=? [osstrPathSep|/path/to/foo|]
  [osstr|\path\to\foo\|] @=? [osstrPathSep|/path/to/foo/|]
  [osstr|.\path\to\foo\|] @=? [osstrPathSep|./path/to/foo/|]
  [osstr|.\|] @=? [osstrPathSep|./|]
  [osstr|.|] @=? [osstrPathSep|.|]

#else

  [osstr|/path/to/foo|] @=? [osstrPathSep|\path\to\foo|]
  [osstr|/path/to/foo/|] @=? [osstrPathSep|\path\to\foo\|]
  [osstr|./path/to/foo|] @=? [osstrPathSep|.\path\to\foo|]
  [osstr|./|] @=? [osstrPathSep|.\|]
  [osstr|.|] @=? [osstrPathSep|.|]

  -- no escaping in quasiquote
  [osstr|//path//to//foo|] @=? [osstrPathSep|\\path\\to\\foo|]

#endif
