{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.FileSystem.OsString (tests) where

import Control.Monad (void)
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.Either (isRight)
import FileSystem.OsString
  ( OsString,
    TildePrefixState
      ( TildePrefixStateNone,
        TildePrefixStateStripped
      ),
    osstr,
    osstrPathSep,
  )
import FileSystem.OsString qualified as FS.OsStr
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
import System.OsString qualified as OsStr
#if WINDOWS
import System.OsString.Internal.Types
  ( OsString (OsString),
    WindowsString (WindowsString),
  )
#else
import System.OsString.Internal.Types
  ( OsString (OsString),
    PosixString (PosixString),
  )
#endif
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "OsString"
    [ encodingTests,
      osstrPathSepTests,
      tildeTests,
      normalizeTests
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

    let encoded = FS.OsStr.encodeLenient fp

    void $ evalNF encoded
  where
    desc = "encodeLenient is total"

testEncodeLenientEqualsEncode :: TestTree
testEncodeLenientEqualsEncode = testPropertyNamed desc "testEncodeLenientEqualsEncode" $ do
  H.property $ do
    string <- forAll genString

    let eEncoded = FS.OsStr.encode string
        encodedLenient = FS.OsStr.encodeLenient string
        encodeSuccess = isRight eEncoded

    annotateShow encodedLenient

    compareWithCoverage encodeSuccess eEncoded encodedLenient
  where
    desc = "encode == encodeLenient for good paths"

testDecodeLenientTotal :: TestTree
testDecodeLenientTotal = testPropertyNamed desc "testDecodeLenientTotal" $ do
  H.property $ do
    osString <- forAll genSketchyOsString

    let decoded = FS.OsStr.decodeLenient osString

    void $ evalNF decoded
  where
    desc = "decodeLenient is total"

testDecodeLenientEqualsDecode :: TestTree
testDecodeLenientEqualsDecode = testPropertyNamed desc "testDecodeLenientEqualsDecode" $ do
  H.property $ do
    osString <- forAll genValidOsString

    let eDecoded = FS.OsStr.decode osString
        decodedLenient = FS.OsStr.decodeLenient osString
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
  let osCharList = OsStr.unsafeFromChar <$> str
  pure $ OsStr.pack osCharList

genSketchyString :: Gen String
genSketchyString = genSomeString 0 Gen.unicodeAll

genValidOsString :: Gen OsString
genValidOsString = FS.OsStr.unsafeEncode <$> genString

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

tildeTests :: TestTree
tildeTests =
  testGroup
    "toTildePrefixState"
    [ testTildeCases
    ]

testTildeCases :: TestTree
testTildeCases = testCase "Cases" $ do
  -- Both unix and windows consider '~/' a tilde prefix.
  TildePrefixStateNone [osstr|foo/bar|] @=? FS.OsStr.toTildePrefixState [osstr|foo/bar|]
  TildePrefixStateStripped [osstr||] @=? FS.OsStr.toTildePrefixState [osstr|~/|]
  TildePrefixStateStripped [osstr||] @=? FS.OsStr.toTildePrefixState [osstr|~|]
  -- Multiple tildes is not a prefix
  TildePrefixStateNone [osstr|~~~|] @=? FS.OsStr.toTildePrefixState [osstr|~~~|]
  TildePrefixStateStripped [osstr|foo/bar|] @=? FS.OsStr.toTildePrefixState [osstr|~/foo/bar|]
  TildePrefixStateNone [osstr|foo/b~ar|] @=? FS.OsStr.toTildePrefixState [osstr|foo/b~ar|]
  TildePrefixStateStripped [osstr|foo/b~ar|] @=? FS.OsStr.toTildePrefixState [osstr|~/foo/b~ar|]
  -- Use osstrPathSep so that we test the "canonical" path separator i.e.
  -- '~\' on unix (redundant for unix, since should be same as above).
  TildePrefixStateNone [osstrPathSep|foo/bar|] @=? FS.OsStr.toTildePrefixState [osstrPathSep|foo/bar|]
  TildePrefixStateStripped [osstrPathSep|foo/bar|] @=? FS.OsStr.toTildePrefixState [osstrPathSep|~/foo/bar|]
  TildePrefixStateNone [osstrPathSep|foo/b~ar|] @=? FS.OsStr.toTildePrefixState [osstrPathSep|foo/b~ar|]
  TildePrefixStateStripped [osstrPathSep|foo/b~ar|] @=? FS.OsStr.toTildePrefixState [osstrPathSep|~/foo/b~ar|]
  -- Both unix and windows should handle this the same.
  TildePrefixStateStripped [osstr|foo/~\bar|] @=? FS.OsStr.toTildePrefixState [osstr|~/foo/~\bar|]
  -- Consecutive prefixes are stripped
  TildePrefixStateStripped [osstr|foo~bar|] @=? FS.OsStr.toTildePrefixState [osstr|~/~/~/foo~bar|]
#if WINDOWS
  -- Windows should recognize the tilde prefix '~\'.
  TildePrefixStateStripped [osstr||] @=? FS.OsStr.toTildePrefixState [osstr|~\|]
  TildePrefixStateStripped [osstr|foo\bar|] @=? FS.OsStr.toTildePrefixState [osstr|~\foo\bar|]
  -- Windows should strip the first prefix then die on the second, since we
  -- only allow one prefix.
  TildePrefixStateStripped [osstr|foo~/bar|] @=? FS.OsStr.toTildePrefixState [osstr|~\foo~/bar|]
  -- Consecutive prefixes are stripped
  TildePrefixStateStripped [osstr|foo~bar|] @=? FS.OsStr.toTildePrefixState [osstr|~/~\~/foo~bar|]
#else
  -- Unix does not recognize '~\' as a tilde prefix.
  TildePrefixStateNone [osstr|~\foo\bar|] @=? FS.OsStr.toTildePrefixState [osstr|~\foo\bar|]
  -- Unix should die on the first prefix, since it's windows only.
  TildePrefixStateNone [osstr|~\foo~/bar|] @=? FS.OsStr.toTildePrefixState [osstr|~\foo~/bar|]
  -- Stops after first posix prefix.
  TildePrefixStateStripped [osstr|~\~/foo~bar|] @=? FS.OsStr.toTildePrefixState [osstr|~/~\~/foo~bar|]
#endif

normalizeTests :: TestTree
normalizeTests =
  testGroup
    "Normalize"
    [ testNormalize
    ]

{- ORMOLU_DISABLE -}

testNormalize :: TestTree
testNormalize = testCase "Normalizes string" $ do
  -- S is two code points, 'O' and "combining diacritical marks".
  2 @=? length s
  "\x4F\x308" @=? s

  -- Decode these to [Word8] for inspection.

  os@(OsString osi) <- FS.OsStr.encodeThrowM s
  let osBytes = coerce osi

      -- Normalized OsString will combine the 'O' and diacrits into a single
      -- code point.
      os'@(OsString osi') = FS.OsStr.normalize os
      osBytes' = coerce osi'

#if WINDOWS
  -- Encoding to WindowsString (UTF-16 little-endian) is 2 bytes.
  2 * windowsBugFactor @=? FS.OsStr.length os

  -- Normalized WindowsString is 1 byte.
  1 * windowsBugFactor @=? FS.OsStr.length os'

  -- O, U+0308 (little-endian). Notice we have 4 elements instead of 2,
  -- because 'unpack :: ShortByteString -> [Word8]'.
  [79, 0, 8, 3] @=? SBS.unpack osBytes

  -- U+D6.
  [214, 0] @=? SBS.unpack osBytes'
#else
  -- Encoding to PosixString (UTF-8) is 3 bytes.
  3 @=? FS.OsStr.length os

  -- O, U+CC, U+88
  -- The latter two are the UTF-8 encoding of U+308.
  [79, 204, 136] @=? SBS.unpack osBytes

  -- Normalized PosixString is 2 bytes.
  2 @=? FS.OsStr.length os'

  -- U+C3, U+96
  -- These are the UTF-8 encoding of U+D6.
  [195, 150] @=? SBS.unpack osBytes'
#endif

  -- Normalized length should be one, since the 'O' and diacrits should be
  -- combined into a single glyph, per text normalization.
  1 @=? FS.OsStr.glyphLength os
  where
    -- s = "Ö"
    s = ['\x4F', '\x308']

-- NOTE: [Windows os-string length bug]
--
-- Bug in os-string ~ 2.0.2 + windows where length counted number of word8
-- bytes, not word16. Affects GHC 9.10 on CI since that is the only version
-- that picks such a version.
--
-- I'm not entirely sure which version it appeared in, only that:
--
--   - It was fixed in os-string 2.0.3.
--   - It occurs for GHC 9.10 on CI, presumably 2.0.2 (bundled with GHC
--     9.10).
#if WINDOWS
windowsBugFactor :: Int
#if MIN_VERSION_os_string(2,0,3) || !MIN_VERSION_os_string(2,0,2)
windowsBugFactor = 1
#else
windowsBugFactor = 2
#endif
#endif

{- ORMOLU_ENABLE -}
