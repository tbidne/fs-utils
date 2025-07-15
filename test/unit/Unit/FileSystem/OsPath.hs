{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.FileSystem.OsPath (tests) where

import Control.Monad (void)
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
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
    "OsPath"
    [ encodingTests,
      ospPathSepTests,
      tildeTests,
      normalizeTests
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

  os@(OsString osi) <- FS.OsP.encodeThrowM s
  let osBytes = coerce osi

      -- Normalized OsString will combine the 'O' and diacrits into a single
      -- code point.
      os'@(OsString osi') = FS.OsP.normalize os
      osBytes' = coerce osi'

#if WINDOWS
  -- Encoding to WindowsString (UTF-16 little-endian) is 2 bytes.
  2 * windowsBugFactor @=? FS.OsP.length os

  -- Normalized WindowsString is 1 byte.
  1 * windowsBugFactor @=? FS.OsP.length os'

  -- O, U+0308 (little-endian). Notice we have 4 elements instead of 2,
  -- because 'unpack :: ShortByteString -> [Word8]'.
  [79, 0, 8, 3] @=? SBS.unpack osBytes

  -- U+D6.
  [214, 0] @=? SBS.unpack osBytes'
#else
  -- Encoding to PosixString (UTF-8) is 3 bytes.
  3 @=? FS.OsP.length os

  -- O, U+CC, U+88
  -- The latter two are the UTF-8 encoding of U+308.
  [79, 204, 136] @=? SBS.unpack osBytes

  -- Normalized PosixString is 2 bytes.
  2 @=? FS.OsP.length os'

  -- U+C3, U+96
  -- These are the UTF-8 encoding of U+D6.
  [195, 150] @=? SBS.unpack osBytes'
#endif

  -- Normalized length should be one, since the 'O' and diacrits should be
  -- combined into a single glyph, per text normalization.
  1 @=? FS.OsP.glyphLength os
  where
    -- s = "Ö"
    s = ['\x4F', '\x308']

-- see NOTE: [Windows os-string length bug]
#if WINDOWS
windowsBugFactor :: Int
#if MIN_VERSION_os_string(2,0,3) || !MIN_VERSION_os_string(2,0,2)
windowsBugFactor = 1
#else
windowsBugFactor = 2
#endif
#endif

{- ORMOLU_ENABLE -}
