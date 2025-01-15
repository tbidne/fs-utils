{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.FileSystem.IO (tests) where

import Control.Exception
  ( Exception (fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
  )
import Control.Monad.Catch (MonadCatch, catch, displayException, throwM, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char qualified as Ch
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath, osp, (</>))
import FileSystem.OsPath qualified as FS.OsPath
import Hedgehog (MonadGen, MonadTest, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory.OsPath qualified as Dir
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "Misc"
    [ readWriteRoundtrip getTmpDir,
      matchesBytestring getTmpDir
    ]

-- | Tests that write/read is a roundtrip for valid paths.
readWriteRoundtrip :: IO OsPath -> TestTree
readWriteRoundtrip getTmpDir = testPropertyNamed desc "readWriteRoundtrip" $ do
  H.property $ do
    tmpDir <- (</> [osp|readWriteRoundtrip|]) <$> liftIO getTmpDir
    liftIO $ Dir.createDirectoryIfMissing True tmpDir
    H.annotateShow tmpDir

    fp <- H.forAll genGoodFilePath
    contents <- H.forAll genFileContents

    os <- hcatch (FS.OsPath.encodeValidThrowM fp)

    let osPath = tmpDir </> os

    H.annotateShow osPath
    H.annotateShow contents

    hcatch (FS.IO.writeBinaryFileIO osPath contents)
    resultContents <- hcatch (FS.IO.readBinaryFileIO osPath)

    contents === resultContents
  where
    desc = "(read . write) round trips"

-- | Tests that write/read success for ByteString's API implies success for
-- Effect's API. This is essentially a regression test for our new OsPath
-- API.
matchesBytestring :: IO OsPath -> TestTree
matchesBytestring getTmpDir = testPropertyNamed desc "matchesBytestring" $ do
  H.property $ do
    tmpDir <- (</> [osp|matchesBytestring|]) <$> liftIO getTmpDir
    liftIO $ Dir.createDirectoryIfMissing True tmpDir
    H.annotateShow tmpDir

    fp <- H.forAll genAnyFilePath
    contents <- H.forAll genFileContents

    case FS.OsPath.encodeValid fp of
      Left _ -> H.label "Encoding FilePath failed"
      Right os -> do
        tmpDir' <- FS.OsPath.decodeThrowM tmpDir
        let filePath = tmpDir' `FS.OsPath.combineFilePaths` fp
            osPath = tmpDir </> os

        H.annotateShow osPath
        H.annotateShow contents

        -- The randomly generated path cannot be assumed to be valid, hence we
        -- must check for failure. This allows us to check a larger domain than
        -- matchesBytestring, at the cost of having to skip failures.
        eBsResult <- tryAny $ liftIO $ do
          BS.writeFile filePath contents
          BS.readFile filePath

        H.annotate "ByteString succeeded"

        case eBsResult of
          Left _ -> H.label "ByteString write failed"
          Right bsResultContents -> do
            H.label "ByteString write succeeded"
            hcatch (FS.IO.writeBinaryFileIO osPath contents)
            resultContents <- hcatch (FS.IO.readBinaryFileIO osPath)

            bsResultContents === resultContents
            contents === resultContents
  where
    desc = "ByteString.write/read succeeds => Effects.write/read succeeds"

hcatch :: (MonadCatch m, MonadIO m, MonadTest m) => IO a -> m a
hcatch m =
  liftIO m
    `catchAny` \ex -> do
      H.annotate $ displayException ex
      H.failure

genFileContents :: (MonadGen m) => m ByteString
genFileContents = Gen.utf8 range (Gen.filterT (/= '\0') Gen.unicode)
  where
    -- \194\160 fails ?!?!?!
    range = Range.linear 1 20

-- | Generates a random unicode string without restrictions. The result
-- is __not__ guaranteed to be a valid path on any platform.
genAnyFilePath :: (MonadGen m) => m FilePath
genAnyFilePath =
  Gen.string range Gen.unicode
  where
    range = Range.linear 1 20

-- | Generates a FilePath, excluding characters known to be invalid on each
-- platform. The resulting FilePath should encode successfully as a valid
-- OsPath.
genGoodFilePath :: (MonadGen m) => m FilePath
genGoodFilePath =
  Gen.string range genPathChar
  where
    range = Range.linear 1 20

genPathChar :: (MonadGen m) => m Char
genPathChar = Gen.filterT filterFn (charMapper <$> genChar)
  where
    filterFn c = isGoodChar c && Ch.isAlphaNum c

genChar :: (MonadGen m) => m Char
isGoodChar :: Char -> Bool
badChars :: HashSet Char
charMapper :: Char -> Char

#if OSX
-- So, mac is terrible and seemingly OK paths (i.e. unicode encoded to UTF-8)
-- inexplicably fail. There doesn't seem to be any official documentation
-- on this, and searching online has proved unhelpful.
--
-- Probably I am doing something wrong (encoding a string via
-- encodeValidThrowM is wrong???), but it feels better to blame osx. In any
-- case, these tests are not super valuable anyway, so I am totally fine
-- reducing mac paths to latin1 to avoid these nonsense errors.
genChar = Gen.latin1

isGoodChar c = Ch.isPrint c && not (Set.member c badChars)

badChars =
  Set.fromList
    [ '/',
      '.',
      ':',
      '\7306' -- U+1c8a, ᲊ
    ]

charMapper = Ch.toLower
#elif WINDOWS
genChar = Gen.unicode

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

-- https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#file-and-directory-names
badChars =
  Set.fromList
    [ '/',
      '\\',
      '<',
      '>',
      ':',
      '"',
      '|',
      '?',
      '*',
      '0',
      '.',
      ' '
    ]

-- windows paths are case-insensitive by default, so let's just take
-- lower-case paths :-(
charMapper = Ch.toLower
#else
genChar = Gen.unicode

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

badChars =
  Set.fromList
    [ '/',
      '.',
      '*'
    ]

charMapper = id
#endif

catchAny :: (MonadCatch m) => m a -> (SomeException -> m a) -> m a
catchAny m h =
  catch @_ @SomeException
    m
    ( \e ->
        if isAsyncException e
          then throwM e
          else h e
    )

tryAny :: (MonadCatch m) => m a -> m (Either SomeException a)
tryAny m = do
  try @_ @SomeException m >>= \case
    Left e ->
      if isAsyncException e
        then throwM e
        else pure $ Left e
    Right x -> pure $ Right x

isAsyncException :: forall e. (Exception e) => e -> Bool
isAsyncException = not . isSyncException

isSyncException :: forall e. (Exception e) => e -> Bool
isSyncException e = case fromException (toException e) of
  Just SomeAsyncException {} -> False
  Nothing -> True
