{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides utilities for working with 'OsPath'.
--
-- @since 0.1
module FileSystem.OsPath
  ( -- * Types
    OsPath,

    -- * Encoding

    -- ** Total
    encode,
    encodeLenient,

    -- ** Partial
    encodeThrowM,
    encodeFail,
    unsafeEncode,

    -- * Encoding + Validation

    -- ** Total
    osp,
    ospPathSep,
    encodeValid,
    encodeValidLenient,

    -- ** Partial
    encodeValidThrowM,
    encodeValidFail,
    unsafeEncodeValid,

    -- * Decoding

    -- ** Total
    decode,
    decodeLenient,
    decodeDisplayEx,
    decodeShow,

    -- ** Partial
    decodeThrowM,
    decodeFail,
    unsafeDecode,

    -- * OsString
    toOsString,
    fromOsString,
    fromOsStringThrowM,
    fromOsStringFail,
    unsafeFromOsString,
    reallyUnsafeFromOsString,

    -- * Functions
    (</>),
    (<.>),
    (-<.>),

    -- * Legacy
    (</>!),
    (!</>),
    combineFilePaths,

    -- * Errors
    EncodingException (..),

    -- * Tildes
    toTildePrefixState,
    TildePrefixState (..),
    OsPathOrEmpty (..),
    TildeException (..),
    containsTildePrefix,
  )
where

import Control.Category ((>>>))
import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Maybe (isJust)
import FileSystem.Internal (TildePrefixes)
import FileSystem.Internal qualified as Internal
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Quote
  ( QuasiQuoter
      ( QuasiQuoter,
        quoteDec,
        quoteExp,
        quotePat,
        quoteType
      ),
  )
import System.FilePath qualified as FP
import System.OsPath (OsPath, osp, (-<.>), (<.>), (</>))
import System.OsPath qualified as OsP
import System.OsPath.Encoding (EncodingException (EncodingError))
import System.OsString (OsString)

-- NOTE: -Wno-redundant-constraints is because the HasCallStack is redundant
-- on some of these functions when the exceptions library is too old.
-- Disabling the warning is easier than trying to get it right with cpp.

-- | Like 'osp', except it runs paths through a "replace function" first.
-- On unix, replaces @\\@ with @/@. On windows, does the opposite.
--
-- This is convenient for writing paths in a platform-agnostic way i.e. we
-- are expecting a path
--
-- @
--   "path\/to\/foo" -- unix
--   "path\\to\\foo" -- windows
-- @
--
-- The normal way to handle this would be to use the combine function '(</>)'
-- i.e.
--
-- @
--   [osp|path|] '</>' [osp|to|] '</>' [osp|foo|]
-- @
--
-- This can be quite cumbersome for long paths, so we provide this alternative,
-- allowing:
--
-- @
--   [ospPathSep|path\/to\/foo]
-- @
--
-- Which will automatically convert slashes.
ospPathSep :: QuasiQuoter
ospPathSep =
  QuasiQuoter
    { quoteExp = osp.quoteExp . Internal.replaceSlashes,
      quotePat = osp.quotePat . Internal.replaceSlashes,
      quoteType = osp.quoteType . Internal.replaceSlashes,
      quoteDec = osp.quoteDec . Internal.replaceSlashes
    }

-- | Encodes a 'FilePath' to an 'OsPath'. This is a pure version of filepath's
-- 'OsP.encodeUtf' that returns the 'EncodingException' in the event of an
-- error.
--
-- @since 0.1
encode :: FilePath -> Either EncodingException OsPath
encode = OsP.encodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = Internal.utfEncodings

-- | Total conversion from 'FilePath' to 'OsPath', replacing encode failures
-- with the closest visual match.
--
-- @since 0.1
encodeLenient :: FilePath -> OsPath
encodeLenient = elimEx . OsP.encodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = Internal.utfEncodingsLenient

-- | 'encode' that __also__ checks 'OsP.isValid' i.e. 'encode'
-- only succeeds if the 'FilePath' can be encoded /and/ passes expected
-- invariants.
--
-- @since 0.1
encodeValid :: FilePath -> Either EncodingException OsPath
encodeValid fp = case encode fp of
  Left ex -> Left ex
  Right op ->
    if OsP.isValid op
      then Right op
      else Left $ EncodingError (validFpErr fp op) Nothing

-- | Total conversion from 'FilePath' to 'OsPath', replacing encode failures
-- with the closest visual match. If the result is not valid, makes it valid.
--
-- @since 0.1
encodeValidLenient :: FilePath -> OsPath
encodeValidLenient = OsP.makeValid . encodeLenient

-- | 'encode' that throws 'EncodingException'.
--
-- @since 0.1
encodeThrowM :: (HasCallStack, MonadThrow m) => FilePath -> m OsPath
encodeThrowM =
  encode >>> \case
    Right txt -> pure txt
    Left ex -> throwM ex
{-# INLINEABLE encodeThrowM #-}

-- | 'encodeValid' that throws 'EncodingException'.
--
-- @since 0.1
encodeValidThrowM :: (HasCallStack, MonadThrow m) => FilePath -> m OsPath
encodeValidThrowM =
  encodeValid >>> \case
    Left ex -> throwM ex
    Right op -> pure op
{-# INLINEABLE encodeValidThrowM #-}

-- | 'encodeThrowM' with 'MonadFail'.
--
-- @since 0.1
encodeFail :: (HasCallStack, MonadFail m) => FilePath -> m OsPath
encodeFail fp = case encode fp of
  Right txt -> pure txt
  Left ex -> fail (displayException ex)
{-# INLINEABLE encodeFail #-}

-- | 'encodeValid' with 'MonadFail'.
--
-- @since 0.1
encodeValidFail :: (HasCallStack, MonadFail m) => FilePath -> m OsPath
encodeValidFail fp = case encodeValid fp of
  Left ex -> fail (displayException ex)
  Right op -> pure op
{-# INLINEABLE encodeValidFail #-}

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeEncode :: (HasCallStack) => FilePath -> OsPath
unsafeEncode fp = case encode fp of
  Left ex -> error (displayException ex)
  Right p -> p

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeEncodeValid :: (HasCallStack) => FilePath -> OsPath
unsafeEncodeValid fp = case encodeValid fp of
  Left ex -> error (displayException ex)
  Right op -> op

-- | Decodes an 'OsPath' to a 'FilePath'. This is a pure version of filepath's
-- 'OsP.decodeUtf'.
--
-- @since 0.1
decode :: OsPath -> Either EncodingException FilePath
decode = OsP.decodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = Internal.utfEncodings

-- | Total conversion from 'OsPath' to 'FilePath', replacing decode failures
-- with the closest visual match.
--
-- @since 0.1
decodeLenient :: OsPath -> FilePath
decodeLenient = elimEx . OsP.decodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = Internal.utfEncodingsLenient

-- | 'decode' that throws 'EncodingException'.
--
-- @since 0.1
decodeThrowM :: (HasCallStack, MonadThrow m) => OsPath -> m FilePath
decodeThrowM =
  decode >>> \case
    Right txt -> pure txt
    Left ex -> throwM ex
{-# INLINEABLE decodeThrowM #-}

-- | 'decode' with 'MonadFail'.
--
-- @since 0.1
decodeFail :: (HasCallStack, MonadFail m) => OsPath -> m FilePath
decodeFail p = case decode p of
  Right txt -> pure txt
  Left ex -> fail (displayException ex)
{-# INLINEABLE decodeFail #-}

-- | Total conversion from 'OsPath' to 'String'. If decoding fails, displays
-- the exception.
--
-- @since 0.1
decodeDisplayEx :: OsPath -> String
decodeDisplayEx p = case decode p of
  Left ex -> displayException ex
  Right s -> s

-- | Total conversion from 'OsPath' to 'String'. If decoding fails, falls back
-- to its 'Show' instance.
--
-- @since 0.1
decodeShow :: OsPath -> String
decodeShow p = case decode p of
  Left _ -> show p
  Right s -> s

-- | Unsafely converts an 'OsPath' to a 'FilePath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeDecode :: (HasCallStack) => OsPath -> FilePath
unsafeDecode p = case decode p of
  Left ex -> error (displayException ex)
  Right fp -> fp

validFpErr :: String -> OsPath -> String
validFpErr fp x =
  mconcat
    [ "FilePath '",
      fp,
      "' encoded as OsPath '",
      decodeLenient x,
      "' failed isValid"
    ]

validOsStrErr :: OsString -> String
validOsStrErr str =
  mconcat
    [ "OsString '",
      decodeLenient str,
      "' failed isValid"
    ]

-- | Unsafely combines an 'OsPath' and a 'FilePath' via (</>) with
-- 'unsafeEncode'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(</>!) :: (HasCallStack) => OsPath -> FilePath -> OsPath
p </>! fp = p </> unsafeEncode fp

infixl 9 </>!

-- | Unsafely combines a 'FilePath' and an 'OsPath' via (</>) with
-- 'unsafeEncode'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(!</>) :: (HasCallStack) => FilePath -> OsPath -> OsPath
(!</>) = flip (</>!)

infixl 9 !</>

-- | Legacy alias for FilePaths' </> operator. Exists because the </> exported
-- here is @'(</>)' :: 'OsPath' -> 'OsPath' -> 'OsPath'@.
--
-- @since 0.1
combineFilePaths :: FilePath -> FilePath -> FilePath
combineFilePaths = (FP.</>)

-- | Exception for a path containing a tilde.
newtype TildeException = MkTildeException OsPath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception TildeException where
  displayException (MkTildeException p) =
    "Unexpected tilde in OsPath: " <> decodeLenient p

-- | Represents the "tilde state" for a given path.
data TildePrefixState
  = -- | The path contained no tilde prefix.
    TildePrefixStateNone OsPath
  | -- | The path contained "tilde prefix(es)" e.g. @~/@ or @~\\ (windows only)@,
    -- which have been stripped. The result can be empty, however.
    TildePrefixStateStripped OsPathOrEmpty
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | Retrieves the path's "tilde state". Strips consecutive "tilde prefixes"
-- if they exist. If the string contains no prefixes, returns it unchanged.
--
-- @since 0.1
toTildePrefixState :: OsPath -> TildePrefixState
toTildePrefixState p =
  case stripTildePrefixes p of
    -- No leading tilde; check original string.
    Nothing -> TildePrefixStateNone p
    -- Leading tilde produced empty string; fine, nothing else to check.
    Just OsPathEmpty -> TildePrefixStateStripped OsPathEmpty
    -- Leading tilde w/ non-empty stripped; check stripped.
    Just (OsPathNonEmpty p') -> (TildePrefixStateStripped . OsPathNonEmpty) p'

-- | Sum type representing a possible empty OsPath. The 'OsPath' type _can_
-- be empty, hence 'OsPathNonEmpty' should only be used when the 'OsPath'
-- has been verified first. This type really exists to make it clear when
-- the empty distinction matters.
--
-- @since 0.1
data OsPathOrEmpty
  = -- | OsPath is empty.
    --
    -- @since 0.1
    OsPathEmpty
  | -- | OsPath is non-empty.
    --
    -- @since 0.1
    OsPathNonEmpty OsPath
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | Returns true iff the string has a tilde prefix.
--
-- @since 0.1
containsTildePrefix :: OsPath -> Bool
containsTildePrefix = isJust . stripTildePrefixes

-- | Strip tilde prefix of path @p@, returning @Just p'@ if @p@ was stripped.
-- On unix, strips @~/@. On windows, attempts to strip the same @~/@.
-- If that was unsuccessful, then attempts @~\\@.
--
-- Note that this can return an empty OsPath if the parameter is one of
-- @"~/"@, @"~"@, or @"~\\"@ (windows only).
--
-- @since 0.1
stripTildePrefixes :: OsPath -> Maybe OsPathOrEmpty
stripTildePrefixes =
  fmap toStripped
    . Internal.stripTildePrefixes tildePrefixes
    . toOsString
  where
    -- NOTE: This is predicated on the belief that stripping a prefix does not
    -- change the validity. In particular, stripping a prefix cannot change a
    -- valid path to an invalid path. There is one exception to this: A path
    -- that _only_ contains the tilde prefix e.g. "~/". This will produce an
    -- empty string which is _not_ valid.
    --
    -- We want to allow this, however, because downstream functions will want
    -- to turn an empty path into the plain home directory. Hence we should
    -- __not__ check validity here, which is okay as long as this is the only
    -- instance of "introduced invalidity".
    --
    -- We should strongly signal that the result can be empty, hence OsPathOrEmpty.
    toStripped :: OsString -> OsPathOrEmpty
    toStripped s =
      if s == mempty
        then OsPathEmpty
        else OsPathNonEmpty (reallyUnsafeFromOsString s)

tildePrefixes :: TildePrefixes
tildePrefixes = (toOsString [osp|~/|], toOsString [osp|~\|])

-- | Convert an 'OsPath' to 'OsString'. This is currently the identity
-- function.
--
-- @since 0.1
toOsString :: OsPath -> OsString
toOsString = id

-- | Convert an 'OsString' to 'OsPath'. Currently this merely checks
-- 'OsPath.isValid'.
--
-- @since 0.1
fromOsString :: OsString -> Either EncodingException OsPath
fromOsString s =
  if OsP.isValid s
    then Right s
    else Left (EncodingError (validOsStrErr s) Nothing)

-- | 'fromOsString' that throws the 'EncodingException'.
--
-- @since 0.1
fromOsStringThrowM :: (HasCallStack, MonadThrow m) => OsString -> m OsPath
fromOsStringThrowM =
  fromOsString >>> \case
    Left ex -> throwM ex
    Right p -> pure p

-- | 'fromOsString' for 'MonadFail'.
--
-- @since 0.1
fromOsStringFail :: (HasCallStack, MonadFail m) => OsString -> m OsPath
fromOsStringFail s = case fromOsString s of
  Left ex -> fail $ displayException ex
  Right p -> pure p

-- | Unsafely checks an 'OsString' for validity, dying with 'error' on
-- failure.
--
-- @since 0.1
unsafeFromOsString :: (HasCallStack) => OsString -> OsPath
unsafeFromOsString s = case fromOsString s of
  Left ex -> error $ displayException ex
  Right p -> p

-- | "Converts" from 'OsString' to 'OsPath' without checking any invariants.
-- Used for when we know an operator cannot have
--
-- @since 0.1
reallyUnsafeFromOsString :: OsString -> OsPath
reallyUnsafeFromOsString = id
