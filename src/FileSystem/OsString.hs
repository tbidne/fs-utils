{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides utilities for working with 'OsString'.
--
-- @since 0.1
module FileSystem.OsString
  ( -- * Types
    OsString,

    -- * Encoding

    -- ** Total
    osstr,
    osstrPathSep,
    encode,
    encodeLenient,

    -- ** Partial
    encodeThrowM,
    encodeFail,
    unsafeEncode,

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

    -- * Errors
    EncodingException (..),

    -- * Tildes
    toTildePrefixState,
    TildePrefixState (..),
    TildeException (..),
    containsTildePrefix,

    -- * Functions
    OsStr.length,

    -- * Normalization
    normalize,
    glyphLength,
  )
where

import Control.Category ((>>>))
import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Maybe (isJust)
import Data.Text qualified as T
import FileSystem.Internal (TildePrefixes)
import FileSystem.Internal qualified as Internal
import FileSystem.UTF8 qualified as UTF8
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
import System.OsString (OsString, osstr)
import System.OsString qualified as OsStr
import System.OsString.Encoding (EncodingException (EncodingError))

-- NOTE: -Wno-redundant-constraints is because the HasCallStack is redundant
-- on some of these functions when the exceptions library is too old.
-- Disabling the warning is easier than trying to get it right with cpp.

-- | Like 'osstr', except it runs paths through a "replace function" first.
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
--   [osstr|path|] '</>' [osstr|to|] '</>' [osstr|foo|]
-- @
--
-- This can be quite cumbersome for long paths, so we provide this alternative,
-- allowing:
--
-- @
--   [osstrPathSep|path\/to\/foo]
-- @
--
-- Which will automatically convert slashes.
osstrPathSep :: QuasiQuoter
osstrPathSep =
  QuasiQuoter
    { quoteExp = osstr.quoteExp . Internal.replaceSlashes,
      quotePat = osstr.quotePat . Internal.replaceSlashes,
      quoteType = osstr.quoteType . Internal.replaceSlashes,
      quoteDec = osstr.quoteDec . Internal.replaceSlashes
    }

-- | Encodes a 'String' to an 'OsString'. This is a pure version of String's
-- 'OsStr.encodeUtf' that returns the 'EncodingException' in the event of an
-- error.
--
-- @since 0.1
encode :: String -> Either EncodingException OsString
encode = OsStr.encodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = Internal.utfEncodings

-- | Total conversion from 'String' to 'OsString', replacing encode failures
-- with the closest visual match.
--
-- @since 0.1
encodeLenient :: String -> OsString
encodeLenient = elimEx . OsStr.encodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = Internal.utfEncodingsLenient

-- | 'encode' that throws 'EncodingException'.
--
-- @since 0.1
encodeThrowM :: (HasCallStack, MonadThrow m) => String -> m OsString
encodeThrowM =
  encode >>> \case
    Right txt -> pure txt
    Left ex -> throwM ex
{-# INLINEABLE encodeThrowM #-}

-- | 'encodeThrowM' with 'MonadFail'.
--
-- @since 0.1
encodeFail :: (HasCallStack, MonadFail m) => String -> m OsString
encodeFail fp = case encode fp of
  Right txt -> pure txt
  Left ex -> fail (displayException ex)
{-# INLINEABLE encodeFail #-}

-- | Unsafely converts a 'String' to 'OsString' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeEncode :: (HasCallStack) => String -> OsString
unsafeEncode fp = case encode fp of
  Left ex -> error (displayException ex)
  Right p -> p

-- | Decodes an 'OsString' to a 'String'. This is a pure version of String's
-- 'OsStr.decodeUtf'.
--
-- @since 0.1
decode :: OsString -> Either EncodingException String
decode = OsStr.decodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = Internal.utfEncodings

-- | Total conversion from 'OsString' to 'String', replacing decode failures
-- with the closest visual match.
--
-- @since 0.1
decodeLenient :: OsString -> String
decodeLenient = elimEx . OsStr.decodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = Internal.utfEncodingsLenient

-- | 'decode' that throws 'EncodingException'.
--
-- @since 0.1
decodeThrowM :: (HasCallStack, MonadThrow m) => OsString -> m String
decodeThrowM =
  decode >>> \case
    Right txt -> pure txt
    Left ex -> throwM ex
{-# INLINEABLE decodeThrowM #-}

-- | 'decode' with 'MonadFail'.
--
-- @since 0.1
decodeFail :: (HasCallStack, MonadFail m) => OsString -> m String
decodeFail p = case decode p of
  Right txt -> pure txt
  Left ex -> fail (displayException ex)
{-# INLINEABLE decodeFail #-}

-- | Total conversion from 'OsString' to 'String'. If decoding fails, displays
-- the exception.
--
-- @since 0.1
decodeDisplayEx :: OsString -> String
decodeDisplayEx p = case decode p of
  Left ex -> (displayException ex)
  Right s -> s

-- | Total conversion from 'OsString' to 'String'. If decoding fails, falls back
-- to its 'Show' instance.
--
-- @since 0.1
decodeShow :: OsString -> String
decodeShow p = case decode p of
  Left _ -> show p
  Right s -> s

-- | Unsafely converts an 'OsString' to a 'String' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeDecode :: (HasCallStack) => OsString -> String
unsafeDecode p = case decode p of
  Left ex -> error (displayException ex)
  Right fp -> fp

-- | Exception for a path containing a tilde.
newtype TildeException = MkTildeException OsString
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
    "Unexpected tilde in OsString: " <> decodeLenient p

-- | Represents the "tilde state" for a given path.
data TildePrefixState
  = -- | The path contained no tilde prefix.
    TildePrefixStateNone OsString
  | -- | The path contained "tilde prefix(es)" e.g. @~/@ or @~\\ (windows only)@,
    -- which have been stripped. Note that the returned 'OsString' can be empty.
    TildePrefixStateStripped OsString
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
toTildePrefixState :: OsString -> TildePrefixState
toTildePrefixState p =
  case stripTildePrefixes p of
    -- No leading tilde; check original string.
    Nothing -> TildePrefixStateNone p
    -- Leading tilde; check stripped.
    Just p' -> TildePrefixStateStripped p'

-- | Returns true iff the string has a tilde prefix.
--
-- @since 0.1
containsTildePrefix :: OsString -> Bool
containsTildePrefix = isJust . stripTildePrefixes

-- | Strip tilde prefix of path @p@, returning @Just p'@ if @p@ was stripped.
-- On unix, strips @~/@. On windows, attempts to strip the same @~/@.
-- If that was unsuccessful, then attempts @~\\@.
--
-- The singular character @~@ is returned as the empty string, on both
-- platforms.
--
-- @since 0.1
stripTildePrefixes :: OsString -> Maybe OsString
stripTildePrefixes = Internal.stripTildePrefixes tildePrefixes

tildePrefixes :: TildePrefixes
tildePrefixes = ([osstr|~/|], [osstr|~\|])

-- | Returns the number of "visual characters" i.e. glyphs. This is done by
-- performing unicode normalization then taking the 'Text' length.
-- Note that this is /not/ the same as 'OsStr.length'.
--
-- @since 0.1
glyphLength :: OsString -> Int
glyphLength =
  UTF8.glyphLength
    . T.pack
    . decodeLenient

-- | Performs canonical unicode decomposition/composition. Converts to/from
-- 'Text' via lenient encodings.
--
-- @since 0.1
normalize :: OsString -> OsString
normalize =
  encodeLenient
    . T.unpack
    . UTF8.normalizeC
    . T.pack
    . decodeLenient
