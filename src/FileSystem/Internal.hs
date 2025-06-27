{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | @since 0.1
module FileSystem.Internal
  ( -- * Encodings
    utfEncodings,
    utfEncodingsLenient,

    -- * Tildes
    TildePrefixes,
    stripTildePrefixes,
    containsTilde,

    -- * Misc
    replaceSlashes,
  )
where

import Control.Applicative (Alternative ((<|>)))
import GHC.IO.Encoding.Failure (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Encoding.UTF16 qualified as UTF16
import GHC.IO.Encoding.UTF8 qualified as UTF8
import System.IO (TextEncoding)
import System.IO qualified as IO
import System.OsString (OsString, osstr)
import System.OsString qualified as OsStr
import System.OsString.Encoding (EncodingException)

-- | (UTF8, UTF16LE) encoders.
utfEncodings :: (TextEncoding, TextEncoding)
-- NOTE: [Unix/Windows encodings]
--
-- utf8/utf16le encodings are taken from os-string's encodeUtf implementation.
utfEncodings = (IO.utf8, IO.utf16le)

-- | Like 'utfEncodings' except the encodings are total. We also provide an
-- eliminator for @EncodingException -> a@ (lifted to Either for convenience),
-- because such an @EncodingException@ should be impossible, but the general
-- encode/decode framework returns Either, so we need to handle the impossible
-- Left.
utfEncodingsLenient ::
  ( TextEncoding,
    TextEncoding,
    Either EncodingException a -> a
  )
utfEncodingsLenient =
  ( -- see NOTE: [Unix/Windows encodings]
    --
    -- These encoders are like those defined in utfEncodings, except we use
    -- TransliterateCodingFailure instead of ErrorOnCodingFailure i.e.
    --
    --     mkUTF8/mkUTF16 ErrorOnCodingFailure
    --
    -- These should always succeed.
    UTF8.mkUTF8 TransliterateCodingFailure,
    UTF16.mkUTF16le TransliterateCodingFailure,
    elimEx
  )
  where
    elimEx = either (error . show) id

type TildePrefixes = (OsString, OsString)

{- ORMOLU_DISABLE -}

-- | Strip tilde prefix of path @p@, returning @Just p'@ if @p@ was stripped.
-- On posix, strips @~/@. On windows, attempts to strip the same @~/@.
-- If that was unsuccessful, then attempts @~\\@.
--
-- @since 0.1
stripTildePrefixes :: TildePrefixes -> OsString -> Maybe OsString
stripTildePrefixes (posixPrefix, _windowsPrefix) = go
  where
    go :: OsString -> Maybe OsString
    go p =
      -- 1. A lone ~ is a prefix of an empty string.
      if p == [osstr|~|]
        then Just [osstr||]
        -- 2. If the string contains a prefix (~/ or ~\) then strip it, and
        -- recursively try again. Ths goal is to return a string that does
        -- _not_ start with a tilde prefix. Any other tildes are fine.
        else case OsStr.stripPrefix posixPrefix p of
          Just p' -> go p' <|> Just p'
#if WINDOWS
          Nothing -> case OsStr.stripPrefix _windowsPrefix p of
            Just p' -> go p' <|> Just p'
            Nothing -> Nothing
#else
          Nothing -> Nothing
#endif

-- | Determines if the path contains a tilde character.
--
-- @since 0.1
containsTilde :: OsString -> Bool
containsTilde = OsStr.elem (OsStr.unsafeFromChar '~')

replaceSlashes :: FilePath -> FilePath
replaceSlashes = foldr go ""
  where
#if WINDOWS
  go '/' acc = '\\' : acc
#else
  go '\\' acc = '/' : acc
#endif
  go c acc = c : acc

{- ORMOLU_ENABLE -}
