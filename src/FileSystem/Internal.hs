-- | @since 0.1
module FileSystem.Internal
  ( -- * Encodings
    utfEncodings,
    utfEncodingsLenient,

    -- * Error messages
    encodeFailure,
    decodeFailure,
  )
where

import GHC.IO.Encoding.Failure (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Encoding.UTF16 qualified as UTF16
import GHC.IO.Encoding.UTF8 qualified as UTF8
import System.IO (TextEncoding)
import System.IO qualified as IO
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

decodeFailure :: String -> String -> String -> String -> String -> String
decodeFailure tyName strName fnName p msg =
  mconcat
    [ "[FileSystem.",
      tyName,
      ".",
      fnName,
      "]: Could not decode ",
      tyName,
      " '",
      p,
      "' to ",
      strName,
      ": '",
      msg,
      "'"
    ]

encodeFailure :: String -> String -> String -> String -> String -> String
encodeFailure tyName strName fnName fp msg =
  mconcat
    [ "[FileSystem.",
      tyName,
      ".",
      fnName,
      "]: Could not encode ",
      strName,
      " ",
      fp,
      "' to ",
      tyName,
      ": '",
      msg,
      "'"
    ]
