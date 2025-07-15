module Unit.FileSystem.UTF8 (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import FileSystem.UTF8 qualified as UTF8
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "UTF8"
    [ testNormalize
    ]

testNormalize :: TestTree
testNormalize = testCase "Normalizes text" $ do
  2 @=? length s
  2 @=? T.length t
  1 @=? T.length t'

  1 @=? UTF8.glyphLength t
  1 @=? UTF8.glyphLength t'

  "\x4F\x308" @=? s
  "\x4F\x308" @=? t
  "\xD6" @=? t'

  [79, 204, 136] @=? BS.unpack bs
  [195, 150] @=? BS.unpack bs'
  where
    -- s = "Ö"
    s = ['\x4F', '\x308']
    t = T.pack s
    t' = UTF8.normalizeC t

    bs = TEnc.encodeUtf8 t
    bs' = TEnc.encodeUtf8 t'
