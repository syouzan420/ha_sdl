module MyFile(fileRead,fileWrite) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import Data.Functor((<&>))

fileRead :: FilePath -> IO T.Text 
fileRead fileName = B.readFile fileName <&> decodeUtf8 

fileWrite :: FilePath -> T.Text -> IO ()
fileWrite fileName txt = B.writeFile fileName (encodeUtf8 txt)

