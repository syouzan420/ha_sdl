module MyFile(fileRead) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Data.Functor((<&>))

fileRead :: FilePath -> IO T.Text 
fileRead fileName = B.readFile fileName <&> decodeUtf8 

