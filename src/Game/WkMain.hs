module Game.WkMain (runWaka) where

import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import SDL.Font (Font)
import MyData (textFileName)
import MyFile (fileRead)
import Game.WkData (Waka(..),Rect,Mgn,initWaka)
import Game.WkEvent (startText)
import Game.WkVideo (withVideo)
import Game.WkLoop (wkLoop)

type FileNum = Int

runWaka :: MonadIO m => FileNum -> Text -> [Font] -> m () 
runWaka fln sIndex fonts = do 
  allText <- fileRead (textFileName++show fln++".txt")
  withVideo $ \renderer -> do
    let newWaka = startText sIndex allText initWaka
    S.runStateT (wkLoop renderer fonts) newWaka
