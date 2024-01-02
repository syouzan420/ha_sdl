module Game.WkEvent (wkInput,startText) where

import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import SDL.Input.Keyboard.Codes
import MySDL.MyInput (myInput)
import Data.Maybe (fromMaybe)
import Game.WkData (Waka(..),Input(..))

wkInput :: (MonadIO m) => m Input
wkInput = do
    (kc,_,_,_,_,_) <- myInput
    return $ case kc of
      KeycodeEscape -> Es
      KeycodeK -> Up
      KeycodeUp -> Up
      KeycodeJ -> Dn
      KeycodeDown -> Dn
      KeycodeH -> Lf
      KeycodeLeft -> Lf
      KeycodeL -> Ri
      KeycodeRight -> Ri
      KeycodeReturn -> Rt
      _ -> No

startText :: Text -> Text -> Waka -> Waka 
startText sIndex allText wk = do
  let lns = T.lines allText 
      tset = makeIndex lns
      iText = if null tset then allText 
                           else fromMaybe (fst (head tset)) (lookup sIndex tset)
   in wk{set=tset,tex=iText}
      

makeIndex :: [Text] -> [(Text,Text)]  
makeIndex [] = []
makeIndex (tx:txs) =
 let (ind,ch) = fromMaybe (T.empty,'0') (T.unsnoc tx) 
  in if ch==':' then let (text,xs) = getText txs [] in (ind,text):makeIndex xs 
                else makeIndex txs

getText :: [Text] -> [Text] -> (Text,[Text])
getText [] acc = (T.unlines acc, [])
getText (tx:txs) acc =
  let (ind,ch) = fromMaybe (T.empty,'0') (T.unsnoc tx)
   in if ch==':' then (T.unlines acc,tx:txs) 
                 else getText txs (acc++[tx]) 
