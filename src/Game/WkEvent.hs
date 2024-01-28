{-# LANGUAGE OverloadedStrings #-}
module Game.WkEvent (exeEvent) where

import Data.Char (isDigit)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Linear.V2 (V2(..))
import qualified Control.Monad.State.Strict as S
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Game.WkData (Waka(..))

type StateW m = S.StateT Waka m ()

exeEvent :: (MonadIO m) => T.Text -> StateW m 
exeEvent etx = do
  let etx'
        | etx==T.empty = T.empty
        | T.last etx=='\n' = T.init etx 
        | otherwise = etx
      etxs = T.split (==' ') etx'
  mapM_ exeOneEvent etxs

exeOneEvent :: (MonadIO m) => T.Text -> StateW m 
exeOneEvent evt = do
  let en_ags = T.split (=='_') evt
  let (en:ags) = if null en_ags then ("null":[]) else en_ags
  unless (null ags) $ case en of
    "md" -> changeMode ((head . T.unpack . head) ags) 
    "mv" -> moveDialog (head ags)
    "mp" -> setMap (head ags)
    _    -> return ()
    
changeMode :: (MonadIO m) => Char -> StateW m
changeMode ch = S.get >>= (\wk -> return wk{tmd=if isDigit ch then read [ch] else 1}) >>= S.put 

setMap :: (MonadIO m) => T.Text -> StateW m
setMap indf = do
  wk <- S.get
  let (setWk,gmpWk) = (set wk,gmp wk)
      mData = fromMaybe T.empty (lookup ("map" <> indf) setWk) 
      ngmp = if mData==T.empty then gmpWk else (lines . T.unpack) mData
      nwk = wk{gmp=ngmp}
  S.put nwk

moveDialog :: (MonadIO m) => T.Text -> StateW m
moveDialog ind = do
  wk <- S.get
  let (setWk,texWk,tpsWk) = (set wk,tex wk,tps wk)
  let lu = lookup ind setWk 
  let ntex = case lu of Nothing -> texWk; Just x -> x
  let ntps = case lu of Nothing -> tpsWk; Just _ -> 0
  let nwk = wk{tex=ntex,stx=T.empty,tps=ntps,scr=V2 0 0}
  S.put nwk

