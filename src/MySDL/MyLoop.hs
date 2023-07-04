module MySDL.MyLoop (myLoop) where

import Control.Monad (unless,when)
import Data.IORef (IORef)
import SDL (get, ($=))
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time (delay)
import MyAction (myAction,beforeDraw,afterDraw)
import MySDL.MyDraw (myDraw)
import Data.Text (Text)
import MyData (State(..),Attr(..),delayTime)
import MyEvent (inputEvent)

myLoop :: IORef State -> Renderer -> [Font] -> [Texture] -> IO ()
myLoop state re fonts itexs = do
  st <- get state
  (st',isKeyPressed,isQuit) <- inputEvent st
  let isUpdateTps = tps st /= tps st'
      nicr = (tps st /= tps st') || icr st'
      ncrc = if isUpdateTps then 0 else crc st'
      nst = beforeDraw st'{crc=ncrc,icr=nicr}
      isUpdateDraw = tex st /= tex nst || icr st /= icr nst || isUpdateTps || isKeyPressed
  when isUpdateDraw $ myDraw re fonts itexs (beforeDraw nst)
  state $= afterDraw nst 
  delay delayTime
  unless isQuit (myLoop state re fonts itexs)
