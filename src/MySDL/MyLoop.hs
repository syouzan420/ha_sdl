module MySDL.MyLoop (myLoop) where

import Control.Monad (unless)
import Data.IORef (IORef)
import SDL (get, ($=))
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time (delay)
import MyAction (myAction,beforeDraw,afterDraw)
import MySDL.MyDraw (myDraw)
import Data.Text (Text)
import MyData (State(..),delayTime)
import MyEvent (inputEvent)

myLoop :: IORef State -> Renderer -> [Font] -> [Texture] -> IO ()
myLoop state re fonts itexs = do
  st <- get state
  (nst,isQuit) <- inputEvent st
  myDraw re fonts itexs (beforeDraw nst) 
  state $= afterDraw nst 
  delay delayTime
  unless isQuit (myLoop state re fonts itexs)
