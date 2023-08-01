module MySDL.MyLoop (myLoop) where

import Control.Monad (unless,when)
import Data.IORef (IORef)
import SDL (get, ($=))
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time (delay)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import MyAction (beforeDraw,afterDraw,makeTextData)
import MySDL.MyDraw (myDraw)
import MyData (State(..),Attr(..),delayTime,textFileName,textPosFile)
import MyEvent (inputEvent)
import MyFile (fileRead,fileWrite)

myLoop :: IORef State -> Renderer -> [Font] -> [Texture] -> IO ()
myLoop state re fonts itexs = do
  st <- get state
  (st',isKeyPressed,isMousePressed,isNewFile,isLoadFile,isQuit) <- inputEvent st
  let isUpdateTps = tps st /= tps st'
      nicr = (tps st /= tps st') || icr st'
      ncrc = if isUpdateTps then 0 else crc st'
      nst = beforeDraw st'{crc=ncrc,icr=nicr}
      isUpdateDraw = tex st /= tex nst || icr st /= icr nst || isUpdateTps 
                                       || isKeyPressed || isMousePressed
                                       || isNewFile || isLoadFile
      textData = makeTextData nst
      getAtr d = let (_,_,gatr,_) = last d in gatr
      natr = if null textData then atr nst else getAtr textData
      nscr = scr natr
  when isUpdateDraw $ myDraw re fonts itexs textData (beforeDraw nst)
  (ntex,nfps,ntps) <- if isNewFile then do
    fileWrite (textFileName++show (fps nst)++".txt") (tex nst)
    nextFileNum <- nextNewFileNum (fps nst + 1)
    return (T.empty,nextFileNum,0)
                                   else
                      if isLoadFile then do
    fileWrite (textFileName++show (fps nst)++".txt") (tex nst)
    loadFileNum <- loadExistFileNum (fps nst + 1)
    let nextFileName = textFileName++show loadFileNum++".txt"
    loadText <- fileRead nextFileName  
    return (loadText,loadFileNum,0)
                                    else
    return (tex nst,fps nst,tps nst)
  state $= afterDraw nst{tex=ntex,atr=(atr nst){scr=nscr},fps=nfps,tps=ntps}
  delay delayTime
  when isQuit $ do 
    fileWrite (textFileName++show (fps nst)++".txt") (tex nst)
    fileWrite textPosFile (T.pack$unwords [show (fps nst),show (tps nst)])
  unless isQuit (myLoop state re fonts itexs)

nextNewFileNum :: Int -> IO Int
nextNewFileNum i = do 
  let fileName = textFileName++show i++".txt" 
  fileExist <- doesFileExist fileName 
  if fileExist then nextNewFileNum (i+1) else return i

loadExistFileNum :: Int -> IO Int
loadExistFileNum i = do
  let fileName = textFileName++show i++".txt"
  fileExist <- doesFileExist fileName
  if fileExist then return i else loadExistFileNum 0
