module MySDL.MyLoop (myLoop) where

import Control.Monad (unless,when)
import Data.IORef (IORef)
import SDL (get, ($=))
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time (delay)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import Linear.V2 (V2(..))
import MyAction (beforeDraw,afterDraw,makeTextData)
import MySDL.MyDraw (myDraw)
import MyData (State(..),Attr(..),Dots,Jump,delayTime,textFileName,textPosFile,dotFileName,jumpNameFile)
import MyEvent (inputEvent)
import MyFile (fileRead,fileWrite)
import MySDL.MyLoad (textToDots)

myLoop :: IORef State -> Renderer -> [Font] -> [Texture] -> IO ()
myLoop state re fonts itexs = do
  st <- get state
  (st',isKeyPressed,isMousePressed,isNewFile,isLoadFile,isQuit) <- inputEvent st
  let isUpdateTps = tps st /= tps st'
      nicr = (tps st /= tps st') || icr st'
      ncrc = if isUpdateTps then 0 else crc st'
      nst = beforeDraw st'{crc=ncrc,icr=nicr}
      isUpdateText = tex st /= tex nst || icr st /= icr nst || isUpdateTps 
                                       || isKeyPressed 
                                       || isNewFile || isLoadFile
      isUpdateDraw = isMousePressed || isUpdateText
      isOnlyMouse = isMousePressed && not isUpdateText
      textData = makeTextData nst
      getAtr d = let (_,_,gatr,_) = last d in gatr
      natr = if null textData then atr nst else getAtr textData
      nscr = if isNewFile || isLoadFile then V2 0 0 else scr natr
      njps = jps natr
      nfjp = fjp natr
      nsjn = sjn natr
  when isUpdateDraw $ myDraw re fonts itexs textData isOnlyMouse (beforeDraw nst)
  (ntex,nfps,ntps,ndts) <- if isNewFile then do
    fileWrite (textFileName++show (fps nst)++".txt") (tex nst)
    fileWrite (dotFileName++show (fps nst)++".txt") (dotsToText$dts nst)
    nextFileNum <- nextNewFileNum (fps nst + 1)
    return (T.empty,nextFileNum,0,[])
                                   else
                      if isLoadFile then do
    fileWrite (textFileName++show (fps nst)++".txt") (tex nst)
    fileWrite (dotFileName++show (fps nst)++".txt") (dotsToText$dts nst)
    loadFileNum <- loadExistFileNum (fps nst + 1)
    let nextFileName = textFileName++show loadFileNum++".txt"
        nextDotFile = dotFileName++show loadFileNum++".txt"
    loadText <- fileRead nextFileName  
    loadDotText <- fileRead nextDotFile
    let dots = textToDots (T.words loadDotText)
    return (loadText,loadFileNum,0,dots)
                                    else
    return (tex nst,fps nst,tps nst,dts nst)
  state $= afterDraw nst{tex=ntex,dts=ndts,atr=(atr nst){scr=nscr,jps=njps,fjp=nfjp,sjn=nsjn},fps=nfps,tps=ntps}
  delay delayTime
  when isQuit $ do 
    fileWrite (textFileName++show (fps nst)++".txt") (tex nst)
    fileWrite (dotFileName++show (fps nst)++".txt") (dotsToText$dts nst)
    fileWrite textPosFile (T.pack$unwords [show (fps nst),show (tps nst)])
    fileWrite jumpNameFile (jumpsToText njps)
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

dotsToText :: Dots -> T.Text
dotsToText dots = T.unwords$foldl (\acc (V2 x y,c) -> acc++[T.pack$show x,T.pack$show y,T.pack$show c]) [] dots

jumpsToText :: [Jump] -> T.Text
jumpsToText jmps = T.unwords$foldl (\acc ((fln,fnm),(tgp,tgn)) -> acc++[T.pack$show fln,fnm,T.pack$show tgp,tgn]) [] jmps
