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
import MyData (State(..),Attr(..),Dot,Jump,delayTime,textFileName,textPosFile,dotFileName,jumpNameFile)
import MyEvent (inputEvent)
import MyFile (fileRead,fileWrite)
import MySDL.MyLoad (textToDots)

myLoop :: IORef State -> Renderer -> [Font] -> [Texture] -> IO ()
myLoop state re fonts itexs = do
  st <- get state
  (st',isKeyPressed,isMousePressed,isNewFile,isLoadFile,isJump,isJBak,isQuit) <- inputEvent st
  let isUpdateTps = tps st /= tps st'
      nicr = isUpdateTps || icr st'
      ncrc = if isUpdateTps then 0 else crc st'
      nst = beforeDraw st'{crc=ncrc,icr=nicr}
      isUpdateText = tex st /= tex nst || icr st /= icr nst || isUpdateTps 
                                       || isKeyPressed || iup nst 
      isUpdateDraw = isMousePressed || isUpdateText
      isOnlyMouse = isMousePressed && not isUpdateText
      textData = if isUpdateDraw then makeTextData nst else []
      getAtr d = let (_,_,gatr,_) = last d in gatr
      natr = if null textData then atr nst else getAtr textData
      nscr = if isNewFile || isLoadFile || isJump then V2 0 0 else scr natr
      (njps,nfjp,jbkAt,nsjn,fpsSt) = (jps natr, fjp natr, jbk natr, sjn natr, fps nst)
  when isUpdateDraw $ myDraw re fonts itexs textData isOnlyMouse (beforeDraw nst)
  (ntex,nfps,ntps,ndts,niup,njbk) <- if isNewFile then do
    fileWriteR fpsSt nst
    nextFileNum <- nextNewFileNum (fpsSt + 1)
    return (T.empty,nextFileNum,0,[],True,jbkAt)
                           else if isLoadFile then do
    fileWriteR fpsSt nst
    loadFileNum <- loadExistFileNum (fpsSt + 1)
    (loadText, dots) <- fileReadR loadFileNum
    return (loadText,loadFileNum,0,dots,True,jbkAt)
                           else if isJump then do
    let (loadFileNum,textPos) = snd$nfjp!!nsjn
    fileWriteR fpsSt nst
    (loadText, dots) <- fileReadR loadFileNum
    let jb = jbkAt ++ [(fpsSt,tps nst)]
    return (loadText,loadFileNum,textPos,dots,True,jb)
                           else if isJBak then do
    let canJBack = not (null jbkAt)
        (loadFileNum,textPos) = if canJBack then last jbkAt else (fpsSt,tps nst)
        nextFileName = textFileName++show loadFileNum++".txt"
        nextDotFile = dotFileName++show loadFileNum++".txt"
    loadText <- if canJBack then fileRead nextFileName else return (tex nst)
    loadDotText <- if canJBack then fileRead nextDotFile else return T.empty 
    let dots = if canJBack then textToDots (T.words loadDotText) else dts nst
    return (loadText,loadFileNum,textPos,dots,True,init jbkAt)
                           else
    return (tex nst,fpsSt,tps nst,dts nst,False,jbkAt)
  state $= afterDraw nst{tex=ntex,dts=ndts,atr=(atr nst){scr=nscr,jps=njps,fjp=nfjp,jbk=njbk,sjn=nsjn},fps=nfps,tps=ntps,iup=niup}
  delay delayTime
  when isQuit $ do 
    fileWriteR fpsSt nst
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

dotsToText :: [Dot] -> T.Text
dotsToText dots = T.unwords$foldl (\acc (V2 x y,c) -> acc++[T.pack$show x,T.pack$show y,T.pack$show c]) [] dots

jumpsToText :: [Jump] -> T.Text
jumpsToText jmps = T.unwords$foldl (\acc ((fln,fnm),(tgp,tgn)) -> acc++[T.pack$show fln,fnm,T.pack$show tgp,tgn]) [] jmps

fileWriteR :: Int -> State -> IO ()
fileWriteR fp st = do
    fileWrite (textFileName++show fp++".txt") (tex st)
    fileWrite (dotFileName++show fp++".txt") (dotsToText$dts st)

fileReadR :: Int -> IO (T.Text, [Dot])
fileReadR lfn = do
    let nextFileName = textFileName++show lfn++".txt"
        nextDotFile = dotFileName++show lfn++".txt"
    loadText <- fileRead nextFileName  
    loadDotText <- fileRead nextDotFile
    let dots = textToDots (T.words loadDotText)
    return (loadText, dots)
