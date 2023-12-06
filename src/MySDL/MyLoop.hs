{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyLoop (myLoop) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Control.Monad.State.Strict as S
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time (delay)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import Linear.V2 (V2(..))
import MySDL.MyDraw (myDraw)
import MyData (State(..),Attr(..),Dot,Input(..),JBak,FrJp,delayTime,textFileName,textPosFile
              ,dotFileName,jumpNameFile)
import MyAction (beforeDraw,afterDraw,makeTextData)
import MyLib (textToDots,dotsToText,jumpsToText)
import MyEvent (inputEvent)
import MyFile (fileRead,fileWrite)
import MyCode (exeCode)
import General (isLastElem)

data FC = NewFile | LoadNextFile | LoadFile | JumpFile | JumpBackFile | DoNothing deriving Eq
type ChangeFile = (T.Text,Int,Int,[Dot],Bool,[JBak])

myLoop :: Renderer -> [Font] -> [Texture] -> S.StateT State IO ()
myLoop re fonts itexs = do
  st <- S.get
  inp <- inputEvent 
  st' <- S.get
  let isUpdateTps = tps st /= tps st'
      nicr = isUpdateTps || icr st'
      ncrc = if isUpdateTps then 0 else crc st'
      bst = beforeDraw st'{crc=ncrc,icr=nicr,ipr=True}
  S.put bst
  when (inp==EXE) $ mapM_ exeCode (cod bst)  
  cst <- S.get
  let  msgSt = msg cst
  when (isLastElem msgSt "codeExe") $ mapM_ exeCode (cod cst) 
  cst' <- S.get
--  when (not (null msgSt) && last msgSt=="codeExe") $ print (dfn cst') 
  let isUpdateText = tex st /= tex cst' || icr st /= icr cst' || isUpdateTps 
                                        || inp==PKY || iup cst' 
      isUpdateDraw = inp==PMO || inp==EXE || isUpdateText
      isOnlyMouse = inp==PMO && not isUpdateText
      textData = if isUpdateDraw then makeTextData cst' else []
      getAtr d = let (_,_,gatr,_) = last d in gatr
      natr = if null textData then atr cst' else getAtr textData
      nscr = if inp==NFL || inp==LFL || inp==JMP then V2 0 0 else scr natr
      (njps,nfjp,jbkAt,nsjn,fpsSt) = (jps natr, fjp natr, jbk natr, sjn natr, fps cst')
      isLoadTgt = isLastElem msgSt "loadFile"
      tFjp = if isLoadTgt then read (last (init msgSt)) else 0
  when isUpdateDraw $ myDraw re fonts itexs textData isOnlyMouse (beforeDraw cst')
  let fc
       | inp==NFL = NewFile
       | inp==LFL = LoadNextFile
       | isLoadTgt = LoadFile
       | inp==JMP = JumpFile
       | inp==JBK = JumpBackFile
       | otherwise = DoNothing
  (ntex,nfps,ntps,ndts,niup,njbk) <- case fc of
        NewFile -> newFile fpsSt jbkAt cst' 
        LoadNextFile -> loadNextFile fpsSt jbkAt cst' 
        LoadFile -> loadFile fpsSt tFjp jbkAt cst'
        JumpFile -> jumpFile nfjp jbkAt nsjn fpsSt cst' 
        JumpBackFile -> jumpBackFile jbkAt fpsSt cst'
        _ -> return (tex cst',fpsSt,tps cst',dts cst',False,jbkAt)
  let nst = afterDraw cst'{tex=ntex,dts=ndts,msg=[],atr=(atr cst'){scr=nscr,jps=njps,fjp=nfjp,jbk=njbk,sjn=nsjn},fps=nfps,tps=ntps,iup=niup}
  delay delayTime
  S.put nst 
  if inp==QIT then do 
    fileWriteR fpsSt nst
    fileWrite textPosFile (T.pack$unwords [show (fps nst),show (tps nst)])
    fileWrite jumpNameFile (jumpsToText njps)
    return ()
              else myLoop re fonts itexs

newFile :: (MonadIO m) => Int -> [JBak] -> State -> m ChangeFile 
newFile fpsSt jbkAt st = do
    fileWriteR fpsSt st
    nextFileNum <- nextNewFileNum (fpsSt + 1)
    return (T.empty,nextFileNum,0,[],True,jbkAt)

loadNextFile :: (MonadIO m) => Int -> [JBak] -> State -> m ChangeFile
loadNextFile fpsSt jbkAt st = do
    fileWriteR fpsSt st
    loadFileNum <- loadExistFileNum (fpsSt + 1)
    (loadText, dots) <- fileReadR loadFileNum
    return (loadText,loadFileNum,0,dots,True,jbkAt)

loadFile :: (MonadIO m) => Int -> Int -> [JBak] -> State -> m ChangeFile
loadFile fpsSt tFps jbkAt st = do
    fileWriteR fpsSt st
    loadFileNum <- loadExistFileNum tFps 
    (loadText, dots) <- fileReadR loadFileNum
    return (loadText,loadFileNum,0,dots,True,jbkAt)

jumpFile :: (MonadIO m) => [FrJp] -> [JBak] -> Int -> Int -> State -> m ChangeFile
jumpFile fjpAt jbkAt sjnAt fpsSt st = do
      let (loadFileNum,textPos) = snd$fjpAt!!sjnAt
      fileWriteR fpsSt st
      (loadText, dots) <- fileReadR loadFileNum
      let jb = jbkAt ++ [(fpsSt,tps st)]
      return (loadText,loadFileNum,textPos,dots,True,jb)

jumpBackFile :: (MonadIO m) => [JBak] -> Int -> State -> m ChangeFile
jumpBackFile jbkAt fpsSt st = do
      let canJBack = not (null jbkAt)
          (loadFileNum,textPos) = if canJBack then last jbkAt else (fpsSt,tps st)
          nextFileName = textFileName++show loadFileNum++".txt"
          nextDotFile = dotFileName++show loadFileNum++".txt"
      loadText <- if canJBack then fileRead nextFileName else return (tex st)
      loadDotText <- if canJBack then fileRead nextDotFile else return T.empty 
      let dots = if canJBack then textToDots (T.words loadDotText) else dts st
      return (loadText,loadFileNum,textPos,dots,True,if canJBack then init jbkAt else jbkAt)

nextNewFileNum :: (MonadIO m) => Int -> m Int
nextNewFileNum i = do 
  let fileName = textFileName++show i++".txt" 
  fileExist <- liftIO $ doesFileExist fileName 
  if fileExist then nextNewFileNum (i+1) else return i

loadExistFileNum :: (MonadIO m) => Int -> m Int
loadExistFileNum i = do
  let fileName = textFileName++show i++".txt"
  fileExist <- liftIO $ doesFileExist fileName
  if fileExist then return i else loadExistFileNum 0

fileWriteR :: (MonadIO m) => Int -> State -> m ()
fileWriteR fp st = do
  fileWrite (textFileName++show fp++".txt") (tex st)
  fileWrite (dotFileName++show fp++".txt") (dotsToText$dts st)

fileReadR :: (MonadIO m) => Int -> m (T.Text, [Dot])
fileReadR lfn = do
  let nextFileName = textFileName++show lfn++".txt"
      nextDotFile = dotFileName++show lfn++".txt"
  loadText <- fileRead nextFileName  
  loadDotText <- fileRead nextDotFile
  let dots = textToDots (T.words loadDotText)
  return (loadText, dots)
