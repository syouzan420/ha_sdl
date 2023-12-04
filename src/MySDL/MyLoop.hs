{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyLoop (myLoop) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as S
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Texture)
import SDL.Time (delay)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import Linear.V2 (V2(..))
import MySDL.MyDraw (myDraw)
import MyData (State(..),Attr(..),Dot,Input(..),delayTime,textFileName,textPosFile
              ,dotFileName,jumpNameFile)
import MyAction (beforeDraw,afterDraw,makeTextData)
import MyLib (textToDots,dotsToText,jumpsToText)
import MyEvent (inputEvent)
import MyFile (fileRead,fileWrite)
import MyCode (exeCode)

myLoop :: Renderer -> [Font] -> [Texture] -> S.StateT State IO ()
myLoop re fonts itexs = do
  st <- S.get
  inp <- inputEvent 
  st' <- S.get
  let isUpdateTps = tps st /= tps st'
      nicr = isUpdateTps || icr st'
      ncrc = if isUpdateTps then 0 else crc st'
      bst = beforeDraw st'{crc=ncrc,icr=nicr,ipr=True}
      cst = if inp==EXE then foldl exeCode bst (cod bst) else bst  
      msgSt = msg cst
  let cst' = if not (null msgSt) && last msgSt=="codeExe" then foldl exeCode cst (cod cst) else cst
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
  finalSt <- liftIO $ do
    when isUpdateDraw $ myDraw re fonts itexs textData isOnlyMouse (beforeDraw cst')
    (ntex,nfps,ntps,ndts,niup,njbk) <- case inp of 
     NFL -> do
       fileWriteR fpsSt cst'
       nextFileNum <- nextNewFileNum (fpsSt + 1)
       return (T.empty,nextFileNum,0,[],True,jbkAt)
     LFL -> do
       fileWriteR fpsSt cst'
       loadFileNum <- loadExistFileNum (fpsSt + 1)
       (loadText, dots) <- fileReadR loadFileNum
       return (loadText,loadFileNum,0,dots,True,jbkAt)
     JMP -> do
       let (loadFileNum,textPos) = snd$nfjp!!nsjn
       fileWriteR fpsSt cst'
       (loadText, dots) <- fileReadR loadFileNum
       let jb = jbkAt ++ [(fpsSt,tps cst')]
       return (loadText,loadFileNum,textPos,dots,True,jb)
     JBK -> do
       let canJBack = not (null jbkAt)
           (loadFileNum,textPos) = if canJBack then last jbkAt else (fpsSt,tps cst')
           nextFileName = textFileName++show loadFileNum++".txt"
           nextDotFile = dotFileName++show loadFileNum++".txt"
       loadText <- if canJBack then fileRead nextFileName else return (tex cst')
       loadDotText <- if canJBack then fileRead nextDotFile else return T.empty 
       let dots = if canJBack then textToDots (T.words loadDotText) else dts cst'
       return (loadText,loadFileNum,textPos,dots,True,init jbkAt)
     _   -> return (tex cst',fpsSt,tps cst',dts cst',False,jbkAt)
    let nst = afterDraw cst'{tex=ntex,dts=ndts,msg=[],atr=(atr cst'){scr=nscr,jps=njps,fjp=nfjp,jbk=njbk,sjn=nsjn},fps=nfps,tps=ntps,iup=niup}
    delay delayTime
    when (inp==QIT) $ do 
      fileWriteR fpsSt nst
      fileWrite textPosFile (T.pack$unwords [show (fps nst),show (tps nst)])
      fileWrite jumpNameFile (jumpsToText njps)
      return ()
    return nst
  S.put finalSt
  if inp==QIT then return () else myLoop re fonts itexs

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
