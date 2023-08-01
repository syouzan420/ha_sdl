{-#LANGUAGE OverloadedStrings #-}
module MySDL.MyLoad (myLoad,textToDots) where

import qualified SDL.Font as F
import qualified SDL.Image as I
import SDL.Video.Renderer (Surface)
import qualified Data.Text as T 
import Linear.V2 (V2(..))
import System.Directory (doesFileExist)
import MyFile (fileRead)
import MyData (Dots,fontSize,fontFiles,imageFiles,textFileName,textPosFile,dotFileName)

myLoad :: IO ([F.Font],[Surface],T.Text,(Int,Int),Dots)
myLoad = do
  fonts <- loadFonts fontSize fontFiles
  imageS <- loadImages imageFiles
  tposText <- fileRead textPosFile
  let ws = words$T.unpack tposText
      tpos = (read$head ws,read$head$tail ws)
  texts <- loadText textFileName (fst tpos) 
  dotsText <- loadText dotFileName (fst tpos)
  let dots = if dotsText==T.empty then [] else textToDots (T.words dotsText)
  return (fonts,imageS,texts,tpos,dots)

loadImages :: [FilePath] -> IO [Surface]
loadImages = mapM I.load

loadFonts :: F.PointSize -> [FilePath] -> IO [F.Font]
loadFonts fs = mapM (`F.load` fs) 

loadText :: FilePath -> Int -> IO T.Text
loadText filename i = do
  let wfn = filename++show i++".txt"
  dfe <- doesFileExist wfn
  if dfe then fileRead wfn
         else return T.empty 

textToDots :: [T.Text] -> Dots
textToDots [] = []
textToDots (x:y:c:xs) = (V2 (read$T.unpack x) (read$T.unpack y),read$T.unpack c):textToDots xs
