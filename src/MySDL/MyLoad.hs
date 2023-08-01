{-#LANGUAGE OverloadedStrings #-}
module MySDL.MyLoad (myLoad) where

import qualified SDL.Font as F
import qualified SDL.Image as I
import SDL.Video.Renderer (Surface)
import qualified Data.Text as T 
import System.Directory (doesFileExist)
import MyFile (fileRead)
import MyData (fontSize,fontFiles,imageFiles,textFileName,textPosFile)

myLoad :: IO ([F.Font],[Surface],T.Text,(Int,Int))
myLoad = do
  fonts <- loadFonts fontSize fontFiles
  imageS <- loadImages imageFiles
  tposText <- fileRead textPosFile
  let ws = words$T.unpack tposText
      tpos = (read$head ws,read$head$tail ws)
  texts <- loadText textFileName (fst tpos) 
  return (fonts,imageS,texts,tpos)

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
                                    
                                                
