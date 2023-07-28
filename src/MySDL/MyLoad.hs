{-#LANGUAGE OverloadedStrings #-}
module MySDL.MyLoad (myLoad) where

import qualified SDL.Font as F
import qualified SDL.Image as I
import SDL.Video.Renderer (Surface)
import MyData (fontSize,fontFiles,imageFiles)

myLoad :: IO ([F.Font],[Surface])
myLoad = do
  fonts <- loadFonts fontSize fontFiles
  imageS <- loadImages imageFiles
  return (fonts,imageS)

loadImages :: [FilePath] -> IO [Surface]
loadImages = mapM I.load

loadFonts :: F.PointSize -> [FilePath] -> IO [F.Font]
loadFonts fs = mapM (`F.load` fs) 
