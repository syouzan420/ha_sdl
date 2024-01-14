module Game.WkDraw (wkDraw) where

import Control.Monad (when,unless)
import Control.Monad.IO.Class (MonadIO)
import SDL.Vect (Point(P),V2(..),V4(..))
import SDL.Internal.Numbered (fromNumber)
import SDL.Video (Renderer)
import SDL.Video.Renderer (Surface,Texture,SurfacePixelFormat(..),PixelFormat(..)
                          ,present,lockSurface,unlockSurface,surfacePixels
                          ,surfaceFormat,createRGBSurfaceFrom,createTextureFromSurface)
import SDL.Font (Font)
import qualified SDL.Raw.Types as SDLT
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Storable (peek)
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8)
import Data.List (transpose)
import MySDL.MyDraw (initDraw,textsDraw)
import Game.WkData (Waka(..),Size,Pos,OMap)
import Game.WkLib (cosList,shiftList)
import MyData (TextData,WMode(..))

type MapSize = Size
type EffectNum = Int
type IsTextShowing = Bool

wkDraw :: (MonadIO m) => Renderer -> [Font] -> [[Surface]] -> TextData 
                            -> MapSize -> IsTextShowing -> Waka -> m ()
wkDraw re fonts surfs textData msize ish wk = do
  initDraw re
  when ish $ textsDraw re fonts T True False (tps wk) textData
  unless (msize==V2 0 0) $ mapDraw re (surfs!!0) (pps wk) (omp wk) (aco wk)
  present re

mapDraw :: (MonadIO m) => Renderer -> [Surface] -> Pos -> OMap -> Int -> m ()
mapDraw re surfs ps omap count = undefined

createTextures :: Renderer -> [Surface] -> [Texture]
createTextures = undefined

createTexture :: Renderer -> Surface -> EffectNum -> Int -> IO Texture
createTexture re surf i count = do
  if i==0 then createTextureFromSurface re surf
          else do 
            nsurf <- createNewSurface surf count
            createTextureFromSurface re nsurf 

createNewSurface :: Surface -> Int -> IO Surface
createNewSurface surf count = do
  lockSurface surf
  SurfacePixelFormat pointerPixFormat <- surfaceFormat surf
  surPixFormat <- peek pointerPixFormat
  let sPixFormat = fromNumber (SDLT.pixelFormatFormat surPixFormat) :: PixelFormat
  pointer <- surfacePixels surf
  frPointer <- newForeignPtr_ (castPtr pointer)
  let mvector = VM.unsafeFromForeignPtr0 frPointer (4*64*64)
  unlockSurface surf
  waveEffect mvector count
  createRGBSurfaceFrom mvector (V2 64 64) (4*64) sPixFormat

waveEffect :: VM.IOVector Word8 -> Int -> IO ()
waveEffect vect t = do
  let defs = cosList 64 1 2 t
  lst <- makeList vect 64 0 (V2 0 0)
  let sList = zipWith (shiftList (V4 255 0 0 0)) lst defs
  let tList = transpose sList
  let s2List = zipWith (shiftList (V4 255 0 0 0)) tList defs
  let fList = transpose s2List
  writeList vect fList 0

writeList :: VM.IOVector Word8 -> [[V4 Word8]] -> Int -> IO ()
writeList _ [] _ = return ()
writeList vect (y:ys) si = do
  writeListX vect y si
  writeList vect ys (si+4*64)

writeListX :: VM.IOVector Word8 -> [V4 Word8] -> Int -> IO ()
writeListX _ [] _ = return ()
writeListX vect ((V4 a b c d):xs) si = do
  mapM_ (uncurry (VM.write vect)) (zip (map (+si) [0,1,2,3]) [a,b,c,d])
  writeListX vect xs (si+4)

makeList :: VM.IOVector Word8 -> Int -> Int -> V2 Int -> IO [[V4 Word8]] 
makeList vect u si (V2 _ q) = do
  if q==u then return [] else do
    x <- makeListX vect u si (V2 0 q)
    xs <- makeList vect u si (V2 0 (q+1))
    return (x : xs)

makeListX :: VM.IOVector Word8 -> Int -> Int -> V2 Int -> IO [V4 Word8]
makeListX vect u si (V2 p q) = do
  if p==u then return [] else do
    x <- makeV4 vect (si+pToI (V2 p q))
    xs <- makeListX vect u si (V2 (p+1) q)
    return (x : xs)

makeV4 :: VM.IOVector Word8 -> Int -> IO (V4 Word8)
makeV4 vect i = do
  [a,b,c,d] <- mapM (VM.read vect) [i..(i+3)]
  return (V4 a b c d)

pToI :: V2 Int -> Int
pToI (V2 p q) = 4*(64*q + p)

