module MyApp(appMain) where

import Data.IORef(newIORef)
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
import MySDL.MyDraw (myDraw)
import MyData (initState)
import MyAction (makeTextData)

appMain :: IO ()
appMain =
  withMyInit $ do
    (fonts,sur) <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        state <- newIORef initState
        myDraw renderer fonts itexs (makeTextData initState) initState 
        myLoop state renderer fonts itexs
