module MyApp(appMain) where

import Data.IORef(newIORef)
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
import MyData (initState)

appMain :: IO ()
appMain =
  withMyInit $ do
    (fonts,sur) <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        state <- newIORef initState
        myLoop state renderer fonts itexs
