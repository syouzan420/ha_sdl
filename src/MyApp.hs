module MyApp(appMain) where

import Data.IORef(newIORef)
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
import MySDL.MyDraw (myDraw)
import MyData (initState,State(..))
import MyAction (makeTextData)
import MyFile (fileRead)

appMain :: IO ()
appMain =
  withMyInit $ do
    (fonts,sur,text,(fpos,tpos)) <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        let newState = initState{tex=text,fps=fpos,tps=tpos} 
        state <- newIORef newState
        myDraw renderer fonts itexs (makeTextData newState) newState 
        myLoop state renderer fonts itexs
