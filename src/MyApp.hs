module MyApp(appMain) where

import qualified Control.Monad.State.Strict as S
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
import MySDL.MyDraw (myDraw)
import MyData (initState,initAttr,State(..),Attr(..))
import MyAction (makeTextData)

appMain :: IO ()
appMain =
  withMyInit $ do
    (fonts,sur,text,(fpos,tpos),dots,jumps) <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        let newState = initState{tex=text,dts=dots,fps=fpos,tps=tpos,atr=initAttr{jps=jumps}} 
        myDraw renderer fonts itexs (makeTextData newState) False newState 
        S.runStateT (myLoop renderer fonts itexs) newState
