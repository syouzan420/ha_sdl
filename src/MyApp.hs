module MyApp(appMain) where

import qualified Control.Monad.State.Strict as S
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
import MyData (initState,initAttr,State(..),Attr(..))

appMain :: IO ()
appMain =
  withMyInit $ do
    (fonts,sur,text,(fpos,tpos),dots,jumps) <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        let newState = initState{tex=text,dts=dots,fps=fpos,tps=tpos,atr=initAttr{jps=jumps}} 
        S.runStateT (myLoop renderer fonts itexs) newState
