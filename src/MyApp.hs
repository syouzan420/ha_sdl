module MyApp(appMain) where

import qualified Control.Monad.State.Strict as S
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
import MyData (initState,initActive,initAttr,State(..),Active(..),Attr(..))

appMain :: IO ()
appMain =
  withMyInit $ do
    (fonts,sur,text,(fpos,tpos),dots,jumps) <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        let newActive = initActive{tex=text,dts=dots,fps=fpos,tps=tpos}
            newState = initState{act=newActive,atr=initAttr{jps=jumps}} 
        S.runStateT (myLoop renderer fonts itexs) newState
