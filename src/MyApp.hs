module MyApp(appMain) where

import qualified Control.Monad.State.Strict as S
import MySDL.MyLoad (myLoad)
import MySDL.MyLoop (myLoop)
import MySDL.MyInit (withMyInit)
import MySDL.MyInitVideo (withMyVideo)
import MyData (initState,initActive,initAttr,initJumping
              ,State(..),Active(..),Attr(..),Jumping(..))

appMain :: IO ()
appMain =
  withMyInit $ do
    (fonts,sur,text,(fpos,tpos),dots,jumps) <- myLoad
    withMyVideo sur $
      \(renderer,itexs) -> do
        let newActive = initActive{tex=text,dts=dots,fps=fpos,tps=tpos}
            newAttr = initAttr{jmp=initJumping{jps=jumps}}
            newState = initState{act=newActive,atr=newAttr} 
        S.runStateT (myLoop renderer fonts itexs) newState
