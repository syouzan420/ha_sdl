module MyAction (myAction,beforeDraw,afterDraw) where

import Data.Text (Text)
import MyData (State(..),Attr(..))

myAction :: State -> State
myAction st = st

beforeDraw :: State -> State 
beforeDraw st = 
  let crcSt = crc st
      icrSt = icr st
      ncrc = if crcSt<10 then crcSt+1 else 0
      nicr = if crcSt<10 then icrSt else not icrSt
   in st{crc=ncrc, icr=nicr}

afterDraw :: State -> State
afterDraw st = st

