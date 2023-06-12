module MyAction (myAction,beforeDraw,afterDraw) where

import Data.Text (Text)
import MyData (State(..),Attr(..))

myAction :: State -> State
myAction st = st

beforeDraw :: State -> State 
beforeDraw st = st 

afterDraw :: State -> State
afterDraw st = st

