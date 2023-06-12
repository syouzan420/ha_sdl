module MySDL.MyInput (myInput) where

import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvent)
import SDL.Input.Keyboard (Keysym(keysymKeycode),Keycode(unwrapKeycode))
import SDL.Input.Keyboard.Codes
import Data.Int(Int32)
import Control.Monad(when)

myInput :: IO Int32 
myInput = do
  event <- pollEvent
  let kc = case event of 
             Just e -> case eventPayload e of 
                KeyboardEvent keyboardEvent ->
                    if keyboardEventKeyMotion keyboardEvent == Pressed
                      then keysymKeycode (keyboardEventKeysym keyboardEvent)
                      else KeycodeUnknown
                _ -> KeycodeUnknown
             Nothing -> KeycodeUnknown
      res = unwrapKeycode kc
  when (res/=0) $ print (unwrapKeycode kc) 
  return (unwrapKeycode kc) 
