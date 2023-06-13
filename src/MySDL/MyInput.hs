module MySDL.MyInput (myInput) where

import SDL.Event (EventPayload(KeyboardEvent,TextInputEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvent
                 ,TextInputEventData(textInputEventText))
import SDL.Input.Keyboard (Keysym(keysymKeycode,keysymModifier),Keycode(unwrapKeycode),KeyModifier(..)
                          ,getModState)
import SDL.Input.Keyboard.Codes
import Data.Int(Int32)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Control.Monad(when)

myInput :: IO (Int32,Char)
myInput = do
  event <- pollEvent
  mds <- getModState
  let (kc,md) = case event of 
             Just e -> case eventPayload e of 
                KeyboardEvent keyboardEvent ->
                    if keyboardEventKeyMotion keyboardEvent == Pressed
                       then let kbeSim = keyboardEventKeysym keyboardEvent
                             in (keysymKeycode kbeSim,keysymModifier kbeSim)
                      else (KeycodeUnknown,mds)
                _ -> (KeycodeUnknown,mds)
             Nothing -> (KeycodeUnknown,mds)
      itx = case event of
              Just e -> case eventPayload e of
                TextInputEvent textInputEvent -> textInputEventText textInputEvent
                _ -> T.empty 
              Nothing -> T.empty
      res = unwrapKeycode kc
      mdres
        | keyModifierLeftShift md || keyModifierRightShift md = 's'
        | keyModifierLeftCtrl md || keyModifierRightCtrl md = 'c'
        | keyModifierLeftAlt md || keyModifierRightAlt md = 'a'
        | otherwise = ' '
  when (res/=0) $ print res >> print mdres 
  if itx==T.empty then return () else TI.putStrLn itx
  return (res,mdres) 
