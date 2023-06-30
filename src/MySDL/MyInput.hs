module MySDL.MyInput (myInput) where

import SDL.Event (EventPayload(KeyboardEvent,TextInputEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvents,pollEvent
                 ,TextInputEventData(textInputEventText))
import SDL.Input.Keyboard (Keysym(keysymKeycode,keysymModifier),Keycode(unwrapKeycode),KeyModifier(..)
                          ,getModState)
import SDL.Input.Keyboard.Codes
import Data.Int(Int32)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Data.List(find)
import MyData(Modif(..))
  
myInput :: IO (Keycode,Modif,T.Text)
myInput = do
  events <- pollEvents
  mds <- getModState
  let kcmd event  =  case eventPayload event of 
                        KeyboardEvent keyboardEvent ->
                          if keyboardEventKeyMotion keyboardEvent == Pressed
                              then let kbeSim = keyboardEventKeysym keyboardEvent
                                    in (keysymKeycode kbeSim,keysymModifier kbeSim)
                              else (KeycodeUnknown,mds)
                        _ -> (KeycodeUnknown,mds)
      getItx event = case eventPayload event of
                        TextInputEvent textInputEvent -> textInputEventText textInputEvent
                        _ -> T.empty 
      (kc,md) = fromMaybe (KeycodeUnknown,mds) $ find (/=(KeycodeUnknown,mds)) (map kcmd events) 
      itx = fromMaybe T.empty $ find (/=T.empty) (map getItx events) 
      res = unwrapKeycode kc
      mdres
        | keyModifierLeftShift md || keyModifierRightShift md = Shf 
        | keyModifierLeftCtrl md || keyModifierRightCtrl md = Ctr 
        | keyModifierLeftAlt md || keyModifierRightAlt md = Alt 
        | otherwise = Non 
  when (res/=0) $ print res >> print mdres 
  if itx==T.empty then return () else TI.putStrLn itx
  return (kc,mdres,itx) 
 

  {--    
myInput :: IO (Int32,Modif)
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
        | keyModifierLeftShift md || keyModifierRightShift md = Shf 
        | keyModifierLeftCtrl md || keyModifierRightCtrl md = Ctr 
        | keyModifierLeftAlt md || keyModifierRightAlt md = Alt 
        | otherwise = Non 
  when (res/=0) $ print res >> print mdres 
  if itx==T.empty then return () else TI.putStrLn itx
  return (res,mdres) 
  
--}
