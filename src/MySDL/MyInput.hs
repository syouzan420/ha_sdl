{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyInput (myInput) where

import SDL.Event (EventPayload(KeyboardEvent,TextInputEvent,TextEditingEvent
                              ,MouseButtonEvent,MouseMotionEvent)
                 ,eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvents
                 ,TextInputEventData(textInputEventText),TextEditingEventData(textEditingEventText)
                 ,MouseButtonEventData(mouseButtonEventMotion,mouseButtonEventPos)
                 ,MouseMotionEventData(mouseMotionEventState,mouseMotionEventPos)
                 ,MouseButton(ButtonLeft))
import SDL.Input.Keyboard (Keysym(keysymKeycode,keysymModifier),KeyModifier(..)
                          ,getModState)
import SDL.Input.Keyboard.Codes
import SDL.Vect(Point(P),V2(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Maybe(fromMaybe)
import Data.List(find)
import Foreign.C.Types(CInt)
import MyData(Modif(..))
  
myInput :: IO (Keycode,Modif,T.Text,V2 CInt,Bool)
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
                        TextEditingEvent textEditingEvent -> textEditingEventText textEditingEvent
                        TextInputEvent textInputEvent -> textInputEventText textInputEvent
                        _ -> T.empty 
      mbtn event = case eventPayload event of
                     MouseButtonEvent mouseButtonEvent ->
                       if mouseButtonEventMotion mouseButtonEvent == Pressed
                          then mouseButtonEventPos mouseButtonEvent
                          else P (V2 (-1) (-1))
                     MouseMotionEvent mouseMotionEvent ->
                       if mouseMotionEventState mouseMotionEvent == [ButtonLeft]
                          then mouseMotionEventPos mouseMotionEvent
                          else P (V2 (-1) (-1))
                     _ -> P (V2 (-1) (-1))
      mmtn event = case eventPayload event of
                     MouseButtonEvent mouseButtonEvent ->
                       mouseButtonEventMotion mouseButtonEvent /= Pressed 
                     MouseMotionEvent mouseMotionEvent ->
                       mouseMotionEventState mouseMotionEvent == [ButtonLeft]
                     _ -> False 
                     
      (kc,md) = fromMaybe (KeycodeUnknown,mds) $ find (/=(KeycodeUnknown,mds)) (map kcmd events) 
      itx = fromMaybe T.empty $ find (/=T.empty) (map getItx events) 
      cPos = fromMaybe (P (V2 (-1) (-1))) $ find (/=P (V2 (-1) (-1))) (map mbtn events)
      ismc = fromMaybe False $ Just (head (map mmtn events))
      mdres
        | keyModifierLeftShift md || keyModifierRightShift md = Shf 
        | keyModifierLeftCtrl md || keyModifierRightCtrl md = Ctr 
        | keyModifierLeftAlt md || keyModifierRightAlt md = Alt 
        | otherwise = Non 
  if itx==T.empty then return () else TI.putStrLn ("itx:"<>itx)
  let mps = let (P (V2 px py)) = cPos in V2 (fromIntegral px) (fromIntegral py)
  if mps==V2 (-1) (-1) then return () else print mps >> print ismc
  let kc' 
       |kc==KeycodeUnknown && itx/=T.empty = case itx of
                  "i" -> KeycodeI; "h" -> KeycodeH; "j" -> KeycodeJ; "k" -> KeycodeK; "l" -> KeycodeL
                  "い" -> KeycodeI
                  _ -> KeycodeUnknown
       |otherwise = kc
  return (kc',mdres,itx,mps,ismc) 
 
