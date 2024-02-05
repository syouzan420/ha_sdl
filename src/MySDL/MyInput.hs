{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyInput (myInput) where

import SDL.Event (EventPayload(KeyboardEvent,TextInputEvent,TextEditingEvent
                              ,MouseButtonEvent,MouseMotionEvent,KeymapChangedEvent)
                 ,eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed,Released),keyboardEventKeysym,pollEvents,pumpEvents
                 ,TextInputEventData(textInputEventText),TextEditingEventData(textEditingEventLength,textEditingEventText)
                 ,MouseButtonEventData(mouseButtonEventMotion,mouseButtonEventPos)
                 ,MouseMotionEventData(mouseMotionEventState,mouseMotionEventPos)
                 ,MouseButton(ButtonLeft))
import SDL.Input.Keyboard (Keysym(keysymKeycode,keysymModifier),KeyModifier(..)
                          ,getModState)
import SDL.Input.Keyboard.Codes
import SDL.Vect(Point(P),V2(..))
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Maybe(fromMaybe)
import Data.List(find)
import Foreign.C.Types(CInt)
import MyData(Modif(..))
  
myInput :: (MonadIO m) => m (Keycode,Modif,T.Text,V2 CInt,Bool,Bool,Bool)
myInput = do
  events <- pollEvents
  mds <- getModState
  let kcmd event  =  case eventPayload event of 
                       KeyboardEvent keyboardEvent ->
                         case keyboardEventKeyMotion keyboardEvent of
                            Pressed -> let kbeSim = keyboardEventKeysym keyboardEvent
                                        in (keysymKeycode kbeSim,keysymModifier kbeSim)
                            _        -> (KeycodeUnknown,mds)
                       _ -> (KeycodeUnknown,mds)
      kir event = case eventPayload event of
                    KeyboardEvent keyboardEvent ->
                      case keyboardEventKeyMotion keyboardEvent of
                        Released -> True
                        _ -> False
                    _ -> False
--  let kcmd event  =  case eventPayload event of 
--                       KeyboardEvent keyboardEvent ->
--                         if keyboardEventKeyMotion keyboardEvent == Pressed
--                             then let kbeSim = keyboardEventKeysym keyboardEvent
--                                   in (keysymKeycode kbeSim,keysymModifier kbeSim)
--                             else (KeycodeUnknown,mds)
--                       _ -> (KeycodeUnknown,mds)
      getItx event = case eventPayload event of
          TextInputEvent textInputEvent -> (textInputEventText textInputEvent,False)
          TextEditingEvent textEditingEvent 
                      -> (textEditingEventText textEditingEvent,True)
          _ -> (T.empty,False)
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
                     
      ir = if (not (null events)) then last (map kir events) else False
      (kc,md) = fromMaybe (KeycodeUnknown,mds) $ find (/=(KeycodeUnknown,mds)) (reverse (map kcmd events)) 
      (itx,ised) = fromMaybe (T.empty,False) $ find (/=(T.empty,False)) $ 
                                  filter (/=(T.empty,True)) (map getItx events) 
      cPos = fromMaybe (P (V2 (-1) (-1))) $ find (/=P (V2 (-1) (-1))) (map mbtn events)
      ismc = fromMaybe False $ Just (head (map mmtn events))
      mdres
        | keyModifierLeftShift md || keyModifierRightShift md = Shf 
        | keyModifierLeftCtrl md || keyModifierRightCtrl md = Ctr 
        | keyModifierLeftAlt md || keyModifierRightAlt md = Alt 
        | otherwise = Non 
  --when (not (null events)) $ liftIO $ print events 
  let mps = let (P (V2 px py)) = cPos in V2 (fromIntegral px) (fromIntegral py)
--  if mps==V2 (-1) (-1) then return () else print mps >> print ismc
--  let skkedit = itx==T.empty && kc/=KeycodeUnknown && kc/=KeycodeLShift && kc/=KeycodeRShift && mdres == Shf  
--  when skkedit $ putStrLn "SkkEditStart"
  let kc' 
       |kc==KeycodeUnknown && itx/=T.empty = case itx of
                  "i" -> KeycodeI; "h" -> KeycodeH; "j" -> KeycodeJ; "k" -> KeycodeK;
                  "l" -> KeycodeL; " " -> KeycodeSpace
                  "い" -> KeycodeI
                  _ -> KeycodeUnknown
       |otherwise = kc
--  when (itx/=T.empty) $ liftIO $ print kc'
--  when ir $ liftIO $ print "released"
  return (kc',mdres,itx,mps,ismc,ised,ir) 
 
