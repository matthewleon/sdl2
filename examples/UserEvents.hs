module UserEvents where

import Control.Concurrent (myThreadId)
import Data.Maybe (Maybe(Nothing))
import qualified Data.Text as Text
import Foreign.Ptr (nullPtr)
import SDL

data TimerEvent = TimerEvent

timerEvent :: IO TimerEvent
timerEvent = do
  t <- show <$> ticks
  tid <- show <$> myThreadId
  putStrLn $ "Created timer event at " ++ t ++ " ticks. Threadid: " ++ tid
  return TimerEvent

main :: IO ()
main = do
  initializeAll
  let eventToUserEvent = const . return $ Just TimerEvent
      userEventToUserEventData = const . return $ UserEventData 0 Nothing 0 nullPtr nullPtr
  registeredEvent <- registerEvent eventToUserEvent userEventToUserEventData
  case registeredEvent of
    Nothing -> putStrLn "Fatal error: unable to register timer events."
    Just registeredTimerEvent -> do
      addTimer 1000 $ mkTimerCb registeredTimerEvent
      putStrLn "press q at any time to quit"
      appLoop registeredTimerEvent

mkTimerCb :: RegisteredEventType IO TimerEvent -> TimerCallback
mkTimerCb (RegisteredEventType pushTimerEvent _) interval = do
  pushResult <- pushTimerEvent =<< timerEvent
  case pushResult of
    EventPushSuccess -> return ()
    EventPushFiltered -> putStrLn "event push was filtered: this is impossible"
    EventPushFailure e -> putStrLn $ "Couldn't push event: " ++ Text.unpack e
  return $ Reschedule interval

appLoop :: RegisteredEventType IO TimerEvent -> IO ()
appLoop (RegisteredEventType pushTimerEvent getTimerEvent) = waitEvent >>= go
  where
  go :: Event -> IO ()
  go ev =
    case eventPayload ev of
      -- Press Q to quit
      KeyboardEvent keyboardEvent
        |  keyboardEventKeyMotion keyboardEvent == Pressed &&
           keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        -> return ()
      UserEvent _ -> do
        maybeTimerEvent <- getTimerEvent ev
        case maybeTimerEvent of
          Just _ -> do
             t <- show <$> ticks
             tid <- show <$> myThreadId
             putStrLn $ "Got timer event from queue at " ++ t ++ " ticks. Threadid: " ++ tid
          Nothing -> return ()
        waitEvent >>= go
      _ -> waitEvent >>= go
