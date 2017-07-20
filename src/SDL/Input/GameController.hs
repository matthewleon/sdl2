{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module SDL.Input.GameController
  ( ControllerButton(..)
  , ControllerButtonState(..)
  , ControllerDeviceConnection(..)
  ) where

import Data.Data (Data)
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import GHC.Int (Int32)
import SDL.Internal.Numbered
import qualified SDL.Raw as Raw

-- | Identifies a gamepad button.
data ControllerButton
  = ControllerButtonInvalid
  | ControllerButtonA
  | ControllerButtonB
  | ControllerButtonX
  | ControllerButtonY
  | ControllerButtonBack
  | ControllerButtonGuide
  | ControllerButtonStart
  | ControllerButtonLeftStick
  | ControllerButtonRightStick
  | ControllerButtonLeftShoulder
  | ControllerButtonRightShoulder
  | ControllerButtonDpadUp
  | ControllerButtonDpadDown
  | ControllerButtonDpadLeft
  | ControllerButtonDpadRight
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerButton Int32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLER_BUTTON_A -> ControllerButtonA
    Raw.SDL_CONTROLLER_BUTTON_B -> ControllerButtonB
    Raw.SDL_CONTROLLER_BUTTON_X -> ControllerButtonX
    Raw.SDL_CONTROLLER_BUTTON_Y -> ControllerButtonY
    Raw.SDL_CONTROLLER_BUTTON_BACK -> ControllerButtonBack
    Raw.SDL_CONTROLLER_BUTTON_GUIDE -> ControllerButtonGuide
    Raw.SDL_CONTROLLER_BUTTON_START -> ControllerButtonStart
    Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK -> ControllerButtonLeftStick
    Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK -> ControllerButtonRightStick
    Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER -> ControllerButtonLeftShoulder
    Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER -> ControllerButtonRightShoulder
    Raw.SDL_CONTROLLER_BUTTON_DPAD_UP -> ControllerButtonDpadUp
    Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN -> ControllerButtonDpadDown
    Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT -> ControllerButtonDpadLeft
    Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT -> ControllerButtonDpadRight
    _ -> ControllerButtonInvalid

instance ToNumber ControllerButton Int32 where
  toNumber ControllerButtonA = Raw.SDL_CONTROLLER_BUTTON_A
  toNumber ControllerButtonB = Raw.SDL_CONTROLLER_BUTTON_B
  toNumber ControllerButtonX = Raw.SDL_CONTROLLER_BUTTON_X
  toNumber ControllerButtonY = Raw.SDL_CONTROLLER_BUTTON_Y
  toNumber ControllerButtonBack =  Raw.SDL_CONTROLLER_BUTTON_BACK
  toNumber ControllerButtonGuide = Raw.SDL_CONTROLLER_BUTTON_GUIDE
  toNumber ControllerButtonStart = Raw.SDL_CONTROLLER_BUTTON_START
  toNumber ControllerButtonLeftStick = Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK
  toNumber ControllerButtonRightStick = Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK
  toNumber ControllerButtonLeftShoulder = Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER
  toNumber ControllerButtonRightShoulder = Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER
  toNumber ControllerButtonDpadUp = Raw.SDL_CONTROLLER_BUTTON_DPAD_UP
  toNumber ControllerButtonDpadDown = Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN
  toNumber ControllerButtonDpadLeft = Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT
  toNumber ControllerButtonDpadRight = Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT
  toNumber ControllerButtonInvalid = Raw.SDL_CONTROLLER_BUTTON_INVALID

-- | Identifies the state of a controller button.
data ControllerButtonState = ControllerButtonPressed | ControllerButtonReleased
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerButtonState Word8 where
  fromNumber n = case n of
    Raw.SDL_PRESSED -> ControllerButtonPressed
    Raw.SDL_RELEASED -> ControllerButtonReleased
    _ -> ControllerButtonPressed

instance ToNumber ControllerButtonState Word8 where
  toNumber ControllerButtonPressed = Raw.SDL_PRESSED
  toNumber ControllerButtonReleased = Raw.SDL_RELEASED

-- | Identified whether the game controller was added, removed, or remapped.
data ControllerDeviceConnection
  = ControllerDeviceAdded
  | ControllerDeviceRemoved
  | ControllerDeviceRemapped
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerDeviceConnection Word32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLERDEVICEADDED -> ControllerDeviceAdded
    Raw.SDL_CONTROLLERDEVICEREMOVED -> ControllerDeviceRemoved
    _ -> ControllerDeviceRemapped

instance ToNumber ControllerDeviceConnection Word32 where
  toNumber ControllerDeviceAdded = Raw.SDL_CONTROLLERDEVICEADDED
  toNumber ControllerDeviceRemoved = Raw.SDL_CONTROLLERDEVICEREMOVED
  toNumber ControllerDeviceRemapped = Raw.SDL_CONTROLLERDEVICEREMAPPED
