{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hacpi
  ( AcpiEvent (..),
    acpiListen,
  )
where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Exception (catch)
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Conc (ThreadId)
import Network.Socket
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering)

data AcpiEvent
  = PowerButton
  | SleepButton
  | AcPlugged
  | AcUnplugged
  | BatteryOn
  | BatteryOff
  | LidClosed
  | LidOpened
  | BrightnessUp
  | BrightnessDown
  deriving (Show)

connectAcpid :: IO Handle
connectAcpid = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s $ SockAddrUnix "/var/run/acpid.socket"
  h <- socketToHandle s ReadMode
  hSetBuffering h LineBuffering
  return h

acpiListen :: (AcpiEvent -> IO ()) -> IO (ThreadId)
acpiListen action = connectAcpid >>= runLoop
  where
    runLoop h = forkIO $ handleEvent h `catch` closeHandle h

    handleEvent h =
      fmap parseEvent (T.hGetLine h) >>= \case
        Right e -> forkIO (action e) >> handleEvent h
        Left _ -> handleEvent h

parseEvent :: T.Text -> Either String AcpiEvent
parseEvent = parseOnly acpiParser

closeHandle :: Handle -> IOError -> IO ()
closeHandle h _ = hClose h

acpiParser :: Parser AcpiEvent
acpiParser =
  buttonPower
    <|> buttonSleep
    <|> acAdapterPlugged
    <|> acAdapterUnplugged
    <|> batteryOn
    <|> batteryOff
    <|> lidClose
    <|> lidOpen
    <|> brightnessUp
    <|> brightnessDown

buttonPower :: Parser AcpiEvent
buttonPower =
  "button/power"
    *> space
    *> ("PBTN" <|> "PWRF")
    *> return PowerButton

buttonSleep :: Parser AcpiEvent
buttonSleep =
  "button/sleep"
    *> space
    *> ("SLPB" <|> "SBTN")
    *> return SleepButton

acAdapter :: Parser Char
acAdapter =
  "ac_adapter"
    *> space
    *> ("AC" <|> "ACAD" <|> "ADP0")
    *> space
    *> takeTill isSpace
    *> space

acAdapterPlugged :: Parser AcpiEvent
acAdapterPlugged =
  acAdapter
    *> "00000001"
    *> return AcPlugged

acAdapterUnplugged :: Parser AcpiEvent
acAdapterUnplugged =
  acAdapter
    *> "00000000"
    *> return AcUnplugged

battery :: Parser Char
battery =
  "battery BAT0"
    *> space
    *> takeTill isSpace
    *> space

batteryOn :: Parser AcpiEvent
batteryOn =
  battery
    *> "00000000"
    *> return BatteryOn

batteryOff :: Parser AcpiEvent
batteryOff =
  battery
    *> "00000001"
    *> return BatteryOff

lidClose :: Parser AcpiEvent
lidClose =
  "button/lid LID"
    *> space
    *> "close"
    *> return LidClosed

lidOpen :: Parser AcpiEvent
lidOpen =
  "button/lid LID"
    *> space
    *> "open"
    *> return LidOpened

brightnessUp :: Parser AcpiEvent
brightnessUp =
  "video/brightnessup"
    *> return BrightnessUp

brightnessDown :: Parser AcpiEvent
brightnessDown =
  "video/brightnessdown"
    *> return BrightnessDown
