{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Char
import Data.Connection
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified System.IO.Streams as Streams
import System.IO.Streams.TCP (TCPConnection)
import qualified System.IO.Streams.UnixSocket as UnixSocket

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

main :: IO ()
main = do
  con <- acpidSocket
  readEvents $ source con

acpidSocket :: IO TCPConnection
acpidSocket = UnixSocket.connect "/var/run/acpid.socket"

readEvents :: Streams.InputStream ByteString -> IO ()
readEvents input = do
  val <- fmap (fmap E.decodeUtf8) (Streams.read input)
  case val of
    (Just text) -> do
      handleEvents text
      readEvents input
    Nothing -> putStrLn "Socket closed"

dispatchEvent :: AcpiEvent -> IO ()
dispatchEvent BrightnessUp = putStrLn "up"
dispatchEvent BrightnessDown = putStrLn "down"
dispatchEvent e = putStrLn $ show e

handleEvents :: T.Text -> IO ()
handleEvents source = handleResult $ parse acpiParser source
  where
    handleResult (Done rest event) = do
      dispatchEvent event
      if (T.null rest)
        then return ()
        else handleEvents rest
    handleResult _ = return ()

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
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return PowerButton

buttonSleep :: Parser AcpiEvent
buttonSleep =
  "button/sleep"
    *> space
    *> ("SLPB" <|> "SBTN")
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
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
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return AcPlugged

acAdapterUnplugged :: Parser AcpiEvent
acAdapterUnplugged =
  acAdapter
    *> "00000000"
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
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
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return BatteryOn

batteryOff :: Parser AcpiEvent
batteryOff =
  battery
    *> "00000001"
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return BatteryOff

lidClose :: Parser AcpiEvent
lidClose =
  "button/lid"
    *> space
    *> "close"
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return LidClosed

lidOpen :: Parser AcpiEvent
lidOpen =
  "button/lid"
    *> space
    *> "open"
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return LidOpened

brightnessUp :: Parser AcpiEvent
brightnessUp =
  "video/brightnessup"
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return BrightnessUp

brightnessDown :: Parser AcpiEvent
brightnessDown =
  "video/brightnessdown"
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return BrightnessDown
