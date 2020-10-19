{-# LANGUAGE OverloadedStrings #-}

module Hacpi
  ( AcpiEvent (..),
    acpiListen,
  )
where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Monad (unless)
import Data.Attoparsec.Text
import Data.Char
import Data.Connection as C
import qualified Data.Text as T
import GHC.Conc (ThreadId)
import qualified System.IO.Streams as S
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

acpidSocket :: IO TCPConnection
acpidSocket = UnixSocket.connect "/var/run/acpid.socket"

acpiListen :: (AcpiEvent -> IO ()) -> IO (ThreadId)
acpiListen action = forkIO $ do
  con <- acpidSocket
  s <- S.decodeUtf8 $ C.source con
  readEvents s
  C.close con
  where
    readEvents s = do
      text <- readStream s
      handleEvent s $ parse acpiParser text

    handleEvent s event = case event of
      (Done r event) -> do
        action event
        S.unRead r s
        readEvents s
      (Fail _ _ _) -> do
        readEvents s
      e@(Partial _) -> do
        isEOF <- S.atEOF s
        unless isEOF $ do
          text <- readStream s
          handleEvent s $ feed e text
        return ()

readStream :: S.InputStream T.Text -> IO T.Text
readStream s = do
  text <- S.read s
  return $ maybe "" id text

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
  "button/lid LID"
    *> space
    *> "close"
    *> takeTill isEndOfLine
    *> (endOfLine <|> endOfInput)
    *> return LidClosed

lidOpen :: Parser AcpiEvent
lidOpen =
  "button/lid LID"
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
