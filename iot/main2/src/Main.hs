module Main where

import Data.Word
import System.Hardware.Haskino

led :: Word8
led = 13

delay :: Word32
delay = 1000

blink :: Arduino ()
blink = do
    setPinMode led OUTPUT
    loop $ do
        digitalWrite led True
        delayMillis delay
        digitalWrite led False
        delayMillis delay

blinkExample :: IO ()
blinkExample = withArduino True "/dev/ttyUSB0" blink

main :: IO ()
main = compileProgram blink "blink.ino"
--main = blinkExample
