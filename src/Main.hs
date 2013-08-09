module Main (main, Direction) where
import           Control.Applicative
import           Control.Monad
import           Control.Concurrent
import           Data.Maybe
import           Data.List

-- | For hysteresis we need state.  Fancontrol needs to remember if it
-- currently lets the temperature go up or tries to cool down.
data Direction = Up | Down
  deriving Show

level :: Direction -> Int -> (String, Direction)
level Up t
  | t >= 55   = ("level auto", Down)
  | t >= 54   = ("level 1",    Up)
  | otherwise = ("level 0",    Up)
level Down t
  | t <= 52   = ("level 0",    Up)
  | otherwise = ("level auto", Down)

command :: String -> IO ()
command = writeFile "/proc/acpi/ibm/fan"

main :: IO ()
main = do
  command "watchdog 2"
  loop Up

loop :: Direction -> IO ()
loop direction = do
  temperature <- readTemperature
  let (arg, newDir) = level direction temperature
  -- putStrLn $ concat $ intersperse " " $ [show temperature, show direction, arg]
  command arg
  threadDelay dt
  loop newDir
  where
    dt = truncate (0.5 * 1e6 :: Double)

-- | Read temperature from all sensors and return the highest value.
readTemperature :: IO Int
readTemperature =
      maximum
    . map read . words
    . fromJust . stripPrefix "temperatures:\t"
  <$> readFile "/proc/acpi/ibm/thermal"
