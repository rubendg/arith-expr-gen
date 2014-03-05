module Main where

import Core
import Gen
import Common

import Control.Monad.Random
import Control.Applicative
import Data.Foldable
import Control.Monad
import Control.Monad.Fix
import Text.Read
import Data.Maybe
import Data.Function
import System.Timeout
import Data.IORef
import System.Time
import Data.Digits
import Data.Ratio
import Data.Either

data PlayStyle = Timeout Int | XQuestions Int

data GameConfig = 
  GameConfig {
    playStyle :: PlayStyle
  , expressionConfig :: Config
  }

data Stats = Stats 
  { exercises :: Int
  , correct :: Int
  , timeElapsed :: Float
  }

instance Show Stats where
  show x = "Number of exercises: " ++ show (exercises x) ++ "\n" ++ 
           "Number of correct answers: " ++ show (correct x) ++ "\n" ++
           "Time elapsed: " ++ show (timeElapsed x) ++ "\n"

play gameConfig stats cont = do
  let formatResult   = roundToDec 2 -- (fractionalPart (expressionConfig gameConfig)) 
      compareResults = cmpDouble 2 -- (fractionalPart (expressionConfig gameConfig)) 

  expr <- evalRandIO $ generateExpression (expressionConfig gameConfig)

  putStr "What is: "
  print expr

  answer <- getAnswer :: IO Double
  
  let expected = eval expr

  newStats <-
    if answer `compareResults` expected 
      then do putStrLn "Correct!"
              return $ stats { 
                  correct = correct stats + 1
                , exercises = exercises stats + 1 
              }
      else do putStrLn ("Err, the correct answer was: " ++ show (formatResult expected))
              return $ stats { 
                exercises = exercises stats + 1 
              }
  
  cont newStats

startGame gameConfig = do

  let initialStats = Stats { exercises = 0, correct = 0, timeElapsed = 0.0 }

  case playStyle gameConfig of
    Timeout n    -> do
      let gameLoop resultRef = do 
            r <- readIORef resultRef
            play gameConfig r (cont resultRef)

          cont resultRef stats = do
            writeIORef resultRef stats
            gameLoop resultRef

      runFor n initialStats gameLoop

    XQuestions n -> do
      let gameLoop stats =
            if (exercises stats) < n
              then play gameConfig stats gameLoop
              else return stats

      gameLoop initialStats

getAnswer :: (Read a) => IO a
getAnswer = do
  putStr "Answer: "
  x <- getLine
  maybe (putStrLn "Not a number, try again" >> getAnswer) return (readMaybe x)

runFor minutes initialValue io = do
  let microsec = minutes * 10 ^ 6 * 60
  result <- newIORef initialValue
  timeout (minutes * 10^6 * 60) (io result)
  readIORef result

calctimeElapsed start end = 
  let diff = diffClockTimes end start
      min  = fromIntegral $ tdMin diff
      secs = (fromIntegral $ tdSec diff) / 10 ^ (fromIntegral $ length $ digits 10 (tdSec diff))
  in min + secs

mul = 
  let negative = False 
  in
    GameConfig { 
        playStyle = XQuestions 10
      , expressionConfig = Config {
          maxNumOps = 2
        , integerPart = uniform [1,2]
        , fractionalPart = uniform [0]
        , opProb = 1 % 6
        , operations = uniform [Mul]
        , primitives = \integerPart fractionalPart -> do
              let min = if negative then (-10 ^ integerPart) else 1
                  max = 10 ^ integerPart

              prim <- uniform 
                [ (Fx . IntV) <$> getRandomR (min, max) ]

              prim
        }
      } 

superEasy = 
  let negative = False 
  in
    GameConfig { 
        playStyle = XQuestions 10
      , expressionConfig = Config {
          maxNumOps = 0
        , integerPart = nonuniform [(1, 2 % 3), (2, 1 % 3)] -- uniform [1]
        , fractionalPart = uniform [0] -- error "none"
        , opProb = 1 % 12
        , operations = nonuniform [(Sub, 2 % 6), (Add,2 % 6), (Mul, 1 % 6), (Div, 1 % 6)]
        , primitives = \integerPart fractionalPart -> do
              let min = if negative then (-10 ^ integerPart) else 1
                  max = 10 ^ integerPart

              prim <- uniform 
                [ (Fx . IntV) <$> getRandomR (min, max) ]

              prim
        }
      } 

--easy = 
--  GameConfig { 
--      playStyle = XQuestions 80
--    , expressionConfig = Config {
--        maxNumOps = 1
--      , integerPart = uniform [1,2]
--      , fractionalPart = uniform [1]
--      , opProb = 1 % 12
--      , operations = nonuniform [(Add,3 % 6), (Mul, 2 % 6), (Div, 1 % 6)]
--      , primitives = \integerPart fractionalPart -> do
--            let min = if negative then (-10 ^ integerPart) else 0
--                max = 10 ^ integerPart

--            prim <- uniform 
--              [ (Fx . IntV) <$> getRandomR (min, max)
--              , (Fx . DoubleV . roundToDec fractionalPart) <$> (getRandomR (fromIntegral min, fromIntegral max) :: Dist Double)
--              ]

--            prim
--      }
--    } 

main = do
  start <- getClockTime

  let negative = False

  stats <- startGame mul

  end <- getClockTime

  return $ stats { timeElapsed = calctimeElapsed start end }


