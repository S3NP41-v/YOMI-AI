module Main where

import Settings
import Model
import Training
import Utils

import Data.Default
import Data.Maybe    ( fromJust )
import Data.List     ( elemIndex )
import Data.Char     ( ord )
import System.Random
import Control.Monad ( replicateM )
import Text.Printf   ( printf )


{-
since this is my first ever AI lets make a test,
lets train an AI that will detect where the substring "Hello World!" appears in a string
you could do that with cold logic, but this is a simple test to see if i even can do it
-}


main :: IO ()
main = do
  settings <- readSettings

  -- hwModel <- trainEvolutionary' logger hwModelConf settings evaluateModel
  -- writeModel "data/hwModel.model" hwModel

  hwModel <- readModel "data/hwModel.model"
  inputs <- replicateM 100 randInput

  putStrLn "\x1b[2J\x1b[H"
  putStrLn "results:"
  mapM_ (\inp -> do
    putStrLn (replicate 100 '-')
    putStrLn $ "target:   " <> show (pos inp)
    putStrLn $ "output:   " <> show (head $ forwardPropagate (vals inp) hwModel)
    putStrLn $ "refrence: " <> string inp
        ) inputs


hwModelConf :: ModelConf
hwModelConf = ModelConf {inputs = 24, hiddenL = 2, hiddenN = 12, outputs = 1}


evaluateModel :: Model -> IO (Float, Model)
evaluateModel model = do
  inputs <- replicateM 100 randInput

  let outputs = map (\inp -> 12 - abs (fromIntegral (pos inp) - head (forwardPropagate (vals inp) model))) inputs

  let score   = sum outputs / 100

  return (score, model)


logger :: LoggingData -> IO ()
logger (LoggingData (f, model) iterations thread models) = do
  putStrLn $ ("\x1b[" <> show (thread + 5) <> ";0H ")
          <> (show thread ++ ": ")
          <> ("iteration: " ++ show iterations ++ " ")
          <> ("fitness: " ++ show f) <> replicate 10 ' '


randInput :: IO Input
randInput = do
  pre   <- getStdRandom (randomR (0, 12)) :: IO Int

  let pre' = replicate pre ' '
  let after' = replicate (12 - pre) ' '

  let xs = pre' ++ "Hello World!" ++ after'
  let fs = map (\x -> 1 / fromIntegral (ord x)) xs

  return (Input xs fs pre)


-- the actuall string, input values, and the position
data Input = Input {string :: String, vals :: [Float], pos :: Int}
  deriving ( Show )
