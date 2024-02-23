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

-- todo: rewrite training code


main :: IO ()
main = do
  undefined

  -- settings <- readSettings

  -- hwModel <- train hwModelConf settings
  -- writeModel "data/hwModel.model" hwModel


-- main :: IO ()
-- main = do
--   hwModel <- readModel "data/hwModel.model"

--   inputs <- replicateM 100 randInput

--   mapM_ (\inp -> printf "%s\npos given: %.4f\npos expected: %d\nevaluation: %.4f\nstring: %s\n" (replicate 50 '-') (head (forwardPropagate (vals inp) hwModel)) (pos inp) (evaluateFromInput hwModel inp) (string inp)) inputs




logger :: (Float, Model) -> IO ()
logger (f, model) = printf "evaluation: %.4f   \n" f


hwModelConf :: ModelConf
hwModelConf = ModelConf {inputs = 24, hiddenL = 2, hiddenN = 12, outputs = 1}



randInput :: IO Input
randInput = do
  pre   <- getStdRandom (randomR (0, 12))       :: IO Int

  pre' <- sequence $ take pre randString'
  after' <- sequence $ take (12 - pre) randString'

  let xs = pre' ++ "Hello World!" ++ after'
  let fs = map (\x -> 1 / fromIntegral (ord x)) xs

  return (Input xs fs pre)


-- the actuall string, input values, and the position
data Input = Input {string :: String, vals :: [Float], pos :: Int}
  deriving ( Show )
