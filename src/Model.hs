{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
module Model
  ( Model(..)
  , ModelConf(..)
  , writeModel
  , readModel
  , makeRandomModel
  , forwardPropagate
  ) where


import Utils

import System.Random ( getStdRandom, randomR )
import Control.Monad ( replicateM )
import Data.Functor  ( (<&>) )
import Data.Char     ( ord )
import Data.List     ( transpose )




forwardPropagate :: [Float] -> Model -> [Float]
forwardPropagate fs (Model bs ws) = forwardPropagate' fs bs (transpose ws)


forwardPropagate' :: [Float] -> [[Float]] -> [[Float]] -> [Float]
forwardPropagate' fs []     ws = fs
forwardPropagate' fs (b:bs) ws = forwardPropagate' (zipWith (\i b -> wSum fs (ws !! i) + b) [0..] b) bs ws


wSum :: [Float] -> [Float] -> Float
wSum []     _      = 0
wSum _      []     = 0
wSum (n:ns) (w:ws) = n * w + wSum ns ws

makeRandomModel :: ModelConf -> IO Model
makeRandomModel mc = do
  inpWeights <- replicateM (inputs mc)     (replicateM (hiddenN mc) randFloat)

  hidWeights <- replicateM (hiddenL mc -1) (replicateM (hiddenN mc) randFloat)

  outWeights <- replicateM (hiddenN mc)    (replicateM (outputs mc) randFloat)

  biases     <- replicateM (hiddenL mc)    (replicateM (hiddenN mc) randFloat)
  out        <- replicateM (outputs mc)    randFloat

  return $ Model (biases <> [out]) (inpWeights <> hidWeights <> outWeights)


writeModel :: FilePath -> Model -> IO ()
writeModel fp model = writeFile fp ("Model {biases = " ++ show (biases model) ++ ", weights = " ++ show (weights model) ++ "}")

readModel :: FilePath -> IO Model
readModel fp = readFile fp <&> read


data ModelConf = ModelConf
  { inputs  :: Int
  , hiddenL :: Int
  , hiddenN :: Int
  , outputs :: Int
  }

data Model = Model
  { biases  :: [[Float]]
  , weights :: [[Float]]
  } deriving ( Read )
