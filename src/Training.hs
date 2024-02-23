module Training 
  ( trainEvolutionary
  ) where


import Settings
import Model
import Utils                    ( randFloat )

import Control.Concurrent       ( forkIO, setNumCapabilities, newEmptyMVar, readMVar )
import Control.Concurrent.MVar  ( newMVar, putMVar, MVar )

import Control.Monad            ( replicateM, when )
import System.Random            ( getStdRandom, randomR )
import Data.Functor             ( (<&>) )
import Data.List                ( sortBy, cycle )
import Data.Bool                ( bool )
import Data.Foldable            ( foldlM )




-- overloaded version without logging
trainEvolutionary :: ModelConf -> Settings -> FitnessEval -> IO Model
trainEvolutionary = trainEvolutionary' (\_ -> pure ())



-- version with logging
trainEvolutionary' :: Logger -> ModelConf -> Settings -> FitnessEval -> IO Model
trainEvolutionary' logger modelConf = do
  -- initialise a random population of the given model
  

  -- start a loop 
  
  
  
  
  
  undefined






-- a logger to keep track of progress
type Logger      = (Float, Model) -> IO ()

-- a function to evaluate the fitness of a model's output given an input
-- where the closer to 0 you are (the closer to expected output) the better
type FitnessEval = [Float] -> [Float] -> Float
