module Training 
  ( trainEvolutionary
  , trainEvolutionary'
  , randomModels
  , LoggingData(..)
  ) where

import Settings
import Model
import Utils

import Control.Concurrent       ( forkIO, setNumCapabilities, newEmptyMVar, killThread )
import Control.Concurrent.MVar  ( newMVar, putMVar, readMVar, tryReadMVar, MVar )
import Control.Monad            ( replicateM, when )
import System.Random            ( getStdRandom, randomR )
import System.Exit              ( exitSuccess )
import Data.Functor             ( (<&>) )
import Data.List                ( sortBy, cycle, minimumBy, maximumBy )
import Data.Bool                ( bool )
import Data.Foldable            ( foldlM )


-- overloaded version without logging
trainEvolutionary :: ModelConf -> Settings -> FitnessEval -> IO Model
trainEvolutionary = trainEvolutionary' (\_ -> return ())


-- version with logging
trainEvolutionary' :: Logger -> ModelConf -> Settings -> FitnessEval -> IO Model
trainEvolutionary' logger modelConf settings fitEval = do
  setNumCapabilities (cores settings)

  putStrLn "training begin with settings:"
  print settings


  -- MVar channels for communication  
  channels <- replicateM (cores settings) newEmptyMVar

  -- starting a training loop for each core specified in settings
  threads <- mapM (\(channel, id') -> do
    models <- randomModels modelConf (population settings)
    forkIO $ trainingLoop logger 0 models settings fitEval channel id'
    ) (zip channels [0..])


  final <- getFirstChannel channels

  -- returning best model
  return (snd final)


trainingLoop :: Logger -> Iteration -> [Model] -> Settings -> FitnessEval -> MVar (Fitness, Model) -> Int -> IO ()
trainingLoop logger iteration models settings fitEval channel tID = do
  -- evaluate scores
  scored <- mapM fitEval models

  -- a call to logger
  logger $ LoggingData (bestModel' scored) iteration tID scored

  case satisfaction settings of
    Iterations n -> when (iteration >= n) $ putMVar channel (bestModel' scored)               >> exitSuccess
    Fitness    f -> when (f <= fst (bestModel' scored)) $ putMVar channel (bestModel' scored) >> exitSuccess

  -- choose best ones
  let tournament = map snd (take (tournamentSize settings) (sortModels scored))

  -- reproduce + mutate
  children <- replicateM (population settings - tournamentSize settings) (reproduce settings tournament)

  -- begin anew
  trainingLoop logger (iteration + 1) (tournament ++ children) settings fitEval channel tID


getFirstChannel :: [MVar (Fitness, Model)] -> IO (Fitness, Model)
getFirstChannel xs = do
  mc <- getFirstChannel' xs
  case mc of
    Just m  -> return m
    Nothing -> getFirstChannel xs


getFirstChannel' :: [MVar (Fitness, Model)] -> IO (Maybe (Fitness, Model))
getFirstChannel' []     = return Nothing
getFirstChannel' (x:xs) = do
  mc <- tryReadMVar x
  case mc of
    Just m -> return $ Just m
    Nothing -> getFirstChannel' xs


reproduce :: Settings -> [Model] -> IO Model
reproduce settings  models = do
  alpha <- randomChoice models
  omega <- randomChoice models

  crossover settings alpha omega


crossover :: Settings -> Model -> Model -> IO Model
crossover settings alpha omega = do
  wSigma <- randInt (1, length (weights alpha) - 2) >>= \split -> return $ take split (weights alpha) <> drop split (weights omega)
  bSigma <- randInt (1, length (biases  alpha) - 2) >>= \split -> return $ take split (biases  alpha) <> drop split (biases  omega)

  mutate <- randFloat <&> (<=) (mutationRate settings) . abs

  if mutate 
  then do
    wSigma' <- mapM (mutateList (mutationSeverity settings)) wSigma
    bSigma' <- mapM (mutateList (mutationSeverity settings)) bSigma
    return $ Model bSigma' wSigma'

  else
    return $ Model bSigma wSigma


mutateList :: Float -> [Float] -> IO [Float]
mutateList severity = mapM (\f -> randFloat >>= \r -> return $ f + (severity * r))


randomModels :: ModelConf -> Int -> IO [Model]
randomModels mc i = replicateM i (makeRandomModel mc)


sortModels :: [(Fitness, Model)] -> [(Fitness, Model)]
sortModels = sortBy (\(f1, _) (f2, _) -> compare f2 f1)

bestModel :: [(Fitness, Model)] -> Model
bestModel = snd . maximumBy (\(f1, _) (f2, _) -> compare f1 f2)

bestModel' :: [(Fitness, Model)] -> (Fitness, Model)
bestModel' = maximumBy (\(f1, _) (f2, _) -> compare f1 f2)


-- a logger to keep track of progress
type Logger      = LoggingData -> IO ()

data LoggingData = LoggingData
  { evaluation  :: (Fitness, Model)
  , iterations  :: Int
  , thread      :: Int
  , models      :: [(Fitness, Model)]
  }


-- a function to evaluate the fitness of a model's output given an input
type FitnessEval = Model -> IO (Fitness, Model)


type Fitness   = Float
type Iteration = Int
