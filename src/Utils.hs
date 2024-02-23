module Utils 
  ( randFloat
  , randString'
  , randChar
  , randInt
  , sigmoid
  , randomChoice
  ) where


import System.Random ( getStdRandom, randomR, randomRIO )
import Data.Char     ( ord )



randFloat :: IO Float
randFloat = getStdRandom (randomR (-1, 1))


randString' :: [IO Char]
randString' = randChar : randString'


randInt :: (Int, Int) -> IO Int
randInt = randomRIO


randChar :: IO Char
randChar = getStdRandom (randomR (' ', 'z'))


sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x) )

randomChoice :: [a] -> IO a
randomChoice xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index
