module Utils 
  ( randFloat
  , randString'
  , randChar
  , sigmoid
  ) where


import System.Random ( getStdRandom, randomR )
import Data.Char     ( ord )



randFloat :: IO Float
randFloat = getStdRandom (randomR (-1, 1))


randString' :: [IO Char]
randString' = randChar : randString'


randChar :: IO Char
randChar = getStdRandom (randomR (' ', 'z'))


sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x) )

