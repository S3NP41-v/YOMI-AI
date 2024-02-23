{-# LANGUAGE OverloadedStrings #-}
module Settings
  ( Settings(..)
  , encodePretty
  , readSettings
  ) where


import Data.Default
import Data.Aeson
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Functor ((<&>))
import qualified Data.ByteString.Lazy as BL



readSettings :: IO Settings
readSettings = do
  file <- BL.readFile "data/settings.json"
  case decode file :: Maybe Settings of
    Nothing       -> error "could not read settings"
    Just settings -> return settings


data Settings = Settings
  { mutationRate      :: Float
  , mutationSeverity  :: Float
  , population        :: Int
  , tournamentSize    :: Int
  , cores             :: Int
  } deriving ( Show, Read )


instance Default Settings where
  def = Settings
    { mutationRate      = 0.05
    , mutationSeverity  = 0.1
    , population        = 16
    , tournamentSize    = 2
    , cores             = 6
    }

instance FromJSON Settings where
  parseJSON (Object v) =
    Settings <$> v .: "mutationRate"
             <*> v .: "mutationSeverity"
             <*> v .: "population"
             <*> v .: "tournamentSize"
             <*> v .: "cores"

instance ToJSON Settings where
  toJSON (Settings mutationRate mutationSeverity population tournamentSize cores) =
    object  [ "mutationRate"      .= mutationRate
            , "mutationSeverity"  .= mutationSeverity
            , "population"        .= population
            , "tournamentSize"    .= tournamentSize
            , "cores"             .= cores
            ]
