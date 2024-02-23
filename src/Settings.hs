{-# LANGUAGE OverloadedStrings #-}
module Settings
  ( Settings(..)
  , Satisfaction(..)
  , encodePretty
  , readSettings
  ) where


import Data.Default
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Functor             ( (<&>) )

import qualified Data.Text as T
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
  , satisfaction      :: Satisfaction
  } deriving ( Show, Read )


instance Default Settings where
  def = Settings
    { mutationRate      = 0.05
    , mutationSeverity  = 0.3
    , population        = 16
    , tournamentSize    = 4
    , cores             = 6
    , satisfaction      = Iterations 1000
    }

instance FromJSON Settings where
  parseJSON (Object v) =
    Settings <$> v .: "mutationRate"
             <*> v .: "mutationSeverity"
             <*> v .: "population"
             <*> v .: "tournamentSize"
             <*> v .: "cores"
             <*> v .: "satisfaction"

instance ToJSON Settings where
  toJSON (Settings mutationRate mutationSeverity population tournamentSize cores satisfaction) =
    object  [ "mutationRate"      .= mutationRate
            , "mutationSeverity"  .= mutationSeverity
            , "population"        .= population
            , "tournamentSize"    .= tournamentSize
            , "cores"             .= cores
            , "satisfaction"      .= satisfaction
            ]


data Satisfaction
  = Iterations Int
  | Fitness Float
  deriving ( Show, Read )

instance ToJSON Satisfaction where
  toJSON (Iterations n) = String $ "Iterations " <> T.pack (show n)
  toJSON (Fitness f)    = String $ "Fitness " <> T.pack (show f)

instance FromJSON Satisfaction where
  parseJSON = withText "Satisfaction" parseSatisfaction


parseSatisfaction :: Applicative f => T.Text -> f Satisfaction
parseSatisfaction t = case T.split (== ' ') t of
  ["Iterations", n] -> pure $ Iterations (read (T.unpack n))
  ["Fitness", f]    -> pure $ Fitness (read (T.unpack f))
  _                 -> error "Could not read settings due to incorrect `satisfaction` field!"
