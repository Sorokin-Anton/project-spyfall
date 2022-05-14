{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Spyfall
import Options.Generic
import System.IO (hFlush, stdout)


data Opts w = Opts
    { players :: w ::: Maybe Int  <?> "Number of players"
    , places  :: w ::: Maybe FilePath <?> "Custom file with places (separated by newlines)"
    , lang :: w ::: Lang <!> "RU" <?> "RU/EN, language of game interface and default places"
    } deriving Generic

instance ParseField Lang
instance ParseRecord Lang
instance ParseFields Lang
instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  opts :: Opts Unwrapped <- unwrapRecord "spyfall game"
  playerNumber <- case opts.players of
    Just n -> pure n
    Nothing ->  do
       putStr $ case opts.lang of RU -> "Введите число игроков: "; EN -> "Enter number of players: "
       hFlush stdout
       readLn

  placesList <- case opts.places of
    Nothing -> pure $ defaultPlaces opts.lang
    Just file -> lines <$> readFile file
  runGame placesList playerNumber opts.lang
