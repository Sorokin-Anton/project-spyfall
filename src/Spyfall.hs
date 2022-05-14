{-# LANGUAGE DeriveGeneric #-}
module Spyfall where

import System.Random
import Control.Monad (forM_)
import Data.List
import GHC.Generics

data Lang = RU | EN deriving (Generic, Show, Read, Eq)

defaultPlaces :: Lang -> [String]
defaultPlaces RU =
         ["База террористов",
          "Самолёт",
          "Киностудия",
          "Ночной клуб",
          "Партизанский отряд",
          "Церковь",
          "Полярная станция",
          "Казино",
          "Супермаркет",
          "Спа-салон",
          "Банк",
          "Хоккейная арена",
          "Корпоративная вечеринка",
          "Отель",
          "Пассажирский поезд",
          "Школа",
          "Зоопарк",
          "Полицейский участок",
          "Театр",
          "Выставка настолок",
          "Больница",
          "Орбитальная станция",
          "Овощебаза",
          "Подводная лодка",
          "Пиратский корабль",
          "Цирк-шапито",
          "Ресторан",
          "Войско крестоносцев",
          "Университет",
          "Лунапарк",
          "Воинская часть",
          "Посольство",
          "Океанский лайнер",
          "Карнавал",
          "Пляж",
          "Станция техобслуживания"
          ]
defaultPlaces EN = [
          "Aeroplane",
          "Bank",
          "Beach",
          "Boulevard Theatre",
          "Casino",
          "Cathedral",
          "Circus Tent",
          "Corporate Party",
          "Crusader Army",
          "Wellness Centre",
          "Consulate",
          "Hospital",
          "Hotel",
          "Army Base",
          "Film Studio",
          "Ocean Liner",
          "Passenger Train",
          "Pirate Ship",
          "Polar Station",
          "Police Station",
          "Restaurant",
          "School",
          "Service Station",
          "Space Station",
          "Submarine",
          "Supermarket",
          "University",
          "Funfair",
          "Art Museum",
          "Sweets Factory",
          "Cat Show",
          "Cemetery",
          "Coal Mine",
          "Construction Site",
          "Gaming Convention",
          "Petrol Station",
          "Harbour Docks",
          "Ice Hockey Stadium",
          "Gaol",
          "Jazz Club",
          "Library",
          "Night Bar",
          "Race Track",
          "Retirement Home",
          "Rock Concert",
          "Sightseeing Bus",
          "Stadium",
          "Underground",
          "The U.N.",
          "Vineyard",
          "Wedding",
          "Zoo"
          ]


runGame :: [String] -> Int -> Lang -> IO ()
runGame places playersNumber lang = do

  placeNumber <- randomRIO (0, length places - 1)
  let place = places !! placeNumber
  strings <- shuffle $  (case lang of RU -> "Ты шпион!"; EN -> "You are a spy!") : replicate (playersNumber - 1) place
  clear
  forM_ strings $ \string -> do
    _ <- getLine -- waiting new player to press enter
    putStrLn string -- telling secret info
    _ <- getLine
    clear -- hiding it and wait for other player


shuffle :: [a] -> IO [a]
shuffle arr = do
  gen <- getStdGen
  let inflist =  randomRs (0 :: Double, 1) gen
  pure . map fst . sortOn snd $ zip arr inflist

clear :: IO ()
clear = putStrLn "\ESC[2J"
