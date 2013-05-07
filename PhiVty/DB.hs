{-# LANGUAGE RankNTypes, ExistentialQuantification, ImpredicativeTypes #-}

module PhiVty.DB (
                 runDB,
                 initialDB,
                 getRandomInt,
                 updateTime,
                 readTime
          ) where

import Control.Monad.ST
import Control.Monad.State
import Data.STRef
import System.Random

data DB a = DB {internalRunDB :: forall s. DBContext s -> ST s a}

instance Monad DB where
  return a = DB $ const $ return a
  k >>= m = DB $ \context -> do
    result <- internalRunDB k context 
    internalRunDB (m result) context

runDB :: DBData -> DB a -> (a, DBData)
runDB db dbAction =
  runST $ do
    context <- newSTRef db
    result <- internalRunDB dbAction $ context
    new_db <- readSTRef context
    return (result, new_db)

type DBContext s = STRef s DBData

data DBData = DBData {
  db_time :: Int,
  db_randomgen :: StdGen,
  db_messagelist :: [String]
}

getRandomInt :: DB Int
getRandomInt =
  DB $ \st -> do
    db_data <- readSTRef st
    let (value, next_gen) = next $ db_randomgen db_data
    writeSTRef st $ db_data {db_randomgen = next_gen}
    return value

initialDB :: Int -> DBData
initialDB random_gen = DBData {
  db_time = 0,
  db_randomgen = mkStdGen random_gen,
  db_messagelist = [] }

updateTime :: DB ()
updateTime =
  DB $ \st -> modifySTRef st (\db_data -> db_data {db_time = db_time db_data + 1})

readTime :: DB Int
readTime =
  DB $ \st -> readSTRef st >>= (\x -> return $ db_time x)
