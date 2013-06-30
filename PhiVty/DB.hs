{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module PhiVty.DB (
                 runDB,
                 dbprint,
                 initialDB,
                 getRandomInt,
          ) where

import Control.Monad.ST
import Control.Monad.State
import Data.STRef
import System.Random


data DB m a = DB {internalRunDB :: forall s. DBContext s m -> ST s a}

instance Monad (DB l) where
  return a = DB $ const $ return a
  k >>= m = DB $ \context -> do
    result <- internalRunDB k context 
    internalRunDB (m result) context

runDB :: (Monad m) => DBData m -> DB m a -> m (a, DBData m)
runDB db dbAction =
  let (result, next_db) = runST $ do {
    context <- newSTRef db;
    result <- internalRunDB dbAction $ context;
    next_db <- readSTRef context;
    return (result, next_db) }
  in
  let messagelist = reverse $ db_messagelist next_db in
  let result_db = next_db {db_messagelist = []} in
  do {
     mapM_ (db_printfunc result_db) messagelist;
     return (result, result_db)
     }

dbprint :: String -> DB m ()
dbprint message =
  DB $ \st -> modifySTRef st (\db_data -> db_data {db_messagelist = message : db_messagelist db_data})

type DBContext s m = STRef s (DBData m)

data DBData m = DBData {
  db_randomgen :: StdGen,
  db_messagelist :: [String],
  db_printfunc :: (Monad m) => String -> m ()
}

getRandomInt :: DB m Int
getRandomInt =
  DB $ \st -> do
    db_data <- readSTRef st
    let (value, next_gen) = next $ db_randomgen db_data
    writeSTRef st $ db_data {db_randomgen = next_gen}
    return value

initialDB :: Int -> (String -> m ()) -> DBData m
initialDB random_gen print_func = DBData {
  db_randomgen = mkStdGen random_gen,
  db_messagelist = [],
  db_printfunc = print_func}
