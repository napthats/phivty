{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module PhiVty.DB (
                 runDB,
                 initialDB,
                 getRandomInt,
                 getMessageLog,
                 setMessageLog,
                 getPrevList,
                 setPrevList,
                 getUIState,
                 setUIState,
                 getCollectionType,
                 setCollectionType,
                 DB(),
                 DBData(),
          ) where


import Control.Monad.ST.Trans
import Control.Monad.State
import System.Random
import PhiVty.Data.UI


modifySTRef :: Monad m => STRef s a -> (a -> a) -> STT s m ()
modifySTRef str func = do
  x <- readSTRef str
  writeSTRef str (func x)

data DB m a = DB {internalRunDB :: forall s. DBContext s -> STT s m a}

instance (Monad l) => Monad (DB l) where
  return a = DB $ const $ return a
  k >>= m = DB $ \context -> do
    result <- internalRunDB k context 
    internalRunDB (m result) context

instance MonadTrans DB where
  lift c =
    DB $ \_ -> lift c

runDB :: (Monad m) => DBData -> DB m a -> m (a, DBData)
runDB db dbAction = do
  (result, next_db) <- runST $ do {
    context <- newSTRef db;
    result <- internalRunDB dbAction $ context;
    next_db <- readSTRef context;
    return (result, next_db) }
--  let messagelist = reverse $ db_messagelist next_db
  let result_db = next_db {db_messagelist = []}
  return (result, result_db)

type DBContext s = STRef s DBData

data DBData = DBData {
  db_randomgen :: StdGen,
  db_messagelist :: [String],
  db_phimessagelog :: [String],
  db_prevphilist :: [String],
  db_uistate :: UIState,
  db_collectiontype :: CollectionType
}

getMessageLog :: Monad m => DB m [String]
getMessageLog =
  DB $ \st -> readSTRef st >>= (\x -> return $ db_phimessagelog x)

setMessageLog :: Monad m => [String] -> DB m ()
setMessageLog mes_list =
  DB $ \st -> modifySTRef st (\db_data -> db_data {db_phimessagelog = mes_list})

getPrevList :: Monad m => DB m [String]
getPrevList =
  DB $ \st -> readSTRef st >>= (\x -> return $ db_prevphilist x)

setPrevList :: Monad m => [String] -> DB m ()
setPrevList mes_list =
  DB $ \st -> modifySTRef st (\db_data -> db_data {db_prevphilist = mes_list})

getCollectionType :: Monad m => DB m CollectionType
getCollectionType =
  DB $ \st -> readSTRef st >>= (\x -> return $ db_collectiontype x)

setCollectionType :: Monad m => CollectionType -> DB m ()
setCollectionType collectiontype =
  DB $ \st -> modifySTRef st (\db_data -> db_data {db_collectiontype = collectiontype})

getUIState :: Monad m => DB m UIState
getUIState =
  DB $ \st -> readSTRef st >>= (\x -> return $ db_uistate x)

setUIState :: Monad m => UIState -> DB m ()
setUIState ui_state =
  DB $ \st -> modifySTRef st (\db_data -> db_data {db_uistate = ui_state})

getRandomInt :: Monad m => DB m Int
getRandomInt =
  DB $ \st -> do
    db_data <- readSTRef st
    let (value, next_gen) = next $ db_randomgen db_data
    writeSTRef st $ db_data {db_randomgen = next_gen}
    return value

initialDB :: Int -> DBData
initialDB random_gen = DBData {
  db_randomgen = mkStdGen random_gen,
  db_messagelist = [],
  db_phimessagelog = [],
  db_prevphilist = [],
  db_uistate = UINormal,
  db_collectiontype = CTNormal
}
