module PhiVty.Cdo (
           Cdo(),
           newCdo,
           getMonad,
           cdo,
           ) where

import Control.Concurrent


data Cdo a = Cdo (MVar a)

newCdo :: (Monad m) => IO (Cdo (m ()))
newCdo = do
  mvar <- newMVar $ return ()
  return $ Cdo mvar

getMonad :: (Monad m) => Cdo (m ()) -> IO (m ())
getMonad (Cdo mvar) = do
  m <- takeMVar mvar
  putMVar mvar $ return ()
  return m

cdo :: (Monad m) => Cdo (m ()) -> m () -> IO ()
cdo (Cdo mvar) m = do
  old_m <- takeMVar mvar
  let new_m = do
                _ <- old_m
                _ <- m
                return ()
  putMVar mvar new_m

