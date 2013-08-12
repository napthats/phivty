module PhiVty.Socket (
                     initSocket,
                     connect,
                     close,
                     send,
                     PhiSocket()
                     ) where

import Network
import System.IO
import Control.Exception
import Control.Concurrent


data PhiSocket = PhiSocket {hostName :: MVar String, port :: MVar Int, internalHandle :: MVar Handle, recvThreadId :: MVar ThreadId}

initSocket :: String -> Int -> IO PhiSocket
initSocket addr pt = do
  m_addr <- newMVar addr
  m_port <- newMVar pt
  m_handle <- newEmptyMVar
  m_recv_thread_id <- newEmptyMVar
  return $ PhiSocket {hostName = m_addr, port = m_port, internalHandle = m_handle, recvThreadId = m_recv_thread_id}

connect :: PhiSocket -> (String -> IO()) -> IO ()
connect soc recv_handler = do
  close soc
  addr <- readMVar $ hostName soc
  pt <- readMVar $ port soc
  hSetBuffering stdout NoBuffering
  h <- connectTo addr (PortNumber $ fromIntegral pt)
  hSetBuffering h LineBuffering
  tId <- forkIO $ do
    sequence_ $ repeat $ do
      --have not to be retrieve?
      --h <- readMVar $ internalHandle soc
      res <- hGetLine h
      recv_handler res
    `catch` (\(SomeException _) -> return () )
    `finally` do
      hClose h
  putMVar (internalHandle soc) h
  putMVar (recvThreadId soc) tId
  return ()

close :: PhiSocket -> IO ()
close soc = do
  maybe_tid <- tryTakeMVar $ recvThreadId soc
  case maybe_tid of
   Nothing -> return ()
   Just tid -> do
    killThread tid
    _ <- readMVar (internalHandle soc)
    _ <- readMVar (recvThreadId soc)
    return ()

send :: String -> PhiSocket -> IO ()
send mes soc = do
  maybe_handle <- tryTakeMVar $ internalHandle soc
  case maybe_handle of
   Nothing -> return ()
   Just internal_handle -> do {
     hPutStrLn internal_handle mes;
     putMVar (internalHandle soc) internal_handle}
     `catch` (\(SomeException _) -> return ())
