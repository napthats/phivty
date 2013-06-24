module PhiVty.Socket (
                     connect,
                     close,
                     send,
                     PhiSocket()
                     ) where

import Network
import System.IO
import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)


data PhiSocket = PhiSocket {internalHandle :: Handle, recvThreadId :: ThreadId}

connect :: String -> Int -> (String -> IO ()) -> IO PhiSocket
connect addr port recv_handler = withSocketsDo $ do
  hSetBuffering stdout NoBuffering
  h <- connectTo addr (PortNumber $ fromIntegral port)
  hSetBuffering h LineBuffering
  tId <- forkIO $ do
    sequence_ $ repeat $ do
      res <- hGetLine h
      recv_handler res
    `catch` (\(SomeException e) -> return () )
    `finally` do
      hClose h
  return $ PhiSocket {internalHandle = h, recvThreadId = tId}

close :: PhiSocket -> IO ()
close soc =
  killThread $ recvThreadId soc

send :: String -> PhiSocket -> IO ()
send mes soc =
  hPutStrLn (internalHandle soc) mes
  `catch` (\(SomeException e) -> return ())
