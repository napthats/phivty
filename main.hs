import PhiVty.UI
import PhiVty.DB
import PhiVty.Socket
import PhiVty.Protocol
import Control.Concurrent
import Control.Monad.Trans
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.List
import Codec.Text.IConv
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Binary.UTF8.String


main :: IO ()
main = do 
  let new_dbdata = initialDB 0
  dbMVar <- newMVar $ return ()
  tchan <- atomically newTChan
  let recv_handler mes =
        atomically $ writeTChan tchan (decodeString . unpack . convert "SJIS" "UTF-8" . pack $ mes)
  soc <- connect "49.212.144.158" 20017 recv_handler
  uidata <- initialPhiUI soc dbMVar
  _ <- forkIO $ do
    send "#open guest3" soc
    send "#map-iv 1" soc
    send "#status-iv 1" soc
    send "#version-cli 05103010" soc
    let loop dbdata = do
          new_mes <- atomically $ readTChan tchan
          case parse new_mes of
            NormalMessage n_mes -> do
              db <- takeMVar dbMVar
              (_, next_dbdata) <- runDB dbdata $ do
                db
                old_mes_list <- getMessageLog
                let new_mes_list = n_mes : old_mes_list
                lift $ setMessage uidata $ intercalate "\n" $ reverse new_mes_list
                setMessageLog new_mes_list
              putMVar dbMVar $ return ()
              loop next_dbdata
            Map m_mes -> do
              setMap uidata m_mes []
              loop dbdata
            Unknown -> do
            loop dbdata
    loop new_dbdata
  runPhiUI uidata
