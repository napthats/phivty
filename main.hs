import PhiVty.UI
import PhiVty.DB
import PhiVty.Socket
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.List
import Codec.Text.IConv
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Binary.UTF8.String

main :: IO ()
main = do 
  uidata <- initialPhiUI
  forkIO $ do
    let new_db = initialDB 0 $ setMessage uidata
    tchan <- atomically newTChan
    let recv_handler mes =
          atomically $ writeTChan tchan (decodeString . unpack . convert "SJIS" "UTF-8" . pack $ mes)
    soc <- connect "49.212.144.158" 20017 recv_handler
    send "#open guest3" soc
    send "#map-iv 1" soc
    send "#status-iv 1" soc
    send "#version-cli 05103010" soc
    let loop db = do
          new_mes <- atomically $ readTChan tchan
          (_, next_db) <- runDB db $ do
            old_mes_list <- getMessageLog
            let new_mes_list = new_mes : old_mes_list
            dbprint $ intercalate "\n" $ reverse new_mes_list
            setMessageLog new_mes_list
          loop next_db
    loop new_db
  runPhiUI uidata
