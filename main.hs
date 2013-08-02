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
import PhiVty.Cdo
import Data.Char


main :: IO ()
main = do 
  let new_dbdata = initialDB 0
  c <- newCdo
  tchan <- atomically newTChan
  let recv_handler mes =
--        atomically $ writeTChan tchan (decodeString . unpack . convert "SJIS" "UTF-8" . pack $ mes)
        atomically $ writeTChan tchan mes
  soc <- connect "49.212.144.158" 20017 recv_handler
  uidata <- initialPhiUI soc c
  _ <- forkIO $ do
    let loop dbdata = do
          m <- getMonad c
          (_, next_dbdata) <- runDB dbdata $ do
            _ <- m
            return ()
--            mes_list <- getMessageLog
--            lift $ setMessage uidata $ intercalate "\n" $ reverse mes_list
          threadDelay 100000
          loop next_dbdata
    loop new_dbdata
  _ <- forkIO $ do
    send "#open guest3" soc
    send "#map-iv 1" soc
    send "#status-iv 1" soc
    send "#version-cli 05103010" soc
    send "#ex-switch eagleeye=form" soc
    send "#ex-map size=57" soc
    send "#ex-map style=turn" soc
    send "#ex-switch ex-move-recv=true" soc
    send "#ex-switch ex-list-mode-end=true" soc
    send "#ex-switch ex-disp-magic=false" soc
    let loop u_mes = do
          new_mes <- atomically $ readTChan tchan
          case parse u_mes new_mes of
            NormalMessage n_mes_raw -> do
              let n_mes = decodeString . unpack . convert "SJIS" "UTF-8" . pack $ n_mes_raw
              cdo c $ do
                old_mes_list <- getMessageLog
                let new_mes_list = n_mes : old_mes_list
                lift $ setMessage uidata $ intercalate "\n" $ reverse new_mes_list
                setMessageLog new_mes_list
              loop Nothing
            Map (m_chip_string, m_op_string, chara_list) -> do
              setMap uidata m_chip_string chara_list
--              cdo c $ do
--                old_mes_list <- getMessageLog
--                let new_mes_list = (concat (map show (map ord m_op_string))) : old_mes_list
--                lift $ setMessage uidata $ intercalate "\n" $ reverse new_mes_list
--                setMessageLog new_mes_list
              loop Nothing
            Unfinished u -> loop $ Just u
            Unknown -> loop Nothing
    loop Nothing
  runPhiUI uidata
