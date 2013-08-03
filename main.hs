import PhiVty.UI
import PhiVty.DB
import PhiVty.Socket
import PhiVty.Protocol
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Codec.Text.IConv
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Binary.UTF8.String
import PhiVty.Cdo
import System.Environment


main :: IO ()
main = do 
  args <- getArgs
  if length args /= 3 then error "main ip port id" else do
  let new_dbdata = initialDB 0
  c <- newCdo
  tchan <- atomically newTChan
  let recv_handler mes =
        atomically $ writeTChan tchan mes
  soc <- connect (args !! 0) (read (args !! 1) :: Int) recv_handler
  uidata <- initialPhiUI soc c
  _ <- forkIO $ do
    let loop dbdata = do
          m <- getMonad c
          (_, next_dbdata) <- runDB dbdata $ do
            _ <- m
            return ()
          threadDelay 100000
          loop next_dbdata
    loop new_dbdata
  _ <- forkIO $ do
    send ("#open " ++ (args !! 2)) soc
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
              addMessage uidata c n_mes
              loop Nothing
            Map (m_dir, m_chip_string, m_op_string, chara_list) -> do
              setMap uidata m_chip_string m_op_string chara_list
              setDirection uidata [m_dir]
              loop Nothing
            Unfinished u -> loop $ Just u
            ExNotice (key, value) ->
              case key of
                "land" -> do
                  setLandName uidata value
                  loop Nothing
                "area" -> do
                  setAreaName uidata value
                  loop Nothing
                _ -> loop Nothing
            Unknown mes -> do
              addMessage uidata c $ '#' : mes
              loop Nothing
    loop Nothing
  runPhiUI uidata
