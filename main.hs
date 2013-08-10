import PhiVty.UI
import PhiVty.DB
import PhiVty.Socket
import PhiVty.Protocol
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import PhiVty.Cdo
import System.Environment
import PhiVty.Data.UI



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
          do {case parse u_mes new_mes of
            NormalMessage n_mes -> do
              addMessage uidata c n_mes
            Map (m_dir, m_chip_string, m_op_string, chara_list) -> do
              setMap uidata m_chip_string m_op_string chara_list
              setDirection uidata [m_dir]
            ExNotice (key, value) ->
              case key of
                "land" -> do
                  setLandName uidata value
                "area" -> do
                  setAreaName uidata value
                _ -> return ()
            PhiList list -> cdo c $ do
              setPrevList list
              lift $ mapM_ (addMessage uidata c) $ "---------------" : list ++ ["---------------"]
            SEdit -> cdo c $ do
              setUIState UISEdit
            Unfinished u -> loop $ Just u
            Unknown "" -> return ()
            Unknown mes -> do
              addMessage uidata c $ '#' : mes
          }
          loop Nothing
    loop Nothing
  runPhiUI uidata
