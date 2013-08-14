import PhiVty.UI
import PhiVty.DB
import Control.Concurrent
import PhiVty.Cdo
import System.Environment


main :: IO ()
main = do 
  args <- getArgs
  if length args /= 3 then error "main ip port id" else do
  let new_dbdata = initialDB 0
  c <- newCdo
  uidata <- initialPhiUI c [("guest", (args !! 0), (read (args !!1) :: Int)), ((args !! 2), (args !! 0), (read (args !! 1) :: Int))]
  _ <- forkIO $ do
    let loop dbdata = do
          m <- getMonad c
          (_, next_dbdata) <- runDB dbdata $ do
            _ <- m
            return ()
          threadDelay 100000
          loop next_dbdata
    loop new_dbdata
  runPhiUI uidata
