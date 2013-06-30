import PhiVty.UI
import PhiVty.DB
import Control.Concurrent

main :: IO ()
main = do 
  uidata <- initialPhiUI
  forkIO $ do
    let db = initialDB 0 print
    return ()
  runPhiUI uidata
  return ()
