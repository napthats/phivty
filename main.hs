import PhiVty.Ui
import PhiVty.DB

main :: IO ()
main = do 
  let db = initialDB 0 print
  (value, new_db) <- runDB db $ do
        rand1 <- getRandomInt
        rand2 <- getRandomInt
        dbprint "hi"
        time1 <- getTime
        dbprint $ show time1
        updateTime
        updateTime
        time2 <- getTime
        return (rand1, rand2, time1, time2)
  print value
  print "end"
  runDB db $ do {
                dbprint "...";
                return ()
                }
  return ()

--  runPhiUi
