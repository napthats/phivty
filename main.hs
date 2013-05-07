import PhiVty.Ui
import PhiVty.DB

main :: IO ()
main = do 
  let db = initialDB 0
  let (value, new_db) = runDB db $ do
        rand1 <- getRandomInt
        rand2 <- getRandomInt
        time1 <- readTime
        updateTime
        updateTime
        time2 <- readTime
        return (rand1, rand2, time1, time2)
  print value
  return ()

--  runPhiUi
