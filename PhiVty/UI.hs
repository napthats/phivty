module PhiVty.UI
       (
       UIData,
       initialPhiUI,
       runPhiUI,
       setMap,
       setMessage
       ) where

import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Control.Concurrent

data UIData = UIData {
            ui_collection :: Collection,
            ui_maptext :: Widget FormattedText,
            ui_message :: Widget FormattedText
            }

initialPhiUI :: IO UIData
initialPhiUI = do
  e <- editWidget
  -- tentative
  e `onActivate` error "exit"
  fg <- newFocusGroup
  _ <- addToFocusGroup fg e
  mes_plain <- plainText (T.pack "hi")
  mes <- centered mes_plain
  maptext <- plainText (T.pack $ makeMapString initialMapList [((3, 3), "m")])
  mp <- bordered maptext >>= centered
  main_box <- (return mp <++> return mes) <--> (return e)
  c <- newCollection
  _ <- addToCollection c main_box fg
  return $ UIData {ui_collection = c, ui_maptext = maptext, ui_message = mes_plain}

runPhiUI :: UIData -> IO ()
runPhiUI uidata = runUi (ui_collection uidata) defaultContext

setMap :: UIData -> String -> [((Int, Int), String)] -> IO ()
setMap uidata str chara_list =
  schedule $ setText (ui_maptext uidata) (T.pack $ makeMapString str chara_list)

setMessage :: UIData -> String -> IO ()
setMessage uidata str =
  schedule $ setText (ui_message uidata) (T.pack $ str)

initialMapList :: String
initialMapList = "????????>% o=??#|{I@??    H??_T:+/??_:::H????????"

mapSize :: Int
mapSize = 7

makeMapString :: String -> [((Int, Int), String)] -> String
makeMapString map_list chara_list =
  let get_chara (x, y) default_chip ord list =
        let filtered_list = (filter (\((xx, yy), _) -> x == xx && y == yy)) list in
        if ord >= 0 && ord < length filtered_list 
          then snd $ filtered_list !! ord
          else default_chip
  in
  fst $ foldl (\(str, (hord, vord)) chr -> let [fchip, schip] = mapChipToString chr in ((str ++ get_chara (hord, vord) [fchip] 0 chara_list ++ get_chara (hord, vord) [schip] 1 chara_list ++ if hord == mapSize - 1 then "\n" else ""), if hord == mapSize - 1 then (0, vord+1) else (hord + 1, vord))) ("", (0, 0)) map_list

mapChipToString :: Char -> String
mapChipToString p =
  case p of
    ' ' -> "  "
    'o' -> ". "
    ':' -> "::"
    '+' -> "+:"
    '_' -> "__"
    'x' -> "**"
    '/' -> "//"
    '>' -> "><"
    's' -> "  "
    'H' -> "$$"
    '[' -> "[]"
    '#' -> "##"
    'I' -> "!!"
    '|' -> "||"
    'T' -> "^^"
    '=' -> "=="
    '@' -> "@@"
    '?' -> "??"
    -- special
    '%' -> "%%"
    '{' -> "{}"
    _ -> error $ p : " is unknown map chip."
