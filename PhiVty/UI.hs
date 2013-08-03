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
import Graphics.Vty.LLInput
import PhiVty.Socket
import PhiVty.DB
import PhiVty.Cdo
--import Control.Concurrent

data UIData = UIData {
            ui_collection :: Collection,
            ui_maptext :: Widget FormattedText,
            ui_message :: Widget FormattedText
            }

inputHandler :: PhiSocket -> String -> IO ()
inputHandler soc mes =
  if mes == ":exit" then error "exit" else send mes soc

mapHandler :: (Monad m) => PhiSocket -> Key -> [Modifier] -> Cdo (DB m ()) -> IO ()
mapHandler soc key modList cdod =
  case key of
    KASCII '1' -> send "hit" soc
    KASCII '2' -> send "go b" soc
    KASCII '3' -> send "cast" soc
    KASCII '4' -> send "go l" soc
    KASCII '5' -> send "turn b" soc
    KASCII '6' -> send "go r" soc
    KASCII '7' -> send "turn l" soc
    KASCII '8' -> send "go" soc
    KASCII '9' -> send "turn r" soc
    KASCII '0' -> do {send "look" soc; send "check" soc}
    KASCII '.' -> send "." soc
    KASCII c -> do
      cdo cdod $ do
        old_mes_list <- getMessageLog
        let new_mes_list = (":Input " ++ [c]) : old_mes_list
        setMessageLog new_mes_list
    _ -> error "????"

initialPhiUI :: (Monad m) => PhiSocket -> Cdo (DB m ()) -> IO UIData
initialPhiUI soc cdod = do
  e <- editWidget
  -- tentative
  e `onActivate` \this -> do
    txt <- getEditText this
    inputHandler soc $ T.unpack txt
  mes_plain <- plainText (T.pack "hi")
  mes <- centered mes_plain
  maptext <- plainText (T.pack $ makeMapString initialMapList [((3, 3), "m")])
  maptext `onKeyPressed` \_ key mod_list -> do {mapHandler soc key mod_list cdod; return True}
  mp <- bordered maptext >>= centered
  main_box <- (return mp <++> return mes) <--> (return e)
  fg <- newFocusGroup
  _ <- addToFocusGroup fg e
  _ <- addToFocusGroup fg maptext
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
