module PhiVty.UI
       (
       UIData,
       initialPhiUI,
       runPhiUI,
       setMap,
       setDirection,
       setLandName,
       setAreaName,
       setMapTitle,
       setMessage,
       addMessage,
       parsePhiTags,
       ) where

import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Graphics.Vty.LLInput
import Graphics.Vty.Attributes
import PhiVty.Socket
import PhiVty.DB
import PhiVty.Cdo
import Data.List.Split
import Control.Monad.Trans
import Data.IORef
import Text.Regex
--import Control.Concurrent

data UIData = UIData {
            ui_collection :: Collection,
            ui_maptext :: Widget FormattedText,
            ui_message :: Widget FormattedText,
            ui_maptitle :: Widget FormattedText,
            v_maptitle :: IORef (String, String, String)
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
    setEditText this $ T.pack ""
  mes_plain <- plainText (T.pack "hi")
  mes <- centered mes_plain
  titletest <- plainText (T.pack " ")
  title <- hCentered titletest
  maptext <- plainText (T.pack $ makeMapString initialMapList initialMapOptionList [((3, 3), "m")])
  maptext `onKeyPressed` \_ key mod_list -> do {mapHandler soc key mod_list cdod; return True}
  mp <- bordered maptext >>= hCentered
  main_box <- (((return title <--> return mp) >>= centered) <++> return mes) <--> (return e)
  fg <- newFocusGroup
  _ <- addToFocusGroup fg e
  _ <- addToFocusGroup fg maptext
  c <- newCollection
  _ <- addToCollection c main_box fg
  v_m <- newIORef ("", "", "")
  return $ UIData {v_maptitle = v_m, ui_collection = c, ui_maptitle = titletest, ui_maptext = maptext, ui_message = mes_plain}

runPhiUI :: UIData -> IO ()
runPhiUI uidata = runUi (ui_collection uidata) defaultContext

setMap :: UIData -> String -> [(Int, Int,Int, Int)] -> [((Int, Int), String)] -> IO ()
setMap uidata str op_list chara_list =
  schedule $ setText (ui_maptext uidata) (T.pack $ makeMapString str op_list chara_list)

setMapTitle :: UIData -> String -> IO ()
setMapTitle uidata mes =
  schedule $ setText (ui_maptitle uidata) (T.pack $ mes)

setDirection :: UIData -> String -> IO ()
setDirection uidata dir = do
  (_, land, area) <- readIORef $ v_maptitle uidata
  setMapTitle uidata $ "[" ++ dir ++ "]" ++ land ++ "(" ++ area ++ ")"
  writeIORef (v_maptitle uidata) (dir, land, area)

setLandName :: UIData -> String -> IO ()
setLandName uidata land = do
  (dir, _, area) <- readIORef $ v_maptitle uidata
  setMapTitle uidata $ "[" ++ dir ++ "]" ++ land ++ "(" ++ area ++ ")"
  writeIORef (v_maptitle uidata) (dir, land, area)
  
setAreaName :: UIData -> String -> IO ()
setAreaName uidata area = do
  (dir, land, _) <- readIORef $ v_maptitle uidata
  setMapTitle uidata $ "[" ++ dir ++ "]" ++ land ++ "(" ++ area ++ ")"
  writeIORef (v_maptitle uidata) (dir, land, area)

setMessage :: UIData -> [String] -> IO ()
setMessage uidata str_list = do
--  schedule $ setText (ui_message uidata) (T.pack $ intercalate "\n" $ str_list)
  schedule $ setTextWithAttrs (ui_message uidata) (concatMap parsePhiTags str_list)
--  schedule $ setTextWithAttrs (ui_message uidata) [((T.pack $ str), fgColor green)]

parsePhiTags :: String -> [(T.Text, Attr)]
parsePhiTags str = _parse def_attr str
  where _parse attr _str =
          let tag2Attr tag =
               case splitOn "=" (drop 2 $ take (length tag - 2) tag) of
                 ["color", color] ->
                   case color of
                     "black" -> fgColor black
                     "red" -> fgColor red
                     "green" -> fgColor green
                     "yellow" -> fgColor yellow
                     "blue" -> fgColor blue
                     "magenta" -> fgColor magenta
                     "cyan" -> fgColor cyan
                     "white" -> fgColor white
                     "+hp" -> fgColor green
                     "+mp" -> fgColor blue
                     "-hp" -> fgColor red
                     "-mp" -> fgColor red
                     "." -> fgColor white
                     _ -> def_attr
                 _ -> def_attr
               in
          case matchRegexAll (mkRegex "/[*][^*]*[*]/") _str of
            Nothing -> [(T.pack (_str ++ "\n"), attr)]
            Just (before_str, tag, after_str, _) -> (T.pack before_str, attr) : (_parse (tag2Attr tag) after_str)



addMessage :: UIData -> (Cdo (DB IO ())) -> String -> IO ()
addMessage uidata c mes = cdo c $ do
  old_mes_list <- getMessageLog
  let new_mes_list = mes : old_mes_list
  lift $ setMessage uidata $ reverse new_mes_list
  setMessageLog $ fst $ splitAt 50 new_mes_list

initialMapList :: String
initialMapList = "????????>% o=??#|{I@??    H??_T:+/??_:::H????????"

initialMapOptionList :: [(Int, Int, Int, Int)]
initialMapOptionList = replicate 49 (0,0,0,0)

mapSize :: Int
mapSize = 7

makeMapString :: String -> [(Int, Int, Int, Int)] -> [((Int, Int), String)] -> String
makeMapString map_list op_list chara_list =
  let get_chara (x, y) default_chip ord list =
        let filtered_list = (filter (\((xx, yy), _) -> x == xx && y == yy)) list in
        if ord >= 0 && ord < length filtered_list 
          then snd $ filtered_list !! ord
          else default_chip
  in
  fst $ foldl (\(str, (hord, vord)) (chr, (i_type, m_flag, _, _)) -> let [fchip, schip] = mapChipToString chr in let schip_or_im = if m_flag > 0 then 't' else if i_type > 0 then 'o' else schip in ((str ++ get_chara (hord, vord) [fchip] 0 chara_list ++ get_chara (hord, vord) [schip_or_im] 1 chara_list ++ if hord == mapSize - 1 then "\n" else ""), if hord == mapSize - 1 then (0, vord+1) else (hord + 1, vord))) ("", (0, 0)) (zip map_list op_list)

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
