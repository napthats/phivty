{-# LANGUAGE OverloadedStrings #-}

module PhiVty.UI
       (
       UIData,
       initialPhiUI,
       runPhiUI,
       ) where

import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Graphics.Vty.LLInput
import Graphics.Vty.Attributes
import PhiVty.Socket
import PhiVty.Protocol
import PhiVty.DB
import PhiVty.Cdo
import Data.List.Split
import Control.Monad.Trans
import Data.IORef
import Text.Regex
import PhiVty.Data.UI
import Control.Concurrent
--import Control.Concurrent

data UIDataInternal = UIDataInternal {
            ui_maptext :: Widget FormattedText,
            ui_message :: Widget FormattedText,
            ui_maptitle :: Widget FormattedText,
            v_maptitle :: IORef (String, String, String)
            }
type UIData = Collection

inputHandler :: PhiSocket -> String -> IO ()
inputHandler soc mes =
  if mes == ":exit" then error "exit" else send mes soc

mapHandler :: PhiSocket -> Key -> [Modifier] -> Cdo (DB IO ()) -> (CollectionType -> IO()) -> IO ()
mapHandler soc key mod_list c change_ct = cdo c $ do
  ui_state <- getUIState
  lift $ _mapHandler ui_state soc key mod_list c change_ct
  setUIState UINormal

_mapHandler :: UIState -> PhiSocket -> Key -> [Modifier] -> Cdo (DB IO ()) -> (CollectionType -> IO()) -> IO ()
_mapHandler UINormal soc key [] _ _ =
  case key of
    KEnd -> send "hit" soc
    KDown -> send "go b" soc
    KPageDown -> send "cast" soc
    KLeft -> send "go l" soc
    KBegin -> send "turn b" soc
    KRight -> send "go r" soc
    KHome -> send "turn l" soc
    KUp -> send "go" soc
    KPageUp -> send "turn r" soc
    KIns -> do {send "look" soc; send "check" soc}
    KDel -> send "." soc
    KASCII '.' -> send "." soc
    KASCII '1' -> send "1" soc
    KASCII '2' -> send "2" soc
    KASCII '3' -> send "3" soc
    KASCII '4' -> send "4" soc
    KASCII '5' -> send "5" soc
    KASCII '6' -> send "6" soc
    KASCII '7' -> send "7" soc
    KASCII '8' -> send "8" soc
    KASCII '9' -> send "9" soc
    KASCII '0' -> send "0" soc
    KASCII 'z' -> send "get" soc
    KASCII 'c' -> send "use" soc
    KASCII 'x' -> send "put" soc
    KASCII 'q' -> send "equip" soc
    KASCII 'w' -> send "unequip" soc
    KASCII 'v' -> send "sort" soc
    KASCII 'a' -> send "read" soc
    KASCII 's' -> send "write" soc
    KASCII 'f' -> send "floor item" soc
    KASCII 'b' -> send "board" soc
    KASCII 'd' -> send "erace" soc
    KASCII 'g' -> send "guard" soc
    KASCII 'h' -> send "hi" soc
    KASCII 'y' -> send "y" soc
    KASCII _ -> return ()
    _ -> return ()
_mapHandler UINormal soc key mod_list c change_ct =
  if elem MMeta mod_list || elem MAlt mod_list
  then case key of
        KASCII 'w' -> do {send "cast" soc; send "wizard eye" soc}
        KASCII 'e' -> do {send "cast" soc; send "eagle eye" soc}
        KASCII 'a' -> do {send "cast" soc; send "analyze" soc}
        KASCII 'c' -> do {send "cast" soc; send "create" soc}
        KASCII 'i' -> do {send "cast" soc; send "identify" soc}
        KASCII 'l' -> do {send "cast" soc; send "wizard lock" soc}
        KASCII 'u' -> do {send "cast" soc; send "unlock" soc}
        KASCII 's' -> do {send "cast" soc; send "search" soc}
        KASCII 'm' -> cdo c $ do
          ct <- getCollectionType
          case ct of
           CTNormal -> do {lift $ schedule $ change_ct CTMenu; setCollectionType CTMenu}
           CTMenu -> do {lift $ schedule $ change_ct CTNormal; setCollectionType CTNormal}
        KASCII _ -> return ()
        _ -> return ()
  else return ()
_mapHandler UISEdit soc key [] c change_ct = cdo c $ do
  prev_philist <- getPrevList
  let first_ord = getFirstOrd prev_philist
  lift $ do {case key of
    KASCII '1' -> send (show first_ord) soc
    KASCII '2' -> send (show $ first_ord + 1) soc
    KASCII '3' -> send (show $ first_ord + 2) soc
    KASCII '4' -> send (show $ first_ord + 3) soc
    KASCII '5' -> send (show $ first_ord + 4) soc
    KASCII '6' -> send (show $ first_ord + 5) soc
    KASCII '7' -> send (show $ first_ord + 6) soc
    KASCII '8' -> send (show $ first_ord + 7) soc
    KASCII '9' -> send (show $ first_ord + 8) soc
    _ -> _mapHandler UINormal soc key [] c change_ct}
  return ()
  where getFirstOrd philist =
          case philist of
            [] -> 1
            fst_elem : _ ->
             (read (dropWhile (\x -> x == '[' || x == ' ')  $ takeWhile ((/=) ']') fst_elem) :: Int) 
_mapHandler UISEdit soc key mod_list c change_ct = _mapHandler UINormal soc key mod_list c change_ct
 
makeWindowWithChara :: MultiStringListData -> Cdo (DB IO ()) -> String -> String -> Int -> Collection -> IO (IO ())
makeWindowWithChara menu_item c chara_id host_name port_num collection = do
  v_m <- newIORef ("", "", "")
  titletest <- plainText " "
  maptext <- plainText (T.pack $ makeMapString initialMapList initialMapOptionList [((3, 3), "m")])
  mes_plain <- plainText " "
  let uidata = UIDataInternal {v_maptitle = v_m, ui_maptitle = titletest, ui_maptext = maptext, ui_message = mes_plain}
  soc <- initSocket host_name port_num

  e <- editWidget
  -- tentative
  e `onActivate` \this -> do
    txt <- getEditText this
    inputHandler soc $ T.unpack txt
    setEditText this ""
  let mes = mes_plain
  title <- hCentered titletest
  mp <- bordered maptext >>= hCentered

  connect_action_mvar <- newEmptyMVar
  let menu_item_innner = case menu_item of MultiStringListData list -> list
  menu <- makeMultiStringList $ MultiStringListData $ menu_item_innner ++ [
    ("Connect", Left $ do {action <- readMVar connect_action_mvar; action}),
    ("Disconnect", Left $ send "exit" soc)]
  upper_left_box_wmenu <- centered menu <--> ((return title <--> return mp) >>= centered)
  setBoxChildSizePolicy upper_left_box_wmenu $ Percentage 50
  upper_box_wmenu <- (return upper_left_box_wmenu <++> return mes)
  setBoxChildSizePolicy upper_box_wmenu $ Percentage 40
  main_box_wmenu <- (return upper_box_wmenu) <--> (return e)
  fg_wmenu <- newFocusGroup
  _ <- addToFocusGroup fg_wmenu e
  _ <- addToFocusGroup fg_wmenu maptext
  _ <- addToFocusGroup fg_wmenu menu

  upper_box <- (((return title <--> return mp) >>= centered) <++> return mes)
  setBoxChildSizePolicy upper_box $ Percentage 40
  main_box <- (return upper_box) <--> (return e)
  fg <- newFocusGroup
  _ <- addToFocusGroup fg e
  _ <- addToFocusGroup fg maptext
  ct_menu <- addToCollection collection main_box_wmenu fg_wmenu
  ct_normal <- addToCollection collection main_box fg
  let change_collection_type ct =
       case ct of
        CTNormal -> ct_normal
        CTMenu -> ct_menu
  maptext `onKeyPressed` \_ key mod_list -> do {mapHandler soc key mod_list c change_collection_type; return True}
  m_u_mes <- newMVar Nothing
  let recv_handler new_mes = do
       u_mes <- takeMVar m_u_mes
       putMVar m_u_mes Nothing
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
           lift $ mapM_ (addMessage uidata c) $ "---------------" :
             snd (foldl (\(ord, acc) elm -> (ord+1, (acc ++ [("(" ++ show ord ++ ")" ++ elm)]))) (1 :: Integer, []) list) ++ ["---------------"]
         SEdit -> cdo c $ do
           setUIState UISEdit
         Close -> close soc
         Unfinished u -> modifyMVar_ m_u_mes $ const $ return $ Just u
         Unknown "" -> return ()
         Unknown un_mes -> do
           addMessage uidata c $ '#' : un_mes
       }
       return ()
  putMVar connect_action_mvar $ do {
    connect soc recv_handler;
    send ("#open " ++ chara_id) soc;
    send "#map-iv 1" soc;
    send "#status-iv 1" soc;
    send "#version-cli 05103010" soc;
    send "#ex-switch eagleeye=form" soc;
    send "#ex-map size=57" soc;
    send "#ex-map style=turn" soc;
    send "#ex-switch ex-move-recv=true" soc;
    send "#ex-switch ex-list-mode-end=true" soc;
    send "#ex-switch ex-disp-magic=false" soc
  }
  return ct_menu

 
initialPhiUI :: Cdo (DB IO ()) -> [(String, String, Int)] -> IO UIData
initialPhiUI cdod chara_list = do
  (action_mvar_list, multi_string_list_data) <- createMultiStringListData chara_list
  collection <- newCollection
  ct_menu_action_list <- mapM (\(chara_id, host_name, port_num) -> makeWindowWithChara multi_string_list_data cdod chara_id host_name port_num collection) chara_list
  mapM_ (\(action_mvar, ct_menu_action) -> putMVar action_mvar ct_menu_action) (zip action_mvar_list ct_menu_action_list)
  return collection

data MultiStringListData = MultiStringListData [(String, Either (IO ()) MultiStringListData)]

createMultiStringListData :: [(String, String, Int)] -> IO([MVar (IO ())], MultiStringListData)
createMultiStringListData chara_list = do
  action_mvar_list <- mapM (\_ -> newEmptyMVar) chara_list
  let msld =
       [("Load charactor", Right $ MultiStringListData $
         map (\((name, _, _), action_mvar) -> (name, Left $ do{action <- readMVar action_mvar; action})) (zip chara_list action_mvar_list))]
  return $ (action_mvar_list, MultiStringListData msld)

-- Names of data must not be duplicated
-- All spaces at head of names will be deleted
makeMultiStringList :: MultiStringListData -> IO (Widget (List T.Text FormattedText))
makeMultiStringList (MultiStringListData msl_data) = do
  vty_list <- newTextList def_attr $ map (T.pack . removeSpace . fst) msl_data
  vty_list `onItemActivated` \(ActivateItemEvent ord name _) ->
    case getData msl_data (T.unpack name) of
     Left io -> io
     Right (MultiStringListData inner_data) -> do
      maybe_next_item <- getListItem vty_list $ ord + 1
      mapM_ (\(inner_name, _) ->
       do
        let level t_name = length (T.unpack t_name) - length (dropWhile ((==) ' ') (T.unpack t_name))
        inner_widget <- plainText $ T.pack $ (replicate (level name + 1) ' ') ++ inner_name
        let expand_list = insertIntoList vty_list (T.pack $ replicate (level name + 1) ' ' ++ inner_name) inner_widget (ord + 1)
        let collaps_list = do{_ <- removeFromList vty_list (ord + 1); return ()}
        case maybe_next_item of
          Nothing -> expand_list
          Just (next_name, _) ->
            if level name >= level next_name
            then expand_list
            else collaps_list
       ) (reverse inner_data)
  return vty_list
  where removeSpace str = dropWhile ((==) ' ') str
        getData _x __name =
         let _getData x _name =
              case x of
               [] -> error "Assertion Error: makeMultiStringList cannot find data."
               (x_name, x_data) : rem_data ->
                 if x_name == _name
                 then x_data
                 else case x_data of
                       Left _ -> _getData rem_data _name
                       Right (MultiStringListData inner_data) ->
                         _getData (inner_data ++ rem_data) _name
          in
          _getData _x $ removeSpace __name

runPhiUI :: UIData -> IO ()
runPhiUI uidata = runUi uidata defaultContext

setMap :: UIDataInternal -> String -> [(Int, Int,Int, Int)] -> [((Int, Int), String)] -> IO ()
setMap uidata str op_list chara_list =
  schedule $ setText (ui_maptext uidata) (T.pack $ makeMapString str op_list chara_list)

setMapTitle :: UIDataInternal -> String -> IO ()
setMapTitle uidata mes =
  schedule $ setText (ui_maptitle uidata) (T.pack $ mes)

setDirection :: UIDataInternal -> String -> IO ()
setDirection uidata dir = do
  (_, land, area) <- readIORef $ v_maptitle uidata
  setMapTitle uidata $ "[" ++ dir ++ "]" ++ land ++ "(" ++ area ++ ")"
  writeIORef (v_maptitle uidata) (dir, land, area)

setLandName :: UIDataInternal -> String -> IO ()
setLandName uidata land = do
  (dir, _, area) <- readIORef $ v_maptitle uidata
  setMapTitle uidata $ "[" ++ dir ++ "]" ++ land ++ "(" ++ area ++ ")"
  writeIORef (v_maptitle uidata) (dir, land, area)
  
setAreaName :: UIDataInternal -> String -> IO ()
setAreaName uidata area = do
  (dir, land, _) <- readIORef $ v_maptitle uidata
  setMapTitle uidata $ "[" ++ dir ++ "]" ++ land ++ "(" ++ area ++ ")"
  writeIORef (v_maptitle uidata) (dir, land, area)

setMessage :: UIDataInternal -> [String] -> IO ()
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



addMessage :: UIDataInternal -> (Cdo (DB IO ())) -> String -> IO ()
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
