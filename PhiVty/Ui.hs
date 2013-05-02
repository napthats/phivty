module PhiVty.Ui
       (
       runPhiUi
       ) where

import Graphics.Vty.Widgets.All
import qualified Data.Text as T


runPhiUi :: IO ()
runPhiUi = do
  e <- editWidget
  -- tentative
  e `onActivate` error "exit"
  fg <- newFocusGroup
  _ <- addToFocusGroup fg e
  mes <- plainText (T.pack "hi") >>= centered
  mp <- plainText (T.pack $ makeMapString initialMapList [((3, 3), "m")]) >>= bordered >>= centered
  main_box <- (return mp <++> return mes) <--> (return e)
  c <- newCollection
  _ <- addToCollection c main_box fg
  runUi c defaultContext
  return ()

initialMapList :: String
initialMapList = "????????>% o=??#|{I@??    H??_T:+/????????"

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
