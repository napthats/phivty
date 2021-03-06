module PhiVty.Protocol (
                       parse,
                       ServerProtocol(..),
                       ) where

import Codec.Text.IConv
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Codec.Binary.UTF8.String
import Data.List.Split
import Data.Bits
import Data.Char

data ServerProtocol =
    Map (Char, String, [(Int, Int, Int, Int)], [((Int, Int), String)])
  | NormalMessage String
  | ExNotice (String, String)
  | PhiList [String]
  | SEdit
  | Close
  | Lag
  | Unfinished ServerProtocol
  | Unknown String

--newtype Umes = ServerProtocol

parse :: (Maybe ServerProtocol) -> String -> ServerProtocol
parse u_mes ('#':protocol) =
  case takeWhile ((/=) ' ') protocol of
    "m57" ->
      case protocol !! 4 of
        'M' ->
          let chara_list = case u_mes of
                             Nothing -> []
                             Just (Map (_, _, _, l)) -> l
                             _ -> [] in
          let (_, map_str) = splitAt 17 protocol in
          let parseOption op =
               ((shiftR op 4) .&. 7, (shiftR op 3) .&. 1, (shiftR op 2) .&. 1, op .&. 1) in
          let (m, o) = foldl
                (\(chip, op) od -> (((map_str !! od) : chip), ((parseOption $ ord (map_str !! (od + 1))) : op)))
                ("", [])
                [96, 94..0] in
          Unfinished $ Map (protocol !! 6, m, o, chara_list)
        'O' ->
          let (dir, ma, op, chara_list) = case u_mes of
                                        Nothing -> (' ', "", [], [])
                                        Just (Map (d, m, o, l)) -> (d, m, o, l)
                                        _ -> (' ', "", [], []) in
          let initial_raw = protocol !! 18 in
          let initial = if ord initial_raw > 32 && ord initial_raw < 127 then initial_raw else 'A' in
          let x = (read [protocol !! 12] :: Int) in
          let y = (read [protocol !! 14] :: Int) in
          Unfinished (Map (dir, ma, op, ((x, y), [initial]) : chara_list))
        '.' ->
          case u_mes of
            Nothing -> Unknown ""
            Just (Map x) -> Map x
            _ -> Unknown ""
        _ ->
          Unknown protocol
    "ex-notice" ->
      let content_list = splitOn "=" $ snd $ splitAt 10 $ phiDecode protocol in
      ExNotice (content_list !! 0, content_list !! 1)
    "list" -> Unfinished $ PhiList []
    "end-list" -> case u_mes of
                   Nothing -> Unknown ""
                   Just (PhiList x) -> PhiList x
                   _ -> Unknown ""
    "s-edit" -> SEdit
    "close" -> Close
    "lag" -> Lag
    _ -> Unknown protocol
parse (Just (PhiList list)) mes = Unfinished $ PhiList $ list ++ [phiDecode mes]
parse _ mes =
  NormalMessage $ phiDecode mes

phiDecode :: String -> String
phiDecode = decodeString . unpack . convert "SJIS" "UTF-8" . pack
