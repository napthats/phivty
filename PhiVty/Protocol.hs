module PhiVty.Protocol (
                       parse,
                       ServerProtocol(..),
                       ) where


data ServerProtocol =
    Map (Char, String, String, [((Int, Int), String)])
  | NormalMessage String
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
                             _ -> error "Invalid server protocol." in
          let (_, map_str) = splitAt 17 protocol in
          let (m, o) = foldl
                (\(chip, op) ord -> (((map_str !! ord) : chip), ((map_str !! (ord + 1)) : op)))
                ("", "")
                [96, 94..0] in
          Unfinished $ Map (protocol !! 6, m, o, chara_list)
        'O' ->
          let (dir, ma, op, chara_list) = case u_mes of
                                        Nothing -> (' ', "", "", [])
                                        Just (Map (d, m, o, l)) -> (d, m, o, l)
                                        _ -> error "Invalid server protocol." in
          let initial = protocol !! 18 in
          let x = (read [protocol !! 12] :: Int) in
          let y = (read [protocol !! 14] :: Int) in
          Unfinished (Map (dir, ma, op, ((x, y), [initial]) : chara_list))
        '.' ->
          case u_mes of
            Nothing -> error "Invalid server protocol."
            Just x -> x
        _ ->
          Unknown protocol
    _ -> Unknown protocol
parse _ mes =
  NormalMessage mes


