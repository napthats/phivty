module PhiVty.Protocol (
                       parse,
                       ServerProtocol(..),
                       ) where


data ServerProtocol =
    Map (String, String)
  | NormalMessage String
  | Unknown

parse :: String -> ServerProtocol
parse ('#':protocol) =
  case takeWhile ((/=) ' ') protocol of
    "m57" ->
      if protocol !! 4 == 'M'
        then
          let (_, map_str) = splitAt 17 protocol in
          Map $ (foldl
            (\(chip, op) ord -> (((map_str !! ord) : chip), ((map_str !! (ord + 1)) : op)))
            ("", "")
            [96, 94..0])
        else Unknown
    _ -> Unknown
parse mes =
  NormalMessage mes


