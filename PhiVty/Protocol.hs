module PhiVty.Protocol (
                       parse,
                       ServerProtocol(..),
                       ) where


data ServerProtocol =
    Map String
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
            (\acc ord ->
              (map_str !! ord) : acc)
            ""
            [96, 94..0])
        else Unknown
    _ -> Unknown
parse mes =
  NormalMessage mes


