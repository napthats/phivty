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
    "map" ->
      if protocol !! 4 == 'M'
        then
          let (_, map_str) = splitAt 17 protocol in
          Map $ "???????" ++ (foldl
            (\acc ord ->
              (if ord `mod` 10 == 0 then "?" else "")
              ++ [map_str !! ord]
              ++ (if ord `mod` 10 == 8 then "?" else "")
              ++ acc)
            "???????"
            [48, 46..0])
        else Unknown
    _ -> Unknown
parse mes =
  NormalMessage mes


