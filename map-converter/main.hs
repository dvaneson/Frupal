import Text.Read (readMaybe)
import Json (ToJson, Json(..), toJson)


data Hero = Hero
          { location :: (Int, Int)
          , energy   :: Int
          , whiffles :: Int
          , items    :: [String]
          } deriving (Show)


instance ToJson Hero where
    toJson h =
        let (x, y) = location h
        in JObject [ ("location", JArray [ JNumber $ fromIntegral x
                                         , JNumber $ fromIntegral y
                                         ])
                   , ("energy", JNumber $ fromIntegral $ energy h)
                   , ("whiffles", JNumber $ fromIntegral $ whiffles h)
                   , ("items", JArray $ JString <$> items h)
                   ]


data MapCell = MapCell
             { row     :: Int
             , column  :: Int
             , visible :: Bool
             , terrain :: Int
             , item    :: String
             } deriving (Show)


instance ToJson MapCell where
    toJson m = JObject [ ("row", JNumber $ fromIntegral $ row m)
                       , ("column", JNumber $ fromIntegral $ column m)
                       , ("visible", JBool $ visible m)
                       , ("terrain", JNumber $ fromIntegral $ terrain m)
                       , ("item", JString $ item m)
                       ]


data GameState = GameState
               { name     :: String
               , size     :: Int
               , hero     :: Hero
               , mapCells :: [MapCell]
               } deriving (Show)


instance ToJson GameState where
    toJson g = JObject [ ("name", JString $ name g)
                       , ("size", JNumber $ fromIntegral $ size g)
                       , ("hero", toJson $ hero g)
                       , ("mapCells", JArray $ toJson <$> mapCells g)
                       ]


expected :: String -> String
expected name = "Expected " ++ name ++ ", got nothing"


expectedType :: String -> String
expectedType name = "Expected " ++ name ++ ", got some other type"


tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs


splitOn :: Char -> String -> [String]
splitOn d [] = []
splitOn d ls =
    let h = takeWhile (/= d) ls
        t = splitOn d $ tail' $ dropWhile (/= d) ls
    in h : t


parseString :: String -> [String] -> Either String (String, [String])
parseString name [] = Left $ expected name
parseString _ (l:ls) = Right (l,ls)


parseInt :: String -> [String] -> Either String (Int, [String])
parseInt name [] = Left $ expected name
parseInt name (l:ls) =
    case readMaybe l :: Maybe Int of
        Nothing -> Left $ expectedType name
        Just n  -> Right (n,ls)


parseSeperator :: [String] -> Either String [String]
parseSeperator [] = Left $ expected "seperator"
parseSeperator (l:ls)
    | head l == '#' = Right ls
    | otherwise     = Left $ expectedType "seperator"


parseLocation :: [String] -> Either String ((Int, Int), [String])
parseLocation [] = Left $ expected "location"
parseLocation (l:ls) = do
    let xs = splitOn ',' l
    (x, xs) <- parseInt "location" xs
    (y, _)  <- parseInt "location" xs
    Right ((x, y), ls)


parseItem :: [String] -> Either String (String, [String])
parseItem = parseString "item"


parseItems :: [String] -> Either String ([String], [String])
parseItems = parseItems' [] where
    parseItems' :: [String] -> [String] -> Either String ([String], [String])
    parseItems' _ [] = Left $ expected "item"
    parseItems' is ls =
        case parseSeperator ls of
            Right ls -> Right (is, ls)
            _        -> do (i,ls) <- parseItem ls
                           parseItems' (i:is) ls


parseHero :: [String] -> Either String (Hero, [String])
parseHero ls = do
    (location, ls) <- parseLocation ls
    (energy, ls)   <- parseInt "energy" ls
    (whiffles, ls) <- parseInt "whiffles" ls
    (items, ls)    <- parseItems ls
    let hero = Hero { location = location
                    , energy   = energy
                    , whiffles = whiffles
                    , items    = items
                    }
    Right (hero, ls)


parseMapCell :: [String] -> Either String (MapCell, [String])
parseMapCell [] = Left $ expected "map cell"
parseMapCell (l:ls) = do
    let xs = splitOn ',' l
    (row, xs)     <- parseInt "row" xs
    (column, xs)  <- parseInt "column" xs
    (visible, xs) <- parseInt "visibility" xs
    (terrain, xs) <- parseInt "terrain" xs
    (item, xs)    <- parseString "item" xs
    let mapCell = MapCell
                { row = row
                , column = column
                , visible = visible > 0
                , terrain = terrain
                , item = item
                }
    Right (mapCell, ls)


parseMapCells :: [String] -> Either String ([MapCell], [String])
parseMapCells = parseMapCells' [] where
    parseMapCells' :: [MapCell] -> [String] -> Either String ([MapCell], [String])
    parseMapCells' cs [] = Right (cs,[])
    parseMapCells' cs ls =
        case parseMapCell ls of
            Right (c, ls) -> parseMapCells' (c:cs) ls
            Left s        -> Left s


parseGameState :: [String] -> Either String GameState
parseGameState ls = do
    (name, ls)    <- parseString "name" ls
    (size, ls)    <- parseInt "size" ls
    ls            <- parseSeperator ls
    (hero, ls)    <- parseHero ls
    (mapCells, _) <- parseMapCells ls
    Right GameState { name     = name
                    , size     = size
                    , hero     = hero
                    , mapCells = mapCells
                    }


main :: IO ()
main = do
    contents <- readFile "frupal-connection.map"
    print $ fmap toJson $ parseGameState $ lines contents

