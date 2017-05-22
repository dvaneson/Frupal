import qualified Service as S
import Json (Json(..))

import System.FilePath (takeDirectory)

data Hero = Hero
          { position :: (Int, Int)
          , energy   :: Int
          , whiffles :: Int
          , items    :: [String]
          } deriving Show


data MapCell = MapCell
             { row     :: Int
             , column  :: Int
             , visible :: Bool
             , terrain :: Int
             , item    :: String
             } deriving Show


data GameState = GameState
               { name     :: String
               , size     :: Int
               , hero     :: Hero
               , mapCells :: [MapCell]
               } deriving Show


parseSeperator :: S.Args -> Maybe ((), S.Args)
parseSeperator args = do
    (arg,args) <- S.parseString args
    if head arg == '#' then Just ((), args) else Nothing


parseItems :: S.Args -> Maybe ([String], S.Args)
parseItems = parseItems' [] where
    parseItems' :: [String] -> S.Args -> Maybe ([String], S.Args)
    parseItems' _ [] = Nothing
    parseItems' items args =
        case parseSeperator args of
            Just ((), args) -> Just (items, args)
            Nothing         -> do (item, args) <- S.parseString args
                                  parseItems' (item:items) args


parseHero :: S.Args -> Maybe (Hero, S.Args)
parseHero [] = Nothing
parseHero (arg:args) = do
    (position, _)    <- S.parsePosition $ S.splitOn ',' arg
    (energy, args)   <- S.parseInt args
    (whiffles, args) <- S.parseInt args
    (items, args)    <- parseItems args
    Just (Hero position energy whiffles items, args)


parseMapCell :: S.Args -> Maybe (MapCell, S.Args)
parseMapCell [] = Nothing
parseMapCell (arg:args) = do
    (row, arg)     <- S.parseInt $ S.splitOn ',' arg
    (column, arg)  <- S.parseInt arg
    (visible, arg) <- S.parseInt arg
    (terrain, arg) <- S.parseInt arg
    (item, _)      <- S.parseString arg
    Just (MapCell row column (visible > 0) terrain item, args)


parseMapCells :: S.Args -> Maybe ([MapCell], S.Args)
parseMapCells = parseMapCells' [] where
    parseMapCells' :: [MapCell] -> S.Args -> Maybe ([MapCell], S.Args)
    parseMapCells' cells [] = Just (cells, [])
    parseMapCells' cells args = do
        (cell, args) <- parseMapCell args
        parseMapCells' (cell:cells) args


parseGameState :: S.Args -> Maybe (GameState, S.Args)
parseGameState args = do
    (name, args)     <- S.parseString args
    (size, args)     <- S.parseInt args
    ((), args)       <- parseSeperator args
    (hero, args)     <- parseHero args
    (mapCells, args) <- parseMapCells args
    Just (GameState name size hero mapCells, args)


write :: String -> (a -> String) -> Maybe a -> IO ()
write _ _ Nothing = return ()
write file toString (Just a) = do
    dir <- takeDirectory <$> S.dataFile
    writeFile (dir ++ "/" ++ file) $ toString a


writePosition :: Maybe Hero -> IO ()
writePosition = 
    write "../position/data" (S.showPosition . position)


showMapCells :: [MapCell] -> String
showMapCells = foldr (\mapCell acc -> acc ++ showMapCell mapCell) "None" where
    showMapCell :: MapCell -> String
    showMapCell cell =
        "\n" ++ show (row cell) ++ " " ++ show (column cell) ++ " " ++ item cell


writeItems :: Maybe GameState -> IO ()
writeItems =
    write "../items/data" (showMapCells . mapCells)


writeEnergy :: Maybe Hero -> IO ()
writeEnergy = 
    write "../energy/data" (show . energy)


handleError :: IO ()
handleError = putStr $ show $ JObject [("error", JBool True)]


printState :: Maybe Hero -> IO ()
printState Nothing = handleError
printState (Just hero) =
    let (r,c) = position hero 
        e = energy hero
        json = JObject [ ("position", JArray [ JNumber $ fromIntegral r
                                             , JNumber $ fromIntegral c
                                             ])
                       , ("item", JString "None")
                       , ("energy", JNumber $ fromIntegral e)
                       ]
    in putStr $ show json


main :: IO ()
main = S.createService $ \args -> do
    gameState <- S.currentData parseGameState
    let maybeHero = hero <$> gameState
    writePosition maybeHero
    writeItems gameState
    writeEnergy maybeHero
    printState maybeHero
