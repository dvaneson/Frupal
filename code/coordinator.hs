import qualified Service as S
import Json (Json(..))
import System.Process (readProcess)
import System.FilePath (takeDirectory)
import System.Environment (getArgs)


data Operation = Move S.Row S.Column
               | NewGame


toOperation :: S.Args -> Maybe Operation
toOperation args = do
    (op, xs) <- S.parseString args
    case op of
        "north"   -> Just $ Move 1 0
        "east"    -> Just $ Move 0 1
        "south"   -> Just $ Move (-1) 0
        "west"    -> Just $ Move 0 (-1)
        "newgame" -> Just NewGame
        _         -> Nothing


parseResult :: String -> (S.Args -> Maybe (a, S.Args)) -> Maybe a
parseResult args f = do
    (result, args) <- S.parseString $ S.splitOn ' ' args
    case result of
        "ok" -> f args >>= \(a,_) -> Just a
        _    -> Nothing


readResult :: FilePath -> S.Args -> IO String
readResult file args = do
    dir <- takeDirectory <$> S.dataFile
    readProcess (dir ++ "/../" ++ file) args ""


readItem :: Maybe S.Position -> IO String
readItem Nothing = return ""
readItem (Just (r,c)) = 
    readResult "items/main" [ "lookup", show r, show c ]


processOutput :: Maybe S.Position -> Maybe String -> Maybe S.Energy -> Maybe String
processOutput maybePos maybeItem maybeEnergy = do
    (row, column) <- maybePos
    item     <- maybeItem
    energy   <- maybeEnergy
    return $ show $ JObject [ ("position", JArray [ JNumber $ fromIntegral row
                                                  , JNumber $ fromIntegral column
                                                  ])
                            , ("item", JString item)
                            , ("energy", JNumber $ fromIntegral energy) 
                            ]


handleError :: IO ()
handleError = putStr $ show $ JObject [("error", JBool True)]


handleMove :: S.Row -> S.Column -> IO ()
handleMove r c = do
    posResult <- readResult "position/main" [ "move", show r, show c ]
    let maybePos = parseResult posResult S.parsePosition
    itemResult <- readItem maybePos
    let maybeItem = parseResult itemResult S.parseString
    energyResult <- readResult "energy/main" [ "sub", "1" ]
    let maybeEnergy = parseResult energyResult S.parseInt
    case processOutput maybePos maybeItem maybeEnergy of
        Nothing -> handleError
        Just x  -> putStr x
                        


handleNewGame :: IO ()
handleNewGame = do
    result <- readResult "new-game/main" []
    putStr result
    


apply :: Maybe Operation -> IO ()
apply Nothing = handleError
apply (Just op) =
    case op of
        Move r c -> handleMove r c
        NewGame  -> handleNewGame        


main :: IO ()
main = do
    args <- getArgs
    apply $ toOperation args
