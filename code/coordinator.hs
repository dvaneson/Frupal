import qualified Service as S
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


parseResult :: String -> (S.Args -> Maybe (a, S.Args)) -> Maybe (a, S.Args)
parseResult args f = do
    (result, args) <- S.parseString $ S.splitOn ' ' args
    case result of
        "ok" -> f args
        _    -> Nothing


readResult :: FilePath -> S.Args -> IO String
readResult file args = do
    dir <- takeDirectory <$> S.dataFile
    readProcess (dir ++ "/../" ++ file) args ""


handleMove :: S.Row -> S.Column -> IO ()
handleMove r c = do
    pos <- readResult "position/main" [ "move", show r, show c ]
    print $ parseResult pos S.parsePosition
    return ()


handleNewGame :: IO ()
handleNewGame = return ()


apply :: Maybe Operation -> IO ()
apply Nothing = return ()
apply (Just op) =
    case op of
        Move r c -> handleMove r c
        NewGame  -> handleNewGame        


main :: IO ()
main = do
    args <- getArgs
    apply $ toOperation args
