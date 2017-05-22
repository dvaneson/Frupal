import System.Environment (lookupEnv, getExecutablePath)
import System.FilePath (takeDirectory)
import System.Process (readProcess)


contentType :: String
contentType = "Content-Type: application/json;charset=us-ascii\n\n"


readCoordinator :: String -> IO String
readCoordinator queryString = do
    path <- takeDirectory <$> getExecutablePath
    let fullPath = (path ++ "/../Frupal/coordinator/main")
    readProcess fullPath [ queryString ] ""


main :: IO ()
main = do
    maybeQs <- lookupEnv "QUERY_STRING"
    case maybeQs of
        Nothing -> putStr $ contentType ++ "{}"
        Just qs -> do 
            result <- readCoordinator qs
            putStr $ contentType ++ result
