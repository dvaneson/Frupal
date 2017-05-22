module Service ( createService, currentData, printResult, showPosition
               , splitOn, parseString, parseInt, parsePosition, dataFile
               , Args, Row, Column, Position, Energy
               ) where


import Control.Exception (catch)
import GHC.IO.Exception (IOException)
import System.Environment (getArgs, getExecutablePath)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)


type Args     = [String]
type Row      = Int
type Column   = Int
type Position = (Row, Column)
type Energy = Int


dataFile :: IO FilePath
dataFile = do
    cwd <- takeDirectory <$> getExecutablePath
    return $ cwd ++ "/data"


currentData :: (Args -> Maybe (a, Args)) -> IO (Maybe a)
currentData f = do
    file <- dataFile
    currentData' f <$> readFile file where
        currentData' :: (Args -> Maybe (a, Args)) -> String -> Maybe a
        currentData' f s = f (lines s) >>= Just . fst


handleError :: IOException -> IO ()
handleError _ = putStr "error"


showPosition :: Position -> String
showPosition (row, column) = show row ++ " " ++ show column


tail' :: [a] -> [a]
tail' []     = []
tail' (_:xs) = xs


splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s =
    takeWhile (/=d) s : splitOn d (tail' (dropWhile (/=d) s))


parseString :: Args -> Maybe (String, Args)
parseString [] = Nothing
parseString (x:xs) = Just (x,xs)


parseInt :: Args -> Maybe (Int, Args)
parseInt [] = Nothing
parseInt (x:xs) = (\i -> (i,xs)) <$> readMaybe x


parsePosition :: Args -> Maybe (Position, Args)
parsePosition [] = Nothing
parsePosition xs = do
    (row,xs)    <- parseInt xs
    (column,xs) <- parseInt xs
    Just ((row, column), xs)


printResult :: Maybe String -> IO ()
printResult Nothing = putStr "error"
printResult (Just s) = do
    file <- dataFile
    writeFile file s
    putStr $ "ok " ++ s


createService :: (Args -> IO ()) -> IO ()
createService f = do
    args <- getArgs
    catch (f args) handleError
