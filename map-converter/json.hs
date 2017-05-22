module Json (ToJson, Json(..), toJson) where

data Json = JNumber Double
          | JString String
          | JBool Bool
          | JArray [Json]
          | JObject [(String, Json)]


showArray :: [Json] -> String
showArray js = "[" ++ showArray' js where
    showArray' :: [Json] -> String
    showArray' [] = "]"
    showArray' [x] = show x ++ "]"
    showArray' (x:xs) = show x ++ ", " ++ showArray' xs


showObject :: [(String, Json)] -> String
showObject js = "{" ++ showObject' js where
    showObject' :: [(String, Json)] -> String
    showObject' [] = "}"
    showObject' [(key, value)] = show key ++ ": " ++ show value ++ "}"
    showObject' ((key, value):xs) =
        show key ++ ": " ++ show value ++ ", " ++ showObject' xs


instance Show Json where
    show (JNumber n) = show n
    show (JString s) = "\"" ++ s ++ "\""
    show (JBool b)   = show $ if b then "true" else "false"
    show (JArray a)  = showArray a
    show (JObject o) = showObject o


class ToJson a where
    toJson :: a -> Json

