import qualified Service as S


type Item   = String
type Items  = [(S.Row, S.Column, Item)]


data Operation = Lookup S.Row S.Column
               | Assoc S.Row S.Column Item


toOperation :: S.Args -> Maybe Operation
toOperation args = do
    (op,xs)     <- S.parseString args
    (row,xs)    <- S.parseInt xs
    (column,xs) <- S.parseInt xs
    case op of
        "lookup" -> Just $ Lookup row column
        "assoc"  -> S.parseString xs >>= (Just . Assoc row column . fst)
        _        -> Nothing


parseItems :: S.Args -> Maybe (Items, S.Args)
parseItems [] = Just ([], [])
parseItems (x:xs) = do
    (row, ys)   <- S.parseInt $ S.splitOn ' ' x
    (column,ys) <- S.parseInt ys
    (item,_)    <- S.parseString ys
    (items,_)   <- parseItems xs
    Just ((row,column,item):items, [])


parsePair :: S.Args -> Maybe ((Item,Items), S.Args)
parsePair [] = Nothing
parsePair xs = do
    (i,xs)  <- S.parseString xs
    (is,xs) <- parseItems xs
    Just ((i,is),xs)


lookupItem :: Item -> S.Row -> S.Column -> Items -> Item
lookupItem fallback _ _ [] = fallback
lookupItem fallback row column ((r,c,i):xs) =
    if r == row && c == column
        then i
        else lookupItem fallback row column xs 


assocItem :: S.Row -> S.Column -> Item -> Items -> Items
assocItem r c i [] = [(r, c, i)]
assocItem r c i ((row,column,item):xs) =
    if r == row && c == column
        then (r,c,i):xs
        else (row,column,item) : assocItem r c i xs


apply :: Operation -> (Item,Items) -> (Either Items Item, Item)
apply (Lookup r c) (i,is) = (Right $ lookupItem i r c is, i)
apply (Assoc r c item) (i,is) = (Left $ assocItem r c item is, i)


showItems :: Items -> String
showItems [] = ""
showItems ((r,c,i):xs) = 
    mconcat [ show r, " ", show c, " ", i, "\n", showItems xs ]


main :: IO ()
main = S.createService $ \args -> do
    pair <- S.currentData parsePair
    case apply <$> toOperation args <*> pair of
        Just result -> case result of
            (Left items, item) -> do
                file <- S.dataFile
                writeFile file $ item ++ "\n" ++ showItems items
                putStr "ok"
            (Right item, _) -> putStr $ "ok " ++ item
        Nothing -> putStr "error"
