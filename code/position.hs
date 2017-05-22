import qualified Service as S


data Operation = Move S.Row S.Column
               | Reset S.Row S.Column


toOperation :: S.Args -> Maybe Operation
toOperation args = do
    (op,xs)    <- S.parseString args
    (row,xs)   <- S.parseInt xs
    (column,_) <- S.parseInt xs
    case op of
        "move"  -> Just $ Move row column
        "reset" -> Just $ Reset row column
        _       -> Nothing


apply :: Operation -> S.Position -> S.Position
apply (Move r c) (row, column) = (row + r, column + c)
apply (Reset r c) _            = (r, c)


main :: IO ()
main = S.createService $ \args -> do
    position <- S.currentData (S.parsePosition . S.splitOn ' ' . head)
    let newPosition = apply <$> toOperation args <*> position
    S.printResult $ S.showPosition <$> newPosition
