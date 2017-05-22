import qualified Service as S


data Operation = Sub S.Energy
               | Reset S.Energy


toOperation :: S.Args -> Maybe Operation
toOperation args = do
    (op,xs) <- S.parseString args
    (num,_) <- S.parseInt xs
    case op of
        "sub"   -> Just $ Sub num
        "reset" -> Just $ Reset num
        _       -> Nothing


apply :: Operation -> S.Energy -> S.Energy
apply (Sub n) x   = x - n
apply (Reset n) _ = n


main :: IO ()
main = S.createService $ \args -> do
    energy <- S.currentData S.parseInt
    let newEnergy = apply <$> toOperation args <*> energy
    S.printResult $ show <$> newEnergy
