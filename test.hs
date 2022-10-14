import Data.Char (ord)

prim_in :: IO Int
prim_in = do
    x <- getChar
    return $ ord x

prim_out :: IO Int -> IO ()
prim_out x = do
    x <- x
    print x

prim_succ :: IO Int -> IO Int
prim_succ x = do
    x <- x
    return $ x + 1

main = prim_out (prim_succ prim_in)
