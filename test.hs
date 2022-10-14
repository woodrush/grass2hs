import Data.Char (ord, chr)

prim_in :: IO Int
prim_in = do
    x <- getChar
    return $ ord x

prim_out :: IO Int -> IO Int
prim_out x = do
    x <- x
    print $ chr x
    return x

prim_succ :: IO Int -> IO Int
prim_succ x = do
    x <- x
    return $ mod (x + 1) 256

prim_w :: IO Int
prim_w = do
    return $ ord 'w'

main = prim_out $ prim_out $ prim_succ $ prim_succ $ prim_w
