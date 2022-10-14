import Data.Char (ord, chr)

nil a b = b

data G a = Func a | Char a

-- prim_in :: IO Int
prim_in = do
    x <- getChar
    return $ Char (ord x)

-- prim_out :: IO (G (t1 -> Int)) -> IO (G (t2 -> Int))
prim_out x = do
    x <- x
    case x of
        (Char x') -> do
            print $ chr x'
        otherwise -> print 0


-- prim_succ :: G t -> IO Int
prim_succ x = do
    x <- x
    case x of
        (Char x') -> return Char (mod (x' + 1) 256)
        otherwise -> return (Char 0)

-- prim_w :: (G a -> G b) -> G c
prim_w = return (Char (ord 'w'))


main = prim_out prim_w

--(prim_succ prim_in)
