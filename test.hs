import Data.Char (ord, chr)

nil a b = b

data G a = Func a | Char a

-- prim_in :: IO Int
-- prim_in = do
--     x <- getChar
--     return $ ord x

-- prim_out :: IO (G (t1 -> Int)) -> IO (G (t2 -> Int))
prim_out x = case x of
    Char x' -> do
        print $ chr (x' prim_w)
        return 0
    otherwise -> return 0


-- prim_succ :: G t -> IO Int
-- prim_succ x = case x of
--     W z -> do
--         x <- z w
--         \a -> do return $ mod (x + 1) 256
--     otherwise -> return 0

-- prim_w :: (G a -> G b) -> G c
prim_w = Char (\x -> ord 'w')


main = prim_out prim_w
