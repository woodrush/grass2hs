import Data.Char (ord, chr)

nil a b = b

data G a = Func a | Char Int

-- prim_in :: IO Int
-- prim_in = do
--     x <- getChar
--     return $ ord x

prim_out :: Int -> IO Int
prim_out x = do
    print $ chr x
    return x


-- prim_succ :: G t -> IO Int
-- prim_succ x = case x of
--     W z -> do
--         x <- z w
--         \a -> do return $ mod (x + 1) 256
--     otherwise -> return 0

prim_w :: (G a -> G b) -> G c
prim_w _ = Char (ord 'w')
    

main = prim_out 65
