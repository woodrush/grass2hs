import Data.Char (ord, chr)

nil a b = b

data G a = Func a | W a

-- prim_in :: IO Int
-- prim_in = do
--     x <- getChar
--     return $ ord x

prim_out :: G (IO Int) -> IO Int
prim_out x = case x of
    W z -> do
            c <- z
            -- let c = 65
            print $ chr c
            return c
    otherwise -> return 0

-- prim_succ :: IO Int -> IO Int
-- prim_succ x = do
--     x <- x
--     return $ mod (x + 1) 256

prim_w :: IO Int
prim_w = return $ ord 'w'
-- case a of
--     W t -> return $ ord 'w'
--     otherwise -> return 0
    

w = W prim_w

main = prim_out $ w
