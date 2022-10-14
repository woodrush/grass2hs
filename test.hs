import Data.Char (ord, chr)

nil a b = b

data G a = Func a | Char a | ExtractChar

-- -- prim_in :: IO (Char Int)
-- prim_in = do
--    x <- getChar
--    return (Char (ord x))
 

-- prim_out :: IO (G (t1 -> Int)) -> IO (G (t2 -> Int))
prim_out x = do
    x <- x
    case x of
        (Char x') -> do
            let x = x' ExtractChar
            print $ chr x
        otherwise -> return ()


-- -- prim_succ :: G t -> IO Int
-- prim_succ x = do
--     x <- x
--     case x of
--         (Char x') -> return (Char (mod (x' + 1) 256))
--         otherwise -> return (Char 0)

-- prim_w :: IO (G Int)
prim_w = return (Char (\x -> case x of
    ExtractChar -> (ord 'w')
    otherwise -> 0))

main = prim_out prim_w
