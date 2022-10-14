{-# LANGUAGE PartialTypeSignatures #-}
import Data.Char (ord, chr)

data G a b = Func a | Char b | CInt Int | ExtractChar

-- nil = Func (\x -> \y -> y)


-- -- -- -- prim_in :: IO (Char Int)
prim_in = do
   x <- getChar
   return $ prim_char (ord x)
 

-- prim_out :: IO (G (t1 -> Int)) -> IO (G (t2 -> Int))
prim_out x = do
    x <- x
    case x of
        (Char x') -> do
            let b = x' ExtractChar
            case b of
                CInt x' ->do
                    print $ chr x'
                    return $ prim_char 0
                otherwise -> error "Non-Int value contained in Char"
        otherwise -> error "Non-Char argument applied to Out"


-- -- -- -- prim_succ :: G t -> IO Int
-- -- -- prim_succ :: IO (G t2) -> G t3
prim_succ x = do
    x <- x
    case x of
        (Char x') -> do
            let x = x' ExtractChar
            case x of
                CInt x' -> return $ prim_char (mod (x' + 1) 256)
                otherwise -> error "Non-Int value contained in Char"
        otherwise -> error "Non-Char argument applied to Succ"

-- nil :: (G t) -> (G t)
nil a b = b
true a b = a

prim_char :: Int -> G _ _
prim_char n = Char (\x -> case x of
    ExtractChar -> (CInt n)
    otherwise -> Func nil)

prim_w :: IO (G _ _)
prim_w = return (prim_char (ord 'w'))

gapply x y = case x of
    Char x' -> do
        case y of
           Char y' -> do
            if x' == y' then
                return nil
            else
                return true
           Func y' -> return nil
    Func x' -> do
        return (x' y)



main = prim_out $ prim_succ $ nil prim_w prim_w
