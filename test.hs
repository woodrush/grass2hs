{-# LANGUAGE PartialTypeSignatures #-}
import Data.Char (ord, chr)

data G a = Func a | Char Int | In | Out | Succ

nil = Func (\x -> \y -> y)
true = Func (\x -> \y -> x)

gapply x y = do
    x <- x
    case x of
        Func x' -> do
            return (x' y)
        Char x' -> do
            y <- y
            case y of
                Char y' -> do
                    if x' == y' then
                        return nil
                    else
                        return true
                otherwise -> return nil
        Succ -> do
            y <- y
            case y of
                Char y' -> return (Char (mod (y' + 1) 256))
                otherwise -> error "Non-Char type applied to Succ" 
        Out -> do
            y <- y
            case y of
                Char y' -> do
                    print $ chr y'
                    return y
                otherwise -> error "Non-Char type applied to Out"
        In -> do
            c <- getChar
            return $ Char (ord c)
        otherwise -> return nil

prim_w :: IO (G _)
prim_w = return (Char (ord 'w'))

prim_out :: IO (G _)
prim_out = return Out

prim_succ :: IO (G _)
prim_succ = return Succ

prim_in :: IO (G _)
prim_in = return In

main = gapply prim_out (gapply prim_succ (gapply prim_in prim_w))
