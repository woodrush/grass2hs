import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error

data G = F (IO G -> IO G) | Char Int | In | Out | Succ

true = return $ F $ \x -> return $ F $ \y -> x
nil = return $ F $ \x -> return $ F $ \y -> y

g :: IO G -> IO G -> IO G
g x y = do
    x <- x
    case x of
        F x' -> do
            x' y
        Char x' -> do
            y <- y
            case y of
                Char y' -> do if x' == y' then true else nil
                otherwise -> nil
        Succ -> do
            y <- y
            case y of
                Char y' -> return $ Char $ mod (y' + 1) 256
                otherwise -> error "Non-Char type applied to Succ" 
        Out -> do
            y <- y
            case y of
                Char y' -> do
                    putChar $ chr y'
                    return y
                otherwise -> error "Non-Char type applied to Out"
        In -> catch
            (do
                c <- getChar
                return $ Char $ ord c)
            (\e -> case e of
                _ | isEOFError e -> y)

prim_in :: IO G
prim_in = return In

prim_w :: IO G
prim_w = return (Char (ord 'w'))

prim_succ :: IO G
prim_succ = return Succ

prim_out :: IO G
prim_out = return Out


f0 = prim_in
f1 = prim_w
f2 = prim_succ
f3 = prim_out

-- f4 = return $ F $ \f4 -> g f3 f1 -- wWWwwwwv
-- f5 = return $ F $ \f5 -> g f4 f5 -- wWWw
-- main = g f5 f5
