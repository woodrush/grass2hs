import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error

data G = F (G -> G) | Char Int | In | Out | Succ

true = return $ F $ \x -> F $ \y -> x
nil = return $ F $ \x -> F $ \y -> y

g :: IO G -> IO G -> IO G
g x y = do
    x <- x
    case x of
        F x' -> do
            y <- y
            return $ x' y
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

main = (g prim_out (g prim_succ (g (g nil prim_w) (g prim_in prim_w))))


f0 = prim_in
f1 = prim_w
f2 = prim_succ
f3 = prim_out
