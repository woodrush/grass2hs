import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error
import System.IO

data G = F (G -> IO G) | App G G | Char Int

ret :: G -> IO G
ret = return

f :: (G -> IO G) -> IO G
f x = ret $ F x

true = F $ \x -> f $ \y -> ret x
nil = F $ \x -> f $ \y -> ret y

g :: G -> G -> IO G
g x y = case (x, y) of
    (App x1 x2, _)     -> do
                            x <- g x1 x2
                            g x y
    (_, App y1 y2)     -> do
                            y <- g y1 y2
                            g x y
    (F x', _)          -> x' y
    (Char x', Char y') -> ret $ if x' == y' then true else nil
    (Char x', _)       -> ret nil

prim_in y = catch
    (do
        c <- getChar
        ret $ Char $ ord c)
    (\e -> case e of
        _ | isEOFError e -> ret y)

prim_succ y = case y of
    Char y'   -> ret $ Char $ mod (y' + 1) 256
    otherwise -> error "Non-Char type applied to Succ"

prim_out y = case y of
    Char y'   -> do
                    putChar $ chr y'
                    hFlush stdout
                    ret y
    otherwise -> error "Non-Char type applied to Out"

f0 = F prim_in
f1 = Char $ ord 'w'
f2 = F prim_succ
f3 = F prim_out
