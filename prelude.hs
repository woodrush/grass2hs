import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error
import System.IO

data G = F (IO G -> IO G) | Char Int | In | Out | Succ

ret :: G -> IO G
ret x = return x

f :: (IO G -> IO G) -> IO G
f x = ret $ F x

true = f $ \x -> f $ \y -> x
nil = f $ \x -> f $ \y -> y


g :: IO G -> IO G -> IO G
g x y = do
    x <- x
    y <- y
    case (x, y) of
        (F x', _) -> x' $ ret y
        (Char x', Char y') -> if x' == y' then true else nil
        (Char x', _) -> nil
        (Succ, Char y') -> ret $ Char $ mod (y' + 1) 256
        (Succ, _) -> error "Non-Char type applied to Succ"
        (Out, Char y') -> do
                    putChar $ chr y'
                    hFlush stdout
                    ret y
        (Out, _) -> error "Non-Char type applied to Out"
        (In, _) -> catch
            (do
                c <- getChar
                ret $ Char $ ord c)
            (\e -> case e of
                _ | isEOFError e -> ret y)

prim_in :: IO G
prim_in = ret In

prim_w :: IO G
prim_w = ret (Char (ord 'w'))

prim_succ :: IO G
prim_succ = ret Succ

prim_out :: IO G
prim_out = ret Out

f0 = prim_in
f1 = prim_w
f2 = prim_succ
f3 = prim_out
