import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error
import System.IO
import Unsafe.Coerce

data G = F (G -> IO G) | App G G | Char Int | In | Out | Succ

ret :: G -> IO G
ret = return

f :: (G -> IO G) -> IO G
f x = ret $ F x

true = F $ \x -> f $ \y -> ret x
nil = F $ \x -> f $ \y -> ret y

uc = unsafeCoerce

ubind :: IO G -> (G -> IO G) -> IO G
ubind a b = uc $ (>>=) a b

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
    (Succ, Char y')    -> ret $ Char $ mod (y' + 1) 256
    (Succ, _)          -> error "Non-Char type applied to Succ"
    (Out, Char y')     -> do
                            putChar $ chr y'
                            hFlush stdout
                            ret y
    (Out, _)           -> error "Non-Char type applied to Out"
    (In, _)            -> catch
                            (do
                                c <- getChar
                                ret $ Char $ ord c)
                            (\e -> case e of
                                _ | isEOFError e -> ret y)

f0 = In
f1 = Char $ ord 'w'
f2 = Succ
f3 = Out
