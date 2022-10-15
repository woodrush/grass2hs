import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error
import System.IO

data G = F (G -> IO G) | App G G | Char Int | In | Out | Succ

ret :: G -> IO G
ret x = return x

f :: (G -> IO G) -> IO G
f x = ret $ F x

true = F $ \x -> f $ \y -> ret x
nil = F $ \x -> f $ \y -> ret y

g :: G -> G -> IO G
g x y = do
    case (x, y) of
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
f4 = F $ \f4 -> do
 f5 <- g f4 f1
 g f5 f1
f5 = F $ \f5 -> f $ \f6 -> ret f6
f6 = F $ \f6 -> do
 f7 <- g f4 f5
 g f3 f7
main = g f6 f6
