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
    case x of
        F x' -> x' $ ret y
        Char x' -> do
            case y of
                Char y' -> if x' == y' then true else nil
                otherwise -> nil
        Succ -> do
            case y of
                Char y' -> ret $ Char $ mod (y' + 1) 256
                otherwise -> error "Non-Char type applied to Succ"
        Out -> do
            case y of
                Char y' -> do
                    putChar $ chr y'
                    hFlush stdout
                    ret y
                otherwise -> error "Non-Char type applied to Out"
        In -> catch
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
f4 = f $ \f4 -> do 
 f5_ <- g f4 f1
 let f5 = ret f5_
 f6_ <- g f5 f1
 let f6 = ret f6_
 f6
f5 = f $ \f5 -> f $ \f6 -> do 
 f6
f6 = f $ \f6 -> do 
 f7_ <- g f4 f5
 let f7 = ret f7_
 f8_ <- g f3 f7
 let f8 = ret f8_
 f8
main = g f6 f6
