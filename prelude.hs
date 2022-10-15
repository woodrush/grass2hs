import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error
import System.IO

data G = F (G -> IO G) | App (IO G) | Char Int | In | Out | Succ

ret :: G -> IO G
ret x = return x

f :: (G -> IO G) -> IO G
f x = ret $ F x

true = f $ \x -> f $ \y -> ret x
nil = f $ \x -> f $ \y -> ret y


g :: G -> G -> IO G
g x y = do
    case (x, y) of
        (App x', _)        -> do
                                x <- x'
                                g x y
        (_, App y')         -> do
                                y <- y'
                                g x y
        (F x', _)          -> x' y
        (Char x', Char y') -> if x' == y' then true else nil
        (Char x', _)       -> nil
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

-- prim_in :: IO G
prim_in = In

-- prim_w :: IO G
prim_w = (Char (ord 'w'))

-- prim_succ :: IO G
prim_succ = Succ

-- prim_out :: IO G
prim_out = Out

f0 = prim_in
f1 = prim_w
f2 = prim_succ
f3 = prim_out
