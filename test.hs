import Data.Char (ord, chr)
import Control.Exception
import System.IO.Error

data G = Func (G -> G) | Char Int | In | Out | Succ

nil = return $ Func $ \x -> Func $ \y -> y
true = return $ Func $ \x -> Func $ \y -> x

gapply :: IO G -> IO G -> IO G
gapply x y = do
    x <- x
    case x of
        Func x' -> do
            y <- y
            return (x' y)
        Char x' -> do
            y <- y
            case y of
                Char y' -> do
                    if x' == y' then
                        true
                    else
                        nil
                otherwise -> nil
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
        In -> catch
            (do
                c <- getChar
                return (Char (ord c)))
            (\e -> case e of
                _ | isEOFError e -> y)

prim_w :: IO G
prim_w = return (Char (ord 'w'))

prim_out :: IO G
prim_out = return Out

prim_succ :: IO G
prim_succ = return Succ

prim_in :: IO G
prim_in = return In

main = (gapply prim_out (gapply prim_succ (gapply (gapply nil prim_w) (gapply prim_in prim_w))))
