import qualified Arr as A
import qualified Par as P
import Seq
import Arr (!)

instance Seq A where
    EmptyS :: A a
    EmptyS = A.empty

    singletonS :: a -> A a
    singletonS x = A.fromList [x]
    
    LengthS :: A a -> Int
    LengthS xs = length xs

    nthS :: A a -> Int -> a
    nthS xs i = (!) xs i

    takeS :: A a -> Int -> A a
    takeS s n = subArray 0 n s
    
    dropS :: A a -> Int -> A a
    dropS s n = subArray n (length s) s

    ShowtS :: A a -> TreeView a (A a)
    ShowtS s = 
showlS s
append s t
fromList xs
joinS s
    tabulateS :: (Int -> a) -> Int -> A a
    tabulateS f n = A.tabulate f n

    mapS :: (a->b) -> A a -> A b
    mapS f s = 
filterS f s
reduceS op b s
scanS op b s
