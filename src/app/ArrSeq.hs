{-# LANGUAGE InstanceSigs #-}
module ArrSeq where
import qualified Arr as A
import qualified Par as P
import Seq

toList :: A.Arr a -> Int -> [a]
toList s 0 = [nthS s 0]
toList s n = (nthS s n):(toList s (n-1)) 

instance Seq A.Arr where
    emptyS :: A.Arr a
    emptyS = A.empty

    singletonS :: a -> A.Arr a
    singletonS x = A.fromList [x]
    
    lengthS :: A.Arr a -> Int
    lengthS xs = A.length xs

    nthS :: A.Arr a -> Int -> a
    nthS xs i = xs A.! i

    tabulateS :: (Int -> a) -> Int -> A.Arr a
    tabulateS f n = A.tabulate f n
    
    mapS :: (a->b) -> A.Arr a -> A.Arr b
    mapS f s = tabulateS (aux f s) (lengthS s)
                    where 
                        aux f s i = f (nthS s i) 

    filterS :: (a -> Bool) -> A.Arr a -> A.Arr a
    filterS f s = joinS (tabulateS (aux f s) (lengthS s))
                    where
                        aux f s i = if f (nthS s i) then singletonS (nthS s i) else emptyS


    appendS :: A.Arr a -> A.Arr a -> A.Arr a
    appendS s t = let
                    ls = (lengthS s) - 1
                    lt = (lengthS t) - 1
                    (xs,xt) = (reverse (toList s ls),reverse (toList t lt))
                    in fromList (xs++xt) 

    takeS :: A.Arr a -> Int -> A.Arr a
    takeS s n = A.subArray 0 n s
    
    dropS :: A.Arr a -> Int -> A.Arr a
    dropS s n = A.subArray n (lengthS s) s

    showtS :: A.Arr a -> TreeView a (A.Arr a)
    showtS s = case (lengthS s) of
                1 -> (ELT (nthS s 1)) 
                n -> NODE (takeS s (div n 2)) 
                          (dropS s (div n 2))

    showlS :: A.Arr a -> ListView a (A.Arr a)
    showlS s = case (lengthS s) of
                0 -> NIL
                n -> CONS (nthS s 1) (dropS s 1) --ver


    joinS :: A.Arr (A.Arr a) -> A.Arr a
    joinS s = A.flatten s

    reduceS :: (a -> a -> a) -> a -> A.Arr a -> a
    reduceS op b s = b 

    scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
    scanS op b s = (s,b)

    fromList :: [a] -> A.Arr a
    fromList xs = A.fromList xs
