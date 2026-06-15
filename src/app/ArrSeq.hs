{-# LANGUAGE InstanceSigs #-}
module ArrSeq where
import qualified Arr as A
import qualified Par as P
import Seq

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
    mapS f s = 
        case (lengthS s) of
            1 -> singletonS (f (nthS s 1))

            --n -> (singletonS (f (nthS s 1))) P.||| 
            n -> mapS f (dropS s 1)


    filterS :: (a -> Bool) -> A.Arr a -> A.Arr a
    filterS f s = undefined
        --case (lengthS) of
        --    1 -> if f (take s 1) then 
        --    n -> undefined

    appendS :: A.Arr a -> A.Arr a -> A.Arr a
    appendS s t = case (lengthS s, lengthS t) of
                    (0, m) -> t
                    (n, 0) -> s
                    (n,m) -> s --mal 

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
    reduceS op b s = undefined

    scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
    scanS op b s = undefined

    fromList :: [a] -> A.Arr a
    fromList xs = A.fromList xs
