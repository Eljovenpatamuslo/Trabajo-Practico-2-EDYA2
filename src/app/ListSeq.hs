{-# LANGUAGE InstanceSigs #-}
module ListSeq where
import qualified Par as P
import Data.List
import Seq

toList :: A.Arr a -> Int -> [a]
toList s 0 = [nthS s 0]
toList s n = (nthS s n):(toList s (n-1)) 

instance Seq [a] where
    emptyS :: [a]
    emptyS = []

    singletonS :: a -> [a]
    singletonS x = [a]
    
    lengthS :: [a] -> Int
    lengthS xs = length xs

    nthS :: [a] -> Int -> a
    nthS xs i = xs !! i

    tabulateS :: (Int -> a) -> Int -> [a]
    tabulateS f n = undefined
    
    mapS :: (a->b) -> [a] -> [b]
    mapS f s = map f s 

    filterS :: (a -> Bool) -> A.Arr a -> A.Arr a
    filterS f s = filter f s

    appendS :: [a] -> [a] -> [a]
    appendS s t = s ++ t --paralelizar

    takeS :: [a] -> Int -> [a]
    takeS s n = undefined
    
    dropS :: [a] -> Int -> [a]
    dropS s n = undefined

    showtS :: [a] -> TreeView a ([a])
    showtS s = case (lengthS s) of
                0 -> EMPTY
                1 -> (ELT (nthS s 0)) 
                n -> NODE (takeS s (div n 2)) 
                          (dropS s (div n 2))

    showlS :: [a] -> ListView a ([a] a)
    showlS s = case (lengthS s) of
                0 -> NIL
                n -> CONS (nthS s 1) (dropS s 1) --ver


    joinS :: [a] ([a]) -> [a]
    joinS s = undefined

    reduceS :: (a -> a -> a) -> a -> [a] -> a
    reduceS op b s = case showtS s of
        EMPTY -> b
        ELT v -> v
        NODE l r -> let (l1,r1) = (reduceS op b l) P.||| (reduceS op b r)
                    in op l1 r1


    scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
    scanS op b s = let
    (,(reduceS op b s))

    fromList :: [a] -> [a]
    fromList xs = xs
