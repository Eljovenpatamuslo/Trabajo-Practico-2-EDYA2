{-# LANGUAGE InstanceSigs #-}
module ListSeq where

-- Importamos el operador directamente para no tener que escribir P.|||
import Par ((|||)) 
import Seq

instance Seq [] where
    emptyS :: [a]
    emptyS = []

    singletonS :: a -> [a]
    singletonS x = [x]
    
    lengthS :: [a] -> Int
    lengthS = length

    nthS :: [a] -> Int -> a
    nthS = (!!) 

    tabulateS :: (Int -> a) -> Int -> [a]
    tabulateS f n = tab 0
        where tab i | i == n    = []
                    | otherwise = let (x, xs) = f i ||| tab (i+1) 
                                  in x : xs

    mapS :: (a -> b) -> [a] -> [b]
    mapS _ []     = []  -- Caso base agregado
    mapS f (x:xs) = let (x', xs') = f x ||| mapS f xs 
                    in x' : xs'

    filterS :: (a -> Bool) -> [a] -> [a]
    filterS _ []     = []  -- Caso base agregado
    filterS p (x:xs) = let (b, xs') = p x ||| filterS p xs 
                       in if b then x : xs' else xs'

    appendS :: [a] -> [a] -> [a]
    appendS = (++)

    takeS :: [a] -> Int -> [a]
    takeS [] _ = []
    takeS _ n | n <= 0 = []
    takeS (x:xs) n = x : takeS xs (n - 1)
    
    dropS :: [a] -> Int -> [a]
    dropS [] _ = []
    dropS s n | n <= 0 = s
    dropS (_:xs) n = dropS xs (n - 1)

    showtS :: [a] -> TreeView a [a]
    showtS []  = EMPTY
    showtS [x] = ELT x
    showtS xs  = let half = length xs `div` 2
                     (l, r) = splitAt half xs
                 in NODE l r

    showlS :: [a] -> ListView a [a]
    showlS []     = NIL
    showlS (x:xs) = CONS x xs

    joinS :: [[a]] -> [a]
    joinS [] = []
    joinS (x:xs) = x ++ joinS xs

    reduceS :: (a -> a -> a) -> a -> [a] -> a
    reduceS op b s = case showtS s of
        EMPTY -> b
        ELT v -> v
        NODE l r -> let (l1, r1) = reduceS op b l ||| reduceS op b r
                    in op l1 r1

    scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)
    scanS f b xs = aux b xs
        where
            aux acc [] = ([], acc)
            aux acc (y:ys) = 
                let (restAccs, final) = aux (f acc y) ys
                in  (acc : restAccs, final)

    fromList :: [a] -> [a]
    fromList xs = xs