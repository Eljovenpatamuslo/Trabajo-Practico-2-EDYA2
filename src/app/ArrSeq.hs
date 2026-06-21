{-# LANGUAGE InstanceSigs #-}
module ArrSeq where

import Seq
import qualified Arr as A
import Par ((|||))
import Arr ((!))

instance Seq A.Arr where
    emptyS :: A.Arr a
    emptyS = A.empty

    singletonS :: a -> A.Arr a
    singletonS x = A.tabulate (const x) 1

    lengthS :: A.Arr a -> Int
    lengthS = A.length

    nthS :: A.Arr a -> Int -> a
    nthS = (!)

    tabulateS :: (Int -> a) -> Int -> A.Arr a
    tabulateS = A.tabulate

    mapS :: (a -> b) -> A.Arr a -> A.Arr b
    mapS f s = A.tabulate (\i -> f (s ! i)) (A.length s)


    filterS :: (a -> Bool) -> A.Arr a -> A.Arr a
    filterS p s = joinS (mapS (\x -> if p x then singletonS x else emptyS) s)

    appendS :: A.Arr a -> A.Arr a -> A.Arr a
    appendS s t = 
        let lenS = A.length s
            lenT = A.length t
        in A.tabulate (\i -> if i < lenS then s ! i else t ! (i - lenS)) (lenS + lenT)

    takeS :: A.Arr a -> Int -> A.Arr a
    takeS s n = 
        let n' = max 0 (min n (A.length s))
        in A.subArray 0 n' s

    dropS :: A.Arr a -> Int -> A.Arr a
    dropS s n = 
        let n' = max 0 (min n (A.length s))
        in A.subArray n' (A.length s - n') s

    showtS :: A.Arr a -> TreeView a (A.Arr a)
    showtS s =
        let len = A.length s
        in if len == 0 then EMPTY
           else if len == 1 then ELT (s ! 0)
           else let half = len `div` 2
                    l = takeS s half
                    r = dropS s half
                in NODE l r

    showlS :: A.Arr a -> ListView a (A.Arr a)
    showlS s =
        if A.length s == 0 then NIL
        else CONS (s ! 0) (dropS s 1)

    joinS :: A.Arr (A.Arr a) -> A.Arr a
    joinS = A.flatten

    reduceS :: (a -> a -> a) -> a -> A.Arr a -> a
    reduceS op b s = case showtS s of
        EMPTY -> b
        ELT v -> v
        NODE l r -> let (l1, r1) = reduceS op b l ||| reduceS op b r
                    in op l1 r1

    scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
    scanS op b s
        | A.length s == 0 = (emptyS, b)
        | A.length s == 1 = (singletonS b, op b (s ! 0))
        | otherwise =
            let n = A.length s
                --sumamos adyacentes de a pares
                halves = A.tabulate (\i -> op (s ! (2*i)) (s ! (2*i+1))) (n `div` 2)
                
                --Llamada recursiva sobre la mitad del tamaño
                (evens, totalHalves) = scanS op b halves
                
                --intercalamos los resultados
                res = A.tabulate (\i ->
                        let idx = i `div` 2
                            base = if idx < A.length evens then evens ! idx else totalHalves
                        in if even i
                           then base
                           else op base (s ! (i - 1))
                    ) n
            in if even n
               then (res, totalHalves)
               else (res, op totalHalves (s ! (n - 1)))

    fromList :: [a] -> A.Arr a
    fromList = A.fromList