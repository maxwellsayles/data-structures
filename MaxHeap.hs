{-# LANGUAGE BangPatterns #-}

{-
Immutable max-heap with

add - O(logn)
removeTop - O(logn)
top - O(1)
-}
module MaxHeap (MaxHeap,
                empty,
                singleton,
                fromList,
                toList,
                top,
                removeTop,
                insert,
                foldr,
                size,
                null) where

import Data.Bits (shiftR)
import Prelude hiding (foldr, null)
import qualified Prelude

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

data MaxHeap a = MaxHeap { size :: {-# UNPACK #-} !Int,
                           root :: Tree a }
                 deriving Show

empty :: MaxHeap a
empty = MaxHeap 0 Leaf

singleton :: a -> MaxHeap a
singleton x = MaxHeap 1 (Node x Leaf Leaf)

fromList :: Ord a => [a] -> MaxHeap a
fromList = Prelude.foldr insert empty

toList :: MaxHeap a -> [a]
toList = MaxHeap.foldr (:) []

top :: Ord a => MaxHeap a -> Maybe a
top (MaxHeap _ Leaf) = Nothing
top (MaxHeap _ (Node !v _ _)) = Just v

{-|
Finds and removes the last element. Then removes the top element
and places the last element on top.  Then restores the heap property.
-}
removeTop :: Ord a => MaxHeap a -> MaxHeap a
removeTop (MaxHeap _ Leaf) = MaxHeap 0 Leaf
removeTop (MaxHeap 1 _) = MaxHeap 0 Leaf
removeTop (MaxHeap s root@(Node _ l r)) =
    let v' = findLast s root
        Node _ l' r' = deleteLast s root
    in  MaxHeap (s-1) $ heapify (Node v' l' r')
    where 
      -- finds the last element in the tree
      findLast :: Int -> Tree a -> a
      findLast 1 (Node v _ _) = v
      findLast i (Node _ l r) = findLast (i `shiftR` 1) $
                                if even i then l else r
                     
      -- returns a new tree with the last element deleted
      deleteLast 1 _ = Leaf
      deleteLast i (Node v l r)
          | even i    = Node v (deleteLast i' l) r
          | otherwise = Node v l (deleteLast i' r)
          where i' = i `shiftR` 1

      -- restores the heap property when all but the root element is a heap
      heapify n@(Node _ Leaf Leaf) = n
      heapify n@(Node pv (Node lv ll lr) Leaf)
          | pv < lv = Node lv (Node pv ll lr) Leaf
          | otherwise = n
      heapify n@(Node pv Leaf (Node rv rl rr))
          | pv < rv = Node rv Leaf (Node pv rl rr)
          | otherwise = n
      heapify n@(Node pv l@(Node lv ll lr) r@(Node rv rl rr))
          | pv < lv && pv < rv =
              if lv < rv then Node rv l (heapify (Node pv rl rr))
              else Node lv (heapify (Node pv ll lr)) r
          | pv < lv = Node lv (heapify (Node pv ll lr)) r
          | pv < rv = Node rv l (heapify (Node pv rl rr))
          | otherwise = n                  

{-|
Insert element x into heap h.
  
If current node is a Leaf, create a Node with x.
Otherwise, branch left or right depending on the size of the heap
insert x at a leaf, and then restore the heap property as we return.
-}
insert :: Ord a => a -> MaxHeap a -> MaxHeap a
insert !x (MaxHeap s h) = MaxHeap s' $ loop s' h
    where s' = s+1
          loop i Leaf = Node x Leaf Leaf
          loop i (Node v l r)
              | i_mod_2 == 0 =
                  if v < lv then Node lv (Node v ll lr) r
                  else Node v l' r
              | otherwise = 
                  if v < rv then Node rv l (Node v rl rr)
                  else Node v l r'
              where (i', i_mod_2)      = i `divMod` 2
                    l'@(Node lv ll lr) = loop i' l
                    r'@(Node rv rl rr) = loop i' r

{-| Fold over the elements of the heap in an arbitrary order. -}
foldr :: (a -> b -> b) -> b -> MaxHeap a -> b
foldr f acc (MaxHeap _ h) = loop h acc
    where loop Leaf = id
          loop (Node v l r) = loop l . loop r . f v

{-| True if the heap is empty. -}
null :: MaxHeap a -> Bool
null (MaxHeap 0 _) = True
null _             = False