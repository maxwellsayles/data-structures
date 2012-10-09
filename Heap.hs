{-
Immutable max-heap with

add - O(logn)
removeTop - O(logn)
top - O(1)
-}
module Heap (Heap,
             empty,
             singleton,
             fromList,
             toList,
             top,
             removeTop,
             insert,
             foldr,
             size) where

import Prelude hiding (foldr)
import qualified Prelude

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show
data Heap a = Heap Int (Tree a) deriving Show

empty :: Heap a
empty = Heap 0 Leaf

singleton :: a -> Heap a
singleton x = Heap 1 (Node x Leaf Leaf)

fromList :: Ord a => [a] -> Heap a
fromList = Prelude.foldr insert empty

toList :: Heap a -> [a]
toList = Heap.foldr (:) [] 

top :: Ord a => Heap a -> Maybe a
top (Heap _ Leaf) = Nothing
top (Heap _ (Node v _ _)) = Just v

{-|
Finds and removes the last element. Then removes the top element
and places the last element on top.  Then restores the heap property.
-}
removeTop :: Ord a => Heap a -> Heap a
removeTop (Heap _ Leaf) = Heap 0 Leaf
removeTop (Heap 1 _) = Heap 0 Leaf
removeTop (Heap s root@(Node _ l r)) = Heap (s-1) $ heapify (Node v' l' r')
    where 
      Node _ l' r' = deleteMin s root
      v' = findMin s root

      -- finds the last element in the tree
      findMin 1 (Node v _ _) = v
      findMin i (Node _ l r) = findMin (i `div` 2) $ if even i then l else r
                     
      -- returns a new tree with the last element deleted
      deleteMin 1 _ = Leaf
      deleteMin i (Node v l r)
          | even i    = Node v (deleteMin i' l) r
          | otherwise = Node v l (deleteMin i' r)
          where i' = i `div` 2

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
insert :: Ord a => a -> Heap a -> Heap a
insert x (Heap s h) = Heap s' $ loop s' h
    where s' = s+1
          loop i Leaf = Node x Leaf Leaf
          loop i (Node v l r)
              | goLeft = 
                  if v < lv then Node lv (Node v ll lr) r
                  else Node v l' r
              | otherwise = 
                  if v < rv then Node rv l (Node v rl rr)
                  else Node v l r'
              where goLeft             = i `mod` 2 == 0
                    i'                 = i `div` 2
                    l'@(Node lv ll lr) = loop i' l
                    r'@(Node rv rl rr) = loop i' r

{-| Fold over the elements of the heap in an arbitrary order. -}
foldr :: (a -> b -> b) -> b -> Heap a -> b
foldr f acc (Heap _ h) = loop h acc
    where loop Leaf = id
          loop (Node v l r) = loop l . loop r . f v

{-| Return the number of elements in the heap. -}
size :: Heap a -> Int
size (Heap s _) = s

