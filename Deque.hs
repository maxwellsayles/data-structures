{-
Double ended queue.

empty, singleton, pushFront, pushBack, size, and null are O(1).
front, back, popFront, and popBack are amortized O(1).

fromList and toList are O(n).
-}

module Deque (empty,
              singleton,
              front,
              back,
              pushFront,
              pushBack,
              popFront,
              popBack,
              size,
              null,
              fromList,
              toList
             ) where

import Prelude hiding (null)

-- front, back, size
data Deque a = Deque [a] [a] Int deriving Show

-- Rebalance a queue when the front is empty by moving half the back over.
-- The front half gets the remainder after division.
balanceF :: Deque a -> Deque a
balanceF (Deque [] bs size) =
    let (bs', fs') = splitAt (size `div` 2) bs
    in  Deque (reverse fs') bs' size
balanceF d = d

-- Rebalance a queue when the back is empty by moving half the front over.
-- The back half gets the remainder after division
balanceB :: Deque a -> Deque a
balanceB (Deque fs [] size) =
    let (fs', bs') = splitAt (size `div` 2) fs
    in  Deque fs' (reverse bs') size
balanceB d = d
     
size :: Deque a -> Int
size (Deque _ _ s) = s

null :: Deque a -> Bool
null d = size d == 0

empty :: Deque a
empty = Deque [] [] 0

singleton :: a -> Deque a
singleton e = Deque [e] [] 0

front :: Deque a -> (a, Deque a)
front d = let d'@(Deque (f:_) _ _) = balanceF d in (f, d')

back :: Deque a -> (a, Deque a)
back d = let d'@(Deque _ (b:_) _) = balanceB d in (b, d')

pushFront :: a -> Deque a -> Deque a
pushFront x (Deque fs bs size) = Deque (x:fs) bs (size+1)

pushBack :: a -> Deque a -> Deque a
pushBack x (Deque fs bs size) = Deque fs (x:bs) (size+1)

popFront :: Deque a -> Deque a
popFront d = let (Deque (_:fs) bs size) = balanceF d
             in  Deque fs bs (size-1)

popBack :: Deque a -> Deque a
popBack d = let (Deque fs (_:bs) size) = balanceB d
            in  Deque fs bs (size-1)

fromList :: [a] -> Deque a
fromList l = Deque l [] (length l)

toList :: Deque a -> [a]
toList (Deque fs bs _) = fs ++ reverse bs

