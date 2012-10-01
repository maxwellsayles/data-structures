{-|
Double ended queue.

empty, singleton, pushFront, pushBack, size, and null are O(1).
front, back, popFront, and popBack are amortized O(1).

fromList and toList are O(n).
-}

module Data.Deque (empty,
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
                   toList) where

import Prelude hiding (null)

{-| Deque. Members are: front, back, size -}
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
     
{-| The number of elements in the deque. -}
size :: Deque a -> Int
size (Deque _ _ s) = s

{-| True if the deque is empty. -}
null :: Deque a -> Bool
null d = size d == 0

{-| Construct an empty deque. -}
empty :: Deque a
empty = Deque [] [] 0

{-| Construct a deque of one item. -}
singleton :: a -> Deque a
singleton e = Deque [e] [] 0

{-| Get the front item from a non-empty deque.
An error will occur if this deque is empty. 
This may rebalance the deque internally and so
is a stateful computation. -}
front :: Deque a -> (a, Deque a)
front d = let d'@(Deque (f:_) _ _) = balanceF d in (f, d')

{-| Get the back item from a non-empty deque.
An error will occur if this deque is empty.
This may rebalance the deque internally and so
is a stateful computation. -}
back :: Deque a -> (a, Deque a)
back d = let d'@(Deque _ (b:_) _) = balanceB d in (b, d')

{-| Push an item onto the front of the deque. -}
pushFront :: a -> Deque a -> Deque a
pushFront x (Deque fs bs size) = Deque (x:fs) bs (size+1)

{-| Push an item onto the back of the deque. -}
pushBack :: a -> Deque a -> Deque a
pushBack x (Deque fs bs size) = Deque fs (x:bs) (size+1)

{-| Pop the front item returning a new deque.
An error will occur if the deque is empty. -}
popFront :: Deque a -> Deque a
popFront d = let (Deque (_:fs) bs size) = balanceF d
             in  Deque fs bs (size-1)

{-| Pop the back item returning a new deque.
An error will occur if the deque is empty. -}
popBack :: Deque a -> Deque a
popBack d = let (Deque fs (_:bs) size) = balanceB d
            in  Deque fs bs (size-1)

{-| Construct a deque from a list, such that the
head of the list is the front of the deque. -}
fromList :: [a] -> Deque a
fromList l = Deque l [] (length l)

{-| Construct a list from a deque, such that the
front of the deque is the head of the list. -}
toList :: Deque a -> [a]
toList (Deque fs bs _) = fs ++ reverse bs

