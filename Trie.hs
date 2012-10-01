{-
Written by Maxwell Sayles.

Implements a Trie data structure.

Maps a list to a value.  Runtime is O(m) for a list of m elements.
-}

module Trie where

import Control.Applicative (pure, (<$>), (<|>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isJust)
import Prelude hiding (lookup)

data Trie a b = Trie (Map a (Trie a b)) (Maybe b) deriving Show

empty :: Trie a b
empty = Trie Map.empty Nothing

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert [] v (Trie tm _) = Trie tm (Just v)
insert (x:xs) v (Trie tm tv) = Trie tm' tv
    where tm' = Map.alter f x tm
          f x = insert xs v <$> (x <|> pure empty)

member :: Ord a => [a] -> Trie a b -> Bool
member xs t = isJust $ lookup xs t

lookup :: Ord a => [a] -> Trie a b -> Maybe b
lookup [] (Trie _ v) = v
lookup (x:xs) (Trie m _) = Map.lookup x m >>= lookup xs


