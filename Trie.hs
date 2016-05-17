{-
Written by Maxwell Sayles.

Implements a Trie Map data structure.
-}

module Trie where

import Control.Applicative (pure, (<$>), (<|>))
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Monoid
import Prelude hiding (lookup, null)

data Trie a b = Trie (Map a (Trie a b)) (Maybe b) deriving Show

instance Foldable (Trie a) where
  foldr f z (Trie tm tv) =
    case tv of
      Nothing -> r
      Just v -> f v r
    where r = M.foldr (\t acc -> F.foldr f acc t) z tm

foldrWithKey :: (([k], v) -> b -> b) -> b -> Trie k v -> b
foldrWithKey f z t = loop z t []
  where loop z (Trie m v) ks =
          case v of
            Nothing -> r
            Just v -> f (reverse ks, v) r
          where r = M.foldrWithKey (\k t acc -> loop acc t (k:ks)) z m
  
elems :: Trie a b -> [b]
elems = F.foldr (:) []
  
empty :: Trie a b
empty = Trie M.empty Nothing

null :: Trie a b -> Bool
null (Trie m Nothing) = M.null m
null _ = False

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert [] v (Trie tm _) = Trie tm (Just v)
insert (x:xs) v (Trie tm tv) = Trie tm' tv
    where tm' = M.alter f x tm
          f t = insert xs v <$> (t <|> pure empty)

member :: Ord a => [a] -> Trie a b -> Bool
member xs t = isJust $ lookup xs t

lookup :: Ord a => [a] -> Trie a b -> Maybe b
lookup [] (Trie _ v) = v
lookup (x:xs) (Trie m _) = M.lookup x m >>= lookup xs

delete :: Ord a => [a] -> Trie a b -> Trie a b
delete [] (Trie m _) = Trie m Nothing
delete (x:xs) (Trie m v) =
  case M.lookup x m of
    Nothing -> Trie m v
    Just t' -> let t'' = delete xs t'
               in if null t''
                  then Trie (M.delete x m) v
                  else Trie (M.insert x t'' m) v
    
