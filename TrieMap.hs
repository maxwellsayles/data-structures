{-
Written by Maxwell Sayles.

Implements a TrieMap Map data structure.
-}

module TrieMap where

import Control.Applicative (pure, (<$>), (<|>))
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Monoid
import Prelude hiding (lookup, null)

data TrieMap a b = TrieMap (Map a (TrieMap a b)) (Maybe b) deriving Show

instance Foldable (TrieMap a) where
  foldr f z (TrieMap tm tv) =
    case tv of
      Nothing -> r
      Just v -> f v r
    where r = M.foldr (\t acc -> F.foldr f acc t) z tm

foldrWithKey :: (([k], v) -> b -> b) -> b -> TrieMap k v -> b
foldrWithKey f z t = loop z t []
  where loop z (TrieMap m v) ks =
          case v of
            Nothing -> r
            Just v -> f (reverse ks, v) r
          where r = M.foldrWithKey (\k t acc -> loop acc t (k:ks)) z m
  
elems :: TrieMap a b -> [b]
elems = F.foldr (:) []
  
empty :: TrieMap a b
empty = TrieMap M.empty Nothing

null :: TrieMap a b -> Bool
null (TrieMap m Nothing) = M.null m
null _ = False

insert :: Ord a => [a] -> b -> TrieMap a b -> TrieMap a b
insert [] v (TrieMap tm _) = TrieMap tm (Just v)
insert (x:xs) v (TrieMap tm tv) = TrieMap tm' tv
    where tm' = M.alter f x tm
          f t = insert xs v <$> (t <|> pure empty)

member :: Ord a => [a] -> TrieMap a b -> Bool
member xs t = isJust $ lookup xs t

lookup :: Ord a => [a] -> TrieMap a b -> Maybe b
lookup [] (TrieMap _ v) = v
lookup (x:xs) (TrieMap m _) = M.lookup x m >>= lookup xs

delete :: Ord a => [a] -> TrieMap a b -> TrieMap a b
delete [] (TrieMap m _) = TrieMap m Nothing
delete (x:xs) (TrieMap m v) =
  case M.lookup x m of
    Nothing -> TrieMap m v
    Just t' -> let t'' = delete xs t'
               in if null t''
                  then TrieMap (M.delete x m) v
                  else TrieMap (M.insert x t'' m) v
    
