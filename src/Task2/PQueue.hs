{-# OPTIONS_GHC -Wall #-}

module Task2.PQueue where

import Common.PriorityQueue
import Task1 (Max (..), Measured (..), Min (..), MinMax (..))
import Task2.Tree

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue {getTree :: Tree (MinMax k) (Entry k v)}
  deriving (Show, Eq)


-- | Priority queue entry wrapper
newtype Entry k v = Entry {getEntry :: (k, v)}
  deriving (Show, Eq)

instance Ord k => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = MinMax (Min k, Max k)


-- * Priority queue instance
instance PriorityQueue PQueue where
  empty = PQueue Empty

  toPriorityQueue = foldr (\(k, v) q -> insert k v q) empty

  entries (PQueue t) = collect t []
    where
      collect Empty acc = acc
      collect (Leaf (Entry (k, v))) acc = (k, v) : acc
      collect (Branch _ a b) acc = collect a (collect b acc)

  insert k v (PQueue Empty) = PQueue $ leaf (Entry (k, v))
  insert k v (PQueue t)    = PQueue $ branch t (leaf (Entry (k, v)))

  extractMin = pick (<=) getMinKey . getTree
  extractMax = pick (>=) getMaxKey . getTree

getMinKey :: MinMax k -> k
getMinKey (MinMax (Min x, _)) = x
getMinKey _ = error "unreachable: empty subtree"

getMaxKey :: MinMax k -> k
getMaxKey (MinMax (_, Max x)) = x
getMaxKey _ = error "unreachable: empty subtree"

pick :: Ord k
     => (k -> k -> Bool)
     -> (MinMax k -> k)
     -> Tree (MinMax k) (Entry k v)
     -> Maybe (v, PQueue k v)
pick _ _ Empty = Nothing
pick _ _ (Leaf (Entry (_, v))) = Just (v, PQueue Empty)
pick cmp key (Branch _ l r) =
  let a = key (measure l)
      b = key (measure r)
  in if cmp a b
     then case pick cmp key l of
            Just (v, PQueue Empty) -> Just (v, PQueue r)
            Just (v, PQueue l') -> Just (v, PQueue $ branch l' r)
            Nothing -> Nothing
     else case pick cmp key r of
            Just (v, PQueue Empty) -> Just (v, PQueue l)
            Just (v, PQueue r') -> Just (v, PQueue $ branch l r')
            Nothing -> Nothing
