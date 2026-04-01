{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.PQueue where

import Common.PriorityQueue (PriorityQueue(..))
import Common.MonoidalTree (MonoidalTree((|>)))
import Task1 (Measured(..), MinMax(..), Min(..), Max(..))
import Task3.Tree (
      Tree(..),
      node2,
      node3,
      RemoveResult(..),
      balance2Left,
      balance2Right,
      balance3Left,
      balance3Middle,
      balance3Right
    )

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v) }
  deriving (Show, Eq)

instance Ord k => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = MinMax (Min k, Max k)

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty

  toPriorityQueue = foldr (\(k, v) q -> insert k v q) empty

  entries (PQueue t) = go t []
    where
      go Empty acc = acc
      go (Leaf (Entry (k, v))) acc = (k, v) : acc
      go (Node2 _ a b) acc = go a (go b acc)
      go (Node3 _ a b c) acc = go a (go b (go c acc))

  insert k v (PQueue t) = PQueue (t |> Entry (k, v))

  extractMin = extract getMin . getTree
  extractMax = extract getMax . getTree

-- * Helper functions for key extraction

getMin :: Ord k => Tree (MinMax k) (Entry k v) -> Min k
getMin t = case measure t of
  MinMax (Min x, _) -> Min x
  _ -> error "getMin on empty tree"

getMax :: Ord k => Tree (MinMax k) (Entry k v) -> Max k
getMax t = case measure t of
  MinMax (_, Max x) -> Max x
  _ -> error "getMax on empty tree"

-- * Generic extraction function

extract :: forall k v b. (Ord k, Eq b, Monoid b)
        => (Tree (MinMax k) (Entry k v) -> b)
        -> Tree (MinMax k) (Entry k v)
        -> Maybe (v, PQueue k v)
extract selectKey tree = case go tree of
    Nothing -> Nothing
    Just (v, res) -> Just (v, PQueue $ fromRemoveResult res)
  where
    fromRemoveResult :: RemoveResult (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v)
    fromRemoveResult (Balanced t) = t
    fromRemoveResult (Hole t)     = t

    go :: Tree (MinMax k) (Entry k v) -> Maybe (v, RemoveResult (MinMax k) (Entry k v))
    go Empty = Nothing
    go (Leaf (Entry (_, v))) = Just (v, Hole Empty)

    go (Node2 _ l r) =
      let lKey = selectKey l
          rKey = selectKey r
      in if lKey <> rKey == lKey
         then case go l of
                Nothing -> Nothing
                Just (v, Balanced l') -> Just (v, Balanced (node2 l' r))
                Just (v, Hole l') -> Just (v, balance2Left l' r)
         else case go r of
                Nothing -> Nothing
                Just (v, Balanced r') -> Just (v, Balanced (node2 l r'))
                Just (v, Hole r') -> Just (v, balance2Right l r')

    go (Node3 _ a b c) =
      let aKey = selectKey a
          bKey = selectKey b
          cKey = selectKey c
          total = aKey <> bKey <> cKey
      in if total == aKey
         then case go a of
                Nothing -> Nothing
                Just (v, Balanced a') -> Just (v, Balanced (node3 a' b c))
                Just (v, Hole a') -> Just (v, balance3Left a' b c)
         else if total == cKey
         then case go c of
                Nothing -> Nothing
                Just (v, Balanced c') -> Just (v, Balanced (node3 a b c'))
                Just (v, Hole c') -> Just (v, balance3Right a b c')
         else case go b of
                Nothing -> Nothing
                Just (v, Balanced b') -> Just (v, Balanced (node3 a b' c))
                Just (v, Hole b') -> Just (v, balance3Middle a b' c)
