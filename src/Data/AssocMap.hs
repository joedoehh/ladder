module Data.AssocMap
  ( AssocMap,
    empty,
    member,
    alter,
    delete,
    insert,
    lookup,
    findWithDefault,
  )
where

import Prelude hiding (lookup)
import Data.Maybe

newtype AssocMap k v = AssocMap [(k, v)]
    deriving (Show)

member :: Eq k => k -> AssocMap k v -> Bool
member key (AssocMap xso) = member' key xso
  where
    member' :: Eq k => k -> [(k, v)] -> Bool
    member' _ [] = False
    member' x ((x', _) : xs)
      | x' == x = True
      | otherwise = member' x xs

alter :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter fo keyo (AssocMap xso) = AssocMap (alter' fo keyo xso)
  where
    alter' :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
    alter' f key [] =
      case f Nothing of
        Nothing -> []
        Just value -> [(key, value)]
    alter' f key ((key', value') : xs)
      | key == key' =
        case f (Just value') of
          Nothing -> xs
          Just value -> (key, value) : xs
      | otherwise =
        (key', value') : alter' f key xs

empty :: AssocMap k v
empty = AssocMap []

delete :: Eq k => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: Eq k => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

lookup :: (Eq k) => k -> AssocMap k v -> Maybe v
lookup keyo (AssocMap xso) = lookup' keyo xso
  where
    lookup' _ [] = Nothing
    lookup' key ((key', value) : xs)
      | key == key' = Just value
      | otherwise = lookup' key xs

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault defaultValue key mapo = fromMaybe defaultValue (lookup key mapo)