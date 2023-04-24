module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems, unique
              ) where
import Prelude hiding(null)
import Data.List(sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null s = case toList s of 
    [] -> True 
    _ -> False

member :: Eq a => a -> Set a -> Bool
member elem s = case s of
    Singleton a -> elem == a
    Union s1 s2 -> member elem s1 || member elem s2
    _ -> False

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList = foldr (Union . Singleton) Empty

toList :: Set a -> [a]
toList s = let
        toListCont s = case s of
            Empty -> id
            Singleton a -> ([a]++)
            Union s1 s2 -> toListCont s1 . toListCont s2
    in toListCont s []

toAscList :: Ord a => Set a -> [a]
toAscList s = let
    quickSortCont l = case l of
        [] -> id
        h:t -> quickSortCont (filter (<h) t) . ([h]++) . quickSortCont (filter (>h) t)
    in quickSortCont (toList s) []

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = Union

insert :: a -> Set a -> Set a
insert elem = Union (Singleton elem)

unique :: Ord a => Set a -> Set a
unique = fromList . toAscList

instance Ord a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    (<>) = Union

instance Monoid (Set a) where
    mempty = empty

instance Show a => Show (Set a) where
    show s = show (toList s)

instance Functor Set where
    fmap f s = fromList (map f (toList s))