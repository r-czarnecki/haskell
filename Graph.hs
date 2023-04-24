module Graph where
import Set(Set)
import qualified Set as Set
import Data.List((\\))
class Graph g where
    empty   :: g a
    vertex  :: a -> g a
    union   :: g a -> g a -> g a
    connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
            | Vertex a
            | Union (Basic a) (Basic a)
            | Connect (Basic a) (Basic a)

instance Graph Relation where
    empty = Relation Set.empty Set.empty
    vertex v = Relation (Set.singleton v) Set.empty
    union g1 g2 = Relation (Set.union (domain g1) (domain g2)) (Set.union (relation g1) (relation g2))
    connect g1 g2 = let
            connectVertex v d = fmap (\elem -> (v, elem)) d
            connectTwoDomains d1 d2 = case Set.toList d1 of
                h:t -> Set.union (connectVertex h d2) (connectTwoDomains (Set.fromList t) d2)
                _ -> Set.empty
            d = connectTwoDomains (domain g1) (domain g2)
        in Relation (domain g) (Set.union (relation g) d) where
            g = union g1 g2

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    connect = Connect

instance Ord a => Eq (Basic a) where
    (==) g1 g2 = fromBasicRel g1 == fromBasicRel g2 where
        fromBasicRel = fromBasic :: Basic a -> Relation a

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
    (<>) = union

instance Monoid (Basic a) where
    mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic g = case g of
        Empty -> empty
        Vertex a -> vertex a
        Union g1 g2 -> union (fromBasic g1) (fromBasic g2)
        Connect g1 g2 -> connect (fromBasic g1) (fromBasic g2)
        
getRepresentation :: (Ord a, Show a) => Basic a -> (Set (a, a), Set a)
getRepresentation g = let
        getVertices edges = Set.fromList (foldr (\(a,b) r -> [a,b] ++ r) [] (Set.toList edges))
        getUnconnected all_v connected_v = Set.fromList (Set.toList (Set.unique all_v) \\ Set.toList connected_v)
        rel_g = (fromBasic :: Basic a -> Relation a) g
    in (Set.unique (relation rel_g), getUnconnected (domain rel_g) (getVertices (relation rel_g)))

instance (Ord a, Show a) => Show (Basic a) where
    show g = let
            showCont (e,v) = ("edges "++) . (show e ++) . (" + vertices "++) . (show v ++)
        in showCont (getRepresentation g) []

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g = let
        writeEdges e = case e of
            (a,b):t -> (show a ++) . (" -> "++) . (show b ++) . (";\n"++) . writeEdges t
            _ -> id
        writeVertices v = case v of
            h:t -> (show h ++) . (";\n"++) . writeVertices t
            _ -> id
        writeGraph (e,v) = ("digraph {\n"++) . writeEdges (Set.toList e) . writeVertices (Set.toList v) . ("}\n"++)
    in writeGraph (getRepresentation g) ""

instance Functor Basic where
    fmap f g = case g of
        Empty -> Empty
        Vertex a -> Vertex (f a)
        Union g1 g2 -> Union (fmap f g1) (fmap f g2)
        Connect g1 g2 -> Connect (fmap f g1) (fmap f g2)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c g = case g of
    Empty -> Empty
    Vertex v -> if v == a || v == b then Vertex c else Vertex v
    Union g1 g2 -> Union (mergeV a b c g1) (mergeV a b c g2)
    Connect g1 g2 -> Connect (mergeV a b c g1) (mergeV a b c g2)

instance Applicative Basic where
    pure f = Vertex f
    (<*>) f g = case f of
        Empty -> Empty
        Vertex v -> fmap v g
        Union g1 g2 -> Union (g1 <*> g) (g2 <*> g)
        Connect g1 g2 -> Connect (g1 <*> g) (g2 <*> g)


instance Monad Basic where
    (>>=) g f = case g of
        Empty -> Empty
        Vertex v -> f v
        Union g1 g2 -> Union (g1 >>= f) (g2 >>= f)
        Connect g1 g2 -> Connect (g1 >>= f) (g2 >>= f)
    return = Vertex
-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = let
        splitVertex v = if v == a then Union (Vertex b) (Vertex c) else Vertex v
    in g >>= splitVertex