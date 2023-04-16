-- Examples of functors
--
-- Trees
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- To prove our new typeclass Tree is a functor, we need WTS it obeys the functor laws:
-- Distributive: for functions f and g, fmap (f . g) is the same as (fmap f) . (fmap g) 
-- Identity: fmap id = id
--
main :: IO ()
-- demonstrating the functor laws 
main = let myTree = (Branch (Branch (Leaf 1) (Leaf 5)) (Leaf 2)) in
        let f = (\n -> n + 1) in
        let g = (\n -> n * 2) in
        let id = (\n -> n) in
        do 
            print (fmap (f . g) myTree)
            print ((fmap f . fmap g) myTree)
            print (fmap id myTree)
