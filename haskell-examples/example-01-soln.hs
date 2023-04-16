-- * installing
--   - Haskell platform 
--     (from haskell.org/platform) 
--     (note the directions for OSX)
--   - GHC 
--     (from haskell.org/ghc, The Haskell Platform is GHC + robust library versions)

-- * running a haskell program
--   - runhaskell or runghc -- both look for (main :: IO ())
--   - ghc also looks for (main :: IO ())
--   - ghci -- doesn't require main, can interactively evaluate
--     (use ghci!)

-- * ghci commands
--   - ":t foo" gets the type of "foo"
--   - ":l foo.hs" loads the file "foo.hs"
--   - ":i Foo" gets information about the type class "Foo"
--   - ":k Foo" returns the kind of "Foo"

-- * file structure
module Main where

-- * basic syntax
--   - functions
--   - types (::)
--   - data types
--   - let
--   - case

-- top level definitions
factorial :: Int -> Int
-- we can define functions based on their actions on specific inputs
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- apostrophes (') are legal in variable names
-- "case e of ... " is analogous to "match e with ... " in OCaml
factorial' :: Int -> Int
factorial' n = case n of
  0 -> 1
  n -> let n' = n - 1 
           result :: Int
           result = n * factorial n'
       in result

cons :: a -> [a] -> [a]
cons x xs = x:xs

mainBasic :: IO ()
mainBasic = return ()

main :: IO ()
main = putStrLn (show (factorial' 3))


-- * types
--   - datatypes
--   - newtypes
--   - type aliases
--   - deriving (when we get to type classes)

-- Q: what is a?
-- A: it's a type variable
-- A: analagous to OCaml (a MyList)
-- type 'a MyList = Nil | Cons of ('a * 'a MyList)
-- OCaml 'type' == Haskell 'data'
-- OCaml 'type' /= Haskell 'type'
data MyList a = Nil | Cons a (MyList a)
--  deriving (Show)
-- deriving (Eq, Ord, Show) 
-- ^^ extremely common, ommitted because we will write our own instance for
-- Show

toHaskellList :: MyList a -> [a]
toHaskellList Nil = []
toHaskellList (Cons x xs) = x : toHaskellList xs

instance (Show a) => Show (MyList a) where
  show ml = show (toHaskellList ml)

myLength :: MyList a -> Int
myLength Nil = 0
myLength (Cons _ xs) = 1 + myLength xs

someList :: MyList Int
someList = Cons 1 Nil

-- newtypes have a single constructor, with a single contained type
newtype MyOtherList a = ConMyOtherList (MyList a)

type MyListAlias a = MyList a

myLength' :: MyListAlias a -> Int
myLength' = myLength

-- this does not work
-- myLength' :: MyOtherList a -> Int
-- myLength' = myLength

-- * type classes
--   - classes
--   - instances
--   - functions with constraints
--   - (perhaps discuss objects vs type classes as exists vs forall)

-- class declarations declare the "interface"
-- what would a custom version of Show look like?
class MyShow a where
  myshow :: a -> String

-- think of type classes as interfaces
-- show cannot have this type:
-- show :: forall a. a -> String 
-- note the "forall a" annotation is valid syntax if you put 
-- {-# LANGUAGE RankNTypes #-} at the top of the file
-- why not?  how do you show a function?
-- why not return "<proc>"
-- show really is "almost" universal
-- show :: (Show a) => a -> String
-- right now, MyList is not showable

data FunWrapper a b = MakeFunction (a -> b)
-- deriving (Show) -- will not work!

-- lets play with monoids!
class MyMonoid a where
  unit :: a
  times :: a -> a -> a
  -- don't forget the laws!
  -- times x unit = x
  -- times unit x = x

instance MyMonoid Int where
  unit = 1
  times x y = x * y
  -- equivalently
  -- times = (*)
  -- or
  -- times x y = (*) x y

-- be careful evaluating [mexponent 2 3] in ghci.  It will give you an
-- error if you don't fix the type of 2, like so: [mexponent (2 :: Int) 3]
mexponent :: (MyMonoid a) => a -> Int -> a
mexponent x 0 = unit
mexponent x n = times x (mexponent x (n-1))

instance MyMonoid [a] where
  unit = []
  times xs ys = xs ++ ys
  -- check the laws!
  -- [] ++ ys == ys -- check
  -- xs ++ [] == xs -- check

-- * monads
--   - definition
--   - do notation
--   - lightweight exceptions (partial vs total programming)

class MyMonad m where
  --               think "pure a -> effectful a"
  --               you can think of IO for effectful
  -- the built in Monad class calls these return and (>>=)
  return' :: a -> m a
  -- you might like to have this function
  -- run :: m a -> a
  -- this does not exist!!!
  bind1' :: m a -> (a -> m b) -> m b
  bind2' :: (a -> m b) -> (m a -> m b)
  bind2' f aM = bind1' aM f
  -- bind1' and bind2' have the same type if you flip the arguments

-- data Maybe a = Nothing | Just a
-- -- OCaml terminology would be
-- data Option a = None | Some a

instance MyMonad Maybe where
  -- you can write these signatures 
  -- if you put {-# LANGUAGE InstanceSigs #-} at the top of the file
  -- return' :: a -> Maybe a
  return' x = Just x
  -- bind1' :: Maybe a -> (a -> Maybe b) -> Maybe b
  bind1' Nothing f = Nothing
  bind1' (Just a) f = f a

-- look up strings associated with 1, 2, 3, and concatenate them.
-- returns nothing if either 1, 2 or 3 are not in the list
complicated :: [(Int, String)] -> Maybe String
complicated things = case lookup 1 things of
  Nothing -> Nothing
  Just s1 -> case lookup 2 things of
      Nothing -> Nothing
      Just s2 -> case lookup 3 things of
          Nothing -> Nothing
          Just s3 -> Just (s1 ++ s2 ++ s3)

complicated2 :: [(Int, String)] -> Maybe String
complicated2 things =
  lookup 1 things >>= \ s1 ->
  lookup 2 things >>= \ s2 ->
  lookup 3 things >>= \ s3 ->
  return (s1 ++ s2 ++ s3)

complicated3 :: [(Int, String)] -> Maybe String
complicated3 things = do
  s1 <- lookup 1 things
  s2 <- lookup 2 things
  s3 <- lookup 3 things
  -- the syntax of let changes inside of 'do notation'
  let result = s1 ++ s2 ++ s3
  -- note there is no 'in' keyword
  return result

-- last note: check out haskell.org/hoogle

-- we didn't get to these topics...
--
-- * laziness
--   - infinite data structures
--   - weak-head-normal form
-- * IO
--   - printing
--   - user input
--   - fully evaluating a computation
-- * helpful tools
--   - hackage docs
--   - prelude source
