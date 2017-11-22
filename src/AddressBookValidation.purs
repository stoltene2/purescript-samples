module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Person(..), person)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldMapDefaultL, foldl, foldr, sequence, traverse)

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  fullName <$> (first  `withError` "First name was missing")
           <*> (middle `withError` "Middle name was missing")
           <*> (last   `withError` "Last name was missing")


fullName :: String -> String -> String -> String
fullName f m l = f <> " " <> m <> " " <> l


withError :: forall a. Maybe a -> String -> Either String a
withError Nothing err = Left err
withError (Just a) err = Right a


nonEmpty :: String -> String -> Either String Unit
nonEmpty field "" = Left (field <> " cannot be empty")
nonEmpty _ _      = Right unit


validatePerson :: Person -> Either String Person
validatePerson (Person o) =
  person <$> (nonEmpty "First name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last name" o.lastName  *> pure o.lastName)
         <*> pure o.homeAddress
         <*> pure o.phones




-- Examples
combineMaybeList :: List (Maybe Int)
combineMaybeList = combineMaybe (Just (Cons 1 (Cons 2 Nil)))


combineMaybeMaybe :: Maybe (Maybe Int)
combineMaybeMaybe = combineMaybe (Just (Just 1 :: Maybe Int))

combineMaybeNothing :: Maybe (Maybe Int)
combineMaybeNothing = combineMaybe (Just Nothing)
-- Nothing


combineMaybeEither :: Either String (Maybe String)
combineMaybeEither = combineMaybe (Just (Left "error"))


-- This is exercise 7.8#3
{-

(Difficult) Write a function combineMaybe which has type forall a
f. Applicative f => Maybe (f a) -> f (Maybe a). This function takes an
optional computation with side- effects, and returns a side-effecting
computation which has an optional result.
-}

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just f) = Just <$> f


-- This is exercise 7.11#1
{-
(Medium) Write a Traversable instance for the following binary tree data structure, which combines side-effects from left-to-right:

> data Tree a = Leaf | Branch (Tree a) a (Tree a)

This corresponds to an in-order traversal of the tree. What about a
preorder traversal? What about reverse order?

-}

newtype Sum = Sum Int

instance sumMonoid :: Monoid Sum where
  mempty = Sum 0

instance sumSemigroup :: Semigroup Sum where
  append (Sum a) (Sum b) = Sum (a + b)

instance showSum :: Show Sum where
  show (Sum a) = show a


data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch l val r) = "(Branch (" <> show l <> ") " <> show val <> " (" <> show r <> "))"


sampleLeaf :: Tree Int
sampleLeaf = Leaf

sampleTree :: Tree Int
sampleTree = Branch (Branch Leaf 1 Leaf) 1 (Branch Leaf 1 Leaf)

sampleStringTree :: Tree String
sampleStringTree = Branch (Branch Leaf "abc" Leaf) "bcd" (Branch Leaf "cde" Leaf)


-- This counts 1 for every non-leaf element in the tree
countElementsInTree :: forall a. Tree a -> Int
countElementsInTree t = let (Sum i) = foldMap (const (Sum 1)) t
                        in i

allValidInTree :: Maybe (Tree Int)
allValidInTree = sequence (Branch Leaf (Just 1) Leaf)

-- How many elements in 4 generated subtrees
--countElementsInTree $ foldr (\a b -> (Branch b a b) :: Tree Int) Leaf (1..4)

instance mapTree :: Functor Tree where
  map f Leaf = Leaf
  map f (Branch l val r) = Branch (map f l) (f val) (map f r)

instance foldableTree :: Foldable Tree where
  foldl _ b Leaf = b
  foldl f b (Branch l val r) =
    let b'   = foldl f b l
        val' = f b' val
    in foldl f val' r

  foldr _ b Leaf = b
  foldr f b (Branch l val r) =
    let b' = foldr f b r
        val' = f val b'
    in foldr f val' l

  foldMap = foldMapDefaultL

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l val r) = Branch <$> (traverse f l) <*> f val <*> (traverse f r)
  sequence Leaf = pure Leaf
  sequence b@(Branch l val r) = traverse id b
--  sequence f (Branch l val r) = Branch <$> sequence l <*> pure val <*> sequence r

allNodesEven :: Maybe (Tree Int)
allNodesEven = traverse (\x -> if x `mod` 2 == 0 then (Just x) else Nothing) (Branch Leaf 2 Leaf)
--(Just (Branch (Leaf) 2 (Leaf)))
