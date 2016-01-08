module Nat where

import Prelude hiding (Enum(..), sum)


--
-- * Part 1: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 0.
zero :: Nat
zero = Zero

-- | The number 1.
one :: Nat
one = Succ zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
pred :: Nat -> Nat
pred (Succ n) = n
pred Zero = Zero


-- | True if the given value is zero.
--
--   >>> isZero zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero n = n == Zero


-- | Convert a natural number to an integer.
--
--   >>> toInt zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt (Zero) = 0
toInt (Succ n) = 1 + (toInt $ n)


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   
add :: Nat -> Nat -> Nat
add (Succ f) (Succ s) = add f (Succ $ Succ s)
add Zero s = s
add s Zero = s


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub (Succ f) (Succ s) = sub f s
sub Zero (Succ s) = Zero
sub (Succ s) Zero = Succ s
sub Zero Zero  = Zero


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt (Succ f) (Succ s) = gt f s
gt Zero _ = False
gt (Succ s) Zero = True



-- | Multiply two natural numbers.
--
--   >>> mult two zero
--   Zero
--
--   >>> mult zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult Zero s = Zero
mult f Zero = Zero
mult f s = multHelp f Zero s

multHelp :: Nat -> Nat -> Nat -> Nat
multHelp const accu (Succ s) = add (add accu const) (multHelp const accu s)
multHelp const accu Zero = accu


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum ns = foldr add Zero ns


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = [one] ++ map (add two) odds
