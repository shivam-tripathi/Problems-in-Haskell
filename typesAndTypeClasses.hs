import Data.List
import Data.Char

-- Statically Typed system
-- Type inference
-- Every expression has a type
-- :t gives the type of the expression
-- :: is read as 'has type of'
-- Explicit types are always in capital.
{--
Due to type inference, while using standard data types one need not give
explicit type declaration. But when user made types are used or when
restriction or removal of ambiguity is needed for type declaration, one
must give explicit type declarations.
--}

-- In function's type declaration, there is no difference between parameters
-- and return type, due to curried nature of functions.
prodOfThree ::  Int -> Int -> Int -> Int
prodOfThree x y z = x * y * z

-- Type variable (small cased) are used when any type can be used. Example, tail
-- or head.
head' :: [a] -> a
head' (x:xs) = x

-- Polymorphic functions have type declaration as type variable.
fst' :: (a,b) -> a
fst' (a,b) = a

-- Typeclasses
-- An interface that defines some behaviour, any type part of a certain Typeclass
-- implements all the behaviour of that typeclass.

{--

:t (==)
(==) :: (Eq a) => a -> a -> Bool

:t (*)
(*) :: (Num a) => a -> a -> a

:t (>)
(>) :: (Ord a) => a -> a -> Bool

:t compare
compare :: (Ord a) => a -> a -> Ordering

:t LT
LT :: Ordering

--}

-- Members of Show typeclass can be presented as strings.
-- show function takes a type constructor can presents it as a string.

-- Member of Read typeclass can be converted from strings.
-- read function takes a string and converts it to type constructor.
-- If the read is applied to only string not used in any expression, then
-- one must provide type explicitly (:: <type>)

{--
:t read
read :: Read a => String -> a

:t show
show :: Show a => a -> String

> read "(3, 'a')" :: (Int, Char)
(3,'a')

--}

-- Enum types can be used in lists, they have defined successor and predecessor.
-- Enums are sequentially ordered types, include:
-- (), Bool, Char, Ordering, Int, Integer, Float and Double
-- How is () an instance of Enum typeclass? Only when it's individual members are
-- of Enum typeclass?

-- Bounded typeclass members have lower and upper bound.
-- maxBound and minBound.
-- maxBound :: <type>
-- minBound :: <type>
-- Integer is not an instance of Bounded typeclass.
-- :t minBound
-- minBound :: Bounded a => a
-- :t maxBound
-- maxBound :: Bounded a => a

-- Num is a numberic typeclass.
-- Non fractional numbers are polymorphic in nature, as they can be Int, Integer, Float, Double.
-- Integral typeclass only has Int and Integer.
-- Fractional typeclass only has Float and Double.

-- Num typeclass members are necessarily part of Show and Eq typeclass too.

{-- Some useful functions:

    fromIntegral >
        :t fromIntegral
        fromIntegral :: (Integral a, Num b) => a -> b

    fromEnum >
        :t fromEnum
        fromEnum :: Enum a => a -> Int

    fromInteger >
        :t fromInteger
        fromInteger :: Num a => Integer -> a

    fromRational >
        :t fromRational
        fromRational :: Fractional a => Rational -> a

--}
