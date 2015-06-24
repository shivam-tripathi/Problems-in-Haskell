
-- Data <datatype name capital cased> = <Value constructors capital cased if non numerical>

-- Algebraic Data Type

data Bool' = True' | False'

-- Value constructors are followed by related fields.
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- Shape is data type and Circle, Rectangle are value constructors.
-- The behaviour of value constructors is similar to functions, they take fields and give type.
-- They take in values and return a datatype.

-- We can pattern match against value constructors.
surfaceArea :: Shape -> Float
surfaceArea (Circle _ _ r) = (*) pi $ r ^ 2
surfaceArea (Rectangle a b c d) = abs $ (a - c) * (b - d)

-- Value constructors can be mapped, curried etc. as their behavior is like functions.
-- 'deriving' is used to make the type part of typeclasses.

-- Data types can be nested. We already have been nesting by using Float type.
-- This also increases abstract nature.
data Point' = Point' Float Float deriving (Show)
data Shape' = Circle' Point' Float | Rectangle' Point' Point' deriving (Show)
surface' :: Shape' -> Float
surface' (Circle' _ r) = (*) pi $ r ^ 2
surface' (Rectangle' (Point' a b) (Point' c d)) = abs $ (a - c) * (b - d)

nudge' :: Shape' -> Float -> Float -> Shape'
nudge' (Circle' (Point' x y) r) sx sy = Circle' (Point' (x+sx) (y+sy)) r
nudge' (Rectangle' (Point' x1 y1 ) (Point' x2 y2)) sx sy = Rectangle' (Point' (x1+sx) (y1+sy)) (Point' (x2+sx) (x2+sy))

{--
Types can be imported with modules.

module <moduleName>
(..
 <type> (..)
 <function>
 ..
) where
    .. rest of the code ..

--}

-- <type> (..) has all the value constructors of <type>
-- We can stop any value constructors to be exported by loosing (..) term.
-- This forces one when using a type by only using functions returning the type.

-- Record Syntax
data Person = Person{
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String
} deriving (Show)

-- In <name> :: field, <name> serves as a function which takes a <type> and returns the value
-- of the related field. Hence <name> is small cased.

data Car = Car{
    company :: String,
    model :: String,
    year :: Int
} deriving (Show)
