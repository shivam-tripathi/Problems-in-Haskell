
-- Data <datatype name capital cased> = <Value constructors capital cased if non
-- numerical>

-- Algebraic Data Type

data Bool' = True' | False'

-- Value constructors are followed by related fields.
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- Shape is data type and Circle, Rectangle are value constructors.
-- The behaviour of value constructors is similar to functions, they take fields
-- and give type.
-- They take in values and return a datatype.

-- We can pattern match against value constructors.
surfaceArea :: Shape -> Float
surfaceArea (Circle _ _ r) = (*) pi $ r ^ 2
surfaceArea (Rectangle a b c d) = abs $ (a - c) * (b - d)

-- Value constructors can be mapped, curried etc. as their behavior is like
-- functions.
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

-- In <name> :: field, <name> serves as a function which takes a <type> and
-- returns the value
-- of the related field. Hence <name> is small cased.

data Car = Car{
    company :: String,
    model :: String,
    year :: Int
} deriving (Show)

-- Type constructors

-- No variable can have only Maybe data type, it has to have some other data
-- type associated with it. Like Maybe Int, or Maybe String etc depending on the
-- type passed, here try.
-- :t Just
-- Just :: a -> Maybe a
-- :t Just "try"
-- Just :: [Char] -> Maybe [Char]

data Car' a b c = Car' {
    company' :: a,
    model' :: b,
    year' :: c
}

-- This helps in increasing polymorhism.
-- This wont help much, as the designed function will be already having fixed
-- data types being handled. Not all data types will be used.

-- We used type constructors when type contained inside the value constructors
-- of the data type is of no consequence. That type inside the value
-- constructors can be specified later.
-- For example, a list of things is abstract, i.e. it does not depend on what is
-- the type of data type it is a part of. Maybe is Just some value or Nothing.

-- Adding typeclass constraint is also possible in defining the data constructor
-- by limiting the datatype passed as argument to that typeclass.

-- For example:
-- data (Ord k) => Map k v = ...
-- (Typeclass <> ) => restricts the argument data type to the typeclass <>.
-- deriving (<>) makes the defined data type an instance of the <> typeclass.

-- It is not recommend to put typeclass restriction in type constructors, as all
-- functions using it will have the same restriction without putting the
-- restriction anyway, and even when there is no use of typeclass restriction,
-- still function will have that restriction.
-- So even if it makes sense, typeclass restriction must be avoided, as we end
-- up writing the restrictions in the functions anyway.

data Vector a = Vector a a a deriving Show

-- Functions taking data type made by type constructors.
vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector a b c) = Vector (a+i) (b+j) (c+k)

vmult :: (Num t) => Vector t -> t -> Vector t
vmult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
scalarMult (Vector i j k) (Vector a b c) = (a*i) + (b*j) + (c*k)

-- Derived instances
-- Typeclass is sort of an interface which defines some behavior.
-- A type can be made instance of a class if it supports its behavior.

-- Eq instances can equated == and /=.
-- Ord instances can be ordered >, <, >=, <=.
-- For example, if we make a datatype an instance of Eq class, then two elements
-- of same data type can be equated. First the value constructors will be
-- compared. Then the fields will be compared. The catch is that the fields must
-- be instances of Eq class too.
-- For example, Just 2 > Just 3 will give False; but Just (*2) > Just (*3) will
-- error as functions are not a part of

-- Once a data type is an instace of a typeclass, then it can be used in all
-- functions that use the data types of that class. Like Eq class is supported
-- by elem function in searching a list of the datatype instance of Eq class.

data Person' = Person' { firstName' :: String
                     , lastName' :: String
                     , age' :: Int
                     } deriving (Eq, Show, Read)

-- Without being an instance of Eq, a datatype cannot be an instance of Ord
-- class.
-- If all value constructors are nullary i.e. take no fields, then the data
-- type can be made a part of Enum class. Enum is for data types with
-- predecessors and successors.

data Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday |
    Sunday deriving (Show, Enum, Eq, Ord, Read, Bounded)
