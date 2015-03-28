import Data.Maybe
-- Making Our Own Data Types

data Point = Point Float Float deriving Show
data Shapes = Circle Point Float | Rectangle Point Point deriving Show

surface :: Shapes -> Float 
surface (Circle _ r) = pi * r ^ 2 
surface (Rectangle ( Point x1 y1 ) ( Point x2 y2 ) )  = ( abs $ x2 - x1) * ( abs $ y2 - y1)

nudge :: Shapes -> Float -> Float -> Shapes 
nudge ( Circle ( Point x y ) r ) a b   = Circle ( Point (x+a) (y+b) ) r
nudge ( Rectangle ( Point x1 y1 ) ( Point x2 y2 )) a b  = Rectangle (Point ( x1 + a ) ( y1 + b )) ( Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shapes
baseCircle r = Circle ( Point 0 0 ) r

---------------------------------------------------------------------

-- Type Values : Value Constructors

data Person = Person String String Int Float String String deriving Show
-- First Name, Last Name, Age, Height, Phone Number, favourite Ice-cream flavor.

guy = Person "Buddy" "notAName" 19 184.2 "526-2928" "ButterScotch"

firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String 
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age
-- Noteworthy that same name arguments can be passed

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ phonenumber _) = phonenumber

favouriteIceCreamFlavour :: Person -> String
favouriteIceCreamFlavour (Person _ _ _ _ _ favouriteicecreamflavour)  = favouriteicecreamflavour

---------------------------------------------------------------------

data BetterPerson = BetterPerson {
	betterFirstName :: String,
	betterLastName :: String,
	betterAge :: Int,
	betterHeight :: Float,
	betterPhoneNumber :: String,
	betterFlavour :: String
} deriving Show

betterGuy = BetterPerson "Guy" "Fawkes" 19 182.1 "526-2928" "None"

---------------------------------------------------------------------

data Car = Car String String Int deriving Show
myCar = Car "Lambourginini" "Aventador" 2015

data BetterCar = BetterCar { company :: String, model :: String, year :: Int } deriving Show
myNewCar = BetterCar "Lambourgini" "Aventador" 2015
myOldCar = BetterCar "Rolls-Royce" "Phantom" 1998

data NewCar a b c = NewCar { newcompany :: a, newmodel :: b, newyear :: c} deriving Show

aCar = BetterCar { company = "Lambo", model = "Aventador", year = 1995}

tellCar :: BetterCar -> String
tellCar ( BetterCar { company = a, model = b, year = c}) = "This car is " ++ a ++ " model " ++ b ++ " of year " ++ show c

{--
Why doesn't it run?
tellCarDiff :: (Show a, b, c) => NewCar a b c -> String
tellCarDiff (NewCar {newcompany = a, newmodel = b, newyear = c}) = "This car is " ++ a ++ " model " ++ b ++ " of year " ++ show c
--}

---------------------------------------------------------------------

-- Type Parameters : Type Constructors

-- data Maybe a = Nothing | Just a deriving (Read, Show)

-- Maybe is type constructor, data type are passes to it and it returns a more complex data type.
-- Maybe, if it receives a Char data type, makes it's value either Nothing or of Char type.
-- Maybe takes value a and then uses Nothing( which takes no argument) or Just a (which does take a value)
-- Therefore Maybe acts polymorphically. It is quite similar to [a] lists.


doing :: Int -> Maybe Int 
doing x = if x == 12 then ( Just x ) else Nothing

func :: Maybe a -> a
func (Just a) = a
func (Nothing) = error "Hello idiot. Wrong value input." 

does = do 
		str <- getLine 
		let d = read str :: Maybe Int
 		putStrLn (show d)

