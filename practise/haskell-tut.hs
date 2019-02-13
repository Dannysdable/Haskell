-- Comments
{-
 Mutiline
-}

import Data.List
import System.IO

-- Int -2^63 2^63
maxInt = maxBound :: Int

minInt = minBound :: Int

-- Double
bigFloat = 3.99999999999 + 0.000000005

always5 :: Int
always5 = 5

sumOfNums = sum [1..1000]

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

modEx = mod 5 4
modEx2 = 5 `mod` 4

negNumEx = 5 + (-4)

num9 = 9 :: Int 
sqrtOf9 = sqrt (fromIntegral num9)

-- Built in math functions
piVal = pi 
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.99
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

--Also sin, cos, tan, asin, atan, acos, sinh,
--tanh, cosh, asinh, atanh, acosh

trueAndFalse = True && False 
trueOrFalse = True || False 
notTrue = not(True)


primeNumbers = [3,5,7,11]

morePrimes = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : 66 : []

multeist = [[3,5,7],[11,13,17]]

morePrimes2 = 2 : morePrimes

lenPrime = length morePrimes2 

revPrime = reverse morePrimes2 

isListEmpty = null morePrimes2 

secondPrime = morePrimes2 !! 1

firstPrime = head morePrimes2 

lastPrime = last morePrimes2 

primeInit = init morePrimes2

first3Primes = take 3 morePrimes2 

removedPrimes = drop 3 morePrimes2 

is7InList = 7 `elem` morePrimes2 

maxPrime = maximum morePrimes2 

minPrime = minimum morePrimes2 

newList = [2,3,5]

prodPrimes = product newList 

zeroToTen = [0..10]

evenList = [3,6..40]
evenList2 = [2,4..40]

letterList = ['A', 'C'..'Z']

--infinePow10 = [10,20..]

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cycleList = take 10 (cycle [1,2,3,4,5])

listTiems2 = [x * 2 | x <- [1..10]]

listTiems3 = [x * 3 | x <- [1..10], x * 3 <= 50]

divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9,1,8,3,4,7,6]

sumOfList = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThen5 = filter (>5) morePrimes

evensUpTo20 = takeWhile (<=20) [2,4..]

multOfListFromLeft = foldl (*) 2 [2,3,4,5]
multOfListFromRight = foldr (*) 2 [2,3,4,5]

pow3List = [3^n | n <- [1..10]]

multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52)
bobsName = fst bobSmith 
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]
namesNAddress = zip names addresses


{--
 - Founction
 -}
addMe :: Int -> Int -> Int
--Function param1 param2 = operations (returned value)
addMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can dirve"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
--whatAge _ = "Nothing Important"
whatAge x = "Nothing Important"

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial(n -1)

prodFact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False 
    | otherwise = True 

isEven n = n `mod` 2 == 0

whatGrade age 
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age > 6) && (age <= 10) = "Elementary School"
    | (age > 10) && (age <= 14) = "Middle School"
    | (age > 14) && (age <= 18) = "High School"
    | otherwise = "Go to college"

batAvgRating :: Double -> Double -> String 
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Average"
    | avg <= 2.250 = "Average Player"
    | avg <= 0.280 = "Your doing pretty good"
    | otherwise = "You're a Superstar"
    where avg = hits / atBats

getLitstItems :: [Int] -> String 
getLitstItems [] = "Your list is empty"
getLitstItems (x:[]) = "Your list starts with " ++ show x
getLitstItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getLitstItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest are " ++ show xs 

getFirstItem :: String -> String 
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

times4 :: Int -> Int 
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]
multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEq :: [Char] -> [Char] -> Bool 
areStringsEq [] [] = True 
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys 
areStringsEq _ _ = False

--func :: Int -> Int
douMult :: (Int -> Int) -> Int 
douMult func = func 3

num3Time4 = douMult times4 


getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus = adds3 4

threePlusList = map adds3 [1,2,3,4,5]

dbl1To10 = map (\x -> x * 2) [1..10]

doubleEvenNumber y = 
    if (y `mod` 2 /= 0)
        then y
        else y * 2

getClass :: Int -> String 
getClass n = case n of 
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary school"
    _ -> "Go away"

data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfield
                deriving Show
barryBonds :: BaseballPlayer -> Bool 
barryBonds Outfield = True 
barryBonds Pitcher = False
barryInOF = print(barryBonds Outfield)
barryInOF2 = print(barryBonds Pitcher)

data Customer = Customer String String Double 
    deriving Show
tomSmith :: Customer 
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ x) = x


data RPS = Rock | Paper | Scissors
shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Beats Rock"
shoot Rock Scissors = "Rock Beats Scissors"
shoot Scissors Paper = "Scissors Beat Paper"
shoot Scissors Rock = "Scissors Loses to Rock"
shoot Paper Scissors = "Paper Loses to Scissors"
shoot Rock Paper = "Rock Loses to Paper"
shoot _ _ = "Error"

data Shape = Circle Float Float Float 
           | Rectangle Float Float Float Float 
           | Rectangle2 Float Float Float Float
    deriving Show 
area :: Shape -> Float 
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle2 x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 - y))
sumValue = putStrLn (show (1 + 2))
sumValue2 = putStrLn . show $ 1 + 2
areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 100
areaOfRect2 = area $ Rectangle2 10 10 100 100


data Employee = Employee { name :: String, 
                           position :: String,
                           idNum :: Int 
                          } deriving (Eq, Show)
samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000}
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1001}
isSamPam = samSmith == pamMarx 
samSmithData = show samSmith

data ShirtSize = S | M | L 
instance Eq ShirtSize where 
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

instance Show ShirtSize where 
    show S = "Small"
    show M = "Medium"
    show L = "Large"
smallAvail = S `elem` [S, M, L]
theSize = show S

class MyEq a where 
    areEqual :: a -> a -> Bool 
instance MyEq ShirtSize where 
    areEqual S S = True 
    areEqual M M = True
    areEqual L L = True 
    areEqual _ _ = False
newSize = areEqual M M 


sayHello = do 
    putStrLn "what's your name"
    name <- getLine
    putStrLn $ "Hello " ++ name

writeToFile = do 
    theFile <- openFile "test.txt" WriteMode
    hPutStrLn theFile ("Random line of text")
    hClose theFile

readFromFile = do 
    theFile2 <- openFile "test.txt" ReadMode 
    contents <- hGetContents theFile2 
    putStr contents 
    hClose theFile2

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]





