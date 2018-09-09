-- :set +m
import Data.Time
import Data.Bifunctor


fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- | Hypothenusa

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = x*2 + y*2


-- We also note that Num is a subclass of Eq, but not of Ord;
-- this is because the order predicates do not apply to complex numbers.
-- The subclass Real of Num, however, is a subclass of Ord as well.

conditional :: Real a => a -> a
conditional x = if x > 100
                then x
                else x*2

-- conditional2 :: (Ord a, Num a) => a -> [Char]
-- :t conditional2

conditional2 x = if x > 100
                then "Over 100"
                else "Under 100"


conanO'Brien = "Hello world!"


-- reverse own implementation


reverse' :: [a] -> [a]
reverse' [] = []
reverse' a = last a : reverse' (init a)

-- Patterns
sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


-- Math language ( where )
bubbleSort :: (Ord a, Show a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:rest) = bubbleSort (init bubbled) ++ [last bubbled]
  where
    (first, second) = if x > y then (y, x) else (x, y)
    bubbled = first : bubbleSort (second:rest)


cheapSort :: (Ord a, Show a) => [a] -> [a]
cheapSort a = if sorted a
                 then a
                 else bubbleSort a


quickSort :: (Ord a, Show a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where smaller = filter (<=x) xs
        larger  = filter (>x) xs

fakeSort ::  (Ord a, Show a) => [a] -> [a]
fakeSort a = a

sortTestPass :: (Ord a, Num a1) => ([a1] -> [a]) -> Bool
sortTestPass f = sorted (f ((take 20 (cycle [1,2,3]))))

benchmark :: (Ord a, Num a1, Show a) => ([a1] -> [a]) -> IO ()
benchmark f = do
  start <- getCurrentTime
  print(f (take 20 (cycle [1,2,3])))
  end <- getCurrentTime
  print(diffUTCTime end start)


triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]


factorial :: Integer -> Integer
factorial n = product [1..n]

-- :t fst
-- :t (==)

-- pattern matching & recursion
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : (filter' f xs)
  | otherwise = filter' f xs

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea


head'' :: [a] -> a
head'' xs = case xs of []    -> error "No head for empty lists!"
                       (x:_) -> x


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

largestDivisible :: (Integral a) => a -> a
largestDivisible y = head (filter (\x -> x `mod` y == 0) [100000,99999..])



data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int } deriving (Show)

ijones = Person {firstName="Indiana", lastName="Jones", age=189}

data Day = Lunes | Martes | Miércoles | Jueves | Viernes | Sábado | Domingo
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

maybeExample :: (Real a) => a -> Maybe a
maybeExample x = if x > 3
                 then Just 3
                 else Nothing

inputOutput = do
  print "What's your name?"
  name <- getLine
  print $ "Hello " ++ name

inputOutput' = do
  print "What's your name?"
  name <- getLine
  print ("Hello " ++ name)

inputOutput'' = do
  print "What's your name?"
  name <- getLine
  print ((++) "Hello " name)

reverseGetLine = do
  line <- fmap reverse getLine
  print line

half x = if even x
  then Just (x `div` 2)
  else Nothing
