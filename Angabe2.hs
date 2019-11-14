import Data.List
type Nat0 = Int
type Nat1 = Int

-- Takes an int and produces a list of the digits in reverse order
-- from assignment 1
reverse_digits_list :: Int -> [Int]
reverse_digits_list 0 = []
reverse_digits_list x = (mod x 10) : (reverse_digits_list $ div x 10)

-- Takes the list from reverse_digits_list and calculates the number it represents
-- threads the sum so far and the current factor to multiply with through a foldr
digits_list_to_int :: [Int] -> Int
digits_list_to_int = fst . foldr (\digit (result, factor) -> (result + digit*factor, factor * 10)) (0,1)

-- Reverses the digits of a number
reverse_digits :: Int -> Int
reverse_digits = digits_list_to_int . reverse_digits_list

-- checks whether a number is prime
is_prime :: Int -> Bool
is_prime n
  |n <= 1 = False 
  -- Generates the list of n's divisors from 2 to n-1, and returns true if that list is empty,
  -- that is, when n is a prime
  |otherwise = null $ filter (\x -> n `mod` x == 0) [2..n-1]

-- checks whether a single number is a double prime
is_dp :: Int -> Bool
is_dp n = is_prime n  && (is_prime $ reverse_digits n)

-- finds all double primes from a to b
dp :: (Nat1,Nat1) -> [Nat1]
dp (a,b) = filter is_dp [a..b]


-- Finds all integer divisors of n, excluding n itself
integer_divisors_of :: Int -> [Int]
integer_divisors_of n = filter (\x -> n `mod` x == 0) [1..n-1]

-- Cuts off a list when it finds a recurring element
stop_at_repeat :: [Nat1] -> [Nat1]
stop_at_repeat list =
  helper [] list
  where
    -- helper compares each elm with all elements in check_list,
    -- the list of previously checked elements.
    -- It is meant to deal with infinite lists generated from folge_inf,
    -- and so does not need to check the empty list case for the second parameter
    helper checked_list (elm:rest) 
      | any (\y -> elm == y) checked_list = reverse checked_list
      | otherwise = helper (elm:checked_list) rest

-- Generates a list of numbers as described in the assignment for folge, 
-- but it doesn't cut out numbers when they repeat.
folge_inf :: Nat1 -> [Nat1]
folge_inf n = n:(folge_inf $ sum $ integer_divisors_of n)

-- generates an infinite list of with folge_inf
-- and ends it at the correct point with stop_at_repeat and the lambda.
-- Without the lambda, the first 0 would be included in the list,
-- which it shouldn't be
folge :: Nat1 -> [Nat1]
folge = filter (\x -> x /= 0) . stop_at_repeat . folge_inf

-- Checks whether all elements in a list are pairwise distinct
pairwise_distinct :: Eq a => [a] -> Bool
pairwise_distinct [] = True
pairwise_distinct (x:xs) = all (x/=) xs && pairwise_distinct xs

-- If all elements in the input list are pairwise distinct,
-- the medianoid will be returned.
-- Otherwise the sum of the elements will be returned
-- 0 is taken as the sum of the empty list, since 0 is the identity under addition
medianoid :: [Nat1] -> Nat1
medianoid [] = 0
medianoid l =
  if pairwise_distinct l then
    (sort l) !! (ceiling $ (fromIntegral ((length l) - 1)) / 2)
  else sum l
