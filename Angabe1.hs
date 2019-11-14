type Nat0 = Int
type Nat1 = Int

streiche :: String -> Int -> Char -> String
streiche str int char 
  | int <= 0 = str
  | otherwise =
    -- The call to foldl threads a tuple of the computed string so far 
    -- and the counter that keeps track of when to skip a char
    reverse $ fst $ foldl compare ([], int) str
    where
      compare (str, count) elm =
        case (count, (elm == char)) of
          -- Right number of the right char -> skip the char and reset the counter
          (1, True)      -> (str, int) 
          -- Right char but wrong number -> add the current char and decrement the counter
          (count, True)  -> (elm:str, count - 1) 
          -- Wrong char -> add current char and don't decrement counter
          (count, False) -> (elm:str, count) 

-- Takes an int and produces a list of the digits in reverse order
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

-- ist_umgekehrt_2er_potenz 0 = False and ist_umgekehrt_2er_potenz 1 = True 
-- doesn't make sense to me since 2^0 = 1 and 2^n != 0 for any n in N
ist_umgekehrt_2er_potenz :: Nat0 -> Bool
ist_umgekehrt_2er_potenz 0 = False
ist_umgekehrt_2er_potenz 1 = True
ist_umgekehrt_2er_potenz num = 
  let umgekehrt = reverse_digits num in
  foldl (\acc elm -> acc || elm == umgekehrt) False $ takeWhile (<=umgekehrt) $ iterate (2*) 1 


-- checks whether a list of digits is a palindrome
ist_palindrom num digits = if digits == reverse digits then num else -1


groesstes_palindrom_in :: [Nat0] -> Int
groesstes_palindrom_in = foldl (\acc num -> max acc $ ist_palindrom num $ reverse_digits_list num) (-1)

-- shorthands for my own testing convenience ;)
s = streiche
i = ist_umgekehrt_2er_potenz
g = groesstes_palindrom_in

