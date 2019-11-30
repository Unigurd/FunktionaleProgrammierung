import Control.Applicative

--
-- Task 1
--

-- generate a stream of increasing factorial numbers
generiere_fak_strom :: [Integer]
generiere_fak_strom = fst $ unzip $ iterate (\(fak,n) -> (fak*n,n+1)) (1,1)


--
-- Task 2
--

type IR_plus = Double -- Nur Werte echt groesser als null
type Stelle = Double
type Genauigkeit = IR_plus
type Approx_Wert = Double
type Strom = [Double]

-- Approximates the exponential function
approximiere_exp :: Stelle -> Genauigkeit -> Approx_Wert
approximiere_exp x epsilon = selektiere epsilon (generiere_exp_strom x)

-- generates a stream of increasingly exact approximations of the result of exp(x)
generiere_exp_strom :: Stelle -> Strom
generiere_exp_strom x = fst $ unzip $ iterate (\(res,n) -> (res+(x**(fromIntegral n))/(fromIntegral $ generiere_fak_strom !! n), n+1)) (1,1)

-- Chooses the first element of a stream where the difference between 
-- an element and the former is no larger than e
selektiere :: Genauigkeit -> Strom -> Approx_Wert
selektiere e (elm1:elm2:rest)
  | abs (elm2 - elm1) <= e = elm2
  | otherwise              = selektiere e (elm2:rest)


--
-- Task 3
--

type Woerterstrom = [String]

-- Generates a stream of all words on the alphabet a to c
generiere_woerter :: Woerterstrom
generiere_woerter = 
  let rev_woerter = do
      s <- ("" : rev_woerter)
      c <- ['a'..'c'] 
      return (c:s)
  in reverse <$> rev_woerter

-- Keeps only palindromes in a stream of words
filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = filter (\word -> word == (reverse word))


--
--  Task 4
--

type Wort = String
type Woerterbuch = [Wort] -- nur Werte endliche Laenge; keine Stroeme.
type Wortleiter = [Wort]

---- The 3 rules of a leiterstufe:

-- Is an element added to the end?
rule1 a@(_:_) b@(_:_) = a == (init b) ||  (init a) == b
rule1 a@(_:_) []      = null (init a)
rule1 [] b@(_:_)      = null (init b)
rule1 _ _             = False

-- Is an element removed from the front?
rule2 (a:as) (b:bs) = as == (b:bs) || (a:as) == bs
rule2 (a:[]) []     = True
rule2 [] (b:[])     = True
rule2 _ _           = False

-- Is a single element changed?
rule3 (a:as) (b:bs)
  | a == b    = rule3 as bs
  | otherwise = as == bs
rule3 _ _ = False

isLeiterstufe a b = rule1 a b || rule2 a b || rule3 a b

-- Is the two word arguments an ascending leiterstufe?
ist_aufsteigende_leiterstufe :: Wort -> Wort -> Bool
ist_aufsteigende_leiterstufe w1 w2 = w1 < w2 && isLeiterstufe w1 w2
ial = ist_aufsteigende_leiterstufe

-- is the supplied list an ascending wortleiter?
ist_aufsteigende_wortleiter :: [Wort] -> Bool
ist_aufsteigende_wortleiter (a:b:rest) = ial a b && iaw (b:rest)
ist_aufsteigende_wortleiter (a:[])     = True
ist_aufsteigende_wortleiter []         = True
iaw = ist_aufsteigende_wortleiter

-- works on lists of lists
-- returns the empty list if empty, and head/last otherwise
emptyOrLast []     = []
emptyOrLast (x:[]) = x
emptyOrLast (x:xs) = emptyOrLast xs
emptyOrHead []     = []
emptyOrHead (x:xs) = x

-- Takes a wortleiter and a list of words
-- and generates a list of all possible ascending wortleiters
addToWortleiter :: Wortleiter -> [String] -> [Wortleiter]
addToWortleiter wortleiter wordList = reverse <$> reverseWortleiter wortleiter wordList
  where
    reverseWortleiter wortleiter (word:rest) =
      if ial (emptyOrHead wortleiter) word
      then reverseWortleiter (word:wortleiter) rest ++ reverseWortleiter wortleiter rest
      else reverseWortleiter wortleiter rest
    reverseWortleiter wortleiter [] = [wortleiter]

-- No idea why, but this one has non-exhaustive pattern matching while the above one does not
addToWortleiter2 :: Wortleiter -> [String] -> [Wortleiter]
addToWortleiter2 wortleiter wordList = reverse <$> reverseWortLeiter2 wortleiter wordList
  where
    reverseWortLeiter2 wortleiter (word:rest) =
      if ial (emptyOrHead wortleiter) word
      then reverseWortleiter2 (word:wortleiter) rest ++ reverseWortleiter2 wortleiter rest
      else reverseWortleiter2 wortleiter rest
    reverseWortleiter2 wortleiter [] = [wortleiter]

-- Takes a list of words and generates a list of all possible ascending wortleiters
-- where, if a word is another in a wordleiter, then it is also after that other in
-- the original list
listToWortleiter :: [String] -> [Wortleiter]
listToWortleiter (word:rest) = addToWortleiter [word] rest ++ listToWortleiter rest
listToWortleiter []          = []

-- Gets a list of all the largest elements
getLargest :: Ord a => [[[a]]] -> [[[a]]]
getLargest = foldl helper []
  where
    helper acc@(head:tail) elm
      | length head < length elm  = [elm]
      | length head == length elm = elm:acc
      | otherwise                 = acc
    helper [] elm = [elm]

-- gets the lexicographically smallest elm in a list
getLexicographicMin :: Ord a => [[[a]]] -> [[a]]
getLexicographicMin (head:tail) = foldl min head tail

-- Returns the lexicographically smallest of the largest 
-- ascending wortleiters that can be created from the supplied list,
-- and where, if a word is another in a wordleiter, then it is also after that other in
-- the original list
gib_max_aufsteigende_wortleiter :: Woerterbuch -> Wortleiter
gib_max_aufsteigende_wortleiter = getLexicographicMin . getLargest . listToWortleiter
gmaw = gib_max_aufsteigende_wortleiter

 
woerterbuch = ["awake","awaken","cat","dig","dog","fig","fin", "fine","fog","log","rake","wake","wine"]
wb = woerterbuch

tests = 
  [gib_max_aufsteigende_wortleiter woerterbuch  == ["dig","fig","fin","fine","wine"],
  length (gib_max_aufsteigende_wortleiter woerterbuch) == 5,
  ist_aufsteigende_leiterstufe "fig" "fin" == True,
  ist_aufsteigende_leiterstufe "fin" "fig" == False,
  ist_aufsteigende_wortleiter woerterbuch == False,
  ist_aufsteigende_wortleiter ["dig","fig","fin"] == True,
  ist_aufsteigende_wortleiter ["fin","fig","dig"] == False]
