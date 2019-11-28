import Control.Applicative

--
-- Task 1
--

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

approximiere_exp :: Stelle -> Genauigkeit -> Approx_Wert
approximiere_exp x epsilon = selektiere epsilon (generiere_exp_strom x)

generiere_exp_strom :: Stelle -> Strom
generiere_exp_strom x = fst $ unzip $ iterate (\(res,n) -> (res+(x**(fromIntegral n))/(fromIntegral $ generiere_fak_strom !! n), n+1)) (1,1)

selektiere :: Genauigkeit -> Strom -> Approx_Wert
selektiere e (elm1:elm2:rest)
  | abs (elm2 - elm1) <= e = elm2
  | otherwise              = selektiere e (elm2:rest)


--
-- Task 3
--

type Woerterstrom = [String]

generiere_woerter :: Woerterstrom
generiere_woerter = 
  let rev_woerter = do
      s <- ("" : rev_woerter)
      c <- ['a'..'c'] 
      return (c:s)
  in reverse <$> rev_woerter

filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = filter (\word -> word == (reverse word))


--
--  Task 4
--

type Wort = String
type Woerterbuch = [Wort] -- nur Werte endliche Laenge; keine Stroeme.
type Wortleiter = [Wort]

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
--il = isLeiterstufe

ist_aufsteigende_leiterstufe :: Wort -> Wort -> Bool
ist_aufsteigende_leiterstufe w1 w2 = w1 < w2 && isLeiterstufe w1 w2
ial = ist_aufsteigende_leiterstufe

ist_aufsteigende_wortleiter :: [Wort] -> Bool
ist_aufsteigende_wortleiter (a:b:rest) = ial a b && iaw (b:rest)
ist_aufsteigende_wortleiter (a:[])     = True
ist_aufsteigende_wortleiter []         = True
iaw = ist_aufsteigende_wortleiter

emptyOrLast []     = []
emptyOrLast (x:[]) = x
emptyOrLast (x:xs) = emptyOrLast xs
emptyOrHead []     = []
emptyOrHead (x:xs) = x


createWortleiter :: Functor f => f a -> f [a]
createWortleiter = fmap (:[])

addToWortleiter :: [String] -> [String] -> [[String]]
addToWortleiter wortleiter [word:rest] = 
  if ial (emptyOrHead wortleiter) word
  then addToWortleiter (word:wortleiter) rest ++ addToWortleiter wortleiter rest
  else addToWortleiter wortleiter rest
addToWortleiter wortleiter [] = [wortleiter]



--wortleiters [head:tail] = 

wortleiterList :: [String] -> [[String]]
wortleiterList = reverse <$> reverseWortleiterList
  where
    reverseWortleiterList =
      foldl (\ acc elm -> [elm:wortLeiter -- if wortLeiter == [""] then [elm] else elm:wortLeiter
                             | wortLeiter <- ([""]:acc), 
                               ial (emptyOrLast wortLeiter) elm]) []
wll = wortleiterList

--gib_max_aufsteigende_wortleiter :: Woerterbuch -> Wortleiter
--gib_max_aufsteigende_wortleiter dict = 


 
woerterbuch = ["awake","awaken","cat","dig","dog","fig","fin", "fine","fog","log","rake","wake","wine"]
wb = woerterbuch

tests = 
  [--gib_max_aufsteigende_wortleiter woerterbuch  == ["dig","fig","fin","fine","wine"],
 -- length (gib_max_aufsteigende_wortleiter woerterbuch) == 5,
  ist_aufsteigende_leiterstufe "fig" "fin" == True,
  ist_aufsteigende_leiterstufe "fin" "fig" == False,
  ist_aufsteigende_wortleiter woerterbuch == False,
  ist_aufsteigende_wortleiter ["dig","fig","fin"] == True,
  ist_aufsteigende_wortleiter ["fin","fig","dig"] == False]
