-- The specifications from the assignment
data Arith_Variable = A1 | A2 | A3 | A4 | A5 | A6 deriving (Eq,Show)
data Log_Variable = L1 | L2 | L3 | L4 | L5 | L6 deriving (Eq,Show)

data Arith_Ausdruck = 
  AK Int              -- Arithmetische Konstante
  | AV Arith_Variable -- Arithmetische Variable
  | Plus Arith_Ausdruck Arith_Ausdruck  -- Addition
  | Minus Arith_Ausdruck Arith_Ausdruck -- Subtraktion
  | Mal Arith_Ausdruck Arith_Ausdruck   -- Multiplikation
  deriving (Eq,Show)

data Log_Ausdruck = 
  LK Bool                          -- Logische Konstante
  | LV Log_Variable                -- Logische Variable
  | Nicht Log_Ausdruck             -- Logische Negation
  | Und Log_Ausdruck Log_Ausdruck  -- Logische Konjunktion
  | Oder Log_Ausdruck Log_Ausdruck -- Logische Disjunktion
  | Gleich Arith_Ausdruck Arith_Ausdruck  -- Wertgleichheit
                                          -- arith. Ausdruecke
  | Kleiner Arith_Ausdruck Arith_Ausdruck -- Linker Ausdruck
                                          -- echt wertkleiner
                                          -- als rechter
                                          -- Ausdruck
  deriving (Eq,Show)

type Arith_Variablenbelegung = Arith_Variable -> Int -- Total definierte Abb.
type Log_Variablenbelegung = Log_Variable -> Bool    -- Total definierte Abb.
type Variablenbelegung = (Arith_Variablenbelegung,Log_Variablenbelegung)

links :: (Either a b) -> a
links (Left x) = x

rechts :: (Either a b) -> b
rechts (Right y) = y

class Evaluierbar a where
  evaluiere :: a -> Variablenbelegung -> Either Int Bool

-- My own solutions

instance Evaluierbar Arith_Ausdruck where
  -- Return a constant
  evaluiere (AK int) _               = Left int
  -- Return the value of a variable
  evaluiere (AV varID) varTab = Left $ (fst varTab) varID
  -- return result of an expression
  evaluiere (Plus exp1 exp2) varTab = 
    Left $ (links $ evaluiere exp1 varTab) + (links $ evaluiere exp2 varTab)
  evaluiere (Minus exp1 exp2) varTab = 
    Left $ (links $ evaluiere exp1 varTab) - (links $ evaluiere exp2 varTab)
  evaluiere (Mal exp1 exp2) varTab =
    Left $ (links $ evaluiere exp1 varTab) * (links $ evaluiere exp2 varTab)


instance Evaluierbar Log_Ausdruck where
  -- return a constant
  evaluiere (LK bool) _ = Right bool
  -- Return the value of a variable
  evaluiere (LV varID) varTab = Right $ (snd varTab) varID
  -- return the value of logical expressions
  evaluiere (Nicht exp) varTab = Right $ not $ rechts $ evaluiere exp varTab
  evaluiere (Und exp1 exp2) varTab = 
    Right $ (rechts $ evaluiere exp1 varTab) && (rechts $ evaluiere exp2 varTab)
  evaluiere (Oder exp1 exp2) varTab =
    Right $ (rechts $ evaluiere exp1 varTab) || (rechts $ evaluiere exp2 varTab)
  -- Return the value of relations on Arith_Ausdruck
  evaluiere (Gleich exp1 exp2) varTab =
    Right $ (links $ evaluiere exp1 varTab) == (links $ evaluiere exp2 varTab)
  evaluiere (Kleiner exp1 exp2) varTab =
    Right $ (links $ evaluiere exp1 varTab) < (links $ evaluiere exp2 varTab)

-- just testing stuff
avb = (\ av -> 3) :: Arith_Variablenbelegung
lvb = (\ lv -> True) :: Log_Variablenbelegung
vb1 = (avb,lvb) :: Variablenbelegung
vb2 = (\ av -> if av == A1 then 42 else avb av,
       \ lv -> if lv /= L6 then False else (mod (avb A3) 3 == 0)) :: Variablenbelegung

aa1 = Mal (Plus (AV A1) (AV A2)) (Mal (AV A1) (AV A2))
aa2 = AK 42
la1 = Nicht (Kleiner aa1 aa1)
la2 = Oder (Nicht (Gleich aa1 aa1)) (Oder (Nicht (LV L3)) (Nicht la1))

oldTests =
  [links (evaluiere aa1 vb1) == 54,
  links (evaluiere aa1 vb2) == 5670,
  links (evaluiere aa2 vb1) == 42,
  links (evaluiere aa2 vb2) == 42,
  rechts (evaluiere la1 vb1) == True,
  rechts (evaluiere la1 vb2) == True,
  rechts (evaluiere la2 vb1) == False,
  rechts (evaluiere la2 vb2) == True]


type Adresse = Int
type Sprungadresse = Adresse

data Anweisung = 
   AZ Arith_Variable Arith_Ausdruck               -- Wertzuweisung an arithmetische Variable
  | LZ Log_Variable Log_Ausdruck                  -- Wertzuweisung an logische Variable
  | FU Log_Ausdruck Sprungadresse Sprungadresse   -- Fallunterscheidung
  | BS Log_Ausdruck Sprungadresse                 -- Bedingter Sprung
  | US Sprungadresse                              -- Unbedingter Sprung
  | MP Adresse Anweisung                          -- Selbstmodifikation des Programms

type Zustand = Variablenbelegung
type Anfangszustand = Zustand
type Endzustand = Zustand
type Zwischenzustand = Zustand
type Programm = [Anweisung]
type EPS = Programm 

f <$> x = fmap f x

-- Maybe as an applicative
(Just f) <*> x = Just $ f x
Nothing  <*> _ = Nothing

[] !-! _ = Nothing
(head:rest) !-! index 
  | index == 0 = Just head
  | index > 0  = rest !-! (index-1)
  | index < 0  = Nothing


--changeElm :: [a] -> Integer -> a -> Maybe [a]
changeElm [] 0 elm = Just [elm]
changeElm [] _ elm = Nothing
changeElm (_:rest) 0 elm = Just (elm:rest)
changeElm (head:rest) i elm = (head:) <$> (changeElm rest (i-1) elm)

-- yuck!
just (Just a) = a

interpretiere_1 :: EPS -> Anfangszustand -> Endzustand
interpretiere_1 eps zustand = last $ interpretiere_2 eps zustand
interpretiere_2 :: EPS -> Anfangszustand -> [Zwischenzustand] 
interpretiere_2 eps zustand = eval eps zustand 0


eval :: EPS -> Zustand -> Int -> [Zwischenzustand]
eval eps zustand@(arithZustand, logZustand) i =
  case eps !-! i of
    Just (AZ var exp) -> 
      zustand:(eval eps ((\x -> if var == x 
                                then links (evaluiere exp zustand) 
                                else arithZustand x),
                         logZustand) (i+1))
    Just (LZ var exp) -> 
      zustand:(eval eps (arithZustand, 
                         (\x -> if var == x 
                                then rechts (evaluiere exp zustand) 
                                else logZustand x)) (i+1))
    Just (FU b tAddress fAddress) -> 
      eval eps zustand $ if rechts (evaluiere b zustand) then tAddress else fAddress
    Just (BS b tAddress) -> eval eps zustand $ if rechts (evaluiere b zustand) then tAddress else (i+1)
    Just (US address) -> eval eps zustand address
    Just (MP address instruction) -> 
      case changeElm eps address instruction of
        Just newEps -> eval newEps zustand address
        Nothing -> eval eps zustand (i+1)
    Nothing -> [zustand]



fib n 
  | n == 0 = 0
  | n == 1 = 1
  | n > 0 = fib (n-2) + fib (n-1)
  | True = n

fac n | n == 0 = 1
  | n > 0 = n * fac (n-1)
  | True = n

-- Programm pi0: Berechnung der Fakultaet fuer den Anfangswert von A1, falls
-- dieser groesser oder gleich 0 ist, in A2:
pi0 = [AZ A2 (AK 1),
       BS (Gleich (AV A1) (AK 1)) 999,
       AZ A2 (Mal (AV A2) (AV A1)),
       AZ A1 (Minus (AV A1) (AK 1)),
       US 1]

lvb0 = (\ lv -> True) :: Log_Variablenbelegung
avb0 = (\ av -> if av == A1 then fib 4 else fib 6 - (fac 3 + 2))
       :: Arith_Variablenbelegung
zst0 = (avb0,lvb0) :: Anfangszustand

avb1 = (\ av -> if av == A2 then 1 else avb0 av)
avb2 = (\ av -> if av == A2 then 3 else avb0 av)
avb3 = (\ av -> case av of A1 -> 2
                           A2 -> 3
                           x  -> avb0 x)
avb4 = (\ av -> case av of A1 -> 2
                           A2 -> 6
                           x  -> avb0 x)
avb5 = (\ av -> case av of A1 -> 1
                           A2 -> 6
                           x  -> avb0 x)

-- test0 = (interpretiere_1 pi0 zst0 == (avb5,lvb0)) -- Informell, keine direkte Ausgabe
                                                  -- am Bildschirm moeglich (s.a. A.2).

-- test1 = interpretiere_2 pi0 zst0 == [(avb0,lvb0),(avb1,lvb0),(avb2,lvb0),
--                                      (avb3,lvb0),(avb4,lvb0),(avb5,lvb0)] -- s.o.

-- test2 = take 2 (drop 3 (interpretiere_2 pi0 zst0)) == [(avb3,lvb0),(avb4,lvb0)] -- s.o.


program1 = interpretiere_1 pi0 zst0
program2 = interpretiere_2 pi0 zst0
-- Unmittelbare Bildschirmausgaben moeglich z.B. fuer:
tests1 = [
  fst (program1) A1 == 1,
  fst (program1) A2 == 6,
  fst (program1) A3 == 0,
  fst (program1) A4 == 0,
  fst (program1) A5 == 0,
  fst (program1) A6 == 0]

tests2 = [
  fst (program2 !! 0) A1 == 3,
  fst (program2 !! 1) A6 == 0,
  fst (program2 !! 2) A1 == 3,
  fst (program2 !! 3) A2 == 3,
  fst (program2 !! 4) A1 == 2,
  fst (program2 !! 5) A1 == 1,
  fst (program2 !! 5) A2 == 6,
  fst (program2 !! 5) A3 == 0]

-- Programm pi1: Leeres Programm
pi1 = []
-- interpretiere_1 pi1 zst0 ->> zst0 -- s.o.
-- interpretiere_2 pi1 zst0 ->> [zst0] -- s.o.

-- Programm pi2: Selbstmodifizierendes Programm
pi2 = [MP ((fib (fac 3)) - (fac (1+2) + 2)) (AZ A3 (AK (5*(fib (fac 3))+2)))]

avb6 = \ av -> 1 :: Arith_Variablenbelegung
zst1 = (avb6,lvb0) :: Anfangszustand
zst2 = (\ av -> if av == A1 then (fac 3) * (fib 7 - fac 3) else avb6 av,
\ lv -> if lv /= L6
then (True == False) && True
else (mod (4 * (avb6 A3)) (2 + fac (avb6 A1)) /= 0) )
:: Anfangszustand

interpretiere_1 pi2 zst1 ->> (\ av -> if av == A3 then 42 else fst zst1 av,snd zst1) -- s.o.
interpretiere_2 pi1 zst1 ->> [zst1,(\ av -> if av == A3 then 42 else fst zst1 av,snd zst1)] -- s.o.
interpretiere_1 pi2 zst2 == interpretiere_1 pi2 zst1
interpretiere_2 pi1 zst2 == interpretiere_2 pi2 zst1

-- Programm pi3: Selbstmodifizierendes, nichtterminierendes Programm
pi3 = [MP ((fac 3 + 2) - (fib (fac 3)))
          (BS ((fac 5) > (fib 5)) (fib (fac 3) - (fac 3 + 2)))] ++ pi0 ++ pi2
interpretiere_1 pi3 zst1 ->> ‘undefiniert wg. Nichtterminierung’
interpretiere_2 pi3 zst1 ->> [zst1,zst1,zst1,.. -- terminiert nicht regulaer
take 5 (interpretiere_2 pi3 zst1)
  ->> [zst1,zst1,zst1,zst1,zst1] -- nicht unmittelbar ausgebbar, s.o.
fst (take 5 (interpretiere_2 pi3 zst1) !! 3) A3 ->> ... ->> 1
snd (take 5 (interpretiere_2 pi3 zst1) !! 2) L5 ->> ... ->> True

