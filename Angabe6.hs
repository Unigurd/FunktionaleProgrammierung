import Prelude hiding ((!!))

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

tests =
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

[] !! _ = Nothing
(head:rest) !! index 
  | index == 0 = Just head
  | index > 0  = rest !! (index-1)
  | index < 0  = Nothing

--changeElm :: [a] -> Integer -> a -> Maybe [a]
changeElm [] 0 elm = Just [elm]
changeElm [] _ elm = Nothing
changeElm (_:rest) 0 elm = Just (elm:rest)
changeElm (head:rest) i elm = (head:) <$> (changeElm rest (i-1) elm)

--interpretiere_1 :: EPS -> Anfangszustand -> Endzustand
--interpretiere_2 :: EPS -> Anfangszustand -> [Zwischenzustand]


eval :: EPS -> Zustand -> Int -> Maybe [Zwischenzustand]
eval eps zustand@(arithZustand, logZustand) i = do
  instruction <- eps !! i
  case instruction of
    AZ var exp -> 
      eval eps ((\x -> if var == x then links (evaluiere exp zustand) else arithZustand x),
                 logZustand) (i+1)
    LZ var exp -> 
       eval eps (arithZustand, 
                 (\x -> if var == x then rechts (evaluiere exp zustand) else logZustand x)) (i+1)

    FU b tAddress fAddress -> eval eps zustand $ if rechts (evaluiere b zustand) then tAddress else fAddress
    BS b tAddress -> eval eps zustand $ if rechts (evaluiere b zustand) then tAddress else (i+1)
    US address -> eval eps zustand address
    MP address instruction -> 
      case changeElm eps address instruction of
        Just newEps -> eval newEps zustand address
        Nothing -> eval eps zustand (i+1)
