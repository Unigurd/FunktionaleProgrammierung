type Nat0 = Int
type Nat1 = Int
data Wochentag = Mo | Di | Mi | Do | Fr | Sa | So deriving (Eq,Show)
type Starttag = Wochentag
type Zeitraum_in_Tagen = Nat1
type Streikaufrufabstand_in_Tagen = Nat1
newtype Partei = P Streikaufrufabstand_in_Tagen deriving Show
type Parteien = [Partei]
type Streiktage = Nat0
type Anzahl_Parteien = Nat1
type Modellszenario = (Starttag,Zeitraum_in_Tagen,Parteien)
type StrikeNum = Nat0

-- More modern notation for fmap
f <$> a = fmap f a

-- Converts a day to a Nat0
dayToNat0 :: Wochentag -> Nat0
dayToNat0 d = 
  case d of
    Mo -> 0
    Di -> 1
    Mi -> 2
    Do -> 3
    Fr -> 4
    Sa -> 5
    So -> 6

allBut :: Wochentag -> [Wochentag]
allBut day = filter (/=day) [Mo, Di, Mi, Do, Fr, Sa, So]

-- How often does a given party P strike in the next z days?
-- Does not take strike-free fridays and sundays into account.
partyStrikes :: Partei -> [StrikeNum]
partyStrikes (P abstand) = strikesForDays abstand
  where 
    strikesForDays 1 = 1:(strikesForDays abstand)
    strikesForDays a = 0:(strikesForDays (a-1))

-- combines 2 lists of strikes
combine2Strikes :: [StrikeNum] -> [StrikeNum] -> [StrikeNum]
combine2Strikes a [] = a
combine2Strikes [] b = b
combine2Strikes (a:as) (b:bs) = (a+b):(combine2Strikes as bs)

-- Combines any amount of lists of strikes
combineStrikes :: [[StrikeNum]] -> [StrikeNum]
combineStrikes = foldl combine2Strikes []

removeDays :: [Wochentag] -> Wochentag -> [StrikeNum] -> [StrikeNum]
removeDays holidays offset strikes = remove daysList strikes
  where 
    daysList = (\x -> (x-(dayToNat0 offset)) `mod` 7) <$> (dayToNat0 <$> holidays)
    remove h (s:ss) = (if (any (==0) h) then 0 else s):(remove ((\x -> (x-1) `mod` 7) <$> h) ss)
    remove _ [] = []

ps = [P 2, P 3, P 4, P 5]

generateStrikeDays :: Wochentag -> [Partei] -> [StrikeNum]
generateStrikeDays startTag parteien = removeDays [Fr, So] startTag $ combineStrikes $ fmap partyStrikes parteien


grossstreiktage :: Modellszenario -> Anzahl_Parteien -> Streiktage
grossstreiktage (startTag, zeitRaum, parteien) anzahl = length $ filter (\x -> x >= anzahl) $ take zeitRaum $ generateStrikeDays startTag parteien

streiktage :: Modellszenario -> Streiktage
streiktage = (`grossstreiktage` 1)

superstreiktage :: Modellszenario -> Streiktage
superstreiktage (startTag, zeitRaum, parteien) = length $ filter (\x -> x >= (length parteien)) $ take zeitRaum $ generateStrikeDays startTag parteien


streiktage_am :: Modellszenario -> Wochentag -> Anzahl_Parteien -> Streiktage
streiktage_am (startTag, zeitRaum, parteien) day anzahl = length $ filter (\x -> x >= anzahl) $ take zeitRaum $ removeDays (allBut day) startTag $ generateStrikeDays startTag parteien

wird_gestreikt :: Modellszenario -> Nat1 -> Bool
wird_gestreikt (startTag, _, parteien) i = (0<) $ (generateStrikeDays startTag parteien) !! i

-------------------------
-------------------------
--                     --
--                     --
--       Part 2        --
--                     --
--                     --
-------------------------
-------------------------

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

instance Evaluierbar Arith_Ausdruck where
  evaluiere (AK int) _               = Left int
  evaluiere (AV varID) varTab = Left $ (fst varTab) varID
  evaluiere (Plus exp1 exp2) varTab = 
    Left $ (links $ evaluiere exp1 varTab) + (links $ evaluiere exp2 varTab)
  evaluiere (Minus exp1 exp2) varTab = 
    Left $ (links $ evaluiere exp1 varTab) - (links $ evaluiere exp2 varTab)
  evaluiere (Mal exp1 exp2) varTab =
    Left $ (links $ evaluiere exp1 varTab) * (links $ evaluiere exp2 varTab)


instance Evaluierbar Log_Ausdruck where
  evaluiere (LK bool) _ = Right bool
  evaluiere (LV varID) varTab = Right $ (snd varTab) varID
  evaluiere (Nicht exp) varTab = Right $ not $ rechts $ evaluiere exp varTab
  evaluiere (Und exp1 exp2) varTab = 
    Right $ (rechts $ evaluiere exp1 varTab) && (rechts $ evaluiere exp2 varTab)
  evaluiere (Oder exp1 exp2) varTab =
    Right $ (rechts $ evaluiere exp1 varTab) || (rechts $ evaluiere exp2 varTab)
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


