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

