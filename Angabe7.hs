import Control.Applicative

generiere_fak_strom :: [Integer]
generiere_fak_strom = fst $ unzip $ iterate (\(fak,n) -> (fak*n,n+1)) (1,1)


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



type Woerterstrom = [String]

generiere_woerter :: Woerterstrom
generiere_woerter =
  let rev_woerter = [ c:s | s <- ("" : rev_woerter), c <- ['a'..'c'] ] 
  in reverse <$> rev_woerter

filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = filter (\word -> word == (reverse word))

