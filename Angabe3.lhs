The required data type declarations and synonyms
\begin{code}
data IN_1 = Eins | Nf IN_1 deriving Show
data ZZ = Null | Plus IN_1 | Minus IN_1 deriving Show
type Zett = Integer
\end{code}

The following two functions convert between Zett and IN_1 and are used in von_Zett_nach_ZZ and von_ZZ_nach_Zett.

\begin{code}
von_IN_1_nach_Zett :: IN_1 -> Zett
von_IN_1_nach_Zett Eins = 1
von_IN_1_nach_Zett (Nf n) = 1 + (von_IN_1_nach_Zett n)
\end{code}

Since negative values are possible with Zett and not with IN_1, we error when given a negative Zett
\begin{code}
von_Zett_nach_IN_1 :: Zett -> IN_1
von_Zett_nach_IN_1 n
  | n == 1 = Eins
  | n > 1  = Nf (von_Zett_nach_IN_1 (n-1))
  | otherwise = error ("von_Zett_nach_IN_1 given invalid Zett: " ++ (show n))
\end{code}

The following two functions convert between Zett and ZZ and uses the helper functions von_IN_1_nach_Zett and von_Zett_nach_IN_1 to count and construct the IN_1 values in ZZ.
\begin{code}
von_Zett_nach_ZZ :: Zett -> ZZ
von_Zett_nach_ZZ n
  | n == 0 = Null
  | n > 0  = Plus (von_Zett_nach_IN_1 n)
  | n < 0  = Minus (von_Zett_nach_IN_1 (-n))
\end{code}

\begin{code}
von_ZZ_nach_Zett :: ZZ -> Zett
von_ZZ_nach_Zett Null = 0
von_ZZ_nach_Zett (Plus n) = (von_IN_1_nach_Zett n)
von_ZZ_nach_Zett (Minus n) = - (von_IN_1_nach_Zett n)
\end{code}

plus_IN adds IN_1's. It runs through one of the arguments until it hits Eins, replaces it with the other argument, and reconstructs the first argument on its way out the recursion.
\begin{code}
plus_IN :: IN_1 -> IN_1 -> IN_1
plus_IN Eins Eins = Nf Eins
plus_IN Eins n@(Nf _) = Nf n
plus_IN m@(Nf _) Eins = Nf m
plus_IN m@(Nf _) (Nf n) = plus_IN (Nf m) n
\end{code}

minus_IN subtracts IN_1's from each other and returns a ZZ, since a negative value can be produced. It removes an Nf from each of its arguments until one of them encounters an Eins, and returns the non-Eins value with the correct sign.
\begin{code}
minus_IN :: IN_1 -> IN_1 -> ZZ
minus_IN Eins Eins = Null
minus_IN Eins (Nf n) = Minus n
minus_IN (Nf m) Eins = Plus m
minus_IN (Nf m) (Nf n) = minus_IN m n
\end{code}

plus adds two ZZ's with the help of plus_IN and minus_IN. The logic here is mostly about different cases of signs.
\begin{code}
plus :: ZZ -> ZZ -> ZZ
plus Null Null = Null
plus m Null = m
plus Null n = n
plus (Plus m) (Plus n) = Plus (plus_IN m n)
plus (Minus m) (Minus n) = Minus (plus_IN m n)
plus (Plus m) (Minus n) = minus_IN m n
plus (Minus m) (Plus n) = minus_IN n m
\end{code}

minus adds two ZZ's with the help of plus_IN and minus_IN. The logic here is mostly about different cases of signs.
\begin{code}
minus :: ZZ -> ZZ -> ZZ
minus Null Null = Null
minus m Null = m
minus Null (Plus n) = Minus n
minus Null (Minus n) = Plus n
minus (Plus m) (Plus n) = minus_IN m n
minus (Minus m) (Minus n) = minus_IN n m
minus (Plus m) (Minus n) =  Plus (plus_IN m n)
minus (Minus m) (Plus n) =  Minus (plus_IN m n)
\end{code}

mal_IN multiplies two IN_1's by adding repeatedly.
\begin{code}
mal_IN :: IN_1 -> IN_1 -> IN_1
mal_IN Eins n = n
mal_IN m Eins = m
mal_IN (Nf m) n = plus_IN n (mal_IN m n)
\end{code}

mal multiplies two ZZ's with the help of mal_IN. The logic here is mostly about signs.
\begin{code}
mal :: ZZ -> ZZ -> ZZ
mal Null Null = Null
mal Null n = Null
mal m Null = Null
mal (Plus m) (Plus n) = Plus (mal_IN m n)
mal (Minus m) (Minus n) = Plus (mal_IN m n)
mal (Plus m) (Minus n) = Minus (mal_IN m n)
mal (Minus m) (Plus n) = Minus (mal_IN m n)
\end{code}

negateZZ negates a ZZ. Used in durch.
\begin{code}
negateZZ :: ZZ -> ZZ
negateZZ Null = Null
negateZZ (Plus n) = Minus n
negateZZ (Minus n) = Plus n
\end{code}

durch_IN_helper is a helper function for durch_IN, and performs the actual division. It always return a value that is one too much, since zero can't be represented by IN_1's. This issue is addresses in durch_IN which returns a ZZ.
Let's say it performs the division m/n. It runs through m, and each time it has gone n further, it adds one to the result value.
The third argument is m which is recursed through.
The fourth is n, which remains unchanged throughout.
the second argument is the result so far, called cum for cumulator.
The first argument keeps track of when it has come n values further through m. When it has, the cumulator is incremented and the first argument is replaced by n in the further recursion.
\begin{code}
durch_IN_helper :: IN_1 -> IN_1 -> IN_1 -> IN_1 -> IN_1
durch_IN_helper Eins cum Eins n = Nf cum
durch_IN_helper (Nf count) cum Eins n = cum
durch_IN_helper Eins cum (Nf m) n =  durch_IN_helper n (Nf cum) m n
durch_IN_helper (Nf count) cum (Nf m) n = durch_IN_helper count cum m n
\end{code}

durch_IN is division on IN_1 and calls durch_IN_helper with the right arguments. Since IN_1 can't represent zero, durch_IN_helper returns a result which is 1 too high. This function subtracts one from the temp result and returns it as a ZZ.
\begin{code}
durch_IN :: IN_1 -> IN_1 -> ZZ
durch_IN m n = case durch_IN_helper n Eins m n of
  Eins -> Null
  Nf res -> Plus res
\end{code}

durch performs division on ZZ by calling durch_IN. The logic here is mostly handling signs.
If the divisor is zero the result is also zero as per the assignment description.
In the case of both a negative and positive input, the result is negative. Since durch_IN rounds downwards, this translates as rounding towards zero here. Therefore one is added to the result before it is negated, giving the effect of a subtraction. One could argue that the code could be simplified by not removing 1 in durch_IN and subtracting one when the result is positive here instead of subtracting one when the result is negative, but then durch_IN could not be used as a stand-alone function for division of IN_1's.
\begin{code}
durch :: ZZ -> ZZ -> ZZ
durch m Null = Null
durch Null n = Null
durch (Plus m) (Plus n) = durch_IN m n
durch (Minus m) (Minus n) = durch_IN m n
durch (Plus m) (Minus n) = negateZZ $ plus (Plus Eins) $ durch_IN m n
durch (Minus m) (Plus n) = negateZZ $ plus (Plus Eins) $ durch_IN m n
\end{code}

gleich_IN compares two IN_1 by running through them until one or two Eins are encountered. If there is only the results are not equal, otherwise they are.
Helper function for gleich.
\begin{code}
gleich_IN :: IN_1 -> IN_1 -> Bool
gleich_IN Eins Eins = True
gleich_IN (Nf m) (Nf n) = gleich_IN m n
gleich_IN _ _ = False
\end{code}

gleich compare ZZ's by calling gleich_IN. This function mostly juggles signs.
\begin{code}
gleich :: ZZ -> ZZ -> Bool
gleich Null Null = True
gleich _ Null = False
gleich Null _ = False
gleich (Plus m) (Plus n) = gleich_IN m n
gleich (Minus m) (Minus n) = gleich_IN m n
gleich _ _ = False
\end{code}

ungleich is the negation of gleich.
\begin{code}
ungleich :: ZZ -> ZZ -> Bool
ungleich m n= not $ gleich m n
\end{code}

groesser_IN figures if one IN_1 is greater than another by running through them until an Eins is encountered, in which case the result becomes obvious.
\begin{code}
groesser_IN :: IN_1 -> IN_1 -> Bool
groesser_IN (Nf m) (Nf n) = groesser_IN m n
groesser_IN (Nf m) Eins = True
groesser_IN _ _ = False
\end{code}

groesser figures if one ZZ is greater than another by calling groesser_IN.
The code here is just sign juggling.
\begin{code}
groesser :: ZZ -> ZZ -> Bool
groesser (Plus m) (Plus n) = groesser_IN m n
groesser (Minus m) (Minus n) = groesser_IN n m
groesser Null Null = False
groesser (Plus _) _ = True
groesser _ (Minus _) = True
groesser _ _ = False
\end{code}

kleiner figures if one ZZ is less than another by seeing if it is neither the same or greater.
\begin{code}
kleiner :: ZZ -> ZZ -> Bool
kleiner m n = not $ ((groesser m n) || gleich m n)
\end{code}

ggleich is the opposite of kleiner.
\begin{code}
ggleich :: ZZ -> ZZ -> Bool
ggleich m n = not $ kleiner m n
\end{code}

kgleich is the opposite of groesser.
\begin{code}
kgleich :: ZZ -> ZZ -> Bool
kgleich m n= not $ groesser m n
\end{code}

Testing functions for my own convenience:

Conversion between types.
Examples: zezz is from Zett to ZZ and ize is from IN_1 to Zett
\begin{code}
ize  = von_IN_1_nach_Zett
zei  = von_Zett_nach_IN_1
zezz = von_Zett_nach_ZZ
zzze = von_ZZ_nach_Zett
\end{code}

easyR if for easy use of relations on ZZ
easyO is for easy use of operators on ZZ
each take an operator/relation and their two arguments, and applies them.
\begin{code}
easyR :: (ZZ -> ZZ -> a) -> Zett -> Zett -> a
easyR f m n = f (zezz m) (zezz n)

easyO :: (ZZ -> ZZ -> ZZ) -> Zett -> Zett -> Zett
easyO f m n = zzze (easyR f m n)
\end{code}

the two compare functions compare relations and operators on ZZ with their counterpart on Zett. They each take the relevant function on both types, and their arguments as Zett
\begin{code}
compareR :: Eq a => (Zett -> Zett -> a) -> (ZZ -> ZZ -> a) -> Zett -> Zett -> Bool
compareR fze fzz m n = (fze m n) == (easyR fzz m n)

compareO :: (Zett -> Zett -> Zett) -> (ZZ -> ZZ -> ZZ) -> Zett -> Zett -> Bool
compareO fze fzz m n = (fze m n) == (easyO fzz m n)
\end{code}

predefined uses of the compare functions. Their name are short for the english names of the functions. p or plus, i for inequal, etc.
\begin{code}
p = compareO (+) plus
m = compareO (-) minus
t = compareO (*) mal
d = compareO (\x y -> if y == 0 then 0 else div x y) durch
e = compareR (==) gleich
i = compareR (/=) ungleich
g = compareR (>) groesser
s = compareR (<) kleiner
ge = compareR (>=) ggleich
se = compareR (<=) kgleich
\end{code}
