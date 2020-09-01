{-# LANGUAGE FlexibleContexts #-}

--NOS CARMICHAEL SPELL v0.0.0.1
-- Authors
--  Enrique Santos
--  Vicent Nos

module Nss where

import System.Environment
import System.Exit
import Distribution.Simple
import Data.List.Ordered
import Data.List.Split 
import Graphics.EasyPlot
import System.Console.CmdArgs
import System.Random
import System.Entropy
import Math.ContinuedFraction.Simple
import Control.Monad
import Data.List.GroupBy
import qualified Data.List as A
import Numeric.Statistics.Median
import Data.Numbers.Primes
import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import qualified Data.List.Split as S2
import Math.NumberTheory.ArithmeticFunctions
import qualified Math.NumberTheory.Primes as P
import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.Primes.Counting
import Codec.Crypto.RSA.Pure
import Data.Bits
import qualified Math.NumberTheory.Primes.Testing as MB
import qualified Data.Map as L
import Math.NumberTheory.Powers.Squares


-- COMPUTE CARMICHAEL DERIVATION

--nss_derivate n s = n^2 - 1 + s . 

-- derivate for N nss strong

nss_derivate n s = (n^(2) -1) +s


nss_derivate_strong n s = (n^(2) -1) +s

-- derivate for N nss strong

nss_derivate_weak n s = (n+1)^2-1 

-- the the even numers
nss_derivate_even n s = (n-1)^2-1

-- the 3 multiples
--
nss_derivate_3 n s = (n-2)^2 -1 

-- nss 6
--
nss_derivate_6 n s = (n-6)^2 -1 

nss_derivate_9 n s = (n-9)^2 -1

nss_derivate_7 n s = (n-5)^2 -1

nss_derivate_4 n s = (n-4)^2 -1

nss_derivate_8 n s = (n+6)^2 -1

nss_derivate_10 n s = (n+5)^2 -1

nss_derivate_11 n s = (n+8)^2  -1

nss_derivate_12 n s = (n+9)^2 -1 






crackdiv n = n+1 - ((div (n) 6) +1) 

crackdiv2 n = n+1 - ((div (n) 6)  )

nss_T n = (period_T,df,df2,try,try2,try3,try4,try5,try6,try7,try8,try9,try10,try11) 
	where
	df  = n - mod n 6
	df2 = 6 - df
	period_T = lcm df (n - df+6)  
	period_T2 = lcm df2 (df2-6)
	try = tryperiod n period_T 
	try2 = tryperiod n period_T2
	try3 = tryperiod n (n^2 - period_T - 1)
	try4 = tryperiod n (n^2 - period_T2 - 1)
	try5 = tryperiod n (n^2 + period_T - 1)
	try6 = tryperiod n (n^2 + period_T2 -1)
	try7 = tryperiod (n^2) (period_T2)
	try8 = tryperiod n (n^2 - period_T + 1)
	try9 = tryperiod n (n^2 - period_T2 + 1)
	try10 = tryperiod n (n^2 + period_T + 1)
	try11 = tryperiod n (n^2 + period_T2 +1)


final n = (tryperiod n f1, tryperiod n f2, tryperiod n (t1)) 
	where
	f1 = lcm (n-5) (n+1)
	f2 = lcm (n-1) (n+5)
	t1 = (tryperiod n (f1))-3 

pmdif n = (powMod 2 n n) - (powMod 3 n n )


nsf n l
	| r2 == 0 || r3 == 0 = (n,l, df*l,m*l)
	| l > 1000 = (0,0,0,0)
	| otherwise = nsf n (l+3)
	where
	m  = mod n 6
	df = 6 - m
	r2 = tryperiod n (n^2 + (n-df*l)*6)
	r3 = tryperiod n (n^2 + (n-m*l)*6)

ns n = (n,b,b2,m,df,r1) 
	where
	m  = mod n 6
	df = 6 - m
	b =((n-m + n+df)^2 )
	b2 =  ((   ((n-m)^2) + ((n+df)^2) - (n+df)) )
	b3 =  (( ((n-m)^2 - (n-m)) * ((n+df)^2) - (n+df)) ) + 1
	r1 = tryperiod n (lcm n (36))

nss n l
	| pr == 0 || pr2 == 0  = (sqi)
	| pr4 == 0 || pr5 == 0 = (sqi2) 
	| pr3 == 0  = (0)	
	| otherwise = nss n (l+1)
	where 
	sqi = (div n 6) +1 
	sqi2 = div n 6 
	pr = tryperiod n (n^2 - (sqi * l) - 1)
	pr2 = tryperiod n (n^2 + (sqi * l ) -1)
	pr3 = tryperiod n (n^2)
	pr4 = tryperiod n (n^2 - (sqi2 *l) -1)
	pr5 = tryperiod n (n^2 + (sqi *l) -1)
	

nss_debug n l
	| pr == 0 || pr2 == 0  = (sqi,l)
	| pr4 == 0 || pr5 == 0 = (sqi2,l) 
	| pr3 == 0  = (sqi,l)	
	| otherwise = nss_debug n (l+1)
	where
	msq = 6 - mod n 6 
	sqi = (div (n+msq) 6) 
	sqi2 = div n 6 + 1
	pr = tryperiod n (n^2 - (sqi * l) - 1)
	pr2 = tryperiod n (n^2 + (sqi * l ) -1)

	pr3 = tryperiod n (n^2)
	pr4 = tryperiod n (n^2 - (sqi2 *l) )
	pr5 = tryperiod n (n^2 + (sqi *l) -1)



powcrack n l
	| tr == 0 = (n,p,l)
	| otherwise = powcrack n (l+192)
	where 
	p = powMod (l+1) 1203761208376812639871834601286738012380761280376081246308172380612083670817630842760821123333 n
	(a,b,tr) = crmod n p 


crmod n m  
	| tr == 0 = (n,m,0) 
	| m > 10000 = (0,0,1)
	| otherwise = crmod n (m+1) 
	where
	tr = tryperiod n m

-- EXTRACT PRIVATE KEY WITH EXPONEN ANT N IN NSS NUMBERS 

nss_privatekey e n s= modular_inverse e (nss n 1)

-- EXTRACT FACTORS in NSS numbers

nss_factorise_ecm n = (sg2-qrest, sg2+qrest) 
	where
	sigma = (n+1)-(totient n)
	sg2 = div sigma 2   
	qrest = integerSquareRoot ((sg2^2)-n)






-- Incomplete aproximation



nssfactors n = (div n gcds, gcds)
	where
	gcds = gcd n $ fst (integerSquareRootRem ((2^n  - 1)))



nss_factorise n t= (sg2-qrest, sg2+qrest) 
	where
	sigma = ((n+1))-(t)
	sg2 = div sigma 2   
	qrest = integerSquareRoot ((sg2^2)-n)





-- CRACK LOOP WITH NSS

nss_sieve n = ( take 1 $ tail (alldecnss n) )  

-- MAP NSS PRODUCT OF PRIMES

-- N bits mapping

--for strong nss

nss_map s x r= map fst (filter (\(x,c)-> c==0) $ map (\x-> (x,tryperiod x (nss_derivate x r))) ([2^s..2^s+x]))

-- for weak nss 

nss_map_weak s x = map fst (filter (\(x,c)-> c==0) $ map (\x-> (x,tryperiod x (nss x 1))) ([2^s..2^s+x]))


-- N bits mapping checking with ECM just products of two primers

nss_find nbits range to = take to $ filter (\(v,c)-> length c==2) (map (\x-> (x,P.factorise x)) (nss_map (nbits) range 0))

-- N bits mappingi without perfect squares or prime numers really slow checking primes, delete for faster mapping, pending chage to a fast comprobation 

nss_map_nsq s x =  filter (\(d)-> snd (integerSquareRootRem d) /= 0 ) (nss_map s x 0) 


-- CHECK PERIOD LENGTH FOR N
tryperiod n period = (powMod (powMod (2) 1826379812379156297616109238798712634987623891298419 n) (modular_inverse 1826379812379156297616109238798712634987623891298419 period) n) - (2) 

-- GET DIVISORS WITH ECM METHOD
divs n = read $ concat (tail (splitOn " " (show (divisors n))))::[Integer]

-- GET SUM OF FACTORS WITH ECM

nss_sum_factors_pow n = n + 1 - (totient n) 


-- DECIMAL EXPANSION, THE PERIOD

-- nss decimal expansion for NSS numbers.


-- Efficient way to calculate decimal expansion
--
--nss_period :: Integer -> Integer -> Integer
--nss_period ::Integral a => a -> a -> a

-- With P Q
tpq p q = out
	where
	tp = div_until_mod_1 (p-1) (p-1)
	tq = div_until_mod_1 (q-1) (q-1)
	out = (lcm tp tq)


-- With N
tn n = tp
	where
	c = carmichael n
	tp = div_until_mod_1 (c) (c)
	

div_until_mod_1 p last
	| period == 1 = div_until_mod_1 dp dp 
	| mp /= 0 = last
	| otherwise = last
	where
	(dp,mp) = divMod (p) 2
	period = powMod 10 dp (p+1)




-- Decimal expansion in a traditional slow way
period n = (length (takeWhile (/=1) $ map (\x -> powMod 10 x n ) ( tail [0,1..n])) ) +1


-- new primality test based in nss factors
--



-- All decodes a number
--
alldecnss n = filter (\(c)-> tryperiod n (n^2) == 0 || tryperiod n (n^2 - c-1)==0 || tryperiod n (n^2 + c-1) == 0 ) $ (tail [0,3..n])


alldec2 n = take 1000 $ filter (\(z,y) -> y == 0 ) (map (\x-> (x , tryperiod n ((x^2) + (x*6))) ) (reverse [1..n]))



alldec n = filter (\(z,y) -> y == 0) (map (\x->(x,tryperiod n x)) [1..n])

tryfast n l = (n,d,p)
	where
	d = ((l^2 +l*6  )  )
	p = tryperiod n d

