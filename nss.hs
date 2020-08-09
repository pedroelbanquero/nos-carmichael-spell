--NOS CARMICHAEL SPELL v0.0.0.1

module Ncs where

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

-- NOS SANTOS SPELL FUNCTIONS
-- Authors 
-- Vicent Nos
-- Enrique Santos


--npe n = (n^2-1)*8 
--sqrdif p = p^2*p^2 - (p^2+p^2 - p^2) 
--sqrdif p = p^2 - 1
--nss n = n^2*n^2 - n^2 
--sqrdif4 n = n^2*n^2 - (n-1)    
--sqrdif2 n s = (n^2 -  s)   

-- COMPUTE CARMICHAEL DERIVATION

--nss_derivate n s = n^2 - 1 + s . 

nss_derivate n s = (n^(2) -1) * 2^256 * 3^256 * 5^256 * 6 ^ 256 * 7^256 +s

-- EXTRACT PRIVATE KEY WITH EXPONEN ANT N IN NSS NUMBERS 

nss_privatekey e n s= modular_inverse e (nss_derivate n s)

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



-- NOT WORKS, NEEDED FIND PROPORTION BETWEEN "NSS TOTIENT" AND TOTIENT
nss_factorise n = (sg2-qrest, sg2+qrest) 
	where
	sigma = (n^2 + 1)-(nss_derivate n 0)
	sg2 = div sigma 2   
	qrest = integerSquareRoot ((sg2^2)-n)





-- CRACK LOOP WITH NSS

nss_crack n s l
	| ch == 0 = sq
	| l ==s = 0
	| otherwise = nss_crack n (s+1) l
	where
		sq = nss_derivate n (s+1)
		ch = tryperiod n sq


-- MAP NSS PRODUCT OF PRIMES

-- N bits mapping

nss_map s x r= map fst (filter (\(x,c)-> c==0) $ map (\x-> (x,tryperiod x (nss_derivate x r))) ([2^s..2^s+x]))

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

--nss_period = 


-- Decimal expansion length , the period, in a ECM method fast calculation
nssecm_period n= fst ( (take 1 $ filter (\(x,y)-> y==1) $ map (\x -> (x,powMod 10 x n) ) ( tail (reverse (divs (carmichael n))) ) ) !! 0)

-- Decimal expansion in a traditional slow way
period n = (length (takeWhile (/=1) $ map (\x -> powMod 10 x n ) ( tail [0,1..n])) ) +1





