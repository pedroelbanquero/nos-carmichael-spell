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

-- NOS CARMICHAEL SPELL FUNCTIONS

--npe n = (n^2-1)*8 
--sqrdif p = p^2*p^2 - (p^2+p^2 - p^2) 
--sqrdif p = p^2 - 1
--ncs n = n^2*n^2 - n^2 
--sqrdif4 n = n^2*n^2 - (n-1)    
--sqrdif2 n s = (n^2 -  s)   

-- COMPUTE CARMICHAEL DERIVATION

ncs_derivate n s = n^2*n^2 - n^2 + s

-- EXTRACT PRIVATE KEY WITH EXPONEN ANT N IN NCS NUMBERS 

ncs_privatekey e n s= modular_inverse e (ncs_derivate n s)

-- CRACK LOOP WITH NCS

ncs_crack n s l
	| ch == 0 = sq
	| l ==s = 0
	| otherwise = ncs_crack n (s+1) l
	where
		sq = ncs_derivate n (s+1)
		ch = tryperiod n sq


-- MAP NCS PRODUCT OF PRIMES
ncs_map s x = map fst (filter (\(x,c)-> c==0) $ map (\x-> (x,tryperiod x (ncs_derivate x s))) ([2^s..2^s+x]))

ncs_find nbits range = take 1 $ filter ((v,x)->length x==2) (map (\x-> (x,P.factorise x)) (ncs_map (nbits) range))


-- FACTORIZE WITH N AND (TOTIENT OR CARMICHAEL OR PERIOD)
ncs_factors n t = (head (take 1 $ filter (\x->  gcd n (x+1)/=1 ) (tail (reverse (divs t))))+1)
ncs_fac n t = (ncs_factors n t, div n (ncs_factors n t) )


-- CHECK PERIOD LENGTH FOR N
tryperiod n period = (powMod (powMod (2) 65537 n) (modular_inverse 65537 period) n) - (2) 

-- GET DIVISORS WITH ECM METHOD
divs n = read $ concat (tail (splitOn " " (show (divisors n))))::[Integer]

-- GET SUM OF FACTORS
ncs_sum_factors_pow n = integerSquareRootRem n

-- DECIMAL EXPANSION, THE PERIOD
-- ncs decimal expansion for NCS numbers.

--ncs_period = 


-- Decimal expansion length , the period, in a ECM method fast calculation
ncsecm_period n= fst ( (take 1 $ filter (\(x,y)-> y==1) $ map (\x -> (x,powMod 10 x n) ) ( tail (reverse (divs (carmichael n))) ) ) !! 0)

-- Decimal expansion in a traditional slow way
period n = (length (takeWhile (/=1) $ map (\x -> powMod 10 x n ) ( tail [0,1..n])) ) +1





