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
-- requiere n as a first parameter a odd number prime less to buid the ramifications of the prime waves.

nsf n s = (n^(2) -1) -s


-- EXTRACT PRIVATE KEY WITH EXPONEN ANT N IN NSS NUMBERS 

nss_privatekey e n s= modular_inverse e (nsf n s)

-- EXTRACT FACTORS in NSF numbers

nsf_factorise_ecm n = (sg2-qrest, sg2+qrest) 
	where
	sigma = (n+1)-(totient n)
	sg2 = div sigma 2   
	qrest = integerSquareRoot ((sg2^2)-n)


nsf_factorise n t= (sg2-qrest, sg2+qrest) 
	where
	sigma = ((n+1))-(t)
	sg2 = div sigma 2   
	qrest = integerSquareRoot ((sg2^2)-n)




-- MAP NSF PRODUCT OF PRIMES

-- N bits mapping

--for strong nss

nsf_map s x r= map fst (filter (\(x,c)-> c==0) $ map (\x-> (x,tryperiod x (ns x r))) ([2^s..2^s+x]))


-- N bits mapping checking with ECM just products of two primers

nss_find nbits range to = take to $ filter (\(v,c)-> length c==2) (map (\x-> (x,P.factorise x)) (nsf_map (nbits) range 0))

-- N bits mappingi without perfect squares or prime numers really slow checking primes, delete for faster mapping, pending chage to a fast comprobation 

nss_map_nsq s x r =  filter (\(d)-> snd (integerSquareRootRem d) /= 0 ) (nsf_map s x r) 


-- CHECK PERIOD LENGTH FOR N Using RSA
tryperiod n period = (powMod (powMod (2) 1826379812379156297616109238798712634987623891298419 n) (modular_inverse 1826379812379156297616109238798712634987623891298419 period) n) - (2) 

-- GET DIVISORS WITH ECM METHOD
divs n = read $ concat (tail (splitOn " " (show (divisors n))))::[Integer]

-- GET SUM OF FACTORS WITH ECM

sum_factors n = n + 1 - (totient n) 


-- DECIMAL EXPANSION, THE PERIOD



-- Efficient way to calculate decimal expansion

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

-- All who decodes msg decodes a number
-- in diferent kind of field

alldecnss n = filter (\(c)-> tryperiod n (n^2) == 0 || tryperiod n (n^2 - c-1)==0 || tryperiod n (n^2 + c-1) == 0 ) $ (tail [0,3..n])


alldec2 n = take 1000 $ filter (\(z,y) -> y == 0 ) (map (\x-> (x , tryperiod n ((x^2) + (x*6))) ) (reverse [1..n]))


alldec n = filter (\(z,y) -> y == 0) (map (\x->(x,tryperiod n x)) [1..n])

