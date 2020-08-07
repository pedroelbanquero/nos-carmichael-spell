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

npe n = (n^2-1)*8 

mapprsqr2 s x = map fst (filter (\(x,c)-> c==0) $ map (\x-> (x,tryperiod x (npe x))) [2^s..2^s+x])

sqrdif p = p^2*p^2 - (p^2+p^2 - p^2) 
sqrdif p = p^2 - 1
sqrdif3 n = n^2*n^2 - n^2 
sqrdif4 n = n^2*n^2 - (n-1)    
sqrdif2 n s = (n^2 -  s)   

sqrcrack n s l
	| ch == 0 = sq
	| l ==s = 0
	| otherwise = sqrcrack n (s+1) l
	where
		sq = sqrdif2 n (s+1)
		ch = tryperiod n sq


mapprsqr s x = map fst (filter (\(x,c)-> c==0) $ map (\x-> (x,tryperiod x (sqrdif3 x))) ([2^s..2^s+x]))

fcar n c 
	| gcdtry /= 1 && gcdtry /= n = gcdtry
	| otherwise = fcar n (ds2) 
	where
	dat = (reverse (divs c))
	ds = head dat
	ds2 = head (tail dat)
	gcdtry = gcd n ( ds+1)


tryperiod n period = (powMod (powMod (2) 65537 n) (modular_inverse 65537 period) n) - (2) 


divs n = read $ concat (tail (splitOn " " (show (divisors n))))::[Integer]


