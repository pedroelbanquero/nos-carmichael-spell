# rsa vs factorization

PRETTY IMAGEN GRAPH MODULOS VS TIME

# PRIME PERIOD GRIMOIRE VOL. 2

The present document try to probe how rsa is more easy to solve than factorizacion of semiprimes, the important part of the public key. 

The code of NOS-CARMICHAEL SPELL has been writed for haskell in this repository.

## Grimoire basis spells

- Rsa problem 

  - Encrypt
  
  m = powMod m e n = mc

  - Descrypt

  mc = powMod
  
- Some known things

p^2 mod 6 = 1

N^2 = x6+1 * y6+1

Totient = N - (sum factors) - 1

totiend mod carmichael = 0

period T = powMod 10 T N = 1

carmichael(n) mod T(n) = 0
totient(n) mod T(n) = 0


# Spells logics

Deduction of a perfect prime square

p^2 * p^2 = n^2

Deduction for diferent factors

p^2 * q^2 = n^2

Then 

n^2 - p^2+1 = totient ^ 2 for squares

n^2 - (p^2 + q^2) + 1 = totient ^ 2 

Then 

t = times of decimal expansion length

( (sqrt x6+1)* (sqrt y6+1) ) - ((sqrt 6x+1) + (sqrt y6+1)) +1 = t * T -> Solution rsa


# Writing the spells in the magic book




# Cast spells

- Tests for 8 bits

*Libs.Events> map fst (take 100 $ filter (\(v,x)->length x==2) (map (\x-> (x,P.factorise x)) (mapprsqr2 (8) 100000)))

[259,323,341,377,403,425,451,466,481,482,485,511,514,533,545,559,629,671,674,679,687,703,746,771,781,794,799,866,873,891,965,981,989,993,1027,1154,1161,1189,1191,1205,1241,1247,1261,1271,1285,1299,1346,1351,1387,1417,1469,1535,1539,1541,1649,1651,1661,1687,1751,1799,1853,1891,1923,2019,2023,2047,2059,2071,2169,2171,2191,2313,2434,2471,2501,2507,2509,2619,2651,2701,2705,2761,2813,2863,2911,2977,2989,3017,3133,3205,3277,3281,3341,3365,3379,3439,3459,3479,3566,3641]



- Tests for 16 bits

*Libs.Events> take 1 $ filter (\(v,x)->length x==2) (map (\x-> (x,P.factorise x)) (mapprsqr2 (16) 100000))

[(65683,[(Prime 19,1),(Prime 3457,1)])]

(0.02 secs, 3,472,040 bytes)


- Tests for 32 bits

*Libs.Events> take 1 $ filter (\(v,x)->length x==2) (map (\x-> (x,P.factorise x)) (mapprsqr2 (32) 100000))

[(4294967297,[(Prime 641,1),(Prime 6700417,1)])]

(0.01 secs, 150,696 bytes)


- Tests for 64 bits

*Libs.Events> take 1 $ filter (\(v,x)->length x==2) (map (\x-> (x,P.factorise x)) (mapprsqr2 (64) 100000))

[(18446744073709551617,[(Prime 274177,1),(Prime 67280421310721,1)])]

(0.01 secs, 2,343,568 bytes)


- Tests for 128 bits

*Libs.Events> take 1 $ filter (\(v,x)->length x==2) (map (\x-> (x,P.factorise x)) (mapprsqr2 (128) 100000))

[(340282366920938463463374607431768211457,[(Prime 59649589127497217,1),(Prime 5704689200685129054721,1)])]

(0.68 secs, 799,341,704 bytes)


- Test for 256 bits

*Libs.Events> take 1 $ filter (\(v,x)->length x==2) (map (\x-> (x,P.factorise x)) (mapprsqr2 (256) 100000))

[(115792089237316195423570985008687907853269984665640564039457584007913129639937,[(Prime 1238926361552897,1),(Prime 93461639715357977769163558199606896584051237541638188580280321,1)])]


# Conclusion

All semiprimes who have the same proportion n - sum factors in n^2 are really more easy to calculate a private key to decrypt a message.

Many keys are vulnerable with just one operation to solve the cypher message, and some operations more to factorize the number. Just some seconds to get keys of 512, 1024 or 2048 bits vulnerables to NOS CARMICHAEL SPELL

# Author

Vicent Nos Ripolles


