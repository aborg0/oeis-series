# oeis-series

![Scala CI](https://github.com/aborg0/oeis-series/workflows/Scala%20CI/badge.svg)

**WIP**

Demo site: https://aborg0.github.io/oeis-series/

***Development***

Use the sbt task `oeisSeriesJS/fastOptJS::webpack` to generate the necessary js files

***Usage***

Try the following:

 - `left(m) := betweenAdjacentSameDenominatorsCountedOnce(m, 0)`
 - `left(m) := betweenAdjacentSameDenominatorsCountedOnce(m, 1)`
   - `left(m) := { m < 2: betweenAdjacentSameDenominatorsCountedOnce(m, 1); : betweenAdjacentSameDenominatorsCountedOnce(m, 1) - betweenAdjacentSameDenominatorsCountedOnce(m - 2, 1)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedOnce(m, 2)`
   - `left(m) := { m < 6: betweenAdjacentSameDenominatorsCountedOnce(m, 2); : betweenAdjacentSameDenominatorsCountedOnce(m, 2) - betweenAdjacentSameDenominatorsCountedOnce(m - 6, 2)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedOnce(m, 3)`
   - `left(m) := { m < 12: betweenAdjacentSameDenominatorsCountedOnce(m, 3); : betweenAdjacentSameDenominatorsCountedOnce(m, 3) - betweenAdjacentSameDenominatorsCountedOnce(m - 12, 3)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedOnce(m, 4)`
   - `left(m) := { m < 20: betweenAdjacentSameDenominatorsCountedOnce(m, 4); : betweenAdjacentSameDenominatorsCountedOnce(m, 4) - betweenAdjacentSameDenominatorsCountedOnce(m - 20, 4)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedOnce(m, 5)`
   - `left(m) := { m < 30: betweenAdjacentSameDenominatorsCountedOnce(m, 5); : betweenAdjacentSameDenominatorsCountedOnce(m, 5) - betweenAdjacentSameDenominatorsCountedOnce(m - 30, 5)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedOnce(m, 6)`
   - `left(m) := { m < 42: betweenAdjacentSameDenominatorsCountedOnce(m, 6); : betweenAdjacentSameDenominatorsCountedOnce(m, 6) - betweenAdjacentSameDenominatorsCountedOnce(m - 42, 6)}`
 - ... Note that `2 = 1*2`, `6 = 2*3`, `12 = 3*4`, `20 = 4*5`, `30 = 5*6`, `42 = 6*7`, ...
 
 - let's see from the middle to the left/right (it is symmetric)
 - `middle(m) := betweenAdjacentSameDenominatorsCountedOnce(m * 2 + 1, m)`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedOnce(m * 2, m)`
   - `middle(m) := { m < 2: betweenAdjacentSameDenominatorsCountedOnce(m * 2, m); : betweenAdjacentSameDenominatorsCountedOnce(m * 2, m) - betweenAdjacentSameDenominatorsCountedOnce((m - 2) * 2, m - 2)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 1, m)`
   - `middle(m) := { m <= 6: betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 1, m); : betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 1, m) - betweenAdjacentSameDenominatorsCountedOnce((m - 6) * 2 - 1, m - 6)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 2, m)`
   - `middle(m) := { m <= 12: betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 2, m); : betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 2, m) - betweenAdjacentSameDenominatorsCountedOnce((m - 12) * 2 - 2, m - 12)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 3, m)`
   - `middle(m) := { m <= 32: betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 3, m); : betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 3, m) - betweenAdjacentSameDenominatorsCountedOnce((m - 30) * 2 - 3, m - 30)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 4, m)`
   - `middle(m) := { m <= 65: betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 4, m); : betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 4, m) - betweenAdjacentSameDenominatorsCountedOnce((m - 60) * 2 - 4, m - 60)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 5, m)`
   - `middle(m) := { m <= 212: betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 5, m); : betweenAdjacentSameDenominatorsCountedOnce(m * 2 - 5, m) - betweenAdjacentSameDenominatorsCountedOnce((m - 210) * 2 - 5, m - 210)}`
 - ... Note that `2`, `6`, `12`, `30`, `60`, `210`, ... might suggest that the next pattern length might be `420`.
 
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 0)`
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 1)`
   - `left(m) := { m <= 2: betweenAdjacentSameDenominatorsCountedEach(m, 1); : betweenAdjacentSameDenominatorsCountedEach(m, 1) - betweenAdjacentSameDenominatorsCountedEach(m - 2, 1)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 2)`
   - `left(m) := { m <= 6: betweenAdjacentSameDenominatorsCountedEach(m, 2); : betweenAdjacentSameDenominatorsCountedEach(m, 2) - betweenAdjacentSameDenominatorsCountedEach(m - 6, 2)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 3)`
   - `left(m) := { m <= 12: betweenAdjacentSameDenominatorsCountedEach(m, 3); : betweenAdjacentSameDenominatorsCountedEach(m, 3) - betweenAdjacentSameDenominatorsCountedEach(m - 12, 3)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 4)`
   - `left(m) := { m <= 20: betweenAdjacentSameDenominatorsCountedEach(m, 4); : betweenAdjacentSameDenominatorsCountedEach(m, 4) - betweenAdjacentSameDenominatorsCountedEach(m - 20, 4)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 5)`
   - `left(m) := { m <= 30: betweenAdjacentSameDenominatorsCountedEach(m, 5); : betweenAdjacentSameDenominatorsCountedEach(m, 5) - betweenAdjacentSameDenominatorsCountedEach(m - 30, 5)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 6)`
   - `left(m) := { m <= 42: betweenAdjacentSameDenominatorsCountedEach(m, 6); : betweenAdjacentSameDenominatorsCountedEach(m, 6) - betweenAdjacentSameDenominatorsCountedEach(m - 42, 6)}`
 - `left(m) := betweenAdjacentSameDenominatorsCountedEach(m, 7)`
   - `left(m) := { m <= 56: betweenAdjacentSameDenominatorsCountedEach(m, 7); : betweenAdjacentSameDenominatorsCountedEach(m, 7) - betweenAdjacentSameDenominatorsCountedEach(m - 56, 7)}`
 - ... Note that `2 = 1*2`, `6 = 2*3`, `12 = 3*4`, `20 = 4*5`, `30 = 5*6`, `42 = 6*7`, `56 = 7*8`, ...
 
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2 + 1, m)`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2, m)`
   - `middle(m) := { m < 2: betweenAdjacentSameDenominatorsCountedEach(m * 2, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2, m) - betweenAdjacentSameDenominatorsCountedEach((m - 2) * 2, m - 2)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2 - 1, m)`
   - `middle(m) := { m <= 5: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 1, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 1, m) - betweenAdjacentSameDenominatorsCountedEach((m - 3) * 2 - 1, m - 3)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2 - 2, m)`
   - `middle(m) := { m <= 11: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 2, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 2, m) - betweenAdjacentSameDenominatorsCountedEach((m - 8) * 2 - 2, m - 8)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2 - 3, m)`
   - `middle(m) := { m <= 19: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 3, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 3, m) - betweenAdjacentSameDenominatorsCountedEach((m - 15) * 2 - 3, m - 15)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2 - 4, m)`
   - `middle(m) := { m <= 29: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 4, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 4, m) - betweenAdjacentSameDenominatorsCountedEach((m - 24) * 2 - 4, m - 24)}`
   - `middle(m) := { m <= 17: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 4, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 4, m) - betweenAdjacentSameDenominatorsCountedEach((m - 12) * 2 - 4, m - 12)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2 - 5, m)`
   - `middle(m) := { m <= 38: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 5, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 5, m) - betweenAdjacentSameDenominatorsCountedEach((m - 35) * 2 - 5, m - 35)}`
 - `middle(m) := betweenAdjacentSameDenominatorsCountedEach(m * 2 - 6, m)`
   - `middle(m) := { m <= 52: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 6, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 6, m) - betweenAdjacentSameDenominatorsCountedEach((m - 48) * 2 - 6, m - 48)}`
   - `middle(m) := { m <= 28: betweenAdjacentSameDenominatorsCountedEach(m * 2 - 6, m); : betweenAdjacentSameDenominatorsCountedEach(m * 2 - 6, m) - betweenAdjacentSameDenominatorsCountedEach((m - 24) * 2 - 6, m - 24)}`
 - ... Note that `3 = 1*3`, `8 = 2*4`, `15 * 3*5`, `24 = 4*6`, `35 = 5*7`, `48 = 6*8`, ...
 
 - Check the rows for primes of the `betweenAdjacentSameDenominatorsCountedEach`:
   - `same(m) := betweenAdjacentSameDenominatorsCountedEach(3, m)`
     - `same(m) := betweenAdjacentSameDenominatorsCountedEach(4, m)`!
   - `same(m) := betweenAdjacentSameDenominatorsCountedEach(5, m)`
   - `same(m) := betweenAdjacentSameDenominatorsCountedEach(7, m)`
   - `same(m) := betweenAdjacentSameDenominatorsCountedEach(11, m)`
   - `same(m) := betweenAdjacentSameDenominatorsCountedEach(13, m)`
   - `same(m) := betweenAdjacentSameDenominatorsCountedEach(17, m)`
   - ... (It is not too hard to prove that this is always the same `(p-1)/2` for odd prime `p`, except for the number of fractions (with denominator less than `p`) between `0` and `1/p` and `(p-1)/p` and `1` as there are `0`.)
 - Let's see some Carmichael numbers too:
   - `notsame(m) := betweenAdjacentSameDenominatorsCountedEach(561, m)`
   - `notsame(m) := betweenAdjacentSameDenominatorsCountedEach(1105, m)`
   - `notsame(m) := betweenAdjacentSameDenominatorsCountedEach(1729, m)`
   - `notsame(m) := betweenAdjacentSameDenominatorsCountedEach(2465, m)`
   - `notsame(m) := betweenAdjacentSameDenominatorsCountedEach(2821, m)`
   - `notsame(m) := betweenAdjacentSameDenominatorsCountedEach(6601, m)`
   - `notsame(m) := betweenAdjacentSameDenominatorsCountedEach(8911, m)`
