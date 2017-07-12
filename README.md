# Haskell Hash Table Benchmark

[![Build Status](https://travis-ci.org/hongchangwu/hashtable-benchmark.svg?branch=master)](https://travis-ci.org/hongchangwu/hashtable-benchmark)

Benchmark of implementations of hash table like containers in Haskell:

- `Data.Map`
- `Data.IntMap`
- `Data.HashMap`
- `Data.HashTable`

## Build

```
stack setup
stack build
```

## Run

```
stack exec benchmark
```

## Example
An example on my laptop is shown here(at lts-8.21, 2.5 GHz Intel Core i7).

```
benchmarking insert/Data.Map.Lazy/sorted
time                 2.301 s    (1.797 s .. 2.773 s)
                     0.994 R²   (0.978 R² .. 1.000 R²)
mean                 2.199 s    (2.091 s .. 2.275 s)
std dev              114.1 ms   (0.0 s .. 131.2 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.Map.Lazy/random
time                 489.5 ms   (426.7 ms .. 577.5 ms)
                     0.995 R²   (0.994 R² .. 1.000 R²)
mean                 442.2 ms   (418.9 ms .. 459.8 ms)
std dev              26.80 ms   (0.0 s .. 30.36 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.Map.Strict/sorted
time                 2.574 s    (318.7 ms .. NaN s)
                     0.921 R²   (0.732 R² .. 1.000 R²)
mean                 2.691 s    (2.396 s .. 2.921 s)
std dev              357.8 ms   (0.0 s .. 399.9 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking insert/Data.Map.Strict/random
time                 472.1 ms   (460.6 ms .. 490.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 492.2 ms   (484.1 ms .. 498.6 ms)
std dev              9.993 ms   (0.0 s .. 11.12 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.IntMap/sorted
time                 695.6 ms   (525.9 ms .. NaN s)
                     0.985 R²   (0.960 R² .. 1.000 R²)
mean                 691.5 ms   (646.1 ms .. 716.9 ms)
std dev              40.12 ms   (0.0 s .. 43.94 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.IntMap/random
time                 406.3 ms   (345.8 ms .. NaN s)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 401.7 ms   (393.4 ms .. 408.7 ms)
std dev              11.21 ms   (67.99 as .. 12.17 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashMap.Lazy/sorted
time                 3.019 s    (2.879 s .. 3.093 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.724 s    (2.613 s .. 2.784 s)
std dev              97.23 ms   (0.0 s .. 104.4 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashMap.Lazy/random
time                 51.62 ms   (48.49 ms .. 54.51 ms)
                     0.996 R²   (0.994 R² .. 0.999 R²)
mean                 59.28 ms   (56.63 ms .. 62.11 ms)
std dev              4.452 ms   (3.389 ms .. 6.180 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking insert/Data.HashMap.Strict/sorted
time                 2.704 s    (2.099 s .. 3.265 s)
                     0.994 R²   (0.977 R² .. 1.000 R²)
mean                 2.674 s    (2.565 s .. 2.752 s)
std dev              117.0 ms   (0.0 s .. 134.4 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashMap.Strict/random
time                 58.08 ms   (56.95 ms .. 59.57 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 59.73 ms   (58.92 ms .. 60.35 ms)
std dev              1.240 ms   (698.3 μs .. 1.917 ms)

benchmarking insert/Data.HashTable.ST.Basic/sorted
time                 1.126 s    (1.000 s .. 1.198 s)
                     0.999 R²   (NaN R² .. 1.000 R²)
mean                 988.3 ms   (912.5 ms .. 1.032 s)
std dev              68.11 ms   (0.0 s .. 76.18 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Basic/random
time                 762.7 ms   (741.6 ms .. 804.3 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 743.8 ms   (743.0 ms .. 744.3 ms)
std dev              782.0 μs   (0.0 s .. 901.4 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Cuckoo/sorted
time                 1.918 s    (1.542 s .. NaN s)
                     0.995 R²   (0.991 R² .. 1.000 R²)
mean                 1.754 s    (1.636 s .. 1.830 s)
std dev              113.5 ms   (0.0 s .. 130.5 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Cuckoo/random
time                 249.2 ms   (244.0 ms .. 256.6 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 255.9 ms   (253.6 ms .. 260.2 ms)
std dev              4.107 ms   (110.2 μs .. 4.960 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Linear/sorted
time                 6.624 s    (2.077 s .. 9.684 s)
                     0.950 R²   (NaN R² .. 1.000 R²)
mean                 5.550 s    (4.837 s .. 6.088 s)
std dev              824.5 ms   (0.0 s .. 932.7 ms)
variance introduced by outliers: 46% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Linear/random
time                 142.5 ms   (120.1 ms .. 162.2 ms)
                     0.988 R²   (0.981 R² .. 0.999 R²)
mean                 165.1 ms   (155.2 ms .. 174.0 ms)
std dev              12.78 ms   (9.619 ms .. 16.40 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.Map.Lazy
time                 210.4 ms   (195.0 ms .. 226.2 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 236.2 ms   (227.0 ms .. 246.2 ms)
std dev              12.80 ms   (7.948 ms .. 16.81 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.Map.Strict
time                 282.9 ms   (246.8 ms .. 345.9 ms)
                     0.985 R²   (0.973 R² .. 1.000 R²)
mean                 258.8 ms   (249.7 ms .. 276.2 ms)
std dev              17.19 ms   (47.20 μs .. 20.11 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking lookup/Data.IntMap
time                 203.2 ms   (196.9 ms .. 207.7 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 206.2 ms   (203.5 ms .. 207.3 ms)
std dev              2.099 ms   (78.15 μs .. 2.676 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.HashMap.Lazy
time                 203.3 ms   (192.8 ms .. 212.7 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 208.0 ms   (203.8 ms .. 210.1 ms)
std dev              3.618 ms   (1.487 ms .. 4.815 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.HashMap.Strict
time                 180.4 ms   (170.9 ms .. 188.7 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 179.2 ms   (176.8 ms .. 182.1 ms)
std dev              3.307 ms   (1.934 ms .. 5.057 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.HashTable.ST.Basic
time                 276.9 ms   (187.1 ms .. 336.2 ms)
                     0.979 R²   (0.927 R² .. 1.000 R²)
mean                 250.2 ms   (216.9 ms .. 264.9 ms)
std dev              24.64 ms   (8.865 ms .. 33.96 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking lookup/Data.HashTable.ST.Cuckoo
time                 445.5 ms   (382.0 ms .. 522.0 ms)
                     0.995 R²   (0.994 R² .. 1.000 R²)
mean                 464.0 ms   (445.0 ms .. 476.1 ms)
std dev              18.13 ms   (0.0 s .. 20.84 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking lookup/Data.HashTable.ST.Linear
time                 392.3 ms   (362.3 ms .. 421.3 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 374.2 ms   (362.9 ms .. 380.7 ms)
std dev              10.11 ms   (0.0 s .. 11.24 ms)
variance introduced by outliers: 19% (moderately inflated)
```
