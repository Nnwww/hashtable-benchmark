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
time                 2.484 s    (2.138 s .. 3.276 s)
                     0.988 R²   (0.978 R² .. 1.000 R²)
mean                 2.229 s    (2.133 s .. 2.420 s)
std dev              165.3 ms   (0.0 s .. 165.7 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking insert/Data.Map.Lazy/random
time                 546.2 ms   (480.8 ms .. 620.3 ms)
                     0.998 R²   (0.992 R² .. 1.000 R²)
mean                 497.7 ms   (462.9 ms .. 515.2 ms)
std dev              30.10 ms   (67.99 as .. 30.28 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.Map.Strict/sorted
time                 2.583 s    (2.359 s .. 2.838 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 2.580 s    (2.526 s .. 2.608 s)
std dev              46.64 ms   (0.0 s .. 48.09 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.Map.Strict/random
time                 570.6 ms   (471.2 ms .. 695.8 ms)
                     0.995 R²   (0.982 R² .. 1.000 R²)
mean                 546.8 ms   (515.4 ms .. 566.8 ms)
std dev              30.15 ms   (0.0 s .. 34.72 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.IntMap/sorted
time                 1.041 s    (920.7 ms .. 1.167 s)
                     0.998 R²   (0.993 R² .. NaN R²)
mean                 900.1 ms   (817.5 ms .. 943.9 ms)
std dev              71.94 ms   (136.0 as .. 75.80 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking insert/Data.IntMap/random
time                 424.1 ms   (416.8 ms .. 436.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 454.7 ms   (444.0 ms .. 462.9 ms)
std dev              12.66 ms   (0.0 s .. 14.23 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashMap.Lazy/sorted
time                 3.795 s    (3.212 s .. 4.658 s)
                     0.992 R²   (0.990 R² .. 1.000 R²)
mean                 3.226 s    (2.965 s .. 3.419 s)
std dev              293.1 ms   (0.0 s .. 333.9 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking insert/Data.HashMap.Lazy/random
time                 69.74 ms   (63.26 ms .. 75.37 ms)
                     0.981 R²   (0.945 R² .. 1.000 R²)
mean                 68.62 ms   (66.50 ms .. 71.90 ms)
std dev              4.440 ms   (2.782 ms .. 6.525 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking insert/Data.HashMap.Strict/sorted
time                 3.550 s    (3.356 s .. 3.727 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.239 s    (3.139 s .. 3.292 s)
std dev              87.46 ms   (0.0 s .. 92.48 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashMap.Strict/random
time                 66.79 ms   (63.60 ms .. 70.53 ms)
                     0.996 R²   (0.990 R² .. 1.000 R²)
mean                 68.44 ms   (67.18 ms .. 69.38 ms)
std dev              1.978 ms   (1.142 ms .. 2.690 ms)

benchmarking insert/Data.HashTable.ST.Basic/sorted
time                 1.080 s    (920.4 ms .. 1.221 s)
                     0.996 R²   (0.996 R² .. 1.000 R²)
mean                 972.9 ms   (911.4 ms .. 1.014 s)
std dev              61.68 ms   (0.0 s .. 71.22 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Basic/random
time                 789.1 ms   (747.9 ms .. 811.5 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 830.1 ms   (812.1 ms .. 840.9 ms)
std dev              16.50 ms   (0.0 s .. 18.74 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Cuckoo/sorted
time                 1.863 s    (1.543 s .. 2.047 s)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 1.745 s    (1.642 s .. 1.804 s)
std dev              91.99 ms   (0.0 s .. 101.4 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Cuckoo/random
time                 294.0 ms   (281.7 ms .. 313.1 ms)
                     0.998 R²   (0.992 R² .. 1.000 R²)
mean                 284.7 ms   (277.5 ms .. 290.8 ms)
std dev              7.953 ms   (2.412 ms .. 10.57 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Linear/sorted
time                 6.985 s    (4.824 s .. 8.392 s)
                     0.986 R²   (0.976 R² .. 1.000 R²)
mean                 5.935 s    (5.629 s .. 6.149 s)
std dev              321.0 ms   (0.0 s .. 369.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert/Data.HashTable.ST.Linear/random
time                 166.9 ms   (128.9 ms .. 206.8 ms)
                     0.979 R²   (0.969 R² .. 1.000 R²)
mean                 202.7 ms   (183.1 ms .. 214.2 ms)
std dev              19.73 ms   (14.57 ms .. 24.07 ms)
variance introduced by outliers: 30% (moderately inflated)

benchmarking lookup/Data.Map.Lazy
time                 291.4 ms   (272.2 ms .. 302.7 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 296.1 ms   (289.7 ms .. 300.0 ms)
std dev              6.481 ms   (3.485 ms .. 9.020 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking lookup/Data.Map.Strict
time                 293.2 ms   (278.1 ms .. 326.3 ms)
                     0.995 R²   (0.971 R² .. 1.000 R²)
mean                 286.5 ms   (280.2 ms .. 298.0 ms)
std dev              10.97 ms   (671.5 μs .. 13.59 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking lookup/Data.IntMap
time                 211.5 ms   (202.8 ms .. 217.7 ms)
                     0.999 R²   (0.993 R² .. 1.000 R²)
mean                 211.0 ms   (207.3 ms .. 216.6 ms)
std dev              5.600 ms   (2.320 ms .. 8.033 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.HashMap.Lazy
time                 190.7 ms   (186.3 ms .. 200.9 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 187.4 ms   (184.6 ms .. 190.5 ms)
std dev              3.973 ms   (2.053 ms .. 5.491 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.HashMap.Strict
time                 229.0 ms   (219.6 ms .. 250.6 ms)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 225.5 ms   (221.2 ms .. 233.8 ms)
std dev              6.958 ms   (2.296 ms .. 9.614 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking lookup/Data.HashTable.ST.Basic
time                 293.7 ms   (216.3 ms .. 370.4 ms)
                     0.961 R²   (0.814 R² .. 1.000 R²)
mean                 297.7 ms   (259.2 ms .. 326.6 ms)
std dev              37.59 ms   (14.77 ms .. 49.68 ms)
variance introduced by outliers: 37% (moderately inflated)

benchmarking lookup/Data.HashTable.ST.Cuckoo
time                 502.0 ms   (483.1 ms .. 520.7 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 493.6 ms   (487.6 ms .. 497.3 ms)
std dev              5.599 ms   (0.0 s .. 6.371 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking lookup/Data.HashTable.ST.Linear
time                 447.1 ms   (399.4 ms .. 499.1 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 432.4 ms   (418.2 ms .. 441.4 ms)
std dev              13.53 ms   (0.0 s .. 15.57 ms)
variance introduced by outliers: 19% (moderately inflated)
```
