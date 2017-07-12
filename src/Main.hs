{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Main (main) where

import           Criterion.Main
import qualified Data.HashMap.Lazy    as HL
import qualified Data.HashMap.Strict  as HS
import           Data.HashTable.Class (HashTable)
import           Data.HashTable.IO    (BasicHashTable, CuckooHashTable,
                                       IOHashTable, LinearHashTable)
import qualified Data.HashTable.IO    as IOHashTable
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.List            (foldl')
import qualified Data.Map.Lazy        as ML
import qualified Data.Map.Strict      as MS
import           Test.QuickCheck      (generate, vector)

size :: Int
size = 1000000

type InsertInput = ([Int], [Int])

generateInsertInput :: Int -> IO InsertInput
generateInsertInput k = (,) <$> return [1 .. k] <*> generate (vector k)

type LookupInput = [Int]

generateLookupInput :: Int -> IO LookupInput
generateLookupInput k = generate (vector k)

main :: IO ()
main = do
  (htB, htC, htL) <- hashTables
  defaultMain
    [ env (generateInsertInput size) $
      \ ~(sorted, random) ->
         bgroup
           "insert"
           [ bgroup
               "Data.Map.Lazy"
               [ bench "sorted" $ whnf insertML sorted
               , bench "random" $ whnf insertML random
               ]
           , bgroup
               "Data.Map.Strict"
               [ bench "sorted" $ whnf insertMS sorted
               , bench "random" $ whnf insertMS random
               ]
           , bgroup
               "Data.IntMap"
               [ bench "sorted" $ whnf insertIntMap sorted
               , bench "random" $ whnf insertIntMap random
               ]
           , bgroup
               "Data.HashMap.Lazy"
               [ bench "sorted" $ whnf insertHL sorted
               , bench "random" $ whnf insertHL random
               ]
           , bgroup
               "Data.HashMap.Strict"
               [ bench "sorted" $ whnf insertHS sorted
               , bench "random" $ whnf insertHS random
               ]
           , bgroup
               "Data.HashTable.ST.Basic"
               [ bench "sorted" $
                 whnfIO
                   (insertHashTableIO sorted :: IO (BasicHashTable Int Int))
               , bench "random" $
                 whnfIO
                   (insertHashTableIO random :: IO (BasicHashTable Int Int))
               ]
           , bgroup
               "Data.HashTable.ST.Cuckoo"
               [ bench "sorted" $
                 whnfIO
                   (insertHashTableIO sorted :: IO (CuckooHashTable Int Int))
               , bench "random" $
                 whnfIO
                   (insertHashTableIO random :: IO (CuckooHashTable Int Int))
               ]
           , bgroup
               "Data.HashTable.ST.Linear"
               [ bench "sorted" $
                 whnfIO
                   (insertHashTableIO sorted :: IO (LinearHashTable Int Int))
               , bench "random" $
                 whnfIO
                   (insertHashTableIO random :: IO (LinearHashTable Int Int))
               ]
           ]
    , env (generateLookupInput size) $
      \ks ->
         bgroup
           "lookup"
           [ bench "Data.Map.Lazy"            $ nf lookupML ks
           , bench "Data.Map.Strict"          $ nf lookupMS ks
           , bench "Data.IntMap"              $ nf lookupIntMap ks
           , bench "Data.HashMap.Lazy"        $ nf lookupHL ks
           , bench "Data.HashMap.Strict"      $ nf lookupHS ks
           , bench "Data.HashTable.ST.Basic"  $ nfIO (lookupHashTable htB ks)
           , bench "Data.HashTable.ST.Cuckoo" $ nfIO (lookupHashTable htC ks)
           , bench "Data.HashTable.ST.Linear" $ nfIO (lookupHashTable htL ks)
           ]
    ]

insertML :: [Int] -> ML.Map Int Int
insertML = foldl' (\m k -> ML.insert k 1 m) ML.empty

insertMS :: [Int] -> MS.Map Int Int
insertMS = foldl' (\m k -> MS.insert k 1 m) MS.empty

insertIntMap :: [Int] -> IntMap Int
insertIntMap = foldl' (\m k -> IntMap.insert k 1 m) IntMap.empty

insertHL :: [Int] -> HL.HashMap Int Int
insertHL = foldl' (\m k -> HL.insert k 1 m) HL.empty

insertHS :: [Int] -> HS.HashMap Int Int
insertHS = foldl' (\m k -> HS.insert k 1 m) HS.empty

insertHashTableIO
  :: HashTable h
  => [Int] -> IO (IOHashTable h Int Int)
insertHashTableIO xs = do
  ht <- IOHashTable.new
  mapM_ (\k -> IOHashTable.insert ht k 1) xs
  return ht

aML :: ML.Map Int Int
aML = ML.fromAscList [(k, 1) | k <- [1 .. size]]

lookupML :: [Int] -> [Maybe Int]
lookupML = map (`ML.lookup` aML)

aMS :: MS.Map Int Int
aMS = MS.fromAscList [(k, 1) | k <- [1 .. size]]

lookupMS :: [Int] -> [Maybe Int]
lookupMS = map (`MS.lookup` aMS)

aIntMap :: IntMap Int
aIntMap = IntMap.fromAscList [(k, 1) | k <- [1 .. size]]

lookupIntMap :: [Int] -> [Maybe Int]
lookupIntMap = map (`IntMap.lookup` aIntMap)

aHL :: HL.HashMap Int Int
aHL = HL.fromList [(k, 1) | k <- [1 .. size]]

lookupHL :: [Int] -> [Maybe Int]
lookupHL = map (`HL.lookup` aHL)

aHS :: HS.HashMap Int Int
aHS = HS.fromList [(k, 1) | k <- [1 .. size]]

lookupHS :: [Int] -> [Maybe Int]
lookupHS = map (`HS.lookup` aHS)

hashTables :: IO (BasicHashTable Int Int, CuckooHashTable Int Int, LinearHashTable Int Int)
hashTables = (,,) <$> aHashTable <*> aHashTable <*> aHashTable

aHashTable :: forall h. HashTable h => IO (IOHashTable h Int Int)
aHashTable = IOHashTable.fromList [(k, 1) | k <- [1 .. size]]

lookupHashTable :: HashTable h => IOHashTable h Int Int -> [Int] -> IO [Maybe Int]
lookupHashTable ht = mapM (IOHashTable.lookup ht)
