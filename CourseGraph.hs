{- |
Module      : coursegraph
Description : Utility to build a course relatedness graph

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-11-26

The Python module of the same name was simply too slow, so I rewrote
it in Haskell.

-}

module CourseGraph where

import Data.Char
import Data.Function
import Data.List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

-- MissingH
import Data.String.Utils

import Util
import ReadDB

allEntries :: IO [Entry]
allEntries = readDB smallDBName >>= (return . makeEntries)

unigramFeatures :: String -> [String]
unigramFeatures = words

bigramFeatures :: String -> [String]
bigramFeatures str = map (\(x,y) -> x ++ "|" ++ y) $ zip wl (tail wl)
  where wl = words str

stringFeatures :: String -> [String]
stringFeatures str = unigramFeatures str -- ++ bigramFeatures str

titleFeatures :: String -> FeatureSet
titleFeatures title = Set.fromList [ Title w | w <- stringFeatures title ]

codeFeatures :: String -> FeatureSet
codeFeatures code = Set.fromList $ map Maybe.fromJust $ filter Maybe.isJust [
  Just $ CodeName letters,
  if length numbers > 0 then Just $ Code100s $ numbers!!0 else Nothing, 
  if length numbers > 1 then Just $ Code10s $ numbers!!1 else Nothing
  -- if length numbers > 0 then Just $ Code11s $ tail numbers else Nothing
  ]
  where letters = filter isAlpha code
        numbers = filter isDigit code

instrFeatures :: String -> FeatureSet
instrFeatures instrs =
  Set.fromList [ Instructor $ strip w | w <- split "," instrs, length w > 0 ]

unitFeatures :: Int -> Int -> FeatureSet
unitFeatures minUnits maxUnits =
  Set.fromList [ MinUnits minUnits, MaxUnits maxUnits ]

descFeatures :: String -> FeatureSet
descFeatures desc = Set.fromList [ Desc w | w <- stringFeatures desc ]

extractFeatures :: Entry -> FeatureSet
extractFeatures entry = Set.unions [
  titleFeats, codeFeats, instrFeats, unitFeats, descFeats, codeComboFeats
  ]
  where titleFeats = titleFeatures $ titleKey entry
        codeFeats = codeFeatures $ codeKey entry
        instrFeats = instrFeatures $ instructorsKey entry
        unitFeats = unitFeatures (minUnitsKey entry) (maxUnitsKey entry)
        descFeats = descFeatures $ descriptionKey entry
        codeComboFeats = Set.fromList [
          Combo feat1 feat2 |
          feat1 <- Set.toList codeFeats,
          feat2 <- Set.toList $ Set.unions [titleFeats, instrFeats, descFeats]
                            ]

-- | Increment the given key's value in the map. If the key does not
-- exist, insert it with the value 1.
incMapValue :: (Num v, Ord k) => k -> Map.Map k v -> Map.Map k v
incMapValue k m =
  let v = Map.lookup k m
  in case v of
    Just x -> Map.insert k (x+1) m
    Nothing -> Map.insert k 1 m

getFeatureMap :: [Entry] -> FeatureMap
getFeatureMap entries =
  foldl (\cum entry -> Map.insert entry (extractFeatures entry) cum)
  Map.empty entries

getFeaturePriors :: FeatureMap -> FeaturePriorMap
getFeaturePriors featureMap =
  Map.map (/ fromIntegral (Map.size featureMap)) featureCounts
  where featureCounts = Map.foldl folder Map.empty featureMap
        folder myMap feats =
          Set.foldl (\cum feat -> incMapValue feat cum) myMap feats

-- combineProbs :: [Double] -> Double
-- combineProbs [] = 0
-- combineProbs probs = prod / (prod + invs)
--   where prod = product probs
--         invs = product $ map (1.0 -) probs

combineProbs :: [Double] -> Double
combineProbs = sum

bayesWeight :: FeaturePriorMap -> FeatureSet -> FeatureSet -> Double
bayesWeight featurePriors feats1 feats2 = combineProbs $ Set.toList probs
  where probs = Set.map update feats1
        prior = 0.1
        update feat = if Set.member feat feats2
                      then probUpdate prior 0.9 $ featurePrior
                      else probUpdate prior 0.1 $ 1 - featurePrior
          where featurePrior = featurePriors Map.! feat                        

weight :: Map.Map Feature Double -> FeatureSet -> FeatureSet -> Double
weight = bayesWeight

-- | Find the K most related courses.
buildRelatedCourses :: FeaturePriorMap -> FeatureMap ->
                     Int -> Entry -> [(Entry, Double)]
buildRelatedCourses featurePriors featureMap numToGet entry1 =
  largestKBy (compare `on` snd) numToGet $
  map (\(entry2, feats2) -> (entry2, weight featurePriors feats1 feats2)) $
  Map.assocs featureMap
  where feats1 = featureMap Map.! entry1

-- | Construct a directed graph where each entry has an edge to its 
-- most-related entries in order. Represents the graph as an adjacency
-- list.
-- Note: This could be generalized to any sort of relatedness.
getRelatednessGraph :: (Entry -> [(Entry, Double)]) -> [Entry] -> RelatednessGraph
getRelatednessGraph relatedFun entries =
  Map.fromList $ map (\entry -> (entry, relatedFun entry)) entries

-- | Do the whole process of reading in the DB, constructing the
  -- relatedness graph, and writing it to the DB.
constructRelatednessGraph :: Int -> IO ()
constructRelatednessGraph numRelated =
  do entries <- allEntries
     let featureMap = getFeatureMap entries
     let featurePriors = getFeaturePriors featureMap
     let graph = getRelatednessGraph
                 (buildRelatedCourses featurePriors featureMap numRelated)
                 entries
     writeRelatednessGraph graph
