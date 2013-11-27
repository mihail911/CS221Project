{- |
Module      : coursegraph
Description : Utility to build a course relatedness graph

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-11-26

The Python module of the same name was simply too slow, so I rewrote
it in Haskell.

-}

module CourseGraph where

import Data.Function
import Data.List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

-- MissingH
import Data.String.Utils

import ReadDB

data Feature = Title String
               | CodeName String | Code100s Char | Code10s Char | Code11s String
               | Instructor String
               | MinUnits Int
               | MaxUnits Int
               | Desc String
               | Combo Feature Feature
             deriving (Show, Eq, Ord)

type FeatureSet = Set.Set Feature

allEntries :: IO [Entry]
allEntries = readDB "courseinfodata.db" >>= (return . makeEntries)

titleFeatures :: String -> FeatureSet
titleFeatures title = Set.fromList [ Title w | w <- words title ]

codeFeatures :: String -> FeatureSet
codeFeatures code = Set.fromList $ map Maybe.fromJust $ filter Maybe.isJust [
  Just $ CodeName letters,
  if length numbers > 0 then Just $ Code100s $ numbers!!0 else Nothing, 
  if length numbers > 1 then Just $ Code10s $ numbers!!1 else Nothing, 
  if length numbers > 0 then Just $ Code11s $ tail numbers else Nothing
  ]
  where letters = filter (\c -> c >= 'a' && c <= 'z') code
        numbers = filter (\c -> c >= '0' && c <= '9') code

instrFeatures :: String -> FeatureSet
instrFeatures instrs =
  Set.fromList [ Instructor $ strip w | w <- split "," instrs, length w > 0 ]

unitFeatures :: Int -> Int -> FeatureSet
unitFeatures minUnits maxUnits =
  Set.fromList [ MinUnits minUnits, MaxUnits maxUnits ]

descFeatures :: String -> FeatureSet
descFeatures desc = Set.fromList [ Desc w | w <- words desc ]

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

getFeatureMap :: [Entry] -> Map.Map Entry FeatureSet
getFeatureMap entries =
  foldl (\cum entry -> Map.insert entry (extractFeatures entry) cum)
  Map.empty entries

getFeaturePriors :: Map.Map Entry FeatureSet -> Map.Map Feature Double
getFeaturePriors featureMap =
  Map.map (/ fromIntegral (Map.size featureMap)) featureCounts
  where featureCounts = Map.foldl folder Map.empty featureMap
        folder myMap feats =
          Set.foldl (\cum feat -> incMapValue feat cum) myMap feats

-- | Perform a Bayesian probability update to find P(A | Event).
probUpdate :: Double -> Double -> Double -> Double
probUpdate prior pEventGivenPrior pEvent =
  prior * pEventGivenPrior / pEvent

bayesWeight :: Map.Map Feature Double -> FeatureSet -> FeatureSet -> Double
bayesWeight featurePriors feats1 feats2 = sum $ Set.toList probs
  where probs = Set.map update feats1
        prior = 0.1
        update feat = if Set.member feat feats2
                      then probUpdate prior 0.9 $ featurePrior
                      else probUpdate prior 0.1 $ 1 - featurePrior
          where featurePrior = featurePriors Map.! feat
                        

weight :: Map.Map Feature Double -> FeatureSet -> FeatureSet -> Double
weight = bayesWeight

getRelatedCourses :: Map.Map Feature Double -> Map.Map Entry FeatureSet ->
                     Entry -> Int -> [Entry]
getRelatedCourses featurePriors featureMap entry1 numToGet =
  take numToGet $ map fst $ sortBy (compare `on` (weight featurePriors feats1 . snd)) $
  Map.assocs featureMap
  where feats1 = featureMap Map.! entry1
