{- |
Module      : Main
Description : Runs coursegraph.

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-11-26

-}

module Main where

import Util
import ReadDB
import CourseGraph

testRelatednessReal :: IO ()
testRelatednessReal =
  do entries <- allEntries
     let featureMap = getFeatureMap entries
     let featurePriors = getFeaturePriors featureMap
     let math51 = head [ entry | entry <- entries, codeKey entry == "math51" ]
     print math51
     print $ map (getRelatedCourses featurePriors featureMap 8) entries
     -- print $ getRelatedCourses featurePriors featureMap 8 math51

runTests :: IO ()
runTests = testRelatednessReal

     

main :: IO ()
main = runTests
