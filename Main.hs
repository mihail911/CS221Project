{- |
Module      : Main
Description : Runs coursegraph.

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-11-26

-}

module Main where

import ReadDB
import CourseGraph

testRelatednessReal :: IO ()
testRelatednessReal =
  do entries <- allEntries
     let math51 = [ entry | entry <- entries, codeKey entry == "math51" ]
     print ""

runTests :: IO ()
runTests = testRelatednessReal

     

main :: IO ()
main = runTests
