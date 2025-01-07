-- https://putridparrot.com/blog/unit-testing-haskel-code-with-hunit/

module Main (main) where

-- Test import --
import Test.HUnit
import System.Exit
-- Test import --

-- Modules import --
import Q1
import Q2
import Q3
import Q4
import Q5
import Q6
-- Modules import -- 

main :: IO ()
main = do
    counts <- runTestTT (test [
        testCalcInteractions,
        testEmptyAtoms
        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
        
-- Q1 
-- Test case for calcInteractions with size 6 and given atoms
testCalcInteractions = TestCase $ assertEqual
    "calcInteractions with size 6"
    expectedResult1
    (calcInteractions 6 [(2,3), (4,3), (4,6), (5,5)])
  where
    expectedResult1 = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP South 1 L),
                     (EP East 2 L,EP East 2 R),(EP East 2 R,EP East 2 L),
                     (EP East 3 L,EP West 3 R),(EP East 3 R,EP East 5 L),
                     (EP East 4 L,EP South 6 R),(EP East 4 R,EP South 4 L),
                     (EP East 5 L,EP East 3 R),(EP East 5 R,EP South 5 L),
                     (EP East 6 L,EP West 6 R),(EP East 6 R,EP South 6 L),
                     (EP West 1 L,EP West 2 R),(EP West 1 R,EP East 1 L),
                     (EP West 2 L,EP South 5 R),(EP West 2 R,EP West 1 L),
                     (EP West 3 L,EP West 4 R),(EP West 3 R,EP East 3 L),
                     (EP West 4 L,EP South 3 R),(EP West 4 R,EP West 3 L),
                     (EP West 5 L,EP South 2 R),(EP West 5 R,EP South 2 L),
                     (EP West 6 L,EP South 1 R),(EP West 6 R,EP East 6 L),
                     (EP South 1 L,EP East 1 R),(EP South 1 R,EP West 6 L),
                     (EP South 2 L,EP West 5 R),(EP South 2 R,EP West 5 L),
                     (EP South 3 L,EP South 4 R),(EP South 3 R,EP West 4 L),
                     (EP South 4 L,EP East 4 R),(EP South 4 R,EP South 3 L),
                     (EP South 5 L,EP East 5 R),(EP South 5 R,EP West 2 L),
                     (EP South 6 L,EP East 6 R),(EP South 6 R,EP East 4 L)]

-- Add more test cases if needed
testEmptyAtoms = TestCase $ assertBool
    "calcInteractions with no atoms should return valid paths"
    (not $ null $ calcInteractions 3 [])




