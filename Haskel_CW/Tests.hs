{-
 Module      : COMP2209
  Copyright   : (c) 2025 University of Southampton
  Author      : Sze Long Cheung, Karso 
  Description : test cases for Q1 - Q6

    src1: https://putridparrot.com/blog/unit-testing-haskel-code-with-hunit/ (for HUnit tutorial)
    src2: https://math.stackexchange.com/questions/4427192/use-of-parentheses-in-lambda-calculus-when-%CE%BB-is-involved (for Q3 lambda parenthesis test case)
-}

module Main (main) where

-- Test import --
import Test.HUnit
import System.Exit
import Control.Exception (evaluate, ErrorCall(..), try)
-- Test import --

-- Modules import --
import Q1
import Q2
import qualified Q3
import qualified Q4 
import qualified Q5
import qualified Q6
-- Modules import -- 

main :: IO ()
main = do
    counts <- runTestTT (test [
        -- Q1
        testCalcInteractions_Size0_EmptyAtoms, -- calcInteractions with size 0 with no atoms see if it can handle empty case
        testCalcInteractions_Size30_EmptyAtoms, -- size 30 without atoms to test if the ray give the correct default output path
        testCalcInteractions_Size1_OneAtoms, -- calcInteractions with size 1 with 1 atoms
        testCalcInteractions_Size3_OneAtom, -- one atom in the middle for simple case testing of ray next position and directions
        testCalcInteractions_Size6_CentreAtoms, -- atoms blocked in the middle for advanced case testing of ray reflection and is most focus on East and West face
        testCalcInteractions_Size6_UpwardTriAtoms, -- calcInteractions with size 6 with atoms only on upward triangle
        testCalcInteractions_Size6_DownwardTriAtoms, -- calcInteractions with size 6 with atoms only on downward triangle
        testCalcInteractions_Size30_RandomAtoms, -- check how calcInteractions work with large size and bunch of random atoms to check the efficiency of my code

        -- Q2
        
        -- Q3
        testUnparse_BaseCase, -- test empty case
        testUnparse_CheckDigits, -- test digits (1~3 digits)
        testUnparse_CheckParen, -- test nested parenthesis
        testUnparse_CheckMacroImplement, -- test can Macro replace same definition
        testUnparse_CheckLowerCase, -- test the output when unparse facing lower case macro

        -- Q4
        testParseLamMacro_CheckMacros, -- parsing for the lambda calculus with multiple macros 
        testParseLamMacro_CheckParen, -- check nested parenthesis
        testParseLamMacro_EmptyInput, -- test empty input (should return Nothing)
        testParseLamMacro_InvalidGrammar, -- test invalid grammar (should return Nothing)
        testParseLamMacro_RepeatedMacro, -- test repeated Macros (should return Noting)
        testParseLamMacro_FreeVariables -- should return Nothing

        -- Q5
        

        -- Q6

        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
        
-- Q1 
{- 
    Since atoms must placed on valid positions (coursework spec), this part of checking can be ignored

    Techniques:
        - Empty input
        - Check ray default reflect by applying 0 atoms with size n, where n = any reasonable size
        - Check if the the ray cannot enter the triangle box by applying size 1 with atom at (1,1)
        - Check the reflection of ray by applying one atom in the middle of the triangle box (simple case)
        - Check atoms blocked in the middle of the triangle box for advanced case testing of ray reflection and is mostly focusing on East and West face
        - Check the reflection of upward triangle
        - Check the reflection of downward triangle
        - Check the efficiency of calcInteractions work with large size and bunch of random atoms
-}

----- Test case 1: calcInteractions with size 0 with no atoms
testCalcInteractions_Size0_EmptyAtoms = TestCase $ assertEqual
    "calcInteractions with size 0"
    expectedResult
    (calcInteractions 0 [])
  where
    expectedResult = []

----- Test case 2: calcInteractions with size 30 with no atoms
testCalcInteractions_Size30_EmptyAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 with atoms only on downward triangle"
    expectedResult
    (calcInteractions 30 [])
  where
    expectedResult = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP South 1 L),(EP East 2 L,EP West 2 R),
                      (EP East 2 R,EP South 2 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP South 3 L),
                      (EP East 4 L,EP West 4 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP West 5 R),
                      (EP East 5 R,EP South 5 L),(EP East 6 L,EP West 6 R),(EP East 6 R,EP South 6 L),
                      (EP East 7 L,EP West 7 R),(EP East 7 R,EP South 7 L),(EP East 8 L,EP West 8 R),
                      (EP East 8 R,EP South 8 L),(EP East 9 L,EP West 9 R),(EP East 9 R,EP South 9 L),
                      (EP East 10 L,EP West 10 R),(EP East 10 R,EP South 10 L),(EP East 11 L,EP West 11 R),
                      (EP East 11 R,EP South 11 L),(EP East 12 L,EP West 12 R),(EP East 12 R,EP South 12 L),
                      (EP East 13 L,EP West 13 R),(EP East 13 R,EP South 13 L),(EP East 14 L,EP West 14 R),
                      (EP East 14 R,EP South 14 L),(EP East 15 L,EP West 15 R),(EP East 15 R,EP South 15 L),
                      (EP East 16 L,EP West 16 R),(EP East 16 R,EP South 16 L),(EP East 17 L,EP West 17 R),
                      (EP East 17 R,EP South 17 L),(EP East 18 L,EP West 18 R),(EP East 18 R,EP South 18 L),
                      (EP East 19 L,EP West 19 R),(EP East 19 R,EP South 19 L),(EP East 20 L,EP West 20 R),
                      (EP East 20 R,EP South 20 L),(EP East 21 L,EP West 21 R),(EP East 21 R,EP South 21 L),
                      (EP East 22 L,EP West 22 R),(EP East 22 R,EP South 22 L),(EP East 23 L,EP West 23 R),
                      (EP East 23 R,EP South 23 L),(EP East 24 L,EP West 24 R),(EP East 24 R,EP South 24 L),
                      (EP East 25 L,EP West 25 R),(EP East 25 R,EP South 25 L),(EP East 26 L,EP West 26 R),
                      (EP East 26 R,EP South 26 L),(EP East 27 L,EP West 27 R),(EP East 27 R,EP South 27 L),
                      (EP East 28 L,EP West 28 R),(EP East 28 R,EP South 28 L),(EP East 29 L,EP West 29 R),
                      (EP East 29 R,EP South 29 L),(EP East 30 L,EP West 30 R),(EP East 30 R,EP South 30 L),
                      (EP West 1 L,EP South 30 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP South 29 R),
                      (EP West 2 R,EP East 2 L),(EP West 3 L,EP South 28 R),(EP West 3 R,EP East 3 L),
                      (EP West 4 L,EP South 27 R),(EP West 4 R,EP East 4 L),(EP West 5 L,EP South 26 R),
                      (EP West 5 R,EP East 5 L),(EP West 6 L,EP South 25 R),(EP West 6 R,EP East 6 L),
                      (EP West 7 L,EP South 24 R),(EP West 7 R,EP East 7 L),(EP West 8 L,EP South 23 R),
                      (EP West 8 R,EP East 8 L),(EP West 9 L,EP South 22 R),(EP West 9 R,EP East 9 L),
                      (EP West 10 L,EP South 21 R),(EP West 10 R,EP East 10 L),(EP West 11 L,EP South 20 R),
                      (EP West 11 R,EP East 11 L),(EP West 12 L,EP South 19 R),(EP West 12 R,EP East 12 L),
                      (EP West 13 L,EP South 18 R),(EP West 13 R,EP East 13 L),(EP West 14 L,EP South 17 R),
                      (EP West 14 R,EP East 14 L),(EP West 15 L,EP South 16 R),(EP West 15 R,EP East 15 L),
                      (EP West 16 L,EP South 15 R),(EP West 16 R,EP East 16 L),(EP West 17 L,EP South 14 R),
                      (EP West 17 R,EP East 17 L),(EP West 18 L,EP South 13 R),(EP West 18 R,EP East 18 L),
                      (EP West 19 L,EP South 12 R),(EP West 19 R,EP East 19 L),(EP West 20 L,EP South 11 R),
                      (EP West 20 R,EP East 20 L),(EP West 21 L,EP South 10 R),(EP West 21 R,EP East 21 L),
                      (EP West 22 L,EP South 9 R),(EP West 22 R,EP East 22 L),(EP West 23 L,EP South 8 R),
                      (EP West 23 R,EP East 23 L),(EP West 24 L,EP South 7 R),(EP West 24 R,EP East 24 L),
                      (EP West 25 L,EP South 6 R),(EP West 25 R,EP East 25 L),(EP West 26 L,EP South 5 R),
                      (EP West 26 R,EP East 26 L),(EP West 27 L,EP South 4 R),(EP West 27 R,EP East 27 L),
                      (EP West 28 L,EP South 3 R),(EP West 28 R,EP East 28 L),(EP West 29 L,EP South 2 R),
                      (EP West 29 R,EP East 29 L),(EP West 30 L,EP South 1 R),(EP West 30 R,EP East 30 L),
                      (EP South 1 L,EP East 1 R),(EP South 1 R,EP West 30 L),(EP South 2 L,EP East 2 R),
                      (EP South 2 R,EP West 29 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP West 28 L),
                      (EP South 4 L,EP East 4 R),(EP South 4 R,EP West 27 L),(EP South 5 L,EP East 5 R),
                      (EP South 5 R,EP West 26 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP West 25 L),
                      (EP South 7 L,EP East 7 R),(EP South 7 R,EP West 24 L),(EP South 8 L,EP East 8 R),
                      (EP South 8 R,EP West 23 L),(EP South 9 L,EP East 9 R),(EP South 9 R,EP West 22 L),
                      (EP South 10 L,EP East 10 R),(EP South 10 R,EP West 21 L),(EP South 11 L,EP East 11 R),
                      (EP South 11 R,EP West 20 L),(EP South 12 L,EP East 12 R),(EP South 12 R,EP West 19 L),
                      (EP South 13 L,EP East 13 R),(EP South 13 R,EP West 18 L),(EP South 14 L,EP East 14 R),
                      (EP South 14 R,EP West 17 L),(EP South 15 L,EP East 15 R),(EP South 15 R,EP West 16 L),
                      (EP South 16 L,EP East 16 R),(EP South 16 R,EP West 15 L),(EP South 17 L,EP East 17 R),
                      (EP South 17 R,EP West 14 L),(EP South 18 L,EP East 18 R),(EP South 18 R,EP West 13 L),
                      (EP South 19 L,EP East 19 R),(EP South 19 R,EP West 12 L),(EP South 20 L,EP East 20 R),
                      (EP South 20 R,EP West 11 L),(EP South 21 L,EP East 21 R),(EP South 21 R,EP West 10 L),
                      (EP South 22 L,EP East 22 R),(EP South 22 R,EP West 9 L),(EP South 23 L,EP East 23 R),
                      (EP South 23 R,EP West 8 L),(EP South 24 L,EP East 24 R),(EP South 24 R,EP West 7 L),
                      (EP South 25 L,EP East 25 R),(EP South 25 R,EP West 6 L),(EP South 26 L,EP East 26 R),
                      (EP South 26 R,EP West 5 L),(EP South 27 L,EP East 27 R),(EP South 27 R,EP West 4 L),
                      (EP South 28 L,EP East 28 R),(EP South 28 R,EP West 3 L),(EP South 29 L,EP East 29 R),
                      (EP South 29 R,EP West 2 L),(EP South 30 L,EP East 30 R),(EP South 30 R,EP West 1 L)]

----- Test case 3: calcInteractions with size 1 with 1 atoms
testCalcInteractions_Size1_OneAtoms = TestCase $ assertEqual
    "calcInteractions with size 1 and 1 atom"
    expectedResult
    (calcInteractions 1 [(1,1)])
  where
    expectedResult = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP West 1 L,EP South 1 R),
                      (EP West 1 R,EP West 1 L),(EP South 1 L,EP South 1 R),(EP South 1 R,EP West 1 L)]

----- Test case 4: calcInteractions with size 6 with atoms blocked in the middle
testCalcInteractions_Size3_OneAtom = TestCase $ assertEqual
    "calcInteractions with size 3 and one atom in the middle"
    expectedResult
    (calcInteractions 3 [(2,2)])
  where
    expectedResult = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP West 1 L),(EP East 2 L,EP South 3 R),
                       (EP East 2 R,EP South 2 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP South 3 L),
                       (EP West 1 L,EP East 1 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP South 2 R),
                       (EP West 2 R,EP South 1 L),(EP West 3 L,EP South 1 R),(EP West 3 R,EP East 3 L),
                       (EP South 1 L,EP West 2 R),(EP South 1 R,EP West 3 L),(EP South 2 L,EP East 2 R),
                       (EP South 2 R,EP West 2 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP East 2 L)]

----- Test case 5: calcInteractions with size 6 with atoms blocked in the middle
testCalcInteractions_Size6_CentreAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 and atoms blocked in the middle"
    expectedResult
    (calcInteractions 6 [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6)])
  where
    expectedResult = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP South 6 R),
                      (EP East 2 R,EP East 3 L),(EP East 3 L,EP East 2 R),(EP East 3 R,EP East 5 L),
                      (EP East 4 L,EP South 5 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP East 3 R),
                      (EP East 5 R,EP South 5 L),(EP East 6 L,EP South 4 R),(EP East 6 R,EP South 6 L),
                      (EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP West 3 R),
                      (EP West 2 R,EP South 1 L),(EP West 3 L,EP West 5 R),(EP West 3 R,EP West 2 L),
                      (EP West 4 L,EP South 3 R),(EP West 4 R,EP South 2 L),(EP West 5 L,EP South 2 R),
                      (EP West 5 R,EP West 3 L),(EP West 6 L,EP South 1 R),(EP West 6 R,EP South 3 L),
                      (EP South 1 L,EP West 2 R),(EP South 1 R,EP West 6 L),(EP South 2 L,EP West 4 R),
                      (EP South 2 R,EP West 5 L),(EP South 3 L,EP West 6 R),(EP South 3 R,EP West 4 L),
                      (EP South 4 L,EP East 4 R),(EP South 4 R,EP East 6 L),(EP South 5 L,EP East 5 R),
                      (EP South 5 R,EP East 4 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP East 2 L)]

----- Test case 6: calcInteractions with size 6 with atoms only on upward triangle
testCalcInteractions_Size6_UpwardTriAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 with atoms only on upward triangle"
    expectedResult
    (calcInteractions 6 [(1,1),(3,1),(3,3),(3,5),(4,3),(4,5),(4,7),(5,3),(5,9),(6,1),(6,5),(6,9),(6,11)])
  where
    expectedResult = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP West 2 R),
                      (EP East 2 R,EP West 2 L),(EP East 3 L,EP East 3 R),(EP East 3 R,EP East 3 L),
                      (EP East 4 L,EP East 4 R),(EP East 4 R,EP East 4 L),(EP East 5 L,EP East 5 R),
                      (EP East 5 R,EP East 5 L),(EP East 6 L,EP East 6 R),(EP East 6 R,EP East 6 L),
                      (EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP East 2 R),
                      (EP West 2 R,EP East 2 L),(EP West 3 L,EP West 3 R),(EP West 3 R,EP West 3 L),
                      (EP West 4 L,EP West 5 R),(EP West 4 R,EP South 2 L),(EP West 5 L,EP South 2 R),
                      (EP West 5 R,EP West 4 L),(EP West 6 L,EP South 1 R),(EP West 6 R,EP West 6 L),
                      (EP South 1 L,EP South 1 R),(EP South 1 R,EP West 6 L),(EP South 2 L,EP West 4 R),
                      (EP South 2 R,EP West 5 L),(EP South 3 L,EP South 3 R),(EP South 3 R,EP South 3 L),
                      (EP South 4 L,EP South 4 R),(EP South 4 R,EP South 4 L),(EP South 5 L,EP South 5 R),
                      (EP South 5 R,EP South 5 L),(EP South 6 L,EP South 6 R),(EP South 6 R,EP South 6 L)]

                       

----- Test case 7: calcInteractions with size 6 with atoms only on downward triangle
testCalcInteractions_Size6_DownwardTriAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 with atoms only on downward triangle"
    expectedResult
    (calcInteractions 6 [(2,2),(3,2),(3,4),(4,4),(4,6),(6,2),(6,6),(6,8)])
  where
    expectedResult = [(EP East 1 L,EP West 1 R),(EP East 1 R,EP West 1 L),(EP East 2 L,EP East 2 R),
                      (EP East 2 R,EP East 2 L),(EP East 3 L,EP East 3 R),(EP East 3 R,EP East 3 L),
                      (EP East 4 L,EP South 6 R),(EP East 4 R,EP West 3 L),(EP East 5 L,EP West 5 R),
                      (EP East 5 R,EP South 5 L),(EP East 6 L,EP South 5 R),(EP East 6 R,EP South 6 L),
                      (EP West 1 L,EP East 1 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP West 2 R),
                      (EP West 2 R,EP West 2 L),(EP West 3 L,EP East 4 R),(EP West 3 R,EP West 5 L),
                      (EP West 4 L,EP South 3 R),(EP West 4 R,EP South 2 L),(EP West 5 L,EP West 3 R),
                      (EP West 5 R,EP East 5 L),(EP West 6 L,EP South 1 R),(EP West 6 R,EP South 1 L),
                      (EP South 1 L,EP West 6 R),(EP South 1 R,EP West 6 L),(EP South 2 L,EP West 4 R),
                      (EP South 2 R,EP South 3 L),(EP South 3 L,EP South 2 R),(EP South 3 R,EP West 4 L),
                      (EP South 4 L,EP South 4 R),(EP South 4 R,EP South 4 L),(EP South 5 L,EP East 5 R),
                      (EP South 5 R,EP East 6 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP East 4 L)]

----- Test case 8: calcInteractions with size 30 with random atoms to test efficiency
testCalcInteractions_Size30_RandomAtoms = TestCase $ assertEqual
    "calcInteractions with size 30 with random atoms to test efficiency"
    expectedResult
    (calcInteractions 30 [(4,5), (8,9), (12,15), (15,20), (17,25), (19,30), (21,35), (23,40), (25,45), (27,50),
                          (3,3), (7,7), (11,11), (14,17), (16,23), (18,29), (20,35), (22,41), (24,47), (26,51),
                          (2,1), (6,5), (10,13), (13,19), (15,25), (17,31), (19,36), (21,41), (23,45), (25,49),
                          (1,1), (5,3), (9,11), (12,17), (14,23), (16,29), (18,35), (20,39), (22,43), (24,47),
                          (3,1), (7,5), (11,13), (13,21), (15,27), (17,33), (19,39), (21,43), (23,40), (25,25),
                          (4,3), (8,7), (12,15), (14,23), (16,27), (18,31), (20,27), (22,41), (24,30), (26,26)])
  where
    expectedResult = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),
                     (EP East 2 L,EP East 16 R),(EP East 2 R,EP East 3 L),
                     (EP East 3 L,EP East 2 R),(EP East 3 R,EP East 4 L),
                     (EP East 4 L,EP East 3 R),(EP East 4 R,EP East 7 L),
                     (EP East 5 L,EP East 10 R),(EP East 5 R,EP East 8 L),
                     (EP East 6 L,EP East 13 R),(EP East 6 R,EP East 9 L),
                     (EP East 7 L,EP East 4 R),(EP East 7 R,EP East 10 L),
                     (EP East 8 L,EP East 5 R),(EP East 8 R,EP East 11 L),
                     (EP East 9 L,EP East 6 R),(EP East 9 R,EP East 12 L),
                     (EP East 10 L,EP East 7 R),(EP East 10 R,EP East 5 L),
                     (EP East 11 L,EP East 8 R),(EP East 11 R,EP East 13 L),
                     (EP East 12 L,EP East 9 R),(EP East 12 R,EP East 14 L),
                     (EP East 13 L,EP East 11 R),(EP East 13 R,EP East 6 L),
                     (EP East 14 L,EP East 12 R),(EP East 14 R,EP East 15 L),
                     (EP East 15 L,EP East 14 R),(EP East 15 R,EP East 16 L),
                     (EP East 16 L,EP East 15 R),(EP East 16 R,EP East 2 L),
                     (EP East 17 L,EP East 17 R),(EP East 17 R,EP East 17 L),
                     (EP East 18 L,EP East 18 R),(EP East 18 R,EP East 18 L),
                     (EP East 19 L,EP West 21 R),(EP East 19 R,EP South 19 L),
                     (EP East 20 L,EP East 20 R),(EP East 20 R,EP East 20 L),
                     (EP East 21 L,EP East 21 R),(EP East 21 R,EP East 21 L),
                     (EP East 22 L,EP East 22 R),(EP East 22 R,EP East 22 L),
                     (EP East 23 L,EP East 23 R),(EP East 23 R,EP East 23 L),
                     (EP East 24 L,EP East 24 R),(EP East 24 R,EP East 24 L),
                     (EP East 25 L,EP East 25 R),(EP East 25 R,EP East 25 L),
                     (EP East 26 L,EP East 26 R),(EP East 26 R,EP East 26 L),
                     (EP East 27 L,EP South 29 R),(EP East 27 R,EP South 27 L),
                     (EP East 28 L,EP West 28 R),(EP East 28 R,EP South 28 L),
                     (EP East 29 L,EP West 29 R),(EP East 29 R,EP South 29 L),
                     (EP East 30 L,EP West 30 R),(EP East 30 R,EP South 30 L),
                     (EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),
                     (EP West 2 L,EP West 2 R),(EP West 2 R,EP West 2 L),
                     (EP West 3 L,EP West 3 R),(EP West 3 R,EP West 3 L),
                     (EP West 4 L,EP West 5 R),(EP West 4 R,EP South 1 L),
                     (EP West 5 L,EP West 7 R),(EP West 5 R,EP West 4 L),
                     (EP West 6 L,EP West 11 R),(EP West 6 R,EP South 2 L),
                     (EP West 7 L,EP West 20 R),(EP West 7 R,EP West 5 L),
                     (EP West 8 L,EP South 23 R),(EP West 8 R,EP South 3 L),
                     (EP West 9 L,EP West 19 R),(EP West 9 R,EP South 5 L),
                     (EP West 10 L,EP South 21 R),(EP West 10 R,EP South 4 L),
                     (EP West 11 L,EP South 20 R),(EP West 11 R,EP West 6 L),
                     (EP West 12 L,EP South 19 R),(EP West 12 R,EP South 7 L),
                     (EP West 13 L,EP West 25 R),(EP West 13 R,EP South 11 L),
                     (EP West 14 L,EP South 17 R),(EP West 14 R,EP South 6 L),
                     (EP West 15 L,EP South 16 R),(EP West 15 R,EP South 10 L),
                     (EP West 16 L,EP South 15 R),(EP West 16 R,EP South 16 L),
                     (EP West 17 L,EP South 14 R),(EP West 17 R,EP South 12 L),
                     (EP West 18 L,EP South 13 R),(EP West 18 R,EP South 8 L),
                     (EP West 19 L,EP South 12 R),(EP West 19 R,EP West 9 L),
                     (EP West 20 L,EP South 11 R),(EP West 20 R,EP West 7 L),
                     (EP West 21 L,EP South 10 R),(EP West 21 R,EP East 19 L),
                     (EP West 22 L,EP South 9 R),(EP West 22 R,EP South 17 L),
                     (EP West 23 L,EP South 8 R),(EP West 23 R,EP South 20 L),
                     (EP West 24 L,EP South 7 R),(EP West 24 R,EP South 15 L),
                     (EP West 25 L,EP South 6 R),(EP West 25 R,EP West 13 L),
                     (EP West 26 L,EP South 5 R),(EP West 26 R,EP South 13 L),
                     (EP West 27 L,EP South 4 R),(EP West 27 R,EP South 25 L),
                     (EP West 28 L,EP South 3 R),(EP West 28 R,EP East 28 L),
                     (EP West 29 L,EP South 2 R),(EP West 29 R,EP East 29 L),
                     (EP West 30 L,EP South 1 R),(EP West 30 R,EP East 30 L),
                     (EP South 1 L,EP West 4 R),(EP South 1 R,EP West 30 L),
                     (EP South 2 L,EP West 6 R),(EP South 2 R,EP West 29 L),
                     (EP South 3 L,EP West 8 R),(EP South 3 R,EP West 28 L),
                     (EP South 4 L,EP West 10 R),(EP South 4 R,EP West 27 L),
                     (EP South 5 L,EP West 9 R),(EP South 5 R,EP West 26 L),
                     (EP South 6 L,EP West 14 R),(EP South 6 R,EP West 25 L),
                     (EP South 7 L,EP West 12 R),(EP South 7 R,EP West 24 L),
                     (EP South 8 L,EP West 18 R),(EP South 8 R,EP West 23 L),
                     (EP South 9 L,EP South 25 R),(EP South 9 R,EP West 22 L),
                     (EP South 10 L,EP West 15 R),(EP South 10 R,EP West 21 L),
                     (EP South 11 L,EP West 13 R),(EP South 11 R,EP West 20 L),
                     (EP South 12 L,EP West 17 R),(EP South 12 R,EP West 19 L),
                     (EP South 13 L,EP West 26 R),(EP South 13 R,EP West 18 L),
                     (EP South 14 L,EP South 24 R),(EP South 14 R,EP West 17 L),
                     (EP South 15 L,EP West 24 R),(EP South 15 R,EP West 16 L),
                     (EP South 16 L,EP West 16 R),(EP South 16 R,EP West 15 L),
                     (EP South 17 L,EP West 22 R),(EP South 17 R,EP West 14 L),
                     (EP South 18 L,EP South 27 R),(EP South 18 R,EP South 21 L),
                     (EP South 19 L,EP East 19 R),(EP South 19 R,EP West 12 L),
                     (EP South 20 L,EP West 23 R),(EP South 20 R,EP West 11 L),
                     (EP South 21 L,EP South 18 R),(EP South 21 R,EP West 10 L),
                     (EP South 22 L,EP South 26 R),(EP South 22 R,EP South 24 L),
                     (EP South 23 L,EP South 28 R),(EP South 23 R,EP West 8 L),
                     (EP South 24 L,EP South 22 R),(EP South 24 R,EP South 14 L),
                     (EP South 25 L,EP West 27 R),(EP South 25 R,EP South 9 L),
                     (EP South 26 L,EP South 30 R),(EP South 26 R,EP South 22 L),
                     (EP South 27 L,EP East 27 R),(EP South 27 R,EP South 18 L),
                     (EP South 28 L,EP East 28 R),(EP South 28 R,EP South 23 L),
                     (EP South 29 L,EP East 29 R),(EP South 29 R,EP East 27 L),
                     (EP South 30 L,EP East 30 R),(EP South 30 R,EP South 26 L)]



-- Q2


-- Q3
{- 
    Techniques:
        - Base case input
        - Check integers(for example Var 0, Var 456 ... should be valid)
            - Use 1, 2, and 3 digits test cases respectively
        - Parenthesis checking (check if it is most simplified)
            - cases are given from easy to complicated
        - Multiple Macros 
        - Macro implementation (for example unparse (LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))) given in the coursework)
        - Invalid Macro (lower case checking)
-}

----- Test case 1:
testUnparse_BaseCase = TestCase $ assertEqual
    "check base case"
    expectedResult
    (Q3.unparse (Q3.LamDef [] (Q3.LamVar 0)))
  where
    expectedResult = "x0"

----- Test case 2:
testUnparse_CheckDigits = TestCase $ assertEqual
    "check digits"
    expectedResult
    (Q3.unparse (Q3.LamDef [] (Q3.LamAbs 1 (Q3.LamApp (Q3.LamVar 10) (Q3.LamVar 456)))))
  where
    expectedResult = "\955x1\8594x10x456"

----- Test case 3:
testUnparse_CheckParen = TestCase $ assertEqual
    "check parenthesis"
    expectedResult
    (Q3.unparse (Q3.LamDef [] (Q3.LamApp (Q3.LamApp (Q3.LamAbs 1 (Q3.LamVar 1)) (Q3.LamAbs 2 (Q3.LamAbs 3 (Q3.LamApp (Q3.LamVar 3) (Q3.LamVar 1))))) (Q3.LamVar 4)))) --src2
  where
    expectedResult = "(\955x1\8594x1)\955x2\8594\955x3\8594x3x1x4"

----- Test case 4:
testUnparse_CheckMacroImplement = TestCase $ assertEqual
    "check macro implementation"
    expectedResult
    (Q3.unparse (Q3.LamDef [ ("F", Q3.LamAbs 1 (Q3.LamVar 1)), ("G", Q3.LamAbs 1 (Q3.LamVar 1)), ("H", Q3.LamApp (Q3.LamMacro "F") (Q3.LamMacro "G")) ] (Q3.LamApp (Q3.LamMacro "H") (Q3.LamVar 2))))
  where
    expectedResult = "defF=\955x1\8594x1inG=\955x1\8594x1inH=FGinHx2"

----- Test case 5:
testUnparse_CheckLowerCase = TestCase $ do
    result <- try (evaluate (Q3.unparse (Q3.LamDef [] (Q3.LamMacro "f"))))
    case result of
        Left (ErrorCall msg) -> assertEqual 
            "check lower case"
            "Macros Should Be in UpperCase"
            msg
        Right _ -> assertFailure "Expected an exception but got a value, didn't handle lower case carefully"


-- Q4
{- 
    
    Techniques:
        - Empty input
        - Parsing for the lambda calculus with one macros definition
        - Check nested parenthesis
        - Invalid input
            - Grammar
            - Repeated Macro
            - Free variables
        
-}

----- Test case 1:
testParseLamMacro_EmptyInput = TestCase $ assertEqual
    "check empty input"
    expectedResult
    (Q4.parseLamMacro " ")
  where
    expectedResult = Nothing

----- Test case 2:
testParseLamMacro_CheckMacros = TestCase $ assertEqual
    "check multiple macros"
    expectedResult
    (Q4.parseLamMacro "def F = λx1 → λx2 → x1 x2 in def G = λx3 → F x3 x3 in G")
  where
    expectedResult = Just (Q4.LamDef [("F",Q4.LamAbs 1 (Q4.LamAbs 2 (Q4.LamApp (Q4.LamVar 1) (Q4.LamVar 2)))),("G",Q4.LamAbs 3 (Q4.LamApp (Q4.LamApp (Q4.LamMacro "F") (Q4.LamVar 3)) (Q4.LamVar 3)))] (Q4.LamMacro "G"))

----- Test case 3:
testParseLamMacro_CheckParen = TestCase $ assertEqual
    "check nested parenthesis"
    expectedResult
    (Q4.parseLamMacro "def F = λx1 → λx2 → x1 (x2 x1) in λx3 → λx4 → λx5 → (λx6 → x3 (x4 (x5 (x6 F))))")
  where
    expectedResult = Just (Q4.LamDef [("F",Q4.LamAbs 1 (Q4.LamAbs 2 (Q4.LamApp (Q4.LamVar 1) (Q4.LamApp (Q4.LamVar 2) (Q4.LamVar 1)))))] (Q4.LamAbs 3 (Q4.LamAbs 4 (Q4.LamAbs 5 (Q4.LamAbs 6 (Q4.LamApp (Q4.LamVar 3) (Q4.LamApp (Q4.LamVar 4) (Q4.LamApp (Q4.LamVar 5) (Q4.LamApp (Q4.LamVar 6) (Q4.LamMacro "F"))))))))))

----- Test case 4:
testParseLamMacro_InvalidGrammar = TestCase $ assertEqual
    "check invalid grammar"
    expectedResult
    (Q4.parseLamMacro "def F = λx1 → (def G = x1 in λx2 → x2) (def H = λx3 → x3 in def G = x2 in x1)")
  where
    expectedResult = Nothing

----- Test case 5:
testParseLamMacro_RepeatedMacro = TestCase $ assertEqual
    "check repeated macros"
    expectedResult
    (Q4.parseLamMacro "def A = λx1 → x1 in def B = λx2 → A x2 in def C = λx3 → B x3 in def B = λx4 → x4 in C")
  where
    expectedResult = Nothing

----- Test case 6:
testParseLamMacro_FreeVariables = TestCase $ assertEqual
    "check free variables"
    expectedResult
    (Q4.parseLamMacro "def F = λx1 → λx2 → x1 x2 in def G = λx3 → F x3 x4 in G")
  where
    expectedResult = Nothing

-- Q5
{-
    Techniques:
        - Check if the code obey these rules:
            - Rule: [[ x ]] = λ κ → (κ x)
            - [[ X ]] = X for macros
            - [[ λx → E ]] = λκ → (κ λx → [[ E ]])
            - [[ (E1 E2) ]] = λκ → ( [[ E1 ]]  λf → ( [[ E2 ]]  λe → (f e κ) ) )
-}

-- Q6
{-
    Techniques:
-}

