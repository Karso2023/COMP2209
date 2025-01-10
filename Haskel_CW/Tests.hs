{-
  Module      : COMP2209
  Copyright   : (c) 2025 University of Southampton
  Author      : Sze Long Cheung, Karso 
  Description : test cases for Q1 - Q6, approach for each question have been described separately below

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
        testCalcInteractions_Size6_CentreAtoms, -- atoms blocked in the middle for advanced case testing of ray reflection and is most focus on Q1.East and Q1.West face
        testCalcInteractions_Size6_UpwardTriAtoms, -- calcInteractions with size 6 with atoms only on upward triangle
        testCalcInteractions_Size6_DownwardTriAtoms, -- calcInteractions with size 6 with atoms only on downward triangle
        testCalcInteractions_Size30_RandomAtoms, -- check how calcInteractions work with large size and bunch of random atoms to check the efficiency of my code

        -- Q2
        testSolveTBB_case1,
        
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
        testParseLamMacro_FreeVariables, -- should return Nothing

        -- Q5
        testCpsTransform_Rule_1, -- [[ x ]] = λ κ →  (κ x)
        testCpsTransform_Rule_2, -- [[ λx → E ]] = λκ → (κ λx → [[ E ]])
        testCpsTransform_Rule_3, -- [[ (E1 E2) ]] = λκ → ( [[ E1 ]]  λf → ( [[ E2 ]]  λe → (f e κ) ) )
        testCpsTransform_Rule_4, -- [[ def X = E in ME ]]  = def X = [[ E ]]  in  [[ ME ]]
        testCpsTransform_Rule_5, -- [[ X ]] = X
        testCpsTransform_Rules_Combined, -- Rules combined together (the result is quite long)

        -- Q6
        testCompareInnerOuter_1,
        testCompareInnerOuter_2,
        testCompareInnerOuter_3,
        testCompareInnerOuter_4,
        testCompareInnerOuter_5,
        testCompareInnerOuter_6,
        testCompareInnerOuter_7
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
        - Check atoms blocked in the middle of the triangle box for advanced case testing of ray reflection and is mostly focusing on Q1.East and Q1.West face
        - Check the reflection of upward triangle
        - Check the reflection of downward triangle
        - Check the efficiency of calcInteractions work with large size and bunch of random atoms
-}

----- Test case 1: calcInteractions with size 0 with no atoms
testCalcInteractions_Size0_EmptyAtoms = TestCase $ assertEqual
    "calcInteractions with size 0"
    expectedResult
    (Q1.calcInteractions 0 [])
  where
    expectedResult = []

----- Test case 2: calcInteractions with size 30 with no atoms
testCalcInteractions_Size30_EmptyAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 with atoms only on downward triangle"
    expectedResult
    (Q1.calcInteractions 30 [])
  where
    expectedResult = [(Q1.EP Q1.East 1 Q1.L,Q1.EP Q1.West 1 Q1.R),(Q1.EP Q1.East 1 Q1.R,Q1.EP Q1.South 1 Q1.L),(Q1.EP Q1.East 2 Q1.L,Q1.EP Q1.West 2 Q1.R),
                      (Q1.EP Q1.East 2 Q1.R,Q1.EP Q1.South 2 Q1.L),(Q1.EP Q1.East 3 Q1.L,Q1.EP Q1.West 3 Q1.R),(Q1.EP Q1.East 3 Q1.R,Q1.EP Q1.South 3 Q1.L),
                      (Q1.EP Q1.East 4 Q1.L,Q1.EP Q1.West 4 Q1.R),(Q1.EP Q1.East 4 Q1.R,Q1.EP Q1.South 4 Q1.L),(Q1.EP Q1.East 5 Q1.L,Q1.EP Q1.West 5 Q1.R),
                      (Q1.EP Q1.East 5 Q1.R,Q1.EP Q1.South 5 Q1.L),(Q1.EP Q1.East 6 Q1.L,Q1.EP Q1.West 6 Q1.R),(Q1.EP Q1.East 6 Q1.R,Q1.EP Q1.South 6 Q1.L),
                      (Q1.EP Q1.East 7 Q1.L,Q1.EP Q1.West 7 Q1.R),(Q1.EP Q1.East 7 Q1.R,Q1.EP Q1.South 7 Q1.L),(Q1.EP Q1.East 8 Q1.L,Q1.EP Q1.West 8 Q1.R),
                      (Q1.EP Q1.East 8 Q1.R,Q1.EP Q1.South 8 Q1.L),(Q1.EP Q1.East 9 Q1.L,Q1.EP Q1.West 9 Q1.R),(Q1.EP Q1.East 9 Q1.R,Q1.EP Q1.South 9 Q1.L),
                      (Q1.EP Q1.East 10 Q1.L,Q1.EP Q1.West 10 Q1.R),(Q1.EP Q1.East 10 Q1.R,Q1.EP Q1.South 10 Q1.L),(Q1.EP Q1.East 11 Q1.L,Q1.EP Q1.West 11 Q1.R),
                      (Q1.EP Q1.East 11 Q1.R,Q1.EP Q1.South 11 Q1.L),(Q1.EP Q1.East 12 Q1.L,Q1.EP Q1.West 12 Q1.R),(Q1.EP Q1.East 12 Q1.R,Q1.EP Q1.South 12 Q1.L),
                      (Q1.EP Q1.East 13 Q1.L,Q1.EP Q1.West 13 Q1.R),(Q1.EP Q1.East 13 Q1.R,Q1.EP Q1.South 13 Q1.L),(Q1.EP Q1.East 14 Q1.L,Q1.EP Q1.West 14 Q1.R),
                      (Q1.EP Q1.East 14 Q1.R,Q1.EP Q1.South 14 Q1.L),(Q1.EP Q1.East 15 Q1.L,Q1.EP Q1.West 15 Q1.R),(Q1.EP Q1.East 15 Q1.R,Q1.EP Q1.South 15 Q1.L),
                      (Q1.EP Q1.East 16 Q1.L,Q1.EP Q1.West 16 Q1.R),(Q1.EP Q1.East 16 Q1.R,Q1.EP Q1.South 16 Q1.L),(Q1.EP Q1.East 17 Q1.L,Q1.EP Q1.West 17 Q1.R),
                      (Q1.EP Q1.East 17 Q1.R,Q1.EP Q1.South 17 Q1.L),(Q1.EP Q1.East 18 Q1.L,Q1.EP Q1.West 18 Q1.R),(Q1.EP Q1.East 18 Q1.R,Q1.EP Q1.South 18 Q1.L),
                      (Q1.EP Q1.East 19 Q1.L,Q1.EP Q1.West 19 Q1.R),(Q1.EP Q1.East 19 Q1.R,Q1.EP Q1.South 19 Q1.L),(Q1.EP Q1.East 20 Q1.L,Q1.EP Q1.West 20 Q1.R),
                      (Q1.EP Q1.East 20 Q1.R,Q1.EP Q1.South 20 Q1.L),(Q1.EP Q1.East 21 Q1.L,Q1.EP Q1.West 21 Q1.R),(Q1.EP Q1.East 21 Q1.R,Q1.EP Q1.South 21 Q1.L),
                      (Q1.EP Q1.East 22 Q1.L,Q1.EP Q1.West 22 Q1.R),(Q1.EP Q1.East 22 Q1.R,Q1.EP Q1.South 22 Q1.L),(Q1.EP Q1.East 23 Q1.L,Q1.EP Q1.West 23 Q1.R),
                      (Q1.EP Q1.East 23 Q1.R,Q1.EP Q1.South 23 Q1.L),(Q1.EP Q1.East 24 Q1.L,Q1.EP Q1.West 24 Q1.R),(Q1.EP Q1.East 24 Q1.R,Q1.EP Q1.South 24 Q1.L),
                      (Q1.EP Q1.East 25 Q1.L,Q1.EP Q1.West 25 Q1.R),(Q1.EP Q1.East 25 Q1.R,Q1.EP Q1.South 25 Q1.L),(Q1.EP Q1.East 26 Q1.L,Q1.EP Q1.West 26 Q1.R),
                      (Q1.EP Q1.East 26 Q1.R,Q1.EP Q1.South 26 Q1.L),(Q1.EP Q1.East 27 Q1.L,Q1.EP Q1.West 27 Q1.R),(Q1.EP Q1.East 27 Q1.R,Q1.EP Q1.South 27 Q1.L),
                      (Q1.EP Q1.East 28 Q1.L,Q1.EP Q1.West 28 Q1.R),(Q1.EP Q1.East 28 Q1.R,Q1.EP Q1.South 28 Q1.L),(Q1.EP Q1.East 29 Q1.L,Q1.EP Q1.West 29 Q1.R),
                      (Q1.EP Q1.East 29 Q1.R,Q1.EP Q1.South 29 Q1.L),(Q1.EP Q1.East 30 Q1.L,Q1.EP Q1.West 30 Q1.R),(Q1.EP Q1.East 30 Q1.R,Q1.EP Q1.South 30 Q1.L),
                      (Q1.EP Q1.West 1 Q1.L,Q1.EP Q1.South 30 Q1.R),(Q1.EP Q1.West 1 Q1.R,Q1.EP Q1.East 1 Q1.L),(Q1.EP Q1.West 2 Q1.L,Q1.EP Q1.South 29 Q1.R),
                      (Q1.EP Q1.West 2 Q1.R,Q1.EP Q1.East 2 Q1.L),(Q1.EP Q1.West 3 Q1.L,Q1.EP Q1.South 28 Q1.R),(Q1.EP Q1.West 3 Q1.R,Q1.EP Q1.East 3 Q1.L),
                      (Q1.EP Q1.West 4 Q1.L,Q1.EP Q1.South 27 Q1.R),(Q1.EP Q1.West 4 Q1.R,Q1.EP Q1.East 4 Q1.L),(Q1.EP Q1.West 5 Q1.L,Q1.EP Q1.South 26 Q1.R),
                      (Q1.EP Q1.West 5 Q1.R,Q1.EP Q1.East 5 Q1.L),(Q1.EP Q1.West 6 Q1.L,Q1.EP Q1.South 25 Q1.R),(Q1.EP Q1.West 6 Q1.R,Q1.EP Q1.East 6 Q1.L),
                      (Q1.EP Q1.West 7 Q1.L,Q1.EP Q1.South 24 Q1.R),(Q1.EP Q1.West 7 Q1.R,Q1.EP Q1.East 7 Q1.L),(Q1.EP Q1.West 8 Q1.L,Q1.EP Q1.South 23 Q1.R),
                      (Q1.EP Q1.West 8 Q1.R,Q1.EP Q1.East 8 Q1.L),(Q1.EP Q1.West 9 Q1.L,Q1.EP Q1.South 22 Q1.R),(Q1.EP Q1.West 9 Q1.R,Q1.EP Q1.East 9 Q1.L),
                      (Q1.EP Q1.West 10 Q1.L,Q1.EP Q1.South 21 Q1.R),(Q1.EP Q1.West 10 Q1.R,Q1.EP Q1.East 10 Q1.L),(Q1.EP Q1.West 11 Q1.L,Q1.EP Q1.South 20 Q1.R),
                      (Q1.EP Q1.West 11 Q1.R,Q1.EP Q1.East 11 Q1.L),(Q1.EP Q1.West 12 Q1.L,Q1.EP Q1.South 19 Q1.R),(Q1.EP Q1.West 12 Q1.R,Q1.EP Q1.East 12 Q1.L),
                      (Q1.EP Q1.West 13 Q1.L,Q1.EP Q1.South 18 Q1.R),(Q1.EP Q1.West 13 Q1.R,Q1.EP Q1.East 13 Q1.L),(Q1.EP Q1.West 14 Q1.L,Q1.EP Q1.South 17 Q1.R),
                      (Q1.EP Q1.West 14 Q1.R,Q1.EP Q1.East 14 Q1.L),(Q1.EP Q1.West 15 Q1.L,Q1.EP Q1.South 16 Q1.R),(Q1.EP Q1.West 15 Q1.R,Q1.EP Q1.East 15 Q1.L),
                      (Q1.EP Q1.West 16 Q1.L,Q1.EP Q1.South 15 Q1.R),(Q1.EP Q1.West 16 Q1.R,Q1.EP Q1.East 16 Q1.L),(Q1.EP Q1.West 17 Q1.L,Q1.EP Q1.South 14 Q1.R),
                      (Q1.EP Q1.West 17 Q1.R,Q1.EP Q1.East 17 Q1.L),(Q1.EP Q1.West 18 Q1.L,Q1.EP Q1.South 13 Q1.R),(Q1.EP Q1.West 18 Q1.R,Q1.EP Q1.East 18 Q1.L),
                      (Q1.EP Q1.West 19 Q1.L,Q1.EP Q1.South 12 Q1.R),(Q1.EP Q1.West 19 Q1.R,Q1.EP Q1.East 19 Q1.L),(Q1.EP Q1.West 20 Q1.L,Q1.EP Q1.South 11 Q1.R),
                      (Q1.EP Q1.West 20 Q1.R,Q1.EP Q1.East 20 Q1.L),(Q1.EP Q1.West 21 Q1.L,Q1.EP Q1.South 10 Q1.R),(Q1.EP Q1.West 21 Q1.R,Q1.EP Q1.East 21 Q1.L),
                      (Q1.EP Q1.West 22 Q1.L,Q1.EP Q1.South 9 Q1.R),(Q1.EP Q1.West 22 Q1.R,Q1.EP Q1.East 22 Q1.L),(Q1.EP Q1.West 23 Q1.L,Q1.EP Q1.South 8 Q1.R),
                      (Q1.EP Q1.West 23 Q1.R,Q1.EP Q1.East 23 Q1.L),(Q1.EP Q1.West 24 Q1.L,Q1.EP Q1.South 7 Q1.R),(Q1.EP Q1.West 24 Q1.R,Q1.EP Q1.East 24 Q1.L),
                      (Q1.EP Q1.West 25 Q1.L,Q1.EP Q1.South 6 Q1.R),(Q1.EP Q1.West 25 Q1.R,Q1.EP Q1.East 25 Q1.L),(Q1.EP Q1.West 26 Q1.L,Q1.EP Q1.South 5 Q1.R),
                      (Q1.EP Q1.West 26 Q1.R,Q1.EP Q1.East 26 Q1.L),(Q1.EP Q1.West 27 Q1.L,Q1.EP Q1.South 4 Q1.R),(Q1.EP Q1.West 27 Q1.R,Q1.EP Q1.East 27 Q1.L),
                      (Q1.EP Q1.West 28 Q1.L,Q1.EP Q1.South 3 Q1.R),(Q1.EP Q1.West 28 Q1.R,Q1.EP Q1.East 28 Q1.L),(Q1.EP Q1.West 29 Q1.L,Q1.EP Q1.South 2 Q1.R),
                      (Q1.EP Q1.West 29 Q1.R,Q1.EP Q1.East 29 Q1.L),(Q1.EP Q1.West 30 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.West 30 Q1.R,Q1.EP Q1.East 30 Q1.L),
                      (Q1.EP Q1.South 1 Q1.L,Q1.EP Q1.East 1 Q1.R),(Q1.EP Q1.South 1 Q1.R,Q1.EP Q1.West 30 Q1.L),(Q1.EP Q1.South 2 Q1.L,Q1.EP Q1.East 2 Q1.R),
                      (Q1.EP Q1.South 2 Q1.R,Q1.EP Q1.West 29 Q1.L),(Q1.EP Q1.South 3 Q1.L,Q1.EP Q1.East 3 Q1.R),(Q1.EP Q1.South 3 Q1.R,Q1.EP Q1.West 28 Q1.L),
                      (Q1.EP Q1.South 4 Q1.L,Q1.EP Q1.East 4 Q1.R),(Q1.EP Q1.South 4 Q1.R,Q1.EP Q1.West 27 Q1.L),(Q1.EP Q1.South 5 Q1.L,Q1.EP Q1.East 5 Q1.R),
                      (Q1.EP Q1.South 5 Q1.R,Q1.EP Q1.West 26 Q1.L),(Q1.EP Q1.South 6 Q1.L,Q1.EP Q1.East 6 Q1.R),(Q1.EP Q1.South 6 Q1.R,Q1.EP Q1.West 25 Q1.L),
                      (Q1.EP Q1.South 7 Q1.L,Q1.EP Q1.East 7 Q1.R),(Q1.EP Q1.South 7 Q1.R,Q1.EP Q1.West 24 Q1.L),(Q1.EP Q1.South 8 Q1.L,Q1.EP Q1.East 8 Q1.R),
                      (Q1.EP Q1.South 8 Q1.R,Q1.EP Q1.West 23 Q1.L),(Q1.EP Q1.South 9 Q1.L,Q1.EP Q1.East 9 Q1.R),(Q1.EP Q1.South 9 Q1.R,Q1.EP Q1.West 22 Q1.L),
                      (Q1.EP Q1.South 10 Q1.L,Q1.EP Q1.East 10 Q1.R),(Q1.EP Q1.South 10 Q1.R,Q1.EP Q1.West 21 Q1.L),(Q1.EP Q1.South 11 Q1.L,Q1.EP Q1.East 11 Q1.R),
                      (Q1.EP Q1.South 11 Q1.R,Q1.EP Q1.West 20 Q1.L),(Q1.EP Q1.South 12 Q1.L,Q1.EP Q1.East 12 Q1.R),(Q1.EP Q1.South 12 Q1.R,Q1.EP Q1.West 19 Q1.L),
                      (Q1.EP Q1.South 13 Q1.L,Q1.EP Q1.East 13 Q1.R),(Q1.EP Q1.South 13 Q1.R,Q1.EP Q1.West 18 Q1.L),(Q1.EP Q1.South 14 Q1.L,Q1.EP Q1.East 14 Q1.R),
                      (Q1.EP Q1.South 14 Q1.R,Q1.EP Q1.West 17 Q1.L),(Q1.EP Q1.South 15 Q1.L,Q1.EP Q1.East 15 Q1.R),(Q1.EP Q1.South 15 Q1.R,Q1.EP Q1.West 16 Q1.L),
                      (Q1.EP Q1.South 16 Q1.L,Q1.EP Q1.East 16 Q1.R),(Q1.EP Q1.South 16 Q1.R,Q1.EP Q1.West 15 Q1.L),(Q1.EP Q1.South 17 Q1.L,Q1.EP Q1.East 17 Q1.R),
                      (Q1.EP Q1.South 17 Q1.R,Q1.EP Q1.West 14 Q1.L),(Q1.EP Q1.South 18 Q1.L,Q1.EP Q1.East 18 Q1.R),(Q1.EP Q1.South 18 Q1.R,Q1.EP Q1.West 13 Q1.L),
                      (Q1.EP Q1.South 19 Q1.L,Q1.EP Q1.East 19 Q1.R),(Q1.EP Q1.South 19 Q1.R,Q1.EP Q1.West 12 Q1.L),(Q1.EP Q1.South 20 Q1.L,Q1.EP Q1.East 20 Q1.R),
                      (Q1.EP Q1.South 20 Q1.R,Q1.EP Q1.West 11 Q1.L),(Q1.EP Q1.South 21 Q1.L,Q1.EP Q1.East 21 Q1.R),(Q1.EP Q1.South 21 Q1.R,Q1.EP Q1.West 10 Q1.L),
                      (Q1.EP Q1.South 22 Q1.L,Q1.EP Q1.East 22 Q1.R),(Q1.EP Q1.South 22 Q1.R,Q1.EP Q1.West 9 Q1.L),(Q1.EP Q1.South 23 Q1.L,Q1.EP Q1.East 23 Q1.R),
                      (Q1.EP Q1.South 23 Q1.R,Q1.EP Q1.West 8 Q1.L),(Q1.EP Q1.South 24 Q1.L,Q1.EP Q1.East 24 Q1.R),(Q1.EP Q1.South 24 Q1.R,Q1.EP Q1.West 7 Q1.L),
                      (Q1.EP Q1.South 25 Q1.L,Q1.EP Q1.East 25 Q1.R),(Q1.EP Q1.South 25 Q1.R,Q1.EP Q1.West 6 Q1.L),(Q1.EP Q1.South 26 Q1.L,Q1.EP Q1.East 26 Q1.R),
                      (Q1.EP Q1.South 26 Q1.R,Q1.EP Q1.West 5 Q1.L),(Q1.EP Q1.South 27 Q1.L,Q1.EP Q1.East 27 Q1.R),(Q1.EP Q1.South 27 Q1.R,Q1.EP Q1.West 4 Q1.L),
                      (Q1.EP Q1.South 28 Q1.L,Q1.EP Q1.East 28 Q1.R),(Q1.EP Q1.South 28 Q1.R,Q1.EP Q1.West 3 Q1.L),(Q1.EP Q1.South 29 Q1.L,Q1.EP Q1.East 29 Q1.R),
                      (Q1.EP Q1.South 29 Q1.R,Q1.EP Q1.West 2 Q1.L),(Q1.EP Q1.South 30 Q1.L,Q1.EP Q1.East 30 Q1.R),(Q1.EP Q1.South 30 Q1.R,Q1.EP Q1.West 1 Q1.L)]

----- Test case 3: calcInteractions with size 1 with 1 atoms
testCalcInteractions_Size1_OneAtoms = TestCase $ assertEqual
    "calcInteractions with size 1 and 1 atom"
    expectedResult
    (Q1.calcInteractions 1 [(1,1)])
  where
    expectedResult = [(Q1.EP Q1.East 1 Q1.L,Q1.EP Q1.East 1 Q1.R),(Q1.EP Q1.East 1 Q1.R,Q1.EP Q1.East 1 Q1.L),(Q1.EP Q1.West 1 Q1.L,Q1.EP Q1.South 1 Q1.R),
                      (Q1.EP Q1.West 1 Q1.R,Q1.EP Q1.West 1 Q1.L),(Q1.EP Q1.South 1 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.South 1 Q1.R,Q1.EP Q1.West 1 Q1.L)]

----- Test case 4: calcInteractions with size 6 with atoms blocked in the middle
testCalcInteractions_Size3_OneAtom = TestCase $ assertEqual
    "calcInteractions with size 3 and one atom in the middle"
    expectedResult
    (Q1.calcInteractions 3 [(2,2)])
  where
    expectedResult = [(Q1.EP Q1.East 1 Q1.L,Q1.EP Q1.West 1 Q1.R),(Q1.EP Q1.East 1 Q1.R,Q1.EP Q1.West 1 Q1.L),(Q1.EP Q1.East 2 Q1.L,Q1.EP Q1.South 3 Q1.R),
                       (Q1.EP Q1.East 2 Q1.R,Q1.EP Q1.South 2 Q1.L),(Q1.EP Q1.East 3 Q1.L,Q1.EP Q1.West 3 Q1.R),(Q1.EP Q1.East 3 Q1.R,Q1.EP Q1.South 3 Q1.L),
                       (Q1.EP Q1.West 1 Q1.L,Q1.EP Q1.East 1 Q1.R),(Q1.EP Q1.West 1 Q1.R,Q1.EP Q1.East 1 Q1.L),(Q1.EP Q1.West 2 Q1.L,Q1.EP Q1.South 2 Q1.R),
                       (Q1.EP Q1.West 2 Q1.R,Q1.EP Q1.South 1 Q1.L),(Q1.EP Q1.West 3 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.West 3 Q1.R,Q1.EP Q1.East 3 Q1.L),
                       (Q1.EP Q1.South 1 Q1.L,Q1.EP Q1.West 2 Q1.R),(Q1.EP Q1.South 1 Q1.R,Q1.EP Q1.West 3 Q1.L),(Q1.EP Q1.South 2 Q1.L,Q1.EP Q1.East 2 Q1.R),
                       (Q1.EP Q1.South 2 Q1.R,Q1.EP Q1.West 2 Q1.L),(Q1.EP Q1.South 3 Q1.L,Q1.EP Q1.East 3 Q1.R),(Q1.EP Q1.South 3 Q1.R,Q1.EP Q1.East 2 Q1.L)]

----- Test case 5: calcInteractions with size 6 with atoms blocked in the middle
testCalcInteractions_Size6_CentreAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 and atoms blocked in the middle"
    expectedResult
    (Q1.calcInteractions 6 [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6)])
  where
    expectedResult = [(Q1.EP Q1.East 1 Q1.L,Q1.EP Q1.East 1 Q1.R),(Q1.EP Q1.East 1 Q1.R,Q1.EP Q1.East 1 Q1.L),(Q1.EP Q1.East 2 Q1.L,Q1.EP Q1.South 6 Q1.R),
                      (Q1.EP Q1.East 2 Q1.R,Q1.EP Q1.East 3 Q1.L),(Q1.EP Q1.East 3 Q1.L,Q1.EP Q1.East 2 Q1.R),(Q1.EP Q1.East 3 Q1.R,Q1.EP Q1.East 5 Q1.L),
                      (Q1.EP Q1.East 4 Q1.L,Q1.EP Q1.South 5 Q1.R),(Q1.EP Q1.East 4 Q1.R,Q1.EP Q1.South 4 Q1.L),(Q1.EP Q1.East 5 Q1.L,Q1.EP Q1.East 3 Q1.R),
                      (Q1.EP Q1.East 5 Q1.R,Q1.EP Q1.South 5 Q1.L),(Q1.EP Q1.East 6 Q1.L,Q1.EP Q1.South 4 Q1.R),(Q1.EP Q1.East 6 Q1.R,Q1.EP Q1.South 6 Q1.L),
                      (Q1.EP Q1.West 1 Q1.L,Q1.EP Q1.West 1 Q1.R),(Q1.EP Q1.West 1 Q1.R,Q1.EP Q1.West 1 Q1.L),(Q1.EP Q1.West 2 Q1.L,Q1.EP Q1.West 3 Q1.R),
                      (Q1.EP Q1.West 2 Q1.R,Q1.EP Q1.South 1 Q1.L),(Q1.EP Q1.West 3 Q1.L,Q1.EP Q1.West 5 Q1.R),(Q1.EP Q1.West 3 Q1.R,Q1.EP Q1.West 2 Q1.L),
                      (Q1.EP Q1.West 4 Q1.L,Q1.EP Q1.South 3 Q1.R),(Q1.EP Q1.West 4 Q1.R,Q1.EP Q1.South 2 Q1.L),(Q1.EP Q1.West 5 Q1.L,Q1.EP Q1.South 2 Q1.R),
                      (Q1.EP Q1.West 5 Q1.R,Q1.EP Q1.West 3 Q1.L),(Q1.EP Q1.West 6 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.West 6 Q1.R,Q1.EP Q1.South 3 Q1.L),
                      (Q1.EP Q1.South 1 Q1.L,Q1.EP Q1.West 2 Q1.R),(Q1.EP Q1.South 1 Q1.R,Q1.EP Q1.West 6 Q1.L),(Q1.EP Q1.South 2 Q1.L,Q1.EP Q1.West 4 Q1.R),
                      (Q1.EP Q1.South 2 Q1.R,Q1.EP Q1.West 5 Q1.L),(Q1.EP Q1.South 3 Q1.L,Q1.EP Q1.West 6 Q1.R),(Q1.EP Q1.South 3 Q1.R,Q1.EP Q1.West 4 Q1.L),
                      (Q1.EP Q1.South 4 Q1.L,Q1.EP Q1.East 4 Q1.R),(Q1.EP Q1.South 4 Q1.R,Q1.EP Q1.East 6 Q1.L),(Q1.EP Q1.South 5 Q1.L,Q1.EP Q1.East 5 Q1.R),
                      (Q1.EP Q1.South 5 Q1.R,Q1.EP Q1.East 4 Q1.L),(Q1.EP Q1.South 6 Q1.L,Q1.EP Q1.East 6 Q1.R),(Q1.EP Q1.South 6 Q1.R,Q1.EP Q1.East 2 Q1.L)]

----- Test case 6: calcInteractions with size 6 with atoms only on upward triangle
testCalcInteractions_Size6_UpwardTriAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 with atoms only on upward triangle"
    expectedResult
    (Q1.calcInteractions 6 [(1,1),(3,1),(3,3),(3,5),(4,3),(4,5),(4,7),(5,3),(5,9),(6,1),(6,5),(6,9),(6,11)])
  where
    expectedResult = [(Q1.EP Q1.East 1 Q1.L,Q1.EP Q1.East 1 Q1.R),(Q1.EP Q1.East 1 Q1.R,Q1.EP Q1.East 1 Q1.L),(Q1.EP Q1.East 2 Q1.L,Q1.EP Q1.West 2 Q1.R),
                      (Q1.EP Q1.East 2 Q1.R,Q1.EP Q1.West 2 Q1.L),(Q1.EP Q1.East 3 Q1.L,Q1.EP Q1.East 3 Q1.R),(Q1.EP Q1.East 3 Q1.R,Q1.EP Q1.East 3 Q1.L),
                      (Q1.EP Q1.East 4 Q1.L,Q1.EP Q1.East 4 Q1.R),(Q1.EP Q1.East 4 Q1.R,Q1.EP Q1.East 4 Q1.L),(Q1.EP Q1.East 5 Q1.L,Q1.EP Q1.East 5 Q1.R),
                      (Q1.EP Q1.East 5 Q1.R,Q1.EP Q1.East 5 Q1.L),(Q1.EP Q1.East 6 Q1.L,Q1.EP Q1.East 6 Q1.R),(Q1.EP Q1.East 6 Q1.R,Q1.EP Q1.East 6 Q1.L),
                      (Q1.EP Q1.West 1 Q1.L,Q1.EP Q1.West 1 Q1.R),(Q1.EP Q1.West 1 Q1.R,Q1.EP Q1.West 1 Q1.L),(Q1.EP Q1.West 2 Q1.L,Q1.EP Q1.East 2 Q1.R),
                      (Q1.EP Q1.West 2 Q1.R,Q1.EP Q1.East 2 Q1.L),(Q1.EP Q1.West 3 Q1.L,Q1.EP Q1.West 3 Q1.R),(Q1.EP Q1.West 3 Q1.R,Q1.EP Q1.West 3 Q1.L),
                      (Q1.EP Q1.West 4 Q1.L,Q1.EP Q1.West 5 Q1.R),(Q1.EP Q1.West 4 Q1.R,Q1.EP Q1.South 2 Q1.L),(Q1.EP Q1.West 5 Q1.L,Q1.EP Q1.South 2 Q1.R),
                      (Q1.EP Q1.West 5 Q1.R,Q1.EP Q1.West 4 Q1.L),(Q1.EP Q1.West 6 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.West 6 Q1.R,Q1.EP Q1.West 6 Q1.L),
                      (Q1.EP Q1.South 1 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.South 1 Q1.R,Q1.EP Q1.West 6 Q1.L),(Q1.EP Q1.South 2 Q1.L,Q1.EP Q1.West 4 Q1.R),
                      (Q1.EP Q1.South 2 Q1.R,Q1.EP Q1.West 5 Q1.L),(Q1.EP Q1.South 3 Q1.L,Q1.EP Q1.South 3 Q1.R),(Q1.EP Q1.South 3 Q1.R,Q1.EP Q1.South 3 Q1.L),
                      (Q1.EP Q1.South 4 Q1.L,Q1.EP Q1.South 4 Q1.R),(Q1.EP Q1.South 4 Q1.R,Q1.EP Q1.South 4 Q1.L),(Q1.EP Q1.South 5 Q1.L,Q1.EP Q1.South 5 Q1.R),
                      (Q1.EP Q1.South 5 Q1.R,Q1.EP Q1.South 5 Q1.L),(Q1.EP Q1.South 6 Q1.L,Q1.EP Q1.South 6 Q1.R),(Q1.EP Q1.South 6 Q1.R,Q1.EP Q1.South 6 Q1.L)]

                       

----- Test case 7: calcInteractions with size 6 with atoms only on downward triangle
testCalcInteractions_Size6_DownwardTriAtoms = TestCase $ assertEqual
    "calcInteractions with size 6 with atoms only on downward triangle"
    expectedResult
    (Q1.calcInteractions 6 [(2,2),(3,2),(3,4),(4,4),(4,6),(6,2),(6,6),(6,8)])
  where
    expectedResult = [(Q1.EP Q1.East 1 Q1.L,Q1.EP Q1.West 1 Q1.R),(Q1.EP Q1.East 1 Q1.R,Q1.EP Q1.West 1 Q1.L),(Q1.EP Q1.East 2 Q1.L,Q1.EP Q1.East 2 Q1.R),
                      (Q1.EP Q1.East 2 Q1.R,Q1.EP Q1.East 2 Q1.L),(Q1.EP Q1.East 3 Q1.L,Q1.EP Q1.East 3 Q1.R),(Q1.EP Q1.East 3 Q1.R,Q1.EP Q1.East 3 Q1.L),
                      (Q1.EP Q1.East 4 Q1.L,Q1.EP Q1.South 6 Q1.R),(Q1.EP Q1.East 4 Q1.R,Q1.EP Q1.West 3 Q1.L),(Q1.EP Q1.East 5 Q1.L,Q1.EP Q1.West 5 Q1.R),
                      (Q1.EP Q1.East 5 Q1.R,Q1.EP Q1.South 5 Q1.L),(Q1.EP Q1.East 6 Q1.L,Q1.EP Q1.South 5 Q1.R),(Q1.EP Q1.East 6 Q1.R,Q1.EP Q1.South 6 Q1.L),
                      (Q1.EP Q1.West 1 Q1.L,Q1.EP Q1.East 1 Q1.R),(Q1.EP Q1.West 1 Q1.R,Q1.EP Q1.East 1 Q1.L),(Q1.EP Q1.West 2 Q1.L,Q1.EP Q1.West 2 Q1.R),
                      (Q1.EP Q1.West 2 Q1.R,Q1.EP Q1.West 2 Q1.L),(Q1.EP Q1.West 3 Q1.L,Q1.EP Q1.East 4 Q1.R),(Q1.EP Q1.West 3 Q1.R,Q1.EP Q1.West 5 Q1.L),
                      (Q1.EP Q1.West 4 Q1.L,Q1.EP Q1.South 3 Q1.R),(Q1.EP Q1.West 4 Q1.R,Q1.EP Q1.South 2 Q1.L),(Q1.EP Q1.West 5 Q1.L,Q1.EP Q1.West 3 Q1.R),
                      (Q1.EP Q1.West 5 Q1.R,Q1.EP Q1.East 5 Q1.L),(Q1.EP Q1.West 6 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.West 6 Q1.R,Q1.EP Q1.South 1 Q1.L),
                      (Q1.EP Q1.South 1 Q1.L,Q1.EP Q1.West 6 Q1.R),(Q1.EP Q1.South 1 Q1.R,Q1.EP Q1.West 6 Q1.L),(Q1.EP Q1.South 2 Q1.L,Q1.EP Q1.West 4 Q1.R),
                      (Q1.EP Q1.South 2 Q1.R,Q1.EP Q1.South 3 Q1.L),(Q1.EP Q1.South 3 Q1.L,Q1.EP Q1.South 2 Q1.R),(Q1.EP Q1.South 3 Q1.R,Q1.EP Q1.West 4 Q1.L),
                      (Q1.EP Q1.South 4 Q1.L,Q1.EP Q1.South 4 Q1.R),(Q1.EP Q1.South 4 Q1.R,Q1.EP Q1.South 4 Q1.L),(Q1.EP Q1.South 5 Q1.L,Q1.EP Q1.East 5 Q1.R),
                      (Q1.EP Q1.South 5 Q1.R,Q1.EP Q1.East 6 Q1.L),(Q1.EP Q1.South 6 Q1.L,Q1.EP Q1.East 6 Q1.R),(Q1.EP Q1.South 6 Q1.R,Q1.EP Q1.East 4 Q1.L)]

----- Test case 8: calcInteractions with size 30 with random atoms to test efficiency
testCalcInteractions_Size30_RandomAtoms = TestCase $ assertEqual
    "calcInteractions with size 30 with random atoms to test efficiency"
    expectedResult
    (Q1.calcInteractions 30 [(4,5), (8,9), (12,15), (15,20), (17,25), (19,30), (21,35), (23,40), (25,45), (27,50),
                          (3,3), (7,7), (11,11), (14,17), (16,23), (18,29), (20,35), (22,41), (24,47), (26,51),
                          (2,1), (6,5), (10,13), (13,19), (15,25), (17,31), (19,36), (21,41), (23,45), (25,49),
                          (1,1), (5,3), (9,11), (12,17), (14,23), (16,29), (18,35), (20,39), (22,43), (24,47),
                          (3,1), (7,5), (11,13), (13,21), (15,27), (17,33), (19,39), (21,43), (23,40), (25,25),
                          (4,3), (8,7), (12,15), (14,23), (16,27), (18,31), (20,27), (22,41), (24,30), (26,26)])
  where
    expectedResult = [(Q1.EP Q1.East 1 Q1.L,Q1.EP Q1.East 1 Q1.R),(Q1.EP Q1.East 1 Q1.R,Q1.EP Q1.East 1 Q1.L),
                     (Q1.EP Q1.East 2 Q1.L,Q1.EP Q1.East 16 Q1.R),(Q1.EP Q1.East 2 Q1.R,Q1.EP Q1.East 3 Q1.L),
                     (Q1.EP Q1.East 3 Q1.L,Q1.EP Q1.East 2 Q1.R),(Q1.EP Q1.East 3 Q1.R,Q1.EP Q1.East 4 Q1.L),
                     (Q1.EP Q1.East 4 Q1.L,Q1.EP Q1.East 3 Q1.R),(Q1.EP Q1.East 4 Q1.R,Q1.EP Q1.East 7 Q1.L),
                     (Q1.EP Q1.East 5 Q1.L,Q1.EP Q1.East 10 Q1.R),(Q1.EP Q1.East 5 Q1.R,Q1.EP Q1.East 8 Q1.L),
                     (Q1.EP Q1.East 6 Q1.L,Q1.EP Q1.East 13 Q1.R),(Q1.EP Q1.East 6 Q1.R,Q1.EP Q1.East 9 Q1.L),
                     (Q1.EP Q1.East 7 Q1.L,Q1.EP Q1.East 4 Q1.R),(Q1.EP Q1.East 7 Q1.R,Q1.EP Q1.East 10 Q1.L),
                     (Q1.EP Q1.East 8 Q1.L,Q1.EP Q1.East 5 Q1.R),(Q1.EP Q1.East 8 Q1.R,Q1.EP Q1.East 11 Q1.L),
                     (Q1.EP Q1.East 9 Q1.L,Q1.EP Q1.East 6 Q1.R),(Q1.EP Q1.East 9 Q1.R,Q1.EP Q1.East 12 Q1.L),
                     (Q1.EP Q1.East 10 Q1.L,Q1.EP Q1.East 7 Q1.R),(Q1.EP Q1.East 10 Q1.R,Q1.EP Q1.East 5 Q1.L),
                     (Q1.EP Q1.East 11 Q1.L,Q1.EP Q1.East 8 Q1.R),(Q1.EP Q1.East 11 Q1.R,Q1.EP Q1.East 13 Q1.L),
                     (Q1.EP Q1.East 12 Q1.L,Q1.EP Q1.East 9 Q1.R),(Q1.EP Q1.East 12 Q1.R,Q1.EP Q1.East 14 Q1.L),
                     (Q1.EP Q1.East 13 Q1.L,Q1.EP Q1.East 11 Q1.R),(Q1.EP Q1.East 13 Q1.R,Q1.EP Q1.East 6 Q1.L),
                     (Q1.EP Q1.East 14 Q1.L,Q1.EP Q1.East 12 Q1.R),(Q1.EP Q1.East 14 Q1.R,Q1.EP Q1.East 15 Q1.L),
                     (Q1.EP Q1.East 15 Q1.L,Q1.EP Q1.East 14 Q1.R),(Q1.EP Q1.East 15 Q1.R,Q1.EP Q1.East 16 Q1.L),
                     (Q1.EP Q1.East 16 Q1.L,Q1.EP Q1.East 15 Q1.R),(Q1.EP Q1.East 16 Q1.R,Q1.EP Q1.East 2 Q1.L),
                     (Q1.EP Q1.East 17 Q1.L,Q1.EP Q1.East 17 Q1.R),(Q1.EP Q1.East 17 Q1.R,Q1.EP Q1.East 17 Q1.L),
                     (Q1.EP Q1.East 18 Q1.L,Q1.EP Q1.East 18 Q1.R),(Q1.EP Q1.East 18 Q1.R,Q1.EP Q1.East 18 Q1.L),
                     (Q1.EP Q1.East 19 Q1.L,Q1.EP Q1.West 21 Q1.R),(Q1.EP Q1.East 19 Q1.R,Q1.EP Q1.South 19 Q1.L),
                     (Q1.EP Q1.East 20 Q1.L,Q1.EP Q1.East 20 Q1.R),(Q1.EP Q1.East 20 Q1.R,Q1.EP Q1.East 20 Q1.L),
                     (Q1.EP Q1.East 21 Q1.L,Q1.EP Q1.East 21 Q1.R),(Q1.EP Q1.East 21 Q1.R,Q1.EP Q1.East 21 Q1.L),
                     (Q1.EP Q1.East 22 Q1.L,Q1.EP Q1.East 22 Q1.R),(Q1.EP Q1.East 22 Q1.R,Q1.EP Q1.East 22 Q1.L),
                     (Q1.EP Q1.East 23 Q1.L,Q1.EP Q1.East 23 Q1.R),(Q1.EP Q1.East 23 Q1.R,Q1.EP Q1.East 23 Q1.L),
                     (Q1.EP Q1.East 24 Q1.L,Q1.EP Q1.East 24 Q1.R),(Q1.EP Q1.East 24 Q1.R,Q1.EP Q1.East 24 Q1.L),
                     (Q1.EP Q1.East 25 Q1.L,Q1.EP Q1.East 25 Q1.R),(Q1.EP Q1.East 25 Q1.R,Q1.EP Q1.East 25 Q1.L),
                     (Q1.EP Q1.East 26 Q1.L,Q1.EP Q1.East 26 Q1.R),(Q1.EP Q1.East 26 Q1.R,Q1.EP Q1.East 26 Q1.L),
                     (Q1.EP Q1.East 27 Q1.L,Q1.EP Q1.South 29 Q1.R),(Q1.EP Q1.East 27 Q1.R,Q1.EP Q1.South 27 Q1.L),
                     (Q1.EP Q1.East 28 Q1.L,Q1.EP Q1.West 28 Q1.R),(Q1.EP Q1.East 28 Q1.R,Q1.EP Q1.South 28 Q1.L),
                     (Q1.EP Q1.East 29 Q1.L,Q1.EP Q1.West 29 Q1.R),(Q1.EP Q1.East 29 Q1.R,Q1.EP Q1.South 29 Q1.L),
                     (Q1.EP Q1.East 30 Q1.L,Q1.EP Q1.West 30 Q1.R),(Q1.EP Q1.East 30 Q1.R,Q1.EP Q1.South 30 Q1.L),
                     (Q1.EP Q1.West 1 Q1.L,Q1.EP Q1.West 1 Q1.R),(Q1.EP Q1.West 1 Q1.R,Q1.EP Q1.West 1 Q1.L),
                     (Q1.EP Q1.West 2 Q1.L,Q1.EP Q1.West 2 Q1.R),(Q1.EP Q1.West 2 Q1.R,Q1.EP Q1.West 2 Q1.L),
                     (Q1.EP Q1.West 3 Q1.L,Q1.EP Q1.West 3 Q1.R),(Q1.EP Q1.West 3 Q1.R,Q1.EP Q1.West 3 Q1.L),
                     (Q1.EP Q1.West 4 Q1.L,Q1.EP Q1.West 5 Q1.R),(Q1.EP Q1.West 4 Q1.R,Q1.EP Q1.South 1 Q1.L),
                     (Q1.EP Q1.West 5 Q1.L,Q1.EP Q1.West 7 Q1.R),(Q1.EP Q1.West 5 Q1.R,Q1.EP Q1.West 4 Q1.L),
                     (Q1.EP Q1.West 6 Q1.L,Q1.EP Q1.West 11 Q1.R),(Q1.EP Q1.West 6 Q1.R,Q1.EP Q1.South 2 Q1.L),
                     (Q1.EP Q1.West 7 Q1.L,Q1.EP Q1.West 20 Q1.R),(Q1.EP Q1.West 7 Q1.R,Q1.EP Q1.West 5 Q1.L),
                     (Q1.EP Q1.West 8 Q1.L,Q1.EP Q1.South 23 Q1.R),(Q1.EP Q1.West 8 Q1.R,Q1.EP Q1.South 3 Q1.L),
                     (Q1.EP Q1.West 9 Q1.L,Q1.EP Q1.West 19 Q1.R),(Q1.EP Q1.West 9 Q1.R,Q1.EP Q1.South 5 Q1.L),
                     (Q1.EP Q1.West 10 Q1.L,Q1.EP Q1.South 21 Q1.R),(Q1.EP Q1.West 10 Q1.R,Q1.EP Q1.South 4 Q1.L),
                     (Q1.EP Q1.West 11 Q1.L,Q1.EP Q1.South 20 Q1.R),(Q1.EP Q1.West 11 Q1.R,Q1.EP Q1.West 6 Q1.L),
                     (Q1.EP Q1.West 12 Q1.L,Q1.EP Q1.South 19 Q1.R),(Q1.EP Q1.West 12 Q1.R,Q1.EP Q1.South 7 Q1.L),
                     (Q1.EP Q1.West 13 Q1.L,Q1.EP Q1.West 25 Q1.R),(Q1.EP Q1.West 13 Q1.R,Q1.EP Q1.South 11 Q1.L),
                     (Q1.EP Q1.West 14 Q1.L,Q1.EP Q1.South 17 Q1.R),(Q1.EP Q1.West 14 Q1.R,Q1.EP Q1.South 6 Q1.L),
                     (Q1.EP Q1.West 15 Q1.L,Q1.EP Q1.South 16 Q1.R),(Q1.EP Q1.West 15 Q1.R,Q1.EP Q1.South 10 Q1.L),
                     (Q1.EP Q1.West 16 Q1.L,Q1.EP Q1.South 15 Q1.R),(Q1.EP Q1.West 16 Q1.R,Q1.EP Q1.South 16 Q1.L),
                     (Q1.EP Q1.West 17 Q1.L,Q1.EP Q1.South 14 Q1.R),(Q1.EP Q1.West 17 Q1.R,Q1.EP Q1.South 12 Q1.L),
                     (Q1.EP Q1.West 18 Q1.L,Q1.EP Q1.South 13 Q1.R),(Q1.EP Q1.West 18 Q1.R,Q1.EP Q1.South 8 Q1.L),
                     (Q1.EP Q1.West 19 Q1.L,Q1.EP Q1.South 12 Q1.R),(Q1.EP Q1.West 19 Q1.R,Q1.EP Q1.West 9 Q1.L),
                     (Q1.EP Q1.West 20 Q1.L,Q1.EP Q1.South 11 Q1.R),(Q1.EP Q1.West 20 Q1.R,Q1.EP Q1.West 7 Q1.L),
                     (Q1.EP Q1.West 21 Q1.L,Q1.EP Q1.South 10 Q1.R),(Q1.EP Q1.West 21 Q1.R,Q1.EP Q1.East 19 Q1.L),
                     (Q1.EP Q1.West 22 Q1.L,Q1.EP Q1.South 9 Q1.R),(Q1.EP Q1.West 22 Q1.R,Q1.EP Q1.South 17 Q1.L),
                     (Q1.EP Q1.West 23 Q1.L,Q1.EP Q1.South 8 Q1.R),(Q1.EP Q1.West 23 Q1.R,Q1.EP Q1.South 20 Q1.L),
                     (Q1.EP Q1.West 24 Q1.L,Q1.EP Q1.South 7 Q1.R),(Q1.EP Q1.West 24 Q1.R,Q1.EP Q1.South 15 Q1.L),
                     (Q1.EP Q1.West 25 Q1.L,Q1.EP Q1.South 6 Q1.R),(Q1.EP Q1.West 25 Q1.R,Q1.EP Q1.West 13 Q1.L),
                     (Q1.EP Q1.West 26 Q1.L,Q1.EP Q1.South 5 Q1.R),(Q1.EP Q1.West 26 Q1.R,Q1.EP Q1.South 13 Q1.L),
                     (Q1.EP Q1.West 27 Q1.L,Q1.EP Q1.South 4 Q1.R),(Q1.EP Q1.West 27 Q1.R,Q1.EP Q1.South 25 Q1.L),
                     (Q1.EP Q1.West 28 Q1.L,Q1.EP Q1.South 3 Q1.R),(Q1.EP Q1.West 28 Q1.R,Q1.EP Q1.East 28 Q1.L),
                     (Q1.EP Q1.West 29 Q1.L,Q1.EP Q1.South 2 Q1.R),(Q1.EP Q1.West 29 Q1.R,Q1.EP Q1.East 29 Q1.L),
                     (Q1.EP Q1.West 30 Q1.L,Q1.EP Q1.South 1 Q1.R),(Q1.EP Q1.West 30 Q1.R,Q1.EP Q1.East 30 Q1.L),
                     (Q1.EP Q1.South 1 Q1.L,Q1.EP Q1.West 4 Q1.R),(Q1.EP Q1.South 1 Q1.R,Q1.EP Q1.West 30 Q1.L),
                     (Q1.EP Q1.South 2 Q1.L,Q1.EP Q1.West 6 Q1.R),(Q1.EP Q1.South 2 Q1.R,Q1.EP Q1.West 29 Q1.L),
                     (Q1.EP Q1.South 3 Q1.L,Q1.EP Q1.West 8 Q1.R),(Q1.EP Q1.South 3 Q1.R,Q1.EP Q1.West 28 Q1.L),
                     (Q1.EP Q1.South 4 Q1.L,Q1.EP Q1.West 10 Q1.R),(Q1.EP Q1.South 4 Q1.R,Q1.EP Q1.West 27 Q1.L),
                     (Q1.EP Q1.South 5 Q1.L,Q1.EP Q1.West 9 Q1.R),(Q1.EP Q1.South 5 Q1.R,Q1.EP Q1.West 26 Q1.L),
                     (Q1.EP Q1.South 6 Q1.L,Q1.EP Q1.West 14 Q1.R),(Q1.EP Q1.South 6 Q1.R,Q1.EP Q1.West 25 Q1.L),
                     (Q1.EP Q1.South 7 Q1.L,Q1.EP Q1.West 12 Q1.R),(Q1.EP Q1.South 7 Q1.R,Q1.EP Q1.West 24 Q1.L),
                     (Q1.EP Q1.South 8 Q1.L,Q1.EP Q1.West 18 Q1.R),(Q1.EP Q1.South 8 Q1.R,Q1.EP Q1.West 23 Q1.L),
                     (Q1.EP Q1.South 9 Q1.L,Q1.EP Q1.South 25 Q1.R),(Q1.EP Q1.South 9 Q1.R,Q1.EP Q1.West 22 Q1.L),
                     (Q1.EP Q1.South 10 Q1.L,Q1.EP Q1.West 15 Q1.R),(Q1.EP Q1.South 10 Q1.R,Q1.EP Q1.West 21 Q1.L),
                     (Q1.EP Q1.South 11 Q1.L,Q1.EP Q1.West 13 Q1.R),(Q1.EP Q1.South 11 Q1.R,Q1.EP Q1.West 20 Q1.L),
                     (Q1.EP Q1.South 12 Q1.L,Q1.EP Q1.West 17 Q1.R),(Q1.EP Q1.South 12 Q1.R,Q1.EP Q1.West 19 Q1.L),
                     (Q1.EP Q1.South 13 Q1.L,Q1.EP Q1.West 26 Q1.R),(Q1.EP Q1.South 13 Q1.R,Q1.EP Q1.West 18 Q1.L),
                     (Q1.EP Q1.South 14 Q1.L,Q1.EP Q1.South 24 Q1.R),(Q1.EP Q1.South 14 Q1.R,Q1.EP Q1.West 17 Q1.L),
                     (Q1.EP Q1.South 15 Q1.L,Q1.EP Q1.West 24 Q1.R),(Q1.EP Q1.South 15 Q1.R,Q1.EP Q1.West 16 Q1.L),
                     (Q1.EP Q1.South 16 Q1.L,Q1.EP Q1.West 16 Q1.R),(Q1.EP Q1.South 16 Q1.R,Q1.EP Q1.West 15 Q1.L),
                     (Q1.EP Q1.South 17 Q1.L,Q1.EP Q1.West 22 Q1.R),(Q1.EP Q1.South 17 Q1.R,Q1.EP Q1.West 14 Q1.L),
                     (Q1.EP Q1.South 18 Q1.L,Q1.EP Q1.South 27 Q1.R),(Q1.EP Q1.South 18 Q1.R,Q1.EP Q1.South 21 Q1.L),
                     (Q1.EP Q1.South 19 Q1.L,Q1.EP Q1.East 19 Q1.R),(Q1.EP Q1.South 19 Q1.R,Q1.EP Q1.West 12 Q1.L),
                     (Q1.EP Q1.South 20 Q1.L,Q1.EP Q1.West 23 Q1.R),(Q1.EP Q1.South 20 Q1.R,Q1.EP Q1.West 11 Q1.L),
                     (Q1.EP Q1.South 21 Q1.L,Q1.EP Q1.South 18 Q1.R),(Q1.EP Q1.South 21 Q1.R,Q1.EP Q1.West 10 Q1.L),
                     (Q1.EP Q1.South 22 Q1.L,Q1.EP Q1.South 26 Q1.R),(Q1.EP Q1.South 22 Q1.R,Q1.EP Q1.South 24 Q1.L),
                     (Q1.EP Q1.South 23 Q1.L,Q1.EP Q1.South 28 Q1.R),(Q1.EP Q1.South 23 Q1.R,Q1.EP Q1.West 8 Q1.L),
                     (Q1.EP Q1.South 24 Q1.L,Q1.EP Q1.South 22 Q1.R),(Q1.EP Q1.South 24 Q1.R,Q1.EP Q1.South 14 Q1.L),
                     (Q1.EP Q1.South 25 Q1.L,Q1.EP Q1.West 27 Q1.R),(Q1.EP Q1.South 25 Q1.R,Q1.EP Q1.South 9 Q1.L),
                     (Q1.EP Q1.South 26 Q1.L,Q1.EP Q1.South 30 Q1.R),(Q1.EP Q1.South 26 Q1.R,Q1.EP Q1.South 22 Q1.L),
                     (Q1.EP Q1.South 27 Q1.L,Q1.EP Q1.East 27 Q1.R),(Q1.EP Q1.South 27 Q1.R,Q1.EP Q1.South 18 Q1.L),
                     (Q1.EP Q1.South 28 Q1.L,Q1.EP Q1.East 28 Q1.R),(Q1.EP Q1.South 28 Q1.R,Q1.EP Q1.South 23 Q1.L),
                     (Q1.EP Q1.South 29 Q1.L,Q1.EP Q1.East 29 Q1.R),(Q1.EP Q1.South 29 Q1.R,Q1.EP Q1.East 27 Q1.L),
                     (Q1.EP Q1.South 30 Q1.L,Q1.EP Q1.East 30 Q1.R),(Q1.EP Q1.South 30 Q1.R,Q1.EP Q1.South 26 Q1.L)]



-- Q2
testSolveTBB_case1 = TestCase $ assertEqual
    "check base case"
    expectedResult
    (solveTBB 4 [(EP East 1 L,EP West 1 R),(EP East 1 R,EP East 5 L),(EP East 2 L,EP West 2 R),(EP East 2 R,EP East 4 L),(EP East 3 L,EP West 3 R),(EP East 3 R,EP South 3 L),(EP East 4 L,EP East 2 R),(EP East 4 R,EP East 6 L),(EP East 5 L,EP East 1 R),(EP East 5 R,EP South 5 L),(EP East 6 L,EP East 4 R),(EP East 6 R,EP West 2 L),(EP East 7 L,EP West 7 R),(EP East 7 R,EP South 7 L),(EP East 8 L,EP South 7 R),(EP East 8 R,EP South 8 L),(EP West 1 L,EP South 8 R),(EP West 1 R,EP East 1 L),(EP West 2 L,EP East 6 R),(EP West 2 R,EP East 2 L),(EP West 3 L,EP West 4 R),(EP West 3 R,EP East 3 L),(EP West 4 L,EP South 5 R),(EP West 4 R,EP West 3 L),(EP West 5 L,EP West 5 R),(EP West 5 R,EP West 5 L),(EP West 6 L,EP South 3 R),(EP West 6 R,EP South 2 L),(EP West 7 L,EP South 2 R),(EP West 7 R,EP East 7 L),(EP West 8 L,EP South 1 R),(EP West 8 R,EP South 6 L),(EP South 1 L,EP South 4 R),(EP South 1 R,EP West 8 L),(EP South 2 L,EP West 6 R),(EP South 2 R,EP West 7 L),(EP South 3 L,EP East 3 R),(EP South 3 R,EP West 6 L),(EP South 4 L,EP South 6 R),(EP South 4 R,EP South 1 L),(EP South 5 L,EP East 5 R),(EP South 5 R,EP West 4 L),(EP South 6 L,EP West 8 R),(EP South 6 R,EP South 4 L),(EP South 7 L,EP East 7 R),(EP South 7 R,EP East 8 L),(EP South 8 L,EP East 8 R),(EP South 8 R,EP West 1 L)])
  where
    expectedResult = [(4,3),(5,1),(6,7),(8,12)]
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
            - [[ x ]] = λ κ → (κ x)
            - [[ λx → E ]] = λκ → (κ λx → [[ E ]])
            - [[ (E1 E2) ]] = λκ → ( [[ E1 ]]  λf → ( [[ E2 ]]  λe → (f e κ) ) )
            - [[ def X = E in ME ]]  = def X = [[ E ]]  in  [[ ME ]]
            - [[ X ]] = X 
-}

----- Test case 1 [[ x ]] = λ κ →  (κ x):
testCpsTransform_Rule_1 = TestCase $ assertEqual
    "check [[ x ]] = λ κ →  (κ x)"
    expectedResult
    (Q5.cpsTransform (Q5.LamDef [] (Q5.LamVar 1)))
  where
    expectedResult = Q5.LamDef [] (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1)))

----- Test case 2 [[ λx → E ]] = λκ → (κ λx → [[ E ]]):
testCpsTransform_Rule_2 = TestCase $ assertEqual
    "check [[ λx → E ]] = λκ → (κ λx → [[ E ]])"
    expectedResult
    (Q5.cpsTransform (Q5.LamDef [] (Q5.LamAbs 1 (Q5.LamAbs 2 (Q5.LamVar 1)))))
  where
    expectedResult = Q5.LamDef [] (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 1 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 2 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1)))))))))

----- Test case 3 [[ (E1 E2) ]] = λκ → ( [[ E1 ]]  λf → ( [[ E2 ]]  λe → (f e κ) ) ):
testCpsTransform_Rule_3 = TestCase $ assertEqual
    "check [[ (E1 E2) ]] = λκ → ( [[ E1 ]]  λf → ( [[ E2 ]]  λe → (f e κ) ) )"
    expectedResult
    (Q5.cpsTransform (Q5.LamDef [] (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 3))))
  where
    expectedResult = Q5.LamDef [] (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 2))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 3))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))

----- Test case 4 [[ def X = E in ME ]]  = def X = [[ E ]]  in  [[ ME ]]
testCpsTransform_Rule_4 = TestCase $ assertEqual
    "check [[ def X = E in ME ]]  = def X = [[ E ]]  in  [[ ME ]]"
    expectedResult
    (Q5.cpsTransform (Q5.LamDef [] (Q5.LamVar 1)))
  where
    expectedResult = Q5.LamDef [] (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1)))

----- Test case 5 [[ X ]] = X
testCpsTransform_Rule_5 = TestCase $ assertEqual
    "check [[ X ]] = X"
    expectedResult
    (Q5.cpsTransform (Q5.LamDef [("X", Q5.LamVar 1)] (Q5.LamMacro "X")))
  where
    expectedResult = Q5.LamDef [("X",Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1)))] (Q5.LamMacro "X")

----- Test case 6 Combined rules
testCpsTransform_Rules_Combined = TestCase $ assertEqual
    "check combined rules"
    expectedResult
    (Q5.cpsTransform (Q5.cpsTransform (Q5.LamDef [("A", Q5.LamAbs 1 (Q5.LamVar 2)), ("B", Q5.LamApp (Q5.LamMacro "A") (Q5.LamMacro "C")), ("C", Q5.LamAbs 3 (Q5.LamApp (Q5.LamMacro "A") (Q5.LamVar 3)))] (Q5.LamAbs 4 (Q5.LamApp (Q5.LamAbs 5 (Q5.LamApp (Q5.LamMacro "B") (Q5.LamVar 4))) (Q5.LamMacro "C"))))))
  where
    expectedResult = Q5.LamDef [("A",Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 1 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 2))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))),("B",Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamMacro "A") (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 1 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamMacro "C") (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 2 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 2))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))),("C",Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 3 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamMacro "A") (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 1 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 3))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 2 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 2))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))))))] (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 4 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 5 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamMacro "B") (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 1 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 0 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 4))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 2 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 2))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 1 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamMacro "C") (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamAbs 2 (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 1))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 2))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))) (Q5.LamAbs 1 (Q5.LamApp (Q5.LamAbs 0 (Q5.LamApp (Q5.LamVar 0) (Q5.LamVar 0))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0)))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))))))))) (Q5.LamAbs 2 (Q5.LamApp (Q5.LamApp (Q5.LamVar 1) (Q5.LamVar 2)) (Q5.LamVar 0))))))))))


-- Q6
{-
    Techniques:
        - Use provided test cases
-}

testCompareInnerOuter_1 = TestCase $ assertEqual
    "test case 1"
    expectedResult
    (compareInnerOuter (LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))) 10)
  where
    expectedResult = (Just 0,Just 0,Just 6,Just 6)

testCompareInnerOuter_2 = TestCase $ assertEqual
    "test case 2"
    expectedResult
    (compareInnerOuter (LamDef [ ("F",LamAbs 1 (LamVar 1)) ] (LamMacro "F")) 10)
  where
    expectedResult = (Just 1,Just 1,Just 3,Just 3)

testCompareInnerOuter_3 = TestCase $ assertEqual
    "test case 3"
    expectedResult
    (compareInnerOuter (LamDef [] ( LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2)))) 10)
  where
    expectedResult = (Just 1,Just 1,Just 8,Just 8)

testCompareInnerOuter_4 = TestCase $ assertEqual
    "test case 4"
    expectedResult
    (compareInnerOuter (LamDef [] (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))) 100)
  where
    expectedResult = (Nothing,Nothing,Nothing,Nothing)

testCompareInnerOuter_5 = TestCase $ assertEqual
    "test case 5"
    expectedResult
    (compareInnerOuter (LamDef [ ("ID",LamAbs 1 (LamVar 1)) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))) 30)
  where
    expectedResult = (Just 4,Just 4,Just 22,Just 22)

testCompareInnerOuter_6 = TestCase $ assertEqual
    "test case 6"
    expectedResult
    (compareInnerOuter (LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamAbs 1 (LamVar 1)) (LamVar 4)))) 30)
  where
    expectedResult = (Just 4,Just 3,Just 21,Just 21)

testCompareInnerOuter_7 = TestCase $ assertEqual
    "test case 7"
    expectedResult
    (compareInnerOuter (LamDef [ ("ID",LamAbs 1 (LamVar 1)) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))) ) (LamMacro "ID") ) ) 1000)
  where
    expectedResult = (Nothing,Just 4,Nothing,Nothing)