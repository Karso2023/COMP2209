{-|
  Module      : COMP2209 Q3
  Copyright   : (c) 2025 University of Southampton
  Author      : Karso Cheung
  Description : 
  
  Write a function that accepts an Abstract Syntax Tree representing an expression of the lambda with macro definitions 
  and returns a string that unparses the tree to provide a source code representation of the expression

  Sources:
  1. Uppercase validation: https://www.reddit.com/r/haskell/comments/qn8ir2/how_do_i_check_whether_a_string_is_all_uppercase/
-}
-- module Q3 where (for Tests.hs)
import Data.Char (isUpper)

-- DO NOT MODIFY THESE DATATYPES

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Make the code look more structured and readable
type MacroEnv = [(String, LamExpr)]

-- Main unparse function for lambda expressions with macros
unparse :: LamMacroExpr -> String
unparse (LamDef [] expr) = unparseExpr [] expr
unparse (LamDef macros expr) = 
    let validMacros = validateAndProcessMacros macros
    in "def" ++ formatMacroDefinitions validMacros ++ "in" ++ unparseExpr validMacros expr
 
-- Validates and processes a list of macro definitions   
-- Ensures all macro names are uppercase
validateAndProcessMacros :: [(String, LamExpr)] -> [(String, LamExpr)]
validateAndProcessMacros [] = []
validateAndProcessMacros ((name, def):rest)
    | isUpper (head name) = (name, def) : validateAndProcessMacros rest
    | otherwise          = error "Macros Should Be in UpperCase"

-- Handles both single and multiple macro cases
formatMacroDefinitions :: [(String, LamExpr)] -> String
formatMacroDefinitions []     = "" -- empty case
formatMacroDefinitions [m]    = formatMacro m -- single case
formatMacroDefinitions (m:ms) = formatMacro m ++ "in" ++ formatMacroDefinitions ms -- multiple case

-- Main function to unparse a single lambda expression
unparseExpr :: MacroEnv -> LamExpr -> String
unparseExpr env expr = case findMacro env expr of
    Just name -> name
    Nothing   -> case expr of
        -- Simple definitions and formatting
        LamVar n      -> "x" ++ show n
        LamMacro s    -> validateMacroName s -- Check if it's in upper case form
        LamAbs n e    -> "λx" ++ show n ++ "→" ++ unparseExpr env e
        LamApp e1 e2  -> formatApplication env e1 e2
  where
    formatApplication env e1 e2 =
        let s1 = unparseExpr env e1
            s2 = unparseExpr env e2
        in if needsParens env e1
           then "(" ++ s1 ++ ")" ++ s2 -- Add paren if needed
           else s1 ++ s2
           
-- Formats a single macro definition
formatMacro :: (String, LamExpr) -> String
formatMacro (name, def) = validateAndFormat name (unparseExpr [] def)
  where
    validateAndFormat name body
        | isUpper (head name) = name ++ "=" ++ body
        | otherwise          = error "Macros Should Be in UpperCase"
        
-- Helper functions for unparseExpr
-- use isUpper to check if the given Macro is in upper case
-- if not upper case return an error message
validateMacroName :: String -> String
validateMacroName s 
    | isUpper (head s) = s
    | otherwise        = error "Macros Should Be in UpperCase"

-- Determines if parentheses are needed 
-- True = Macros need paren
-- False = Macros don't need paren
needsParens :: MacroEnv -> LamExpr -> Bool
needsParens env expr = case findMacro env expr of
    Just _  -> False 
    Nothing -> case expr of
        LamAbs _ _ -> True 
        _          -> False  
    
-- Return Just macro name if found, Nothing otherwise
findMacro :: MacroEnv -> LamExpr -> Maybe String
findMacro macros expr = foldr checkMacro Nothing macros
  where
    checkMacro (name, def) acc
        | def == expr = Just name
        | otherwise   = acc
