{-|
  Module      : COMP2209 Q3
  Copyright   : (c) 2025 University of Southampton
  Author      : Sze Long Cheung, Karso
  Description : Lambda calculus expression parser with macro support
  
  This module implements an unparser for lambda calculus expressions with macro definitions.
  It converts an Abstract Syntax Tree into a string representation, ensuring proper formatting
  and validation of macro names (must be uppercase).

  Sources:
  1. Uppercase validation: https://www.reddit.com/r/haskell/comments/qn8ir2/how_do_i_check_whether_a_string_is_all_uppercase/
-}
module Q3 where
    
import Data.Char (isUpper)

-- DO NOT MODIFY THESE DATATYPES

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Helper types to make the code more readable
type MacroEnv = [(String, LamExpr)]

-- | Finds the most specific macro match for a given expression
findMacro :: MacroEnv -> LamExpr -> Maybe String
findMacro macros expr = foldr checkMacro Nothing macros
  where
    checkMacro (name, def) acc
        | def == expr = Just name
        | otherwise   = acc

-- | Determines if parentheses are needed for a given expression
needsParens :: MacroEnv -> LamExpr -> Bool
needsParens env expr = case findMacro env expr of
    Just _  -> False
    Nothing -> case expr of
        LamAbs _ _ -> True
        _          -> False

-- | Main expression unparser
unparseExpr :: MacroEnv -> LamExpr -> String
unparseExpr env expr = case findMacro env expr of
    Just name -> name
    Nothing   -> case expr of
        LamVar n      -> "x" ++ show n
        LamMacro s    -> validateMacroName s
        LamAbs n e    -> "λx" ++ show n ++ "→" ++ unparseExpr env e
        LamApp e1 e2  -> formatApplication env e1 e2
  where
    formatApplication env e1 e2 =
        let s1 = unparseExpr env e1
            s2 = unparseExpr env e2
        in if needsParens env e1
           then "(" ++ s1 ++ ")" ++ s2
           else s1 ++ s2

-- | Formats a single macro definition
formatMacro :: (String, LamExpr) -> String
formatMacro (name, def) = validateAndFormat name (unparseExpr [] def)
  where
    validateAndFormat name body
        | isUpper (head name) = name ++ "=" ++ body
        | otherwise          = error "Macros Should Be in UpperCase"

-- | Main unparse function for lambda expressions with macros
unparse :: LamMacroExpr -> String
unparse (LamDef [] expr) = unparseExpr [] expr
unparse (LamDef macros expr) = 
    let validMacros = validateAndProcessMacros macros
    in "def" ++ formatMacroDefinitions validMacros ++ "in" ++ unparseExpr validMacros expr

-- | Helper functions for macro processing
validateMacroName :: String -> String
validateMacroName s 
    | isUpper (head s) = s
    | otherwise        = error "Macros Should Be in UpperCase"

validateAndProcessMacros :: [(String, LamExpr)] -> [(String, LamExpr)]
validateAndProcessMacros [] = []
validateAndProcessMacros ((name, def):rest)
    | isUpper (head name) = (name, def) : validateAndProcessMacros rest
    | otherwise          = error "Macros Should Be in UpperCase"

formatMacroDefinitions :: [(String, LamExpr)] -> String
formatMacroDefinitions []     = ""
formatMacroDefinitions [m]    = formatMacro m
formatMacroDefinitions (m:ms) = formatMacro m ++ "in" ++ formatMacroDefinitions ms