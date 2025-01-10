{-|
  Module      : COMP2209 Lambda Macro Parser
  Description : Parser for Lambda Expressions with Macro Definitions
  Copyright   : (c) 2025 University of Southampton
  Author      : Sze Long Cheung, Karso

  This module implements a parser for lambda expressions with macro definitions.
  It constructs abstract syntax trees while ensuring:
    1. Unique macro definitions within terms
    2. Closed terms in macro definitions (no free variables)
    3. Whitespace-independent parsing
-}

module Q4 where
-- Your imports here
import Data.Char (isSpace)
import qualified Data.Set as Set
import Control.Monad (guard)

-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Main parsing function
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro = parse . filter (not . isSpace)
  where
    -- Parse full expression with definitions
    parse s = do
        (defs, expr, rest) <- parseDef s
        guard (null rest)
        return $ LamDef defs expr

    -- Handle macro definitions
    parseDef ('d':'e':'f':s) = do
        (name, '=':s1) <- parseName s
        (expr, s2) <- parseExpr s1
        guard (Set.null $ getFreeVars expr)  -- Check for closed terms
        ('i':'n':s3) <- return s2
        (defs, final, rest) <- parseDef s3
        guard (name `notElem` map fst defs)  -- Check unique names
        return ((name, expr):defs, final, rest)
    parseDef s = do
        (expr, rest) <- parseExpr s
        return ([], expr, rest)

    -- Parse expressions and applications
    parseExpr s = do
        (first, rest) <- parseAtom s
        parseApp first rest
      where
        parseApp left [] = Just (left, [])
        parseApp left s@(c:_) | c `elem` ")in" = Just (left, s)
        parseApp left s = case parseAtom s of
            Nothing -> Just (left, s)
            Just (next, rest) -> parseApp (LamApp left next) rest

    -- Parse basic expressions
    parseAtom ('(':s) = do
        (expr, ')':rest) <- parseExpr s
        return (expr, rest)
    parseAtom ('λ':'x':s) = do
        (num, '→':rest) <- parseNum s
        (body, s1) <- parseExpr rest
        return (LamAbs num body, s1)
    parseAtom ('x':s) = do
        (num, rest) <- parseNum s
        return (LamVar num, rest)
    parseAtom s@(c:_) | c >= 'A' && c <= 'Z' = do
        (name, rest) <- parseName s
        return (LamMacro name, rest)
    parseAtom _ = Nothing

    -- Helper functions for parsing components
    parseName s = 
        let (name, rest) = span (\c -> c >= 'A' && c <= 'Z') s
        in if null name then Nothing else Just (name, rest)

    parseNum s = 
        let (num, rest) = span (\c -> c >= '0' && c <= '9') s
        in if null num then Nothing else Just (read num, rest)

    -- Calculate free variables in expressions
    getFreeVars expr = case expr of
        LamVar n -> Set.singleton n
        LamApp e1 e2 -> getFreeVars e1 `Set.union` getFreeVars e2
        LamAbs n e -> Set.delete n (getFreeVars e)
        LamMacro _ -> Set.empty