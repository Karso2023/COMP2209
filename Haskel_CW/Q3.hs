module Q3 where
-- DO NOT MODIFY THESE DATATYPES

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Helper types to make the code more readable
type MacroEnv = [(String, LamExpr)]

-- Find macro with most specific (largest) match
findMacro :: MacroEnv -> LamExpr -> Maybe String
findMacro macros expr = foldr checkMacro Nothing macros
  where
    checkMacro (name, def) acc
        | def == expr = Just name
        | otherwise = acc

-- Helper to determine if parentheses are needed
needsParens :: MacroEnv -> LamExpr -> Bool
needsParens env expr = case findMacro env expr of
    Just _ -> False
    Nothing -> case expr of
        LamAbs _ _ -> True
        _ -> False

-- Main expression unparser
unparseExpr :: MacroEnv -> LamExpr -> String
unparseExpr env expr = case findMacro env expr of
    Just name -> name
    Nothing -> case expr of
        LamVar n -> "x" ++ show n
        LamMacro s -> s
        LamAbs n e -> "λx" ++ show n ++ "→" ++ unparseExpr env e
        LamApp e1 e2 -> 
            let s1 = unparseExpr env e1
                s2 = unparseExpr env e2
            in if needsParens env e1
               then "(" ++ s1 ++ ")" ++ s2
               else s1 ++ s2

-- Format a single macro definition
formatMacro :: (String, LamExpr) -> String
formatMacro (name, def) = name ++ "=" ++ unparseExpr [] def

-- Main unparse function
unparse :: LamMacroExpr -> String
unparse (LamDef macros expr) = case macros of
    [] -> unparseExpr [] expr
    _  -> "def" ++ concatMap formatMacro macros ++ "in" ++ unparseExpr macros expr