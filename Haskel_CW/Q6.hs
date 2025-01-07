module Q6 where
-- Your imports here
import Data.Maybe (isJust, isNothing)
import Data.List (find)
-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Helper function to substitute a term for a variable
substitute :: Int -> LamExpr -> LamExpr -> LamExpr 
substitute v term (LamVar x)
    | x == v = term
    | otherwise = LamVar x
substitute v term (LamApp e1 e2) = 
    LamApp (substitute v term e1) (substitute v term e2)
substitute v term (LamAbs x body)
    | x == v = LamAbs x body  -- Variable is bound, don't substitute
    | otherwise = LamAbs x (substitute v term body)
substitute _ _ (LamMacro m) = LamMacro m

-- Find macro definition
lookupMacro :: String -> [(String, LamExpr)] -> Maybe LamExpr
lookupMacro name defs = lookup name defs

-- Check if expression is a value (cannot be reduced further)
isValue :: LamExpr -> Bool
isValue (LamAbs _ _) = True
isValue (LamVar _) = True
isValue _ = False

-- Find innermost redex
findInnerRedex :: [(String, LamExpr)] -> LamExpr -> Maybe (LamExpr -> LamExpr, LamExpr)
findInnerRedex defs expr = case expr of
    LamMacro m -> case lookupMacro m defs of
        Just def -> Just (id, def)
        Nothing -> Nothing
    LamApp (LamAbs v body) arg -> 
        if isValue arg 
        then Just (id, substitute v arg body)
        else case findInnerRedex defs arg of
            Just (ctx, redex) -> Just (\x -> LamApp (LamAbs v body) (ctx x), redex)
            Nothing -> Nothing
    LamApp e1 e2 -> 
        case findInnerRedex defs e2 of
            Just (ctx, redex) -> Just (\x -> LamApp e1 (ctx x), redex)
            Nothing -> case findInnerRedex defs e1 of
                Just (ctx, redex) -> Just (\x -> LamApp (ctx x) e2, redex)
                Nothing -> Nothing
    LamAbs v body -> case findInnerRedex defs body of
        Just (ctx, redex) -> Just (\x -> LamAbs v (ctx x), redex)
        Nothing -> Nothing
    LamVar _ -> Nothing

-- Find outermost redex
findOuterRedex :: [(String, LamExpr)] -> LamExpr -> Maybe (LamExpr -> LamExpr, LamExpr)
findOuterRedex defs expr = case expr of
    LamMacro m -> case lookupMacro m defs of
        Just def -> Just (id, def)
        Nothing -> Nothing
    LamApp e1 e2 -> case e1 of
        LamAbs v body -> Just (id, substitute v e2 body)
        _ -> case findOuterRedex defs e1 of
            Just (ctx, redex) -> Just (\x -> LamApp (ctx x) e2, redex)
            Nothing -> case findOuterRedex defs e2 of
                Just (ctx, redex) -> Just (\x -> LamApp e1 (ctx x), redex)
                Nothing -> Nothing
    LamAbs v body -> case findOuterRedex defs body of
        Just (ctx, redex) -> Just (\x -> LamAbs v (ctx x), redex)
        Nothing -> Nothing
    LamVar _ -> Nothing

-- Single step reduction functions
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 (LamDef defs expr) = case findInnerRedex defs expr of
    Just (ctx, redex) -> Just $ LamDef defs (ctx redex)
    Nothing -> Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef defs expr) = case findOuterRedex defs expr of
    Just (ctx, redex) -> Just $ LamDef defs (ctx redex)
    Nothing -> Nothing

-- CPS transformation
cpsTransform :: LamExpr -> LamExpr
cpsTransform expr = case expr of
    LamVar x -> LamAbs 1000 (LamApp (LamVar 1000) (LamVar x))
    LamAbs v body -> LamAbs 1000 (LamApp (LamVar 1000) 
                                        (LamAbs v (cpsTransform body)))
    LamApp e1 e2 -> LamAbs 1000 (LamApp (cpsTransform e1)
                                       (LamAbs 2000 (LamApp (cpsTransform e2)
                                                          (LamAbs 3000 (LamApp (LamApp (LamVar 2000) 
                                                                                     (LamVar 3000))
                                                                             (LamVar 1000))))))
    LamMacro m -> LamAbs 1000 (LamApp (LamVar 1000) (LamMacro m))

-- Apply CPS transformation to entire expression
cpsTransformLamMacro :: LamMacroExpr -> LamMacroExpr
cpsTransformLamMacro (LamDef defs expr) = 
    LamDef (map (\(n,e) -> (n, cpsTransform e)) defs)
           (LamApp (cpsTransform expr) (LamAbs 0 (LamVar 0)))

-- Count reduction steps up to bound
countSteps :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Int -> Maybe Int
countSteps redn expr bound = go expr 0
    where
        go e count
            | count > bound = Nothing
            | isNothing (redn e) = Just count
            | otherwise = go (maybe e id (redn e)) (count + 1)

-- Main comparison function
compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
compareInnerOuter expr bound = 
    let cpsExpr = cpsTransformLamMacro expr
        inner = countSteps innerRedn1 expr bound
        outer = countSteps outerRedn1 expr bound
        innerCPS = countSteps innerRedn1 cpsExpr bound
        outerCPS = countSteps outerRedn1 cpsExpr bound
    in (inner, outer, innerCPS, outerCPS)