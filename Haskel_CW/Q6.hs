{-|
  Module      : COMP2209 Q6
  Copyright   : (c) 2025 University of Southampton
  Author      : Karso Cheung 
  Description :
  Write a function that takes a lambda expression with macros and a positive integer bound 
  and returns a 4-tuple containing the length of different reduction sequences up to a given maximum length.
  
  Some properties:
    -- 1000: The continuation parameter in the outer lambda abstraction 
    -- 2000: Continuation for function application 
    -- 3000: Continuation for argument application 
    -- 0: Final continuation
    
-}
-- module Q6 where (for Tests.hs)
-- Your imports here
import Data.Maybe (isJust, isNothing)
import Data.List (find)
-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Main comparison function
--   1. Innermost reduction on original expression
--   2. Outermost reduction on original expression  
--   3. Innermost reduction on CPS transformed expression
--   4. Outermost reduction on CPS transformed expression
compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
compareInnerOuter expr bound = 
    let cpsExpr = cpsTransformLamMacro expr
        inner = countSteps innerRedn1 expr bound
        outer = countSteps outerRedn1 expr bound
        innerCPS = countSteps innerRedn1 cpsExpr bound
        outerCPS = countSteps outerRedn1 cpsExpr bound
    in (inner, outer, innerCPS, outerCPS)
    

-- Handles variable capture by preserving bound variables
substitute :: Int -> LamExpr -> LamExpr -> LamExpr 
substitute var term expr = case expr of
    LamVar x     | x == var  -> term
                 | otherwise -> LamVar x
    LamApp e1 e2 -> LamApp (substitute var term e1) (substitute var term e2)
    LamAbs x body | x == var  -> LamAbs x body  -- Preserve bound variables
                  | otherwise -> LamAbs x (substitute var term body)
    LamMacro m   -> LamMacro m

-- Check macro definition
lookupMacro :: String -> [(String, LamExpr)] -> Maybe LamExpr
lookupMacro = lookup

-- Check if expression is a value
isValue :: LamExpr -> Bool
isValue (LamAbs _ _) = True
isValue (LamVar _)   = True
isValue _            = False

-- Searches for innermost redex and performs single reduction step
findInnerRedex :: [(String, LamExpr)] -> LamExpr -> Maybe (LamExpr -> LamExpr, LamExpr)
findInnerRedex defs expr = case expr of
    -- Expand macros
    LamMacro m -> (\def -> (id, def)) <$> lookupMacro m defs
    
    -- Apply beta reduction if possible
    LamApp (LamAbs v body) arg | isValue arg -> Just (id, substitute v arg body)
    
    -- Otherwise search subexpressions
    LamApp e1 e2 -> case findInnerRedex defs e2 of
        Just (ctx, redex) -> Just (\x -> LamApp e1 (ctx x), redex)
        Nothing           -> (\(ctx, redex) -> (\x -> LamApp (ctx x) e2, redex)) 
                           <$> findInnerRedex defs e1
    
    LamAbs v body -> (\(ctx, redex) -> (\x -> LamAbs v (ctx x), redex)) 
                     <$> findInnerRedex defs body
    
    _ -> Nothing

-- Search for leftmost-outermost redex and performs single reduction step
findOuterRedex :: [(String, LamExpr)] -> LamExpr -> Maybe (LamExpr -> LamExpr, LamExpr)
findOuterRedex defs = findFirst
  where
    findFirst expr = case expr of
        LamMacro m -> (\def -> (id, def)) <$> lookupMacro m defs
        LamApp (LamAbs v body) arg -> Just (id, substitute v arg body)
        LamApp e1 e2 -> findFirstApp e1 e2
        LamAbs v body -> (\(ctx, redex) -> (\x -> LamAbs v (ctx x), redex)) 
                        <$> findFirst body
        _ -> Nothing
    
    findFirstApp e1 e2 = case findFirst e1 of
        Just (ctx, redex) -> Just (\x -> LamApp (ctx x) e2, redex)
        Nothing -> (\(ctx, redex) -> (\x -> LamApp e1 (ctx x), redex)) 
                  <$> findFirst e2

-- Single step reduction
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 (LamDef defs expr) = 
    (\(ctx, redex) -> LamDef defs (ctx redex)) <$> findInnerRedex defs expr

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef defs expr) = 
    (\(ctx, redex) -> LamDef defs (ctx redex)) <$> findOuterRedex defs expr

-- CPS transformation
cpsTransform :: LamExpr -> LamExpr
cpsTransform expr = case expr of
    LamVar x     -> mkCPS $ LamApp (LamVar 1000) (LamVar x)
    LamAbs v body -> mkCPS $ LamApp (LamVar 1000) (LamAbs v (cpsTransform body))
    LamApp e1 e2 -> mkCPS $ appCPS e1 e2
    LamMacro m   -> mkCPS $ LamApp (LamVar 1000) (LamMacro m)
  where
    mkCPS e = LamAbs 1000 e
    appCPS e1 e2 = LamApp (cpsTransform e1) 
                         (LamAbs 2000 (LamApp (cpsTransform e2)
                                            (mkAppCont)))
    mkAppCont = LamAbs 3000 (LamApp (LamApp (LamVar 2000) 
                                           (LamVar 3000))
                                   (LamVar 1000))

-- Applies CPS transformation to entire expression including macros
cpsTransformLamMacro :: LamMacroExpr -> LamMacroExpr
cpsTransformLamMacro (LamDef defs expr) = 
    LamDef (map (fmap cpsTransform) defs)
           (LamApp (cpsTransform expr) (LamAbs 0 (LamVar 0)))

-- Counting reduction steps
countSteps :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Int -> Maybe Int
countSteps redn expr bound = go expr 0
  where
    go e count | count > bound     = Nothing
               | isNothing (redn e) = Just count 
               | otherwise         = go (maybe e id (redn e)) (count + 1)
