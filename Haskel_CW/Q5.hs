{-|
  Module      : COMP2209 Q5
  Copyright   : (c) 2025 University of Southampton
  Author      : Karso Cheung 
  Description :
  Write a function that translates a lambda expression with macros in to its corresponding CPS translated form.
  
  My code's logic stpes: 
    Convert the four rules into Haskell codes:
        1, [[ x ]] = λ κ → (κ x)
        2, [[ X ]] = X (extended)
        3, [[ λx → E ]] = λκ → (κ λx → [[ E ]])
        4, [[ (E1 E2) ]] = λκ → ( [[ E1 ]]  λf → ( [[ E2 ]]  λe → (f e κ) ) )
        
-}
-- module Q5 where (for Tests.hs)
-- Your imports here

-- DO NOT MODIFY THESE DATATYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- Main function
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef macros expr) = LamDef (map transformMacro macros) (cpsExpr expr)
  where
    transformMacro (name, expr) = (name, cpsExpr expr)

-- Helper function to for different transform's rules
cpsExpr :: LamExpr -> LamExpr
cpsExpr expr = case expr of
    -- Rule: [[ x ]] = λ κ → (κ x)
    LamVar x -> 
        LamAbs 0 (LamApp (LamVar 0) (LamVar x))
    
    -- Rule: [[ X ]] = X 
    LamMacro name -> 
        LamMacro name
    
    -- Rule: [[ λx → E ]] = λκ → (κ λx → [[ E ]])
    LamAbs x body -> 
        LamAbs 0 (LamApp (LamVar 0) (LamAbs x (cpsExpr body)))
    
    -- Rule: [[ (E1 E2) ]] = λκ → ( [[ E1 ]] λf → ( [[ E2 ]] λe → (f e κ) ) )
    LamApp e1 e2 -> 
        LamAbs 0 (                           
            LamApp 
                (cpsExpr e1)                 
                (LamAbs 1                    
                    (LamApp 
                        (cpsExpr e2)         
                        (LamAbs 2            
                            (LamApp 
                                (LamApp (LamVar 1) (LamVar 2))  
                                (LamVar 0)                      
                            )
                        )
                    )
                )
            )
