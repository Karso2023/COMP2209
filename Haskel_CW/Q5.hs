module Q5 where
-- Your imports here

-- DO NOT MODIFY THESE DATATYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)


cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef macros expr) = LamDef (map transformMacro macros) (cpsExpr expr)
  where
    transformMacro (name, expr) = (name, cpsExpr expr)

-- Helper function to transform individual LamExpr
cpsExpr :: LamExpr -> LamExpr
cpsExpr expr = case expr of
    -- Rule: [[ x ]] = λ κ → (κ x)
    LamVar x -> 
        LamAbs 0 (LamApp (LamVar 0) (LamVar x))
    
    -- Rule: [[ X ]] = X for macros
    LamMacro name -> 
        LamMacro name
    
    -- Rule: [[ λx → E ]] = λκ → (κ λx → [[ E ]])
    LamAbs x body -> 
        LamAbs 0 (LamApp (LamVar 0) (LamAbs x (cpsExpr body)))
    
    -- Rule: [[ (E1 E2) ]] = λκ → ( [[ E1 ]] λf → ( [[ E2 ]] λe → (f e κ) ) )
    LamApp e1 e2 -> 
        LamAbs 0 (                           -- λκ →
            LamApp 
                (cpsExpr e1)                 -- [[ E1 ]]
                (LamAbs 1                    -- λf →
                    (LamApp 
                        (cpsExpr e2)         -- [[ E2 ]]
                        (LamAbs 2            -- λe →
                            (LamApp 
                                (LamApp (LamVar 1) (LamVar 2))  -- (f e)
                                (LamVar 0)                      -- κ
                            )
                        )
                    )
                )
            )