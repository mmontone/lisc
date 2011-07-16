--this module evaluates every expression of constant initialization
module ConstantsManagement where

import Types
import Maybe

-- constants optimization function
optimizeConstants = optimizeC newTable

newTable = []
addConst (Const id value _) consts = (id,value):consts

optimizeC chain []                         = []
optimizeC chain (x@(Const _ _ _):decs) =
        let newConstDec = evalConst chain x
            newId       = constId newConstDec
            chain'      = addConst newConstDec (removeTopConst newId chain)
        in newConstDec:optimizeC chain' decs
optimizeC chain (p@(Proc _ _ _ _ _):decs) = optimizeSubProc chain p:optimizeC chain decs
optimizeC chain (            x:decs)    = x:optimizeC chain decs

optimizeSubProc chain (Proc id params decs stmts ln) = Proc id params (optimizeC chain decs) stmts ln

removeTopConst idConst [] = []
removeTopConst idConst ((id,exp):chain) =
    if idConst == id then chain else ((id,exp):removeTopConst idConst chain)

evalConst chain (Const id exp ln) = case eval exp chain of 
                                      y@(Id ide _) -> ConstRef id (Const ide (fromJust(lookup ide chain)) 0) ln
                                      x -> Const id x ln
evalConst _     x                 = x

eval x@(Id id ln) chain = case lookup id chain of
                              Nothing -> error $ "Error: En la linea " ++ show ln ++ ": se intenta usar variable '" ++ id ++ "' para asignar a constante"
                              Just value -> case eval value chain of
                                              CString _ _ -> x -- if the referenced constant holds a string value, it creates a reference
                                              y -> y
eval (And e1 e2 ln) chain = let x = eval e1 chain
                                y = eval e2 chain
                            in case (x,y) of
                                (CBool b1 _,CBool b2 _) -> CBool (b1 && b2) ln
                                _           -> And x y ln
eval (Or e1 e2 ln) chain = let x = eval e1 chain
                               y = eval e2 chain
                           in case (x,y) of
                               (CBool b1 _,CBool b2 _) -> CBool (b1 || b2) ln
                               _            -> Or x y ln
eval (Not e ln) chain = let x = eval e chain
                        in case x of
                            CBool b ln1 -> CBool (not b) ln
                            _           -> Not x ln
eval (Eq e1 e2 ln) chain = let x = eval e1 chain
                               y = eval e2 chain
                           in case (x,y) of
                               (CBool b1 _,CBool b2 _)     -> CBool (b1 == b2) ln
                               (CFloat f1 _,CFloat f2 _)   -> CBool (f1 == f2) ln
                               (CString s1 _,CString s2 _) -> CBool (s1 == s2) ln
                               (CInt i1 _,CInt i2 _)       -> CBool (i1 == i2) ln
                               (Id a _, Id b _)            -> CBool (a == b)   ln
                               (_,_)                       -> Eq x y ln
eval (NEq e1 e2 ln) chain = case eval (Eq (eval e1 chain) (eval e2 chain) ln) chain of
                                  CBool b ln -> CBool (not b) ln
                                  Eq e1' e2' ln -> NEq e1' e2' ln
eval (Lt e1 e2 ln) chain = let x = eval e1 chain
                               y = eval e2 chain
                           in case (x,y) of
                               (CBool b1 _,CBool b2 _)     -> CBool (b1 < b2) ln
                               (CFloat f1 _,CFloat f2 _)   -> CBool (f1 < f2) ln
                               (CString s1 _,CString s2 _) -> CBool (s1 < s2) ln
                               (CInt i1 _,CInt i2 _)       -> CBool (i1 < i2) ln
                               (_,_)                       -> Lt x y ln
eval (LEq e1 e2 ln) chain = let x = eval e1 chain
                                y = eval e2 chain
                           in case (x,y) of
                               (CBool b1 _,CBool b2 _)     -> CBool (b1 <= b2) ln
                               (CFloat f1 _,CFloat f2 _)   -> CBool (f1 <= f2) ln
                               (CString s1 _,CString s2 _) -> CBool (s1 <= s2) ln
                               (CInt i1 _,CInt i2 _)       -> CBool (i1 <= i2) ln
                               (_,_)                       -> LEq x y ln
eval (Gt e1 e2 ln) chain = let x = eval e1 chain
                               y = eval e2 chain
                           in case (x,y) of
                               (CBool b1 _,CBool b2 _)     -> CBool (b1 > b2) ln
                               (CFloat f1 _,CFloat f2 _)   -> CBool (f1 > f2) ln
                               (CString s1 _,CString s2 _) -> CBool (s1 > s2) ln
                               (CInt i1 _,CInt i2 _)       -> CBool (i1 > i2) ln
                               (_,_)                       -> Gt x y ln
eval (GEq e1 e2 ln) chain = let x = eval e1 chain
                                y = eval e2 chain
                           in case (x,y) of
                               (CBool b1 _,CBool b2 _)     -> CBool (b1 >= b2) ln
                               (CFloat f1 _,CFloat f2 _)   -> CBool (f1 >= f2) ln
                               (CString s1 _,CString s2 _) -> CBool (s1 >= s2) ln
                               (CInt i1 _,CInt i2 _)       -> CBool (i1 >= i2) ln
                               (_,_)                       -> GEq x y ln
eval (Add e1 e2 ln) chain = let x = eval e1 chain
                                y = eval e2 chain
                            in case (x,y) of
                                (CInt i1 _,CInt i2 _)     -> CInt (i1 + i2) ln
                                (CFloat f1 _,CFloat f2 _) -> CFloat (f1 + f2) ln
                                _             -> Add x y ln
eval (Sub e1 e2 ln) chain = let x = eval e1 chain
                                y = eval e2 chain
                            in case (x,y) of
                                (CInt i1 _,CInt i2 _)     -> CInt (i1 - i2) ln
                                (CFloat f1 _,CFloat f2 _) -> CFloat (f1 - f2) ln
                                _             -> Sub x y ln
eval (Mul e1 e2 ln) chain = let x = eval e1 chain
                                y = eval e2 chain
                            in case (x,y) of
                                (CInt i1 _,CInt i2 _)     -> CInt (i1 * i2) ln
                                (CFloat f1 _,CFloat f2 _) -> CFloat (f1 * f2) ln
                                _             -> Mul x y ln
eval (Div e1 e2 ln) chain = let x = eval e1 chain
                                y = eval e2 chain
                            in case (x,y) of
                                (CInt i1 _,CInt i2 _)     -> CInt (i1 `div` i2) ln
                                (CFloat f1 _,CFloat f2 _) -> CFloat (f1 / f2) ln
                                _             -> Div x y ln
eval      x         _     = x