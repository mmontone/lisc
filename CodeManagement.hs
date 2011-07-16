-- this module implements 'dead code',simple reduction, AND-OR chains,
-- simple redundancy, instructions re-organization and other minor code optimizations
module CodeManagement where

import Types
import Maybe

optimizeCode = optimizeProc newChain

newChain = []
addConst (Const id value _)  consts = (id,value):consts

makeChain = foldr (\(Const id exp _) consts -> (id,exp):consts) []

optimizeProc chain (Proc id params decs stmts ln) =
        let newChain = makeChain (filter isConst decs) ++ chain
            newDecs = optimizeProcs decs newChain
        in  Proc id params newDecs (optimizeStmts newChain stmts) ln

optimizeProcs [] chain = []
optimizeProcs (x@(Proc _ _ _ _ _):xs) chain = optimizeProc chain x: optimizeProcs xs chain
optimizeProcs (x:xs)                chain = x                   : optimizeProcs xs chain

optimizeStmts chain = concatMap (optimizeStmt chain)

optimizeStmt chain (Assign id ex ln) = [Assign id (eval chain ex) ln]
optimizeStmt chain (IfThenElse ex body1 body2 ln) =
                let body1' = optimizeStmts chain body1
                    body2' = optimizeStmts chain body2
                in case eval chain ex of 
                      CBool True  _->  body1'
                      CBool False _-> body2'
                      b -> [IfThenElse b body1' body2' ln]
optimizeStmt chain (IfThen ex body ln) =
                        let body' = optimizeStmts chain body
                        in case eval chain ex of 
                             CBool False _ -> []
                             CBool True _ -> body'
                             b -> [IfThen b body' ln]
optimizeStmt chain (WhileDo ex body ln) = case eval chain ex of
                                             CBool False _ -> []
                                             b -> [WhileDo b (optimizeStmts chain body) ln]
optimizeStmt chain x@(Write ex ln) = [x] --debería haber una referencia acá y no el id
optimizeStmt chain x@(WriteNum ex ln) = [x] 
optimizeStmt chain x@(WriteLn ln) = [x] 
optimizeStmt chain x@(Call id params ln) = [Call id (optimizeParams chain params) ln]


optimizeParams chain = map (eval chain)

deep = foldExp (const 1) (+) id

fixpoint f x = let x' = f x 
               in if x==x' then x else fixpoint f x' 

eval chain = swapAndOptimize . fixpoint (flip reduce chain . liftConstants)

--using conmutative propertie tries to reduce constant numerical expresions
liftConstants (And t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                               in And t1' t2' ln
liftConstants (Or t1 t2 ln) = let t1' = liftConstants t1
                                  t2' = liftConstants t2
                              in Or t1' t2' ln
liftConstants  (Not e ln)   = Not (liftConstants e) ln
liftConstants (Eq t1 t2 ln) = let t1' = liftConstants t1
                                  t2' = liftConstants t2
                              in Eq t1' t2' ln
liftConstants (NEq t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                               in NEq t1' t2' ln
liftConstants (Lt t1 t2 ln) = let t1' = liftConstants t1
                                  t2' = liftConstants t2
                              in Lt t1' t2' ln
liftConstants (LEq t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                               in LEq t1' t2' ln
liftConstants (Gt t1 t2 ln) = let t1' = liftConstants t1
                                  t2' = liftConstants t2
                              in Gt t1' t2' ln
liftConstants (GEq t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                               in GEq t1' t2' ln
liftConstants (Add t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                               in case t1' of
                                   CFloat _ _                   -> Add t2' t1' ln
                                   CInt _ _                     -> Add t2' t1' ln 
                                   Add t11 t12@(CInt _ _) ln'   -> case t2' of 
                                                                    CInt _ _ -> Add t11 (Add t12 t2' ln') ln
                                                                    _        -> Add (Add t11 t2' ln') t12 ln
                                   Add t11 t12@(CFloat _ _) ln' -> case t2' of 
                                                                    CFloat _ _ -> Add t11 (Add t12 t2' ln') ln
                                                                    _          -> Add (Add t11 t2' ln') t12 ln
                                   _                            -> Add t1' t2' ln
liftConstants (Sub t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                               in Sub t1' t2' ln -- non-conmmutative
liftConstants (Mul t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                               in case t1' of
                                   CFloat _ _                   -> Mul t2' t1' ln
                                   CInt _ _                     -> Mul t2' t1' ln 
                                   Mul t11 t12@(CInt _ _) ln'   -> case t2' of 
                                                                    CInt _ _ -> Mul t11 (Mul t12 t2' ln') ln
                                                                    _        -> Mul (Mul t11 t2' ln') t12 ln
                                   Mul t11 t12@(CFloat _ _) ln' -> case t2' of 
                                                                   CFloat _ _ -> Mul t11 (Mul t12 t2' ln') ln
                                                                   _          -> Mul (Mul t11 t2' ln') t12 ln
                                   _                            -> Mul t1' t2' ln
liftConstants (Div t1 t2 ln) = let t1' = liftConstants t1
                                   t2' = liftConstants t2
                              in Div t1' t2' ln --non-conmmutative
liftConstants (Func id params ln) =
                              let params' = map liftConstants params
                              in Func id params' ln
liftConstants  x             = x

--swapAndOptimize --- instructions re-organization
swapAndOptimize (And t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                 in if deep t1' < deep t2' then And t2' t1' ln else And t1' t2' ln
swapAndOptimize (Or t1 t2 ln) = let t1' = swapAndOptimize t1
                                    t2' = swapAndOptimize t2
                                in if deep t1' < deep t2' then Or t2' t1' ln else Or t1' t2' ln
swapAndOptimize (Not e ln)    = Not (swapAndOptimize e) ln
swapAndOptimize (Eq t1 t2 ln) = let t1' = swapAndOptimize t1
                                    t2' = swapAndOptimize t2
                                in if deep t1' < deep t2' then Eq t2' t1' ln else Eq t1' t2' ln
swapAndOptimize (NEq t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                 in if deep t1' < deep t2' then NEq t2' t1' ln else NEq t1' t2' ln
swapAndOptimize (Lt t1 t2 ln) = let t1' = swapAndOptimize t1
                                    t2' = swapAndOptimize t2
                                in if deep t1' < deep t2' then Lt t2' t1' ln else Lt t1' t2' ln
swapAndOptimize (LEq t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                 in if deep t1' < deep t2' then LEq t2' t1' ln else LEq t1' t2' ln
swapAndOptimize (Gt t1 t2 ln) = let t1' = swapAndOptimize t1
                                    t2' = swapAndOptimize t2
                                in if deep t1' < deep t2' then Gt t2' t1' ln else Gt t1' t2' ln
swapAndOptimize (GEq t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                 in if deep t1' < deep t2' then GEq t2' t1' ln else GEq t1' t2' ln
swapAndOptimize (Add t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                 in if deep t1' < deep t2' then Add t2' t1' ln else Add t1' t2' ln
swapAndOptimize (Sub t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                 in Sub t1' t2' ln
swapAndOptimize (Mul t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                 in if deep t1' < deep t2' then Mul t2' t1' ln else Mul t1' t2' ln
swapAndOptimize (Div t1 t2 ln) = let t1' = swapAndOptimize t1
                                     t2' = swapAndOptimize t2
                                in Div t1' t2' ln
swapAndOptimize (Func id params ln) =
                let params' = map swapAndOptimize params
                in Func id params' ln
swapAndOptimize  x             = x



--eval
reduce x@(Id id ln) chain = case lookup id chain of
                              Nothing -> x
                              Just value -> value
reduce (And e1 e2 ln) chain = let x = reduce e1 chain
                                  y = reduce e2 chain
                              in case (x,y) of
                                 (CBool False _,_) -> CBool False ln
                                 (CBool True _,_) -> y
                                 (_,CBool False _) -> CBool False ln
                                 (_,CBool True _) -> x
                                 (CBool b1 _,CBool b2 _) -> CBool (b1 && b2) ln
                                 _           -> And x y ln
reduce (Or e1 e2 ln) chain = let x = reduce e1 chain
                                 y = reduce e2 chain
                             in case (x,y) of
                                (CBool True _,_) -> CBool True ln
                                (CBool False _,_) -> y
                                (_,CBool True _) -> CBool True ln
                                (_,CBool False _) -> x
                                (CBool b1 _,CBool b2 _) -> CBool (b1 || b2) ln
                                _            -> Or x y ln
reduce (Not e ln) chain = let x = reduce e chain
                        in case x of
                            CBool b ln1 -> CBool (not b) ln
                            _           -> Not x ln
reduce (Eq e1 e2 ln) chain = let x = reduce e1 chain
                                 y = reduce e2 chain
                             in case (x,y) of
                                (CBool b1 _,CBool b2 _)     -> CBool (b1 == b2) ln
                                (CFloat f1 _,CFloat f2 _)   -> CBool (f1 == f2) ln
                                (CString s1 _,CString s2 _) -> CBool (s1 == s2) ln
                                (CInt i1 _,CInt i2 _)       -> CBool (i1 == i2) ln
                                (Id a _, Id b _)            -> CBool (a == b)   ln
                                (_,_)                       -> Eq x y ln
reduce (NEq e1 e2 ln) chain =
           case reduce (Eq (reduce e1 chain) (reduce e2 chain) ln) chain of
                CBool b ln -> CBool (not b) ln
                Eq e1' e2' ln -> NEq e1' e2' ln
reduce (Lt e1 e2 ln) chain = let x = reduce e1 chain
                                 y = reduce e2 chain
                             in case (x,y) of
                                (CBool b1 _,CBool b2 _)     -> CBool (b1 < b2) ln
                                (CFloat f1 _,CFloat f2 _)   -> CBool (f1 < f2) ln
                                (CString s1 _,CString s2 _) -> CBool (s1 < s2) ln
                                (CInt i1 _,CInt i2 _)       -> CBool (i1 < i2) ln
                                (_,_)                       -> Lt x y ln
reduce (LEq e1 e2 ln) chain = let x = reduce e1 chain
                                  y = reduce e2 chain
                              in case (x,y) of
                                 (CBool b1 _,CBool b2 _)     -> CBool (b1 <= b2) ln
                                 (CFloat f1 _,CFloat f2 _)   -> CBool (f1 <= f2) ln
                                 (CString s1 _,CString s2 _) -> CBool (s1 <= s2) ln
                                 (CInt i1 _,CInt i2 _)       -> CBool (i1 <= i2) ln
                                 (_,_)                       -> LEq x y ln
reduce (Gt e1 e2 ln) chain = let x = reduce e1 chain
                                 y = reduce e2 chain
                             in case (x,y) of
                                 (CBool b1 _,CBool b2 _)     -> CBool (b1 > b2) ln
                                 (CFloat f1 _,CFloat f2 _)   -> CBool (f1 > f2) ln
                                 (CString s1 _,CString s2 _) -> CBool (s1 > s2) ln
                                 (CInt i1 _,CInt i2 _)       -> CBool (i1 > i2) ln
                                 (_,_)                       -> Gt x y ln
reduce (GEq e1 e2 ln) chain = let x = reduce e1 chain
                                  y = reduce e2 chain
                              in case (x,y) of
                                 (CBool b1 _,CBool b2 _)     -> CBool (b1 >= b2) ln
                                 (CFloat f1 _,CFloat f2 _)   -> CBool (f1 >= f2) ln
                                 (CString s1 _,CString s2 _) -> CBool (s1 >= s2) ln
                                 (CInt i1 _,CInt i2 _)       -> CBool (i1 >= i2) ln
                                 (_,_)                       -> GEq x y ln
reduce (Add e1 e2 ln) chain = let x = reduce e1 chain
                                  y = reduce e2 chain
                              in case (x,y) of
                                  (CInt 0 _,_)              -> y
                                  (_,CInt 0 _)              -> x
                                  (CInt i1 _,CInt i2 _)     -> CInt (i1 + i2) ln
                                  (CFloat 0_,_) -> y
                                  (_,CFloat 0.0 _) -> x
                                  (CFloat f1 _,CFloat f2 _) -> CFloat (f1 + f2) ln
                                  _             -> Add x y ln
reduce (Sub e1 e2 ln) chain = let x = reduce e1 chain
                                  y = reduce e2 chain
                              in case (x,y) of
                                  (CInt i1 _,CInt 0 _)     -> x
                                  (CInt i1 _,CInt i2 _)     -> CInt (i1 - i2) ln
                                  (CFloat f1 _,CFloat f2 _) -> CFloat (f1 - f2) ln
                                  (CFloat f1 _,CFloat 0.0 _) -> x
                                  _             -> Sub x y ln
reduce (Mul e1 e2 ln) chain = let x = reduce e1 chain
                                  y = reduce e2 chain
                              in case (x,y) of
                                  (CInt 0 _,_)              -> CInt 0 ln
                                  (_,CInt 0 _)              -> CInt 0 ln
                                  (CInt i1 _,CInt i2 _)     -> CInt (i1 * i2) ln
                                  (CFloat 0.0 _,_)          -> CFloat 0.0 ln
                                  (_,CFloat 0.0 _)          -> CFloat 0.0 ln
                                  (CFloat f1 _,CFloat f2 _) -> CFloat (f1 * f2) ln
                                  _             -> Mul x y ln
reduce (Div e1 e2 ln) chain = let x = reduce e1 chain
                                  y = reduce e2 chain
                              in case (x,y) of
                                  (CInt i1 _,CInt i2 _)     -> CInt (i1 `div` i2) ln --división por cero????
                                  (CFloat f1 _,CFloat f2 _) -> CFloat (f1 / f2) ln
                                  _             -> Div x y ln
reduce (Func id params ln) chain =
                let params' = map (flip reduce chain) params
                in Func id params' ln
reduce      x         _     = x