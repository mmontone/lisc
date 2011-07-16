module Namemangling where

import Types
import Data.Char
import Control.Monad.State
import Data.List

type Namet = State Declaration 

namemangling::Program -> Program
namemangling (Program (Proc id params decl stmt ln)) = let name = "_main_" ++ changeName id
                                                       in Program ( Proc name 
                                                                         (map changeParam params) 
                                                                         (map (changeDecl name) decl)
                                                                         (map (changeStmt name) stmt)
                                                                         ln)

changeName s = "__" ++ s ++ "_"


changeDecl _ (Proc id params decl stmt ln) = let name = "p_" ++ changeName id
                                             in
                                             Proc name 
                                                  (map changeParam params) 
                                                  (map (changeDecl name) decl)
                                                  (map (changeStmt name) stmt)
                                                  ln

changeDecl _ (Fun id params funType decl stmt ln) = let name = "f_" ++ changeName id
                                                    in
                                                    Fun name 
                                                        (map changeParam params) 
                                                        funType
                                                        (map (changeDecl name) decl)
                                                        (map (changeStmt name) stmt)
                                                        ln

changeDecl fid (Const id exp ln) = (Const (fid ++ changeName id)
                                          (changeExp fid exp)
                                          ln)

changeDecl _ (Var id varType ln) = Var (changeName id)
                                       varType
                                       ln
changeDecl _ d = d

changeParam (Param id paramType ln) = Param (changeName id) paramType ln

changeExp fid (Id id ln)     = Id (changeName id) ln
changeExp fid (Not e ln)      = Not (changeExp fid e) ln
changeExp fid (And e1 e2 ln)  = And (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Or e1 e2 ln)  = Or (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Eq e1 e2 ln)  = Eq (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (NEq e1 e2 ln)  = NEq (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Lt e1 e2 ln)  = Lt (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (LEq e1 e2 ln)  = LEq (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Gt e1 e2 ln)  = Gt (changeExp fid  e1) (changeExp fid e2) ln
changeExp fid (GEq e1 e2 ln)  = GEq (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Add e1 e2 ln)  = Add (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Sub e1 e2 ln)  = Sub (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Mul e1 e2 ln)  = Mul (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Div e1 e2 ln)  = Div (changeExp fid e1) (changeExp fid e2) ln
changeExp fid (Func id exps ln) = Func ("f_" ++ changeName id) (map (changeExp fid) exps) ln
changeExp _ e = e

changeStmt fid (Assign id e ln )             = Assign (changeName id) (changeExp fid e) ln
changeStmt fid (IfThenElse e body1 body2 ln) = IfThenElse (changeExp fid e) 
                                                          (map (changeStmt fid) body1)
                                                          (map (changeStmt fid) body2)
                                                          ln
changeStmt fid (IfThen e body ln)            = IfThen (changeExp fid e) 
                                                      (map (changeStmt fid) body)
                                                      ln
changeStmt fid (WhileDo e body ln)           = WhileDo (changeExp fid e) 
                                                       (map (changeStmt fid) body)
                                                       ln
changeStmt fid (Call id exps ln)              = Call ("p_" ++ changeName id) 
                                                     (map (changeExp fid) exps) ln
                                                
                                          
changeStmt fid (Write (Id id l) ln)       = Write (Id (fid ++ changeName id) l) ln
changeStmt fid (WriteNum e ln)               = WriteNum (changeExp fid e) ln
changeStmt fid (WriteLn ln)                  = WriteLn ln
changeStmt fid (Return e ln)                 = Return (changeExp fid e) ln
changeStmt _ s = s

declStr (Program p) = (Program (declStr' p))


declStr' p | isMod p = let (ds, ss,_) = foldr unionDecl ([], [], 0) (stmt p)
                           ds' = map declStr' (decl p)
                       in p{decl = ds ++ ds', stmt = ss}
declStr' d = d


newStr i = "str_" ++ show i

unionDecl d (ds,ss,i) = let (ds',ss', i') = declStmtStr i d
                        in (ds' ++ ds, ss' ++ ss, i')


declStmtStr i (IfThenElse e body1 body2 ln) = 
             let (ds, body1', i') = foldr unionDecl ([],[],i) body1
                 (ds', body2', i'') = foldr unionDecl ([],[],i') body2
             in (ds ++ ds', [IfThenElse e body1' body2' ln], i'')

declStmtStr i (IfThen e body ln) = 
             let (ds, body', i') = foldr unionDecl ([],[],i) body
             in (ds, [IfThen e body' ln], i')

declStmtStr i (WhileDo e body ln) = 
             let (ds, body', i') = foldr unionDecl ([],[],i) body
             in (ds, [WhileDo e body' ln], i')

declStmtStr i (Write (CString s l1) l) = 
              let c = newStr i
              in ([Const c (CString s l1) l1], [Write  (Id c l) l], i+1) 

declStmtStr i s = ([],[s], i)