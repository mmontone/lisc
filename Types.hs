module Types where

import Prelude 
import PrettyPrint --hiding(Str)

type Identifier   = String
type Identifiers  = [Identifier]
type Declarations = [Declaration]
type Statements   = [Statement]
type Parameters   = [Parameter]
type Exps = [Exp]

data Program     = Program {mainProc::Declaration}

instance Show Program where
   show p@(Program (Proc _ _ _ _ ln)) = show $ ppProgram p
   show _ = error "****"

ppProgram (Program (Proc ide params decs body ln)) =
            space $$ text "program" <+> text ide $$
            nest 3 (ppDecs decs) $$ space $$
            ppBody body

ppDecs = foldr ($$) empty . map (ppDeclaration)
ppBody body = text "begin" $$
              nest 3 (foldr ($$) empty (map (ppStatement) body)) $$
              text "end"

-- Declarations
data Declaration = Const {constId::Identifier,
                          constExp::Exp,
			  constLn::Int}
                 | ConstRef {constRefId::Identifier,
                             constRef::Declaration, -- it must be a constant declaration
    			     constRefLn::Int}
		 | Var   {varId::Identifier,
		          varType::Type,
			  varLn::Int}
		 | Proc  {modId::Identifier,
		          params::Parameters,
		          decl::Declarations,
			  stmt::Statements,
			  ln::Int} 
		 | Fun   {modId::Identifier,
		          params::Parameters,
			  funType:: Type,
			  decl::Declarations,
			  stmt::Statements,
			  ln::Int} deriving Eq

data Parameter = Param {paramId::Identifier,
		        paramType::Type,
			paramLn::Int} deriving (Eq,Show)

instance Show Declaration where
    show = show . ppDeclaration

ppDeclaration (Const ide expr ln)        = int ln <+> text ide <+> colon <+> (text (show expr)) <> semi
ppDeclaration (ConstRef ide ref ln)      = int ln <+> text ide <+> colon <+> (text (show (constExp ref))) <+> text ("(ref. " ++ constId ref ++ ")") <> semi
ppDeclaration (Var ide typ ln)           = int ln <+> ppType typ <+> text ide <> semi
ppDeclaration (Proc ide params decs body _) = text "proc" <+> text ide <+> lparen <> ppParams params <> rparen $$
                                              nest 3 (ppDecs decs) $$
                                              ppBody body <> semi
ppDeclaration (Fun ide params t decs body ln) = int ln <+> text "fun" <+> text ide <+> lparen <> ppParams params <> rparen <+> text ":" <+> text (show t) $$
                                        nest 3 (ppDecs decs) $$
                                        ppBody body <> semi

ppParams ps =
   foldl (\x y -> if isEmpty x then y else x <> semi <+> y) empty (map ppParam ps)

ppParam (Param id t _) = text id <> colon <> text (show t)

isProc (Proc _ _ _ _ _) = True
isProc _ = False

isMod p = isProc p || isFun p

isFun (Fun _ _ _ _ _ _) = True
isFun _ = False

isVar (Var _ _ _) = True
isVar _ = False

isConst (Const _ _ _ ) = True
isConst _ = False

isParam (Param _ _ _) = True
isParam _ = False

-- Types
data Type = Real | Number | Boolean | Str | FunT [Type] Type | ProcT [Type] deriving Eq

instance Show Type where
     show Real = "real"
     show Number = "int"
     show Boolean = "bool"
     show Str = "string"
     show (FunT types t) = "(" ++ (concatMap show types) ++ "): " ++ show t
     show (ProcT types)  = "(" ++ (concatMap show types) ++ ")"


ppType Number = text "int"
ppType Boolean = text "bool"
ppType Real = text "real"
ppType Str = text "string"

isNumber Real = True
isNumber Number = True
isNumber _ = False

isFunT (FunT _ _) = True
isFunT _          = False

type LineNumber = Int

-- Sentences
data Statement	 = Assign {assignId::Identifier,
                           assignExp::Exp,
			   assignLn::LineNumber}
		 | IfThenElse Exp Statements Statements LineNumber
		 | IfThen     Exp Statements LineNumber
		 | WhileDo  Exp Statements LineNumber
		 | Call {callId::Identifier, procParams::Exps,callLn::LineNumber}
		 | Write Exp LineNumber 
		 | WriteNum Exp LineNumber
		 | WriteLn LineNumber
		 | Return {retExp::Exp, retLn::LineNumber} deriving Eq

instance Show Statement where
   show = show . ppStatement

ppStatement (Assign id e ln )             = int ln <+> text id <+> text "=" <+> text (show e) <> semi
ppStatement (IfThenElse e body1 body2 ln) = text "if" <+> text (show e) <+> text "then" $$
                                            ppBody body1 $$
                                            text "else" $$
                                            ppBody body2<> semi
ppStatement (IfThen e body ln)            = text "if" <+> text (show e) <+> text "then" $$
                                            ppBody body <> semi
ppStatement (WhileDo e body ln)           = text "while" <+> text (show e) <+> text "do" $$
                                            ppBody body <> semi
ppStatement (Call id params ln)           = int ln <+> text id <> lparen <> text (showParams params) <> rparen <> semi
ppStatement (Write e ln)                  = int ln <+> text "write" <+> text (show e) <> semi
ppStatement (WriteNum e ln)                  = int ln <+> text "writenum" <+> text (show e) <> semi
ppStatement (Return e ln)               = int ln <+> text "return" <+> text (show e) <> semi

stmtLn (Assign _ _ l) = l
stmtLn (IfThenElse _ _ _ l) = l
stmtLn (IfThen _ _ l) = l
stmtLn (WhileDo _ _ l) = l
stmtLn (Call _ _ l) = l
stmtLn (Write _ l) = l
stmtLn (WriteNum _ l) = l

stmtExps (Assign _ e _) = [e]
stmtExps (IfThenElse e _ _ _) = [e]
stmtExps (IfThen e _ _) = [e]
stmtExps (WhileDo e _ _) = [e]
stmtExps (Write e _) = [e]
stmtExps (WriteNum e _) = [e]
stmtExps _ = [] 

subStmts (IfThenElse _ ss1 ss2 _ ) = ss1 ++ ss2
subStmts (IfThen _ ss _) = ss
subStmts (WhileDo _ ss _) = ss
subStmts _ = []

allStmts d = d : concatMap allStmts (subStmts d)

isAssign (Assign _ _ _) = True
isAssign _ = False

isCond (IfThenElse _ _ _ _) = True
isCond (IfThen _ _ _) = True
isCond _ = False

isWhile (WhileDo _ _ _) = True
isWhile _ = False

isCall (Call _ _ _) = True
isCall _ = False

isRet (Return _ _) = True
isRet _ = False

isWrite (Write _ _)= True
isWrite _ = False

isWriteNum (WriteNum _ _) = True
isWriteNum _ = False

--Expressions
data Exp	 = CBool Bool Int
		 | And Exp Exp Int
		 | Or  Exp Exp Int
		 | Not Exp Int
		 | Eq  Exp Exp Int
		 | NEq Exp Exp Int
		 | Lt  Exp Exp Int
		 | LEq Exp Exp Int
		 | Gt  Exp Exp Int
		 | GEq Exp Exp Int
		 | CInt Int Int
                 | CFloat Float Int
		 | Add Exp Exp Int
		 | Sub Exp Exp Int
		 | Mul Exp Exp Int
		 | Div Exp Exp Int
		 | CString String Int
		 | Id  {expId::String, idLn::Int} 
		 | Func {funcId::String, funcParams::Exps,funcLn::Int}   deriving Eq

foldExp leaf branch unique (Not b     _) = unique (foldExp leaf branch unique b)
foldExp leaf branch unique (And b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Or b1 b2  _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Eq b1 b2  _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (NEq b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Lt b1 b2  _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (LEq b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Gt b1 b2  _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (GEq b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Add b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Sub b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Mul b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique (Div b1 b2 _) = branch (foldExp leaf branch unique b1) (foldExp leaf branch unique b2)
foldExp leaf branch unique x@(Id _ _)      = leaf x
foldExp leaf branch unique x@(CString _ _) = leaf x
foldExp leaf branch unique x@(CInt _ _)    = leaf x
foldExp leaf branch unique x@(CFloat _ _)  = leaf x
foldExp leaf branch unique x@(CBool _ _)   = leaf x
foldExp leaf branch unique x@(Func id params l) = leaf x 

instance Show Exp where
  show (CBool b _)    = show b
  show (CFloat f _)   = show f
  show (CInt n _)     = show n
  show (CString s _)  = s
  show (Id id _ )     = id
  show (Not e _)      = "not " ++ show e
  show (And e1 e2 _)  = show e1 ++ " && " ++ show e2
  show (Or e1 e2 _)   = show e1 ++ " || " ++ show e2
  show (Eq e1 e2 _)   = show e1 ++ " == " ++ show e2
  show (NEq e1 e2 _)  = show e1 ++ " /= " ++ show e2
  show (Lt e1 e2 _)   = show e1 ++ " < " ++ show e2
  show (LEq e1 e2 _)  = show e1 ++ " <= " ++ show e2
  show (Gt e1 e2 _)   = show e1 ++ " > " ++ show e2
  show (GEq e1 e2 _)  = show e1 ++ " >= " ++ show e2
  show (Add e1 e2 _)  = show e1 ++ " + " ++ show e2
  show (Sub e1 e2 _)  = show e1 ++ " - " ++ show e2
  show (Mul e1 e2 _)  = show e1 ++ " * " ++ show e2
  show (Div e1 e2 _)  = show e1 ++ " / " ++ show e2
  show (Func id params _)= id ++ "(" ++ showParams params ++ ")"

showParams []  = ""
showParams [p] = show p
showParams (p:ps) = show p ++ ", " ++ showParams ps

expLn (And _ _ l) = l
expLn (Or _ _ l) = l
expLn (Eq _ _ l) = l
expLn (NEq _ _ l) = l
expLn (Lt _ _ l) = l
expLn (LEq _ _ l) = l
expLn (Gt _ _ l) = l
expLn (GEq _ _ l) = l
expLn (Add _ _ l) = l
expLn (Sub _ _ l) = l
expLn (Mul _ _ l) = l
expLn (Div _ _ l) = l
expLn (Not _ l) = l
expLn (CBool _ l) = l
expLn (CInt _ l) = l
expLn (CString _ l) = l
expLn (Id _ l) = l
expLn (CFloat _ l) = l
expLn (Func _ _ l) = l

isBoolExp e = not (isNumExp e) && not (isStrExp e) && not (isId e)
isNumExp (CInt _ _) = True
isNumExp (CFloat _ _) = True
isNumExp (Add _ _ _) = True
isNumExp (Sub _ _ _) = True
isNumExp (Mul _ _ _) = True
isNumExp (Div _ _ _) = True
isNumExp _ = False

isStrExp (CString _ _) = True
isStrExp _ = False

isFunc (Func _ _ _) = True
isFunc _ = False

isUn (Not _ _) = True
isUn _ = False

subExp (Not e _) = e

isBin (CBool _ _)  = False
isBin (CFloat _ _ ) = False
isBin (CString _ _) = False
isBin (CInt _ _) = False
isBin (Id _ _) = False
isBin (Not _ _) = False
isBin (Func _ _ _) = False
isBin _ = True

isId (Id _ _) = True
isId _ = False

subExp1 (And e1 e2 _) = e1
subExp1 (Or e1 e2 _) = e1
subExp1 (Eq e1 e2 _) = e1
subExp1 (NEq e1 e2 _) = e1
subExp1 (Lt e1 e2 _) = e1
subExp1 (LEq e1 e2 _) = e1
subExp1 (Gt e1 e2 _) = e1
subExp1 (GEq e1 e2 _) = e1
subExp1 (Add e1 e2 _) = e1
subExp1 (Sub e1 e2 _) = e1
subExp1 (Mul e1 e2 _) = e1
subExp1 (Div e1 e2 _) = e1

subExp2 (And e1 e2 _) = e2
subExp2 (Or e1 e2 _) = e2
subExp2 (Eq e1 e2 _) = e2
subExp2 (NEq e1 e2 _) = e2
subExp2 (Lt e1 e2 _) = e2
subExp2 (LEq e1 e2 _) = e2
subExp2 (Gt e1 e2 _) = e2
subExp2 (GEq e1 e2 _) = e2
subExp2 (Add e1 e2 _) = e2
subExp2 (Sub e1 e2 _) = e2
subExp2 (Mul e1 e2 _) = e2
subExp2 (Div e1 e2 _) = e2

-- Tokens 
data Token	= TProgram
                | TEOF
		| TId Identifier
		| TVar		
		| TProc
		| TFun
		| TRet
		| TType Type
		| TCInt Int
                | TCFloat Float
		| TCBool Bool
                | TCString String
		| TBegin	
		| TEnd
		| TAssign	
		| TWrite
		| TWriteNum
		| TWriteLn
		| TIf		
		| TThen		
		| TElse		
		| TWhile	
		| TDo		
		| TAnd		
		| TOr		
		| TNot		
		| TEq		
		| TNEq		
		| TLt		
		| TLE		
		| TGt		
		| TGE
		| TAdd		
		| TSub		
		| TTimes	
		| TDiv
		| TOB		
		| TCB
		| TSep
		| TSepP		
		| TE deriving Show

initValue Number  = CInt 0
initValue Boolean = CBool False

-- Combinators to avoid error handling
m `ifNoErr` k = case m of
                    Nothing -> k
		    Just _  -> m

m `noErrDo`k = case m of
                    Left t    -> k t
	            Right err -> Right err

m `noErr` k = case m of
                Nothing -> k
		Just err -> Right err

m `noErrTake` k = case m of
                    Left t -> k t
		    Right err -> Just err
