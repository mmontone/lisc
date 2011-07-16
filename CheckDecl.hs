-- this module checks that every used variable is declared
-- and every constant is not on the left of an assign statement
module CheckDecl (check, ioCheck) where

import Types
import Debug.Trace
import Data.List

-- static chain
type Vars = Identifiers
type Consts = Identifiers
type Procs = Identifiers
type Funcs = Identifiers
data StaticChain = StaticChain{chainConsts::Consts,
                               chainVars::Vars,
			       chainProcs::Procs,
			       chainFuncs::Funcs} deriving (Eq,Show)

newChain = StaticChain [] [] [] []

inChain::(StaticChain -> [Identifier]) -> Identifier -> StaticChain -> Bool
inChain getter id ch = id `elem` (getter ch)
varInChain = inChain chainVars
constInChain = inChain chainConsts
procInChain = inChain chainProcs
funcInChain = inChain chainFuncs 

asChain = foldr addToChain

addToChain d ch| isConst d = ch{chainConsts = chainConsts ch ++ [constId d]}
addToChain d ch| isVar d = ch{chainVars = chainVars ch ++ [varId d]}
addToChain d ch| isProc d = ch{chainProcs = chainProcs ch ++ [modId d]}
addToChain d ch| isFun d = ch{chainFuncs = chainFuncs ch ++ [modId d]}

--addToChain d ch| isFun d = ch{chainFuncs = chainFuncs ch ++ [modId d]}

-- main functions
ioCheck::Program -> IO(Maybe String)
ioCheck p = let r = check p 
	    in case r of
		Nothing -> return Nothing
		Just s  -> do putStrLn s
                              return r

check::Program -> Maybe String
check prog = checkModDecl newChain (mainProc prog)

-- Aux
checkModDecl::StaticChain -> Declaration -> Maybe String
checkModDecl ch f | isFun f = case find isRet (concatMap allStmts (stmt f)) of
                                   Nothing -> Just $ "Error: la funcion " ++ modId f ++ " declarada en linea " ++ show (ln f) ++ " no contiene una sentencia return" 
                                   Just _ -> checkModDecl' ch f
checkModDecl ch d = checkModDecl' ch d

checkModDecl' ch mod =  
      let localDecls = decl mod
          statements = stmt mod
          parameters = params mod
	  ch' = asChain ch (mod:localDecls)
	  newChain = ch'{chainVars = chainVars ch'++ map paramId (params mod)}
          newChain' = newChain {chainConsts = chainConsts newChain ++ (map paramId parameters)}
      in checkScope mod newChain' statements  `ifNoErr`
         foldr (\p r -> r `ifNoErr` checkModDecl newChain' p) Nothing (filter isMod localDecls)
					  			     
checkScope::Declaration -> StaticChain -> Statements -> Maybe String
checkScope mod ch = foldr (ifNoErr . (checkStmtScope mod ch)) Nothing

checkStmtScope _ ch as | isAssign as =
        if not (varInChain (assignId as) ch) then 
           if  constInChain (assignId as) ch then
               Just $ "Error en asignacion <" ++ show as ++ "> en linea " ++ show (stmtLn as) ++ ", " ++ assignId as ++ " es un valor no modificable"
	   else 
               Just $ "Error en asignacion <" ++ show as ++ "> en linea " ++ show (stmtLn as) ++ ", " ++ assignId as ++ " no es una variable declarada"
        else checkExp ch (assignExp as)	       
checkStmtScope _ ch call | isCall call =
           if not (procInChain (callId call) ch)
           then Just $ "Error: el procedimiento <" ++ callId call ++ "> llamado en la linea " ++ show (stmtLn call) ++ " no fue declarado."
           else checkExps ch (procParams call)
checkStmtScope mod ch ret | isRet ret = 
           if not (isFun mod) 
              then Just $ "Error: sentencia return fuera de una funcion en linea " ++ show (retLn ret)
              else checkExp ch (retExp ret)
checkStmtScope mod ch st = let exps = stmtExps st
			       ss   = subStmts st
                           in checkExps ch exps `ifNoErr` checkScope mod ch ss
                        
checkExps ch = foldr (ifNoErr . (checkExp ch)) Nothing

checkExp ch e | isBin e =
         checkExp ch (subExp1 e) `ifNoErr` checkExp  ch (subExp2 e)

checkExp ch e | isUn e = checkExp ch (subExp e)

checkExp ch e | isId e =
                if varInChain (expId e) ch || constInChain (expId e) ch then Nothing
                else Just $ "Error en expresion " ++ show e ++ " en linea " ++ show (expLn e) ++ ": identificador \"" ++ (expId e) ++ "\" no declarado."

checkExp ch e | isFunc e = if not (funcInChain (funcId e) ch)
                                then Just $ "Error: la funcion <" ++ funcId e ++ "> llamada en la linea " ++ show     (funcLn e) ++ " no fue declarada."
                                else checkExps  ch (funcParams e)
checkExp ch e | otherwise = Nothing