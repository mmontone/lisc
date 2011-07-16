-- this module checks if there are duplicated variables, constants and procedures
module CheckDup where

import Types

type VarsAndConsts = Identifiers -- variables and constants belong to the same namespace
type Functions = Identifiers
type Procedures = Identifiers
type Modules = (Functions, Procedures)
type Table = (VarsAndConsts, Modules)

varsAndConsts = fst
mods = snd
functions = fst . snd
procedures = snd . snd

removeAllIdOccur ident (vacs,ps) = (filter (/= ident) vacs, ps)
removeFunc  id (vac,(fs,ps)) = (vac, (filter (\f -> f /= id) fs,ps))
removeProcs id (vac,(fs,ps)) = (vac, (fs, (filter (\p -> p /= id) ps)))

newVarOrConst (vac, mods) id = ((id:vac), mods)
newFunc   (vac, (f,p)) fun   = (vac ,(fun:f,p))
newProc (vac, (f,p)) proc  = (vac ,(f,proc:p))

existsVarOrConst t id = id `elem` (varsAndConsts t)
existsFun t id = id `elem` functions t
existsMod t id = id `elem` procedures t

newTable = ([],([],[]))

-- main functions
ioCheck p = case check p of
                Nothing -> return Nothing 
                Just s -> do putStrLn s 
                             return $ Just s
            
check::Program -> Maybe String
check prog = fst $ checkModDup newTable (params $ mainProc $ prog) (decl $ mainProc $ prog) 

-- Aux
errorMsg p | isProc p   = "Error: procedimiento \"" ++ modId p ++ "\" duplicado en linea " ++ (show $ ln p)
errorMsg p | isFun  p   = "Error: funcion \"" ++ modId p ++ "\" duplicado en linea " ++ (show $ ln p)
errorMsg c | isConst c = "Error: constante \"" ++ constId c ++ "\" duplicada en linea " ++ (show $ constLn c)
errorMsg v | isVar v   = "Error: variable \"" ++ varId v ++ "\" duplicada en linea " ++ (show $ varLn v)

paramErrorMsg p = "Error: Identificador de parametro \"" ++ paramId p ++ "\" duplicada en linea " ++ (show $ paramLn p)

checkModDup::Table -> [Parameter] -> [Declaration] -> (Maybe String, Table)
checkModDup t [] []     = (Nothing, t)
checkModDup t (p:ps) ds = let (r, t') = checkDupParam t p 
                            in case r of
                               Nothing -> checkModDup t' ps ds
                               Just _ -> (r, t')
checkModDup t ps (d:ds) = let (r, t') = checkDupDecl t d
                            in case r of
                               Nothing -> checkModDup t' ps ds
                               Just _ -> (r, t')
			       
checkDupParam t p = if existsVarOrConst t (paramId p) then
                                   (Just $ paramErrorMsg p, t)
                                 else
                                   (Nothing, newVarOrConst t (paramId p))
                          
checkDupDecl::Table -> Declaration -> (Maybe String, Table)
checkDupDecl t const | isConst const =
    if existsVarOrConst t (constId const) then (Just $ errorMsg const, t)
    else (Nothing, newVarOrConst t (constId const))
checkDupDecl t var | isVar var =
    if existsVarOrConst t (varId var) then (Just $ errorMsg var, t)
    else (Nothing, newVarOrConst t (varId var))
checkDupDecl t fun | isFun fun  =
    if existsFun t (modId fun) then (Just $ errorMsg fun, t)
    else (fst $ checkModDup newTable (params fun) (decl fun), newFunc t (modId fun))
checkDupDecl t proc | isProc proc  =
    if existsMod t (modId proc) then (Just $ errorMsg proc, t)
    else (fst $ checkModDup newTable (params proc) (decl proc), newProc t (modId proc))