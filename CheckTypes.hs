-- module for type checking
module CheckTypes (check, ioCheck )where
import Types
import System.IO.Unsafe

--types chain
type TypesChain = [(Identifier, Type)]

newTChain = []

newType ch id ty  = (id,ty):ch

getType  = snd
getId    = fst

idType id = getType . head . filter ((id ==) . getId) -- para variables

funType id ch = let xs = map getType $ filter ((id==) . getId) ch
                    FunT types t = head $ filter isFunT xs --esto se hace para evitar que funciones y procedimientos tengan el mismo tipo y haya un error falso
                in t -- para saber el tipo del resultado de una función
funParamsTypes id ch = let xs = map getType $ filter ((id==) . getId) ch
                           FunT types t = head $ filter isFunT xs --esto se hace para evitar que funciones y procedimientos tengan el mismo tipo y haya un error falso
                       in types -- para saber el tipo de los parámetros de una función

-- Combinators
m `eqType` (ty, s) = case m of
                       Left t  -> if t == ty
		                  then Nothing
			          else Just s
		       Right err -> Just err

-- main functions 
ioCheck::Program -> IO (Maybe String)
ioCheck prog =      case check prog of
                      Nothing -> return Nothing
		      Just s  -> do putStrLn $ "Error de tipos: " ++ s
        	                    return $ Just s

check::Program -> Maybe String
check prog = checkModTypes newTChain (mainProc prog)

--Aux
expType _ (CBool _ _) = Boolean
expType _ (And _ _ _) = Boolean
expType _ (Not _ _)   = Boolean
expType _ (Or _ _ _ ) = Boolean
expType _ (Eq _ _ _)  = Boolean
expType _ (NEq _ _ _) = Boolean
expType _ (Lt _ _ _)  = Boolean
expType _ (LEq _ _ _) = Boolean
expType _ (Gt _ _ _)  = Boolean
expType _ (GEq _ _ _) = Boolean
expType _ (CInt _ _)  = Number
expType _ (Add _ _ _) = Number
expType _ (Sub _ _ _) = Number
expType _ (Mul _ _ _) = Number
expType _ (Div _ _ _) = Number
expType _ (CFloat _ _)= Real
expType _ (CString _ _) = Str
expType ch f | isFunc f = CheckTypes.funType (funcId f) ch

typeErrorMsg e t = "Error de tipo: la expresion <" ++ show e ++ ">" ++ "en la linea " ++ show (expLn e) ++ " debe ser de tipo " ++ show t

addToTChain::TypesChain -> Declarations -> Either TypesChain String
addToTChain ch = foldl (\mc d -> mc `noErrDo` (addDecl d)) (Left ch)

addDecl::Declaration -> TypesChain -> Either TypesChain String
addDecl c ch | isConst c = inferExpType ch (constExp c) `noErrDo`
                            \ty -> Left $ newType ch (constId c) ty
addDecl v ch | isVar v   = Left $ newType ch (varId v) (varType v)
addDecl p ch | isProc p  = let ch' = newType ch (modId p) (ProcT (map paramType (params p)))--agrego el id del proc con el tipo de los parámetros
                           in Left $ foldr addParam ch' (params p) --agrego c/u de los parámetros
addDecl f ch | isFun f   = let ch' = newType ch (modId f) (FunT (map paramType (params f)) (Types.funType f))--agrego el id de la func con el tipo de los parámetros
                           in Left $ foldr addParam ch' (params f) --agrego c/u de los parámetros

addParam param ch = newType ch (paramId param) (paramType param)

checkModTypes::TypesChain -> Declaration -> Maybe String
checkModTypes ch p =
    addToTChain ch (decl p) `noErrTake`
    \ch' -> checkStmtTypes ch' (stmt p) `ifNoErr`-- se asume que el return es la última sentencia de una función
            if isFun p then checkReturns p ch' else Nothing `ifNoErr`
            foldr (\d r -> r `ifNoErr` (checkModTypes ch' d)) Nothing (filter isMod (decl p))

checkReturns (Fun _ _ t _ stmts _) ch = 
                     let rets = filter isRet stmts
                         inferRet ret = inferExpType ch (retExp ret) `eqType` (t, typeErrorMsg (retExp ret) t)
                     in foldr (ifNoErr . inferRet) Nothing rets
                                 
checkStmtTypes::TypesChain -> Statements -> Maybe String
checkStmtTypes ch = foldr (\s r -> r `ifNoErr` checkStmtType ch s) Nothing

checkStmtType:: TypesChain -> Statement -> Maybe String
checkStmtType ch (Assign v e _ ) = inferExpType ch e `eqType`(idType v ch, typeErrorMsg e (idType v ch)) 
checkStmtType ch (IfThenElse e ss1 ss2 _) = inferExpType ch e `eqType` (Boolean, typeErrorMsg e Boolean) `ifNoErr` 
                                            checkStmtTypes ch ss1 `ifNoErr`
					    checkStmtTypes ch ss2
checkStmtType ch (IfThen e ss _) = inferExpType ch e `eqType` (Boolean, typeErrorMsg e Boolean) `ifNoErr`
                                   checkStmtTypes ch ss
checkStmtType ch (WhileDo e ss _) = inferExpType ch e `eqType` (Boolean, typeErrorMsg e Boolean) `ifNoErr`
                                    checkStmtTypes ch ss
checkStmtType ch (Call id params ln) =
        let paramsTypes  = map (inferExpType ch) params
            paramsTypes' = foldr (\t rec -> case rec of 
                                               y@(Right err) -> y
                                               Left (ProcT rec') -> case t of
                                                                    x@(Right err) -> x
                                                                    Left t' -> Left (ProcT (t': rec')))
                                 (Left (ProcT [])) paramsTypes
        in paramsTypes' `eqType` (idType id ch,  "Error de tipo: los parámetros del llamado a '" ++
                                                 id ++ "'" ++ "en la linea " ++ show ln ++
                                                 " deben coincidir con los tipos dados en la" ++
                                                 " declaración del procedimiento")
checkStmtType ch _ = Nothing 

inferBinOp ch t e e1 e2 =   inferExpType ch e1 `eqType` (t, typeErrorMsg e1 t) `noErr`
                            inferExpType ch e2 `eqType` (t, typeErrorMsg e2 t) `noErr`
		  	    Left (expType ch e)

inferExpType::TypesChain -> Exp -> Either Type String
inferExpType ch (Not e _) = inferExpType ch e `eqType` (Boolean, typeErrorMsg e Boolean) `noErr`
                            Left Boolean
inferExpType ch e | isBin e  && takesNumbers e = checkCoerce ch e (subExp1 e) (subExp2 e)
inferExpType ch e | isBin e  && takesBooleans e = inferBinOp ch Boolean e (subExp1 e) (subExp2 e)
inferExpType ch (Eq e1 e2 _) = inferExpType ch e1 `noErrDo` \t1 ->
                               inferExpType ch e2 `eqType` (t1, typeErrorMsg e2 t1) `noErr`
			       Left Boolean
inferExpType ch e | isId e = Left (idType (expId e) ch)
inferExpType ch f | isFunc f = --se infiere el tipo del resultado pero antes se chequean los tipos de los parámetros
        let paramsTypes  = map (inferExpType ch) (funcParams f)
            paramsTypes' = foldr (\tp rec -> case rec of 
                                               y@(Right err) -> y
                                               Left (ProcT rec) -> case tp of
                                                                    x@(Right err) -> x
                                                                    Left t' -> Left (ProcT (t': rec)))
                                 (Left (ProcT []))
                                 paramsTypes
        in paramsTypes' `eqType` (ProcT (funParamsTypes (funcId f) ch), "Error de tipo: los parámetros del llamado a '" ++
                                                 funcId f ++ "' en la linea " ++ show (funcLn f) ++
                                                 " deben coincidir con los tipos dados en la" ++
                                                 " declaración de la función") `noErr`
           Left (CheckTypes.funType (funcId f) ch)
inferExpType ch e = Left $ expType ch e

checkCoerce ch e e1 e2 = inferExpType ch e1 `noErrDo` \t1 ->
                         inferExpType ch e2 `noErrDo` \t2 ->
                         if (t1 == t2) && (isNumber t1) then
                             if isNumExp e then Left t1 else Left Boolean
                         else Right $ "Error de tipos: las expresiones <" ++ show e1 ++ "> y <" ++ show e2 ++  "> en la linea " ++ show (expLn e) ++ " deben ser del mismo tipo numerico"

checkExpType ch e t = case inferExpType ch e of
                        Left t' -> t' == t
		        Right _ -> False

takesNumbers (Gt _ _ _) = True
takesNumbers (GEq _ _ _) = True
takesNumbers (Lt _ _ _) = True
takesNumbers (LEq _ _ _) = True
takesNumbers (Add _ _ _) = True
takesNumbers (Sub _ _ _) = True
takesNumbers (Mul _ _ _) = True
takesNumbers (Div _ _ _) = True
takesNumbers _ = False

takesBooleans (And _ _ _) = True
takesBooleans (Or _ _ _) = True
takesBooleans _ = False