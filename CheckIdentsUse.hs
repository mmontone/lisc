-- this module checks if all identifiers (variables, constants and modules) declared
-- are used and initialized
module CheckIdentsUse where
import Types
import Data.List

check prog = createWarningsList $ fst $ checkMod (mainProc prog) Empty

data State = NotInitialized | Fine

isFine Fine = True
isFine _    = False

type StateId = (Identifier, State)
type States = [StateId]

data Mark = M Identifier States States Identifiers Identifiers [Mark] Mark | Empty
getParent (M _ _ _ _ _ _ parent) = parent

remove ide = filter (/= ide)
removeId ide = filter (\(id,state) -> id /= ide)

isIn ide list = ide `elem` (map fst list)
putSafe ide = foldr (\x@(id,state) rec -> if ide == id then (ide,Fine):rec else x:rec) []

isNotInitializedIn var Empty = False
isNotInitializedIn var (M _ used unused _ _ _ parent) = 
                        var `isIn_NotInitialized` used ||
                        var `isIn_NotInitialized` unused ||
                        isNotInitializedIn var parent

isIn_NotInitialized var =
   foldr (\(v,state) b-> if v == var && not (isFine state) then True else b) False

--when the identifier is on the left
useSafetyId ide Empty = error (ide ++ " no se encuentra presente en el codigo!")
useSafetyId ide (M id used unused called uncalled children parent) = 
    if ide `isIn` used then M id (putSafe ide used) unused called uncalled children parent
    else if ide `isIn` unused then --it wasn't used yet but now it is used
             M id ((ide,Fine):used) (removeId ide unused) called uncalled children parent 
         else M id used unused called uncalled children (useSafetyId ide parent)-- seek the binding in its parents

seekIdentifierIn ide = foldr (\x@(i,s) rec -> if i == ide then x else rec) (error "no se encontró :S")

--when the identifier is on the right
useId ide Empty = error $ ide ++ " no se encuentra presente en el codigo!"
useId ide x@(M id used unused called uncalled children parent) = 
    if ide `isIn` used then x -- it was already used
    else if ide `isIn` unused then 
                --it wasn't used yet but now it is used
                   let i = seekIdentifierIn ide unused 
                   in M id (i:used) (removeId ide unused) called uncalled children parent
         else M id used unused called uncalled children (useId ide parent)-- seek the binding in its parents

useMod ide Empty = error $ ide ++ " no se encuentra presente en el codigo!"
useMod ide x@(M id used unused called uncalled children parent) = 
    if ide `elem` called then x -- it was used
    else if ide `elem` uncalled then --it wasn't used but now it is
              M id used unused (ide:called) (remove ide uncalled) children parent
         else M id used unused called uncalled children (useMod ide parent) -- seek in parents

createWarningsList (M pid used unused _ uncalled children _) = 
        concat (map createWarningsList children) ++ (map createWarningId unused) ++
               (map createWarningMod uncalled) ++ (map createWarningNotInit used)
          where createWarningId (id,_) = "Warning: identificador '"++ id
                                        ++ "' de modulo '" ++ pid ++ "' no usado"
                createWarningNotInit (id,state) =
                        if isFine state then []
                        else "Warning: identificador '"++ id ++ "' de modulo '" ++
                             pid ++ "' no inicializado y usado en asignacion"
                createWarningMod id = "Warning: submodulo '"++ id ++ "' de '" ++
                                       pid ++ "' nunca invocado"

checkMod m parent =
        let (ids,mods)        = partition (not . isMod) (decl m)
            consts             = filter isConst ids
            unused             = createParamsIds (params m) ++ createIds ids
            uncalled           = createMods mods
            mark               = M (modId m) [] unused [] uncalled children parent
            partialMark        = checkStmts (stmt m) (checkConstsDec consts mark)
            (children,newMark) = recurseOverChildren partialMark mods
        in (newMark,getParent newMark)

recurseOverChildren p [] = ([],p)
recurseOverChildren p (pr:mods) = let (childrenMarks,p') = recurseOverChildren p mods
                                      (childMark,p'') = checkMod pr p'
                                  in (childMark:childrenMarks,p'')

createParamsIds = map (\p -> (paramId p,Fine))

createIds []                  = []
createIds (Var id _ _   :ids) = (id,NotInitialized):createIds ids
createIds (Const id _ _ :ids) = (id,Fine):createIds ids

createMods [] = []
createMods (m:ms) = (modId m):createMods ms

appearsIn ide = foldExp (\x -> if isId x then (expId x) == ide else False) (||) id

checkStmts stmts p = foldl (flip checkStmt) p stmts

--chequear que no esté en la misma asignación!
checkStmt (Assign var exp _)           p =
                if (var `appearsIn` exp) && (var `isNotInitializedIn` p) then removeIdsFromExp exp p
                else removeIdsFromExp exp (useSafetyId var p)
checkStmt (IfThenElse b body1 body2 _) p =
                              checkStmts (body1 ++ body2) (removeIdsFromExp b p)
checkStmt (IfThen b body _)            p = checkStmts body (removeIdsFromExp b p)
checkStmt (WhileDo b body _)           p = checkStmts body (removeIdsFromExp b p)
checkStmt (Call id exps _)             p = useMod id (checkExps p exps)
checkStmt (Write exp _)                p = removeIdsFromExp exp p
checkStmt (WriteNum exp _)             p = removeIdsFromExp exp p
checkStmt (WriteLn _)                  p = p
checkStmt (Return exp _)               p = removeIdsFromExp exp p

checkExps p exps = foldl (flip removeIdsFromExp) p exps

removeIdsFromExp (And e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Or e1 e2  _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Eq e1 e2  _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (NEq e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Lt e1 e2  _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (LEq e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Gt e1 e2  _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (GEq e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Add e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Sub e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Mul e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Div e1 e2 _) p     = removeIdsFromExp e2 (removeIdsFromExp e1 p)
removeIdsFromExp (Not e     _) p     = removeIdsFromExp e p
removeIdsFromExp (Id ide    _) p     = useId ide p
removeIdsFromExp (Func ide exps _) p = useMod ide (checkExps p exps)
removeIdsFromExp x             p = p

checkConstsDec = flip (foldr checkConstDec)
checkConstDec (Const _ exp _) = removeIdsFromExp exp