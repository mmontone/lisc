-- | Este modulo provee operaciones para la generacion de codigo assembler
module CodeGen where

import I386Types
import Control.Monad.State
import Data.List
import Types
import Debug.Trace
import IO
import Control.Exception          

-- Funciones primitivas

newModule::String -> Asm ()
newModule name = do s <- get
                    let prog = getAsmProg s
                    put $ setAsmProg  (addModule name prog) s
                  where
                   addModule name (AsmProg hs cs ps vs) = AsmProg hs cs (ps ++ [ModDecl name []]) vs

-- |Agrega una instruccion al modulo actual
exec::AsmInst -> Asm ()
exec inst = do s <- get
               let label = concatMap (\l -> l ++ ":\n") (getLabels s)
               let prog  = getAsmProg s
               put $ setLabels [] $ setAsmProg (addInstProg (label, inst) prog) s

addInstProg inst (AsmProg hs cs mods vs) = AsmProg hs cs (addInstCurMod inst mods) vs

addInstCurMod inst mods = init mods ++ [addInstMod inst (last mods)]

addInstMod inst (ModDecl n is) = ModDecl n (is ++ [inst])

-- Scope
enterNewExpScope:: Asm ()
enterNewExpScope  = do s <- get
                       let sc = getExpRegs s
                       put $ setExpRegs ((allRegs,Nothing):sc) s -- Una expresion esta autorizada a utilizar todos los registros al principio
                       s <- get
                       assert (getExpRegs s /= []) done
                                        
leaveExpScope::Asm ()
leaveExpScope = do s <- get
                   let sc = getExpRegs s
                   put $ setExpRegs (tail sc) s
-- Labels
newLabel::Asm String
newLabel = do s <- get
              let ln = getLabelNum s
              put $ setLabelNum (ln + 1) s
              return $ "_label_" ++ show ln

putLabel l = do s <- get
                put $ setLabels (l : getLabels s) s

-- Variables libres
useGlobalReg r = do s <- get
                    let free = getGlobalRegs s
                    put $ setGlobalRegs (delete r free) s
                    useExpReg r
                                        

freeGlobalReg r = do s <- get
                     let free = getGlobalRegs s
                     put $ setGlobalRegs (r:free) s
                     freeExpReg r
                     s <- get
                     let newfree = getGlobalRegs s
                     assert (newfree /= []) done
                     assert (dontRepeat newfree) done
                     assert (elem r newfree) done
                     b <- isPushed r
                     assert (not b) done

dontRepeat [] = True
dontRepeat (x:xs) = not (elem x xs) && dontRepeat xs

anyFreeGlobalReg = getFreeGlobalReg (const True)

isFreeGlobalReg r = do s <- get
                       let free = getGlobalRegs s
		       return $ elem r free

-- |Retorna quizas un registro libre que cumpla con el predicado
getFreeGlobalReg::(Reg -> Bool) -> Asm (Maybe Reg)
getFreeGlobalReg p = do s <- get
                        mres <- getResReg
                        let expregs = fst (head (getExpRegs s))
                        case mres of
                           Nothing  -> getFreeGlobalReg' (\r -> p r && elem r expregs)
                           Just res -> do x <- getFreeGlobalReg' (\r -> p r && not (r == res) && elem r expregs)
                                          case x of
                                             (Just r) -> do b <- isResReg r
                                                            assert (not b) (return x)
                                                            
                                             _        -> return x
                                          
getFreeGlobalReg' p = do s <- get
                         let fr = getGlobalRegs s
                         case find p fr of
                           Nothing      -> return Nothing
                           reg@(Just r) -> return reg

-- Variables inseguras (que estan en la pila)
isPushed r = do s <- get
                let us = getPushedRegs s
                return $ elem r us

pushReg r = do s <- get
               assert (not (elem r (getGlobalRegs s))) done
               let us = getPushedRegs s
               put $ setPushedRegs (r:us) s
               -- Guardar el valor del reg en la pila
               exec $ PUSH (RegDir r)
               useExpReg r
               

popReg r = do s <- get
              assert (elem r (getGlobalRegs s)) done
              let us = getPushedRegs s
              put $ setPushedRegs (delete r us) s
              exec $ POP (RegDir r)
              freeExpReg r
              b <- isResReg r
              assert (not b) done

isExpReg r = do s <- get
                let (local,_):sc = getExpRegs s
                return $ elem r local

-- Variables libres locales
anyExpReg = getExpReg $ const True

getExpReg::(Reg -> Bool) -> Asm Reg
getExpReg p = do mres <- getResReg
                 case mres of
                    Nothing  -> getExpReg' p
                    Just res -> do x <- getExpReg' (\r -> p r && not (r == res))
                                   b <- isResReg x
                                   assert (not b) done
                                   b <- isExpReg x
                                   assert (b) (return x)
                                   
                                     

getExpReg' p = do s <- get
                  let (local, _):sc = getExpRegs s
	          case find p local of
                          Nothing  -> -- Ups: no quedan registros locales libres
                                     error "No hay registros locales libres. Esto no puede ocurrir. Solucion: libera variables locales."
                          Just flr -> return flr

freeExpReg r = do s <- get
                  let (local,res):sc = getExpRegs s
                  put $ setExpRegs (((r:local),res):sc) s

useExpReg r = do s <- get
                 let (local,res):sc = getExpRegs s
                 put $ setExpRegs (((delete r local),res):sc) s

-- Funciones
-- Declara que quiere usarse un registro para una operacion
useReg r = do isres <- isResReg r
              useReg' r isres
              
useReg' r True = useExpReg r
                
useReg' r _ =  do inchain <- isInResChain r 
                  useReg'' inchain r 

useReg'' True r = do exec $ PUSH (RegDir r)
                     useExpReg r
useReg'' _    r = do -- Ver que hacer con el registro segun su estado
		      free <- isFreeGlobalReg r
                      if free 
                         then do -- El registro esta libre globalmente
                                 useGlobalReg r
                       
                         else do -- El registro ya se esta usando globalmente
                                 -- Debe ser recuperado de la pila
                                 pushReg r
        

useAnyNewReg = useNewReg (const True)

-- Utiliza un registro que cumpla con el predicado
-- El registro debe liberarse con releaseReg
useNewReg:: (Reg -> Bool) -> Asm Reg
useNewReg  p = do mfr <- getFreeGlobalReg p
                  case mfr of
                    Just fr   -> do -- Obtuvimos un registro libre globalmente
                                    useGlobalReg fr
                                    b <- isResReg fr
                                    assert (not b) done
                                    return fr
                    Nothing   -> do
                                    -- No hay registros libres globalmente
                                    -- Solo puedo usar un registro que no se este usando localmente
				    fr <- getExpReg p
                                    pushReg fr
                                    b <- isResReg fr
                                    assert (not b) done
                                    return fr
                                                                  

-- Libera el uso de un registro determinado
-- El orden de llamada es importante porque se usa la pila
releaseReg :: Reg -> Asm ()
releaseReg r = do isres <- isResReg r
                  releaseReg' r isres

releaseReg' r True = freeExpReg r

releaseReg' r _    = do inchain <- isInResChain r
                        releaseReg'' inchain r                     

releaseReg'' True r = do exec $ POP (RegDir r)
                         freeExpReg r

releaseReg'' _    r = do -- Ver que hacer segun su estado
                         pushed <- isPushed r
                         if pushed 
                           then do -- El registro contenia otro valor, bajar valor de la pila
                                   -- No queda free
                                   popReg r
                           else do -- El registro era global
                                   freeGlobalReg r
                                             
      
-- Copia direcciones de memoria
-- La dir es la pila, entonces baja el dato
-- No setea una var libre
-- Mueve solo si es necesario (las direcciones son distintas)

moveDir _ (ImmDir _)      = error "Error tratando de mover datos"
moveDir StackTop StackTop = error "Error tratando de mover datos"
moveDir (RegDir r1) (RegDir r2) | r1 == r2 = done
--moveDir (MemDir m1 _) (MemDir m2 _) = error "No se puede mover de memoria a memoria"
moveDir m1@(MemDir _ _) m2@(MemDir _ _) = do enterNewExpScope
                                             nr <- useNewReg is32BitsReg
                                             exec $ MOV (RegDir nr) m1
                                             exec $ MOV m2 (RegDir nr)
                                             leaveExpScope
moveDir StackTop dir = exec $ POP dir
moveDir dir StackTop = exec $ PUSH dir
moveDir d1 d2 = exec $ MOV d2 d1


setResReg r = do s <- get
                 let (rs,_):rss = getExpRegs s
                 put $ setExpRegs ((rs,Just r):rss) s

getResReg::Asm (Maybe Reg)
getResReg = do s <- get
               let (_,r):_ = getExpRegs s
	       return r

isResReg r = do s <- get
                let (_,mr):_ = getExpRegs s
                case mr of
                    Nothing -> return False
                    Just r' -> return $ r == r' 

isInResChain r = do s <- get
                    return $ foldr ((||). p) False (map snd (getExpRegs s))
                 where p Nothing = False
                       p (Just res) = r == res

setResDir reg@(RegDir r) = setResReg r
setResDir _ = done
                                
-- Evaluacion de una expresion
-- Evalua la expresion Exp y pone el resultado en el registro Dir
evalExp:: Exp -> Dir -> Asm ()
evalExp (Div e1 e2 _ ) res = do -- Entramos en un nuevo scope
                                enterNewExpScope
                                setResDir res
                                -- El primer operando va en EDX:EAX
                                -- ATENCION: no debo hacer push de la direccion destino
                                useReg EAX
                                useReg EDX
				negLab <- newLabel
                                listo  <- newLabel
                                evalExp e1 $ (RegDir EAX)
                                exec $ ADD (RegDir EAX) (ImmDir 0)
                                exec $ JS (LabDir negLab)
                                exec $ MOV (RegDir EDX) (ImmDir 0)
				exec $ JMP (LabDir listo)
                                putLabel negLab
                                exec $ MOV (RegDir EDX) (ImmDir (-1))
				putLabel listo
                                nr <- useNewReg isData32Reg
                                assert (nr /= EAX && nr /= EDX) done
                                evalExp e2 $ RegDir nr
                                -- Hacemos la division
                                exec $ IDIV $ RegDir nr 
                                -- El resultado esta en EAX. Muevo el resultado.
                                moveDir (RegDir EAX) res   
                                -- Libero los registros usados
                                releaseReg nr
                                releaseReg EDX
                                releaseReg EAX
                                -- Salgo del scope 
                                leaveExpScope
                                
                                

evalExp (Mul e1 e2 _ ) res = do -- Entramos en un nuevo scope
                                enterNewExpScope
                                setResDir res
                                -- El primer operando va en EDX:EAX
                                useReg EAX
                                useReg EDX
                                -- Se aconseja usar moveDir en vez de exec $ MOV ...
                                evalExp e1 $ (RegDir EAX)
                                nr <- useNewReg isData32Reg
                                assert (nr /= EAX && nr /= EDX) done
                                evalExp e2 $ RegDir nr
                                -- Hacemos la mult
                                exec $ IMUL $ RegDir nr 
                                -- Chequeamos overflow si es necesario
                                seeOverflow
                                -- El resultado esta en EDX:EAX. Muevo el resultado.
                                moveDir (RegDir EAX) res   
                                -- Libero los registros usados
                                releaseReg nr
                                releaseReg EDX
				releaseReg EAX
                                -- Salgo del scope 
                                leaveExpScope

evalExp (Add e1 e2 _) res = do enterNewExpScope
                               setResDir res
			       nr1 <- useNewReg isData32Reg
                               evalExp e1 (RegDir nr1)
                               nr2 <- useNewReg isData32Reg
                               evalExp e2 (RegDir nr2)
                               --trace ("After ADD " ++ show s) done
                               -- Hacemos la suma
                               exec $ ADD (RegDir nr1) (RegDir nr2)
                               -- Chequeamos overflow si es necesario
                               seeOverflow
			       moveDir (RegDir nr1) res
                               releaseReg nr2
			       releaseReg nr1
                               leaveExpScope



evalExp (Sub e1 e2 _) res = do enterNewExpScope
                               setResDir res
			       nr1 <- useNewReg isData32Reg
                               evalExp e1 (RegDir nr1)
                               nr2 <- useNewReg isData32Reg
                               evalExp e2 (RegDir nr2)
                               -- Hacemos la resta
                               exec $ SUB (RegDir nr1) (RegDir nr2)
                               -- Chequeamos overflow si es necesario
			       seeOverflow
			       moveDir (RegDir nr1) res
                               releaseReg nr2
			       releaseReg nr1
                               leaveExpScope
               
evalExp (CInt n _) res = moveDir (ImmDir n) res
                         
evalExp (CBool True _) res = moveDir (ImmDir 1) res
evalExp (CBool False _) res = moveDir (ImmDir 0) res

evalExp f         res   | isFunc f = do  mp <- lookupModParams (funcId f)
                                         forEach (zip (reverse (funcParams f)) (map paramId mp)) 
                                                (\(pa,pf) -> do evalExp pa res
					 	                exec $ PUSH res)
                                         exec $ PUSH res
					 exec PUSHA
                                         exec $ CALL (LabDir (funcId f))
					 exec POPA
                                         exec $ POP res
					 forEach mp $ const (exec $ ADD (RegDir ESP) (ImmDir 4))
                                         					  
evalExp (Id id _) res@(RegDir r)   =  withVarDo id (\v ->moveDir v res)
                                      
				      
				      
evalExp (Id id _) res@(MemDir m _) = do enterNewExpScope
                                        nr <- useNewReg isData32Reg
                                        withVarDo id (\v -> moveDir v (RegDir nr))
                                        moveDir (RegDir nr) res
                                        releaseReg nr
                                        leaveExpScope

evalExp e _ = error $ "falta declarar evalExp para " ++ (show e)

allRegsSize::Int
allRegsSize = 32

jumpIfFalse' e1 e2 inst = do   enterNewExpScope
                               nr1 <- useNewReg isData32Reg
                               evalExp e1 (RegDir nr1)
                               nr2 <- useNewReg isData32Reg
                               evalExp e2 (RegDir nr2)
                               exec $ CMP (RegDir nr1) (RegDir nr2)
                               -- Release antes de saltar
                               releaseReg nr1
                               releaseReg nr2
                               leaveExpScope
                               -- Saltar si falso
                               exec inst

jumpIfFalse (Eq e1 e2 _) dir = jumpIfFalse' e1 e2 (JNE dir)
jumpIfFalse (NEq e1 e2 _) dir = jumpIfFalse' e1 e2 (JE dir)
jumpIfFalse (Gt e1 e2 _) dir = jumpIfFalse' e1 e2 (JLE dir)
jumpIfFalse (GEq e1 e2 _) dir = jumpIfFalse' e1 e2 (JL dir)
jumpIfFalse (Lt e1 e2 _) dir = jumpIfFalse' e1 e2 (JGE dir)
jumpIfFalse (LEq e1 e2 _) dir = jumpIfFalse' e1 e2 (JG dir)
                      
                              

jumpIfFalse (And e1 e2 _) dir = do falseLab  <- newLabel
                                   jumpIfFalse e1 (LabDir falseLab)
                                   -- La primera expresion es verdadera
                                   jumpIfFalse e2 dir
                                   -- La primera expresion es falsa, el and es falso   
                                   putLabel falseLab
                                                             

jumpIfFalse (CBool True _) dir = done
jumpIfFalse (CBool False _) dir = exec $ JMP dir

-- Statements
-- Aux
forEach::Monad m => [a] -> (a -> m ()) -> m ()

forEach [] _ = return ()
forEach (x:xs) f  = do f x
                       forEach xs f
    
genStatCode::Statement->Asm()

-- Asignacion
-- Hay que buscar la direccion de la variable y computar con ella
-- ADVERTENCIA: no se puede invocar withVarDo desde dentro de withVarDo
withVarDo id f = do s <- get
                    n <- getNesting                 
                    withVarDo' id f (-(n - 1) * 4) False (getLocalVars s)                    

withVarDo' id f _ _ [] = do s <- get
                            let mid = modId (getMod s)
                            f (MemDir (LocalAddr (LabRef (mid ++ id))) DWORD)

withVarDo' id f i push (vs:vss) =  
               case find ((==id) . lvarId) vs of
                  Nothing -> do -- Buscar la variable en el scope de mas afuera
                                withVarDo' id f (i + 4) True vss
                  Just lv -> -- La variable local fue encontrada
                             -- Direccionarla
			     let action = f (MemDir (LocalAddr $ IndirectRef EBP (lvarIndex lv))(lvarSize lv))
                             in accessScope push i action

accessScope access i f | access =  do exec $ PUSH (RegDir EBP)
                                      moveDir (MemDir (LocalAddr $ IndirectRef EBP i) DWORD) (RegDir EBP)
                                      f
                                      exec $ POP (RegDir EBP)

accessScope _ _ f | otherwise = f
                       

genStatCode (Assign id e _ ) = withVarDo id (\v -> evalExp e v) 
				                                     
-- Condicionales
genStatCode (IfThenElse e ss1 ss2 _) = do elseLab <- newLabel
                                          endLab <- newLabel
                                          jumpIfFalse e (LabDir elseLab)
                                          -- Rama verdadera
                                          forEach ss1 genStatCode
                                          exec $ JMP (LabDir endLab)
                                          -- Rama falsa
                                          putLabel elseLab
                                          forEach ss2 genStatCode
                                          putLabel endLab

genStatCode (IfThen e ss _ ) = do endLab <- newLabel
                                  jumpIfFalse e (LabDir endLab)
                                  -- Entra en el if
                                  forEach ss genStatCode
                                  putLabel endLab

-- Loops
genStatCode (WhileDo e ss _) = do beginLab <- newLabel
                                  endLab   <- newLabel
                                  -- El comienzo del loop
                                  putLabel beginLab
                                  jumpIfFalse e (LabDir endLab)
                                  -- Entra en el loop
                                  forEach ss genStatCode
                                  exec $ JMP (LabDir beginLab)
                                  -- Sale del loop
                                  putLabel endLab

-- Llamadas a procs   ald ./configure make make install
genStatCode (Call id p _) = do mp <- lookupModParams id
                               forEach (zip (reverse p) (map paramId mp))
                                       (\(pa,pf) -> do evalExp pa (RegDir ECX)
		                                       exec $ PUSH (RegDir ECX))
                               exec PUSHA
			       exec $ CALL (LabDir id)
			       exec POPA
			       forEach mp $ const (exec $ ADD (RegDir ESP) (ImmDir 4))
			       
-- Salida de strings constantes
genStatCode (Write (Id s _) _) = do exec $ MOV (RegDir EAX) (ImmDir 4)
                                    exec $ MOV (RegDir EBX) (ImmDir 1)
                                    exec $ MOV (RegDir ECX) (LabDir s)
                                    exec $ MOV (RegDir EDX) (MemDir (LocalAddr $ LabRef $ s ++ "_size") DWORD)
                                    exec $ INT 80
genStatCode (Write (CString s _) _) = error "En este momento el compilador no soporta write seguido de un string. Por favor declare el string como una constante y luego usela como parametro de write"

genStatCode (WriteNum e _) = do exec $ PUSHA
				evalExp e (RegDir EAX)
				exec $ CALL (LabDir "__write_num_")
				exec $ POPA

genStatCode (WriteLn _) = do exec $ PUSH (ImmDir 10)
			     exec $ MOV (RegDir EAX) (ImmDir 4)
                             exec $ MOV (RegDir EBX) (ImmDir 1)
                             exec $ MOV (RegDir ECX) (RegDir ESP)
                             exec $ MOV (RegDir EDX) (ImmDir 1)
			     exec $ INT 80
			     exec $ INC (RegDir ESP)

-- Salir de la funcion
genStatCode (Return e _ ) = evalExp e (MemDir (LocalAddr $ IndirectRef EBP (8 + allRegsSize)) DWORD) 
                   
-- TODO: no puede haber variables de tipo string (su tamaño es indeterminado)
-- Generacion de codigo para procedimientos

placeLocalVars vs =  do s <- get
                        -- Calcular indice
                        nest <- getNesting
                        placeLocalVars' (-(nest-1) * 4) (-) varId varType vs

placeParams begin ps = placeLocalVars' begin (+) paramId paramType ps
                        
placeLocalVars' _ _ _ _ [] = return ()
placeLocalVars' index shift getId getType (v:vs) =
              do s <- get
                 let local:sf = getLocalVars s
                 -- Ubicar la variable en el stack frame local
                 let vsize = typeSize $ getType v
                 let lv = LocalVar (getId v) vsize (shift index (cpuSizeBytes vsize))
                 put $ setLocalVars ((lv:local):sf) s
                 -- Ubicar las demas variables
                 placeLocalVars' (shift index (cpuSizeBytes vsize)) shift getId getType vs
                 

enterModuleScope::Asm()
enterModuleScope = do s <- get
                      let sf = getLocalVars s
                      put $ setLocalVars ([]:sf) s

leaveModuleScope::Asm()
leaveModuleScope = do s <- get
                      let local:sf = getLocalVars s
                      put $ setLocalVars sf s

getNesting::Asm Int
getNesting = do s <- get
                return (length (getLocalVars s) + 1)
                                   
enterModule m    = do -- Obtener variables accesibles
                      let modvars = filter isVar (decl m)
                      -- Calcular tamaño del stack frame
                      let framesize = sum $ map (cpuSizeBytes.typeSize.varType) modvars
                      s <- get
                      nesting <- getNesting
                      -- Crear el stack frame
                      exec $ ENTER framesize nesting
                      -- Ubicar las variables locales del modulo en el stack
                      enterModuleScope
                      placeLocalVars modvars 
		      placeParams ((if isFun m then 8 else 4) + allRegsSize) (params m)
                                    
            
genModCode m                 = do newModule (modId m)
                                  s <- get
                                  put $ setMod m s
                                  -- Generar codigo de entrada
                                  enterModule m
                                  -- Generar el codigo del modulo
                                  forEach (stmt m) genStatCode
                                  -- Salir del modulo
                                  exec $ LEAVE
                                  exec $ RET
                                  -- Generar codigo para los modulos hijos
                                  forEach (filter isMod (decl m)) genModCode
                                  leaveModuleScope                                         
				     
genCode (Program p) = do genModCode p
                         genOverflowProc

lookupModParams id = do s <- get
                        return $ mParams (head (filter ((==id) . mId) (getModParams s)))


seeOverflow = do s <- get
                 let check = checkOverflow (compOptions s)
                 if check 
                    then do nooverflow <- newLabel
                            exec $ JNO (LabDir nooverflow)
		            exec $ CALL (LabDir "_overflow_error")
                            putLabel nooverflow
		    else done

genOverflowProc = do newModule "_overflow_error"
                     genStatCode $ Write (Id "_overflow_msg" undefined) undefined
                     exec $ MOV (RegDir EAX) (ImmDir 1) -- Salir
                     exec $ MOV (RegDir EBX) (ImmDir 1) -- Codigo de error 1
                     exec $ INT 80