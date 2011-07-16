module I386Types where

import Control.Monad.State -- Para el manejo de estado
import PrettyPrint
import Types
import Debug.Trace
-- Pregunta: por que un mismo valor no puede pertenecer a tipos distintos.
--           Como puedo simularlo


-- Registers
data Reg  = AL | BL | CL | DL | AH | BH | CH | DH |         -- 8 bits registers
            AX | BX | CX | DX | SP | BP | SI | DI |         -- 16 bits registers
            EAX | EBX | ECX | EDX | ESP | EBP | ESI | EDI | -- 32 bits registers
            CS | DS | ES | FS | GS | SS                     -- Segment registers
            deriving (Eq, Show)
data Flag  = CF | PF | AJ | ZF | SF | OF deriving (Eq,Show)

-- Functions

is8BitsReg AL = True
is8BitsReg BL = True
is8BitsReg CL = True
is8BitsReg DL = True
is8BitsReg AH = True
is8BitsReg BH = True
is8BitsReg CH = True
is8BitsReg DH = True
is8BitsReg _  = False

is16BitsReg AX = True
is16BitsReg BX = True
is16BitsReg CX = True
is16BitsReg DX = True
is16BitsReg SP = True
is16BitsReg BP = True
is16BitsReg SI = True
is16BitsReg DI = True
is16BitsReg _  = False

is32BitsReg EAX = True
is32BitsReg EBX = True
is32BitsReg ECX = True
is32BitsReg EDX = True
is32BitsReg ESP = True
is32BitsReg EBP = True
is32BitsReg ESI = True
is32BitsReg EDI = True
is32BitsReg _  = False

-- Addresses
type Memory = Int
data MemRef = IndirectRef Reg Int | DirectRef Memory | LabRef String -- [BX] o [1000] (Ram Access)
type Offset  = MemRef
type Segment = MemRef 

--             [BX] o [1000]    | BX : 1000                    
data MemAddr = LocalAddr Offset | FarAddr Segment Offset -- RAM Access

-- Label se refiere tanto a una variable como a un desplazamiento en el codigo
data Dir = RegDir {dirReg::Reg}                       | -- A register or a memory address or the top of the stack
           MemDir {dirMem::MemAddr, dirSize::CPUSize} | -- o una direccion inmediata (int literal)
           StackTop                                   | 
           ImmDir {dirVal::Int}                       |
           LabDir {dirLab::String}                    |
           StrDir {dirStr::String}

isRegDir (RegDir _) = True
isRegDir _ = False

data CPUSize = BYTE | WORD | DWORD deriving Show

-- Devuelve el tamaño de los tipos en bytes
typeSize Number = DWORD
typeSize Boolean = BYTE

-- Devuelve los bytes correspondientes a un CPUSize
cpuSizeBytes BYTE = 1
cpuSizeBytes WORD = 2
cpuSizeBytes DWORD = 4



-- Addresses pretty printing
instance Show MemRef where
  show (IndirectRef r n) | n == 0 = show r
  show (IndirectRef r n) | n < 0  = show r ++ " - " ++ show (abs n)   
  show (IndirectRef r n) | n > 0  = show r ++ " + " ++ show n
  show (DirectRef m) = show m
  show (LabRef s) = s

instance Show MemAddr where
  show (LocalAddr mr) = "[" ++ show mr ++ "]"
  show (FarAddr seg off) = "[" ++ show seg ++ ":" ++ show off ++ "]"
  
instance Show Dir where
  show (RegDir r) = show r
  show (MemDir m s) = show s ++ " " ++ show m 
  show (ImmDir d) = show d
  show (LabDir l) = l
  show (StrDir s) = "'" ++ s ++ "'"

-- Tipos de CPU
data CPUType = CPU8086 | CPU286 | CPU386 | CPU486 | CPU586 | CPUPENTIUM

instance Show CPUType where
  show CPU8086 = "8086"
  show CPU286  = "286"
  show CPU386  = "386"
  

-- Header
data HeaderDecl = USE16 | USE32 | EXTERN String | GLOBAL String | CPU CPUType

instance Show HeaderDecl where
  show USE16 = "USE16"
  show USE32 = "USE32"
  show (GLOBAL s) = "GLOBAL " ++ s
  show (CPU cpu) = "CPU " ++ show cpu

-- Constants
data ConstDecl     = ConstBoolDecl String Bool | ConstIntDecl String Int | ConstStrDecl String String      -- Data section

instance Show ConstDecl where
  show (ConstIntDecl id n) = id ++ ": dd " ++ show n
  show (ConstStrDecl id s) = id ++ ": db '" ++ s ++ "'\n" ++
                             id ++ "_size:" ++ " dd " ++ show (length s)
  show (ConstBoolDecl id b) = id ++ ": db " ++ show (if b then 1 else 0)  

data GlobalVarDecl    = GlobalVarDecl String Int deriving Show  -- Bss section. Int = bytes to alloc


-- Code section. No hay mods dentro de otros
data ModDecl = ModDecl String [(String, AsmInst)] 


-- Proc pretty printing
ident = 10 :: Int

instance Show ModDecl where
  show p = show $ ppModDecl p

ppModDecl (ModDecl n is) = text (n ++ ":") $$
                             foldr (($$) . ppInst) empty is

ppInst (lab, inst) = text lab <> text (show inst)
                      where spaces = last (take (ident - length lab) (iterate (<> space) empty))     

-- i386 instructions
data AsmInst = MOV Dir Dir | -- Ojo: no funciona cualquier combinacion de direcciones
                             -- Con los tipos definidos asi no se chequea
               ADD Dir Dir |
               ADC Dir Dir |
               SUB Dir Dir |
               IMUL Dir     |
               IDIV Dir     |
               PUSH Dir    |  -- Apila en [SS:ESP]
               POP Dir     |   
               CMP Dir Dir |
               CALL Dir    |
               JE Dir      |
               JNE Dir     |
               JS Dir      |
               JNS Dir     |
 	       JZ Dir      |
	       JNZ Dir     |
               JG Dir      |
               JGE Dir     |
               JL Dir      |
               JLE Dir     |
               JC Dir      |
               JNC Dir     |
               JO Dir      |
               JNO Dir     |
               JMP Dir     |
	       INC Dir     |
               RET         |
               INT Int     |
               ENTER Int Int |
	       PUSHA |
	       POPA |
               LEAVE |
               LiteralInst String

instance Show AsmInst where
  show (MOV d1 d2) = "MOV " ++ show d1 ++ ", " ++ show d2
  show (ADD d1 d2) = "ADD " ++ show d1 ++ ", " ++ show d2
  show (ADC d1 d2) = "ADC " ++ show d1 ++ ", " ++ show d2
  show (SUB d1 d2) = "SUB " ++ show d1 ++ ", " ++ show d2
  show (IMUL d) = "IMUL " ++ show d
  show (IDIV d) = "IDIV " ++ show d
  show (PUSH d) = "PUSH " ++ show d
  show (POP d) = "POP " ++ show d
  show (CMP d1 d2) = "CMP " ++ show d1 ++ ", " ++ show d2
  show (CALL d) = "CALL " ++ show d
  show (JE d) = "JE " ++ show d
  show (JNE d) = "JNE " ++ show d
  show (JG d) = "JG " ++ show d
  show (JGE d) = "JGE " ++ show d
  show (JL d) = "JL " ++ show d
  show (JLE d) = "JLE " ++ show d
  show (JC d) = "JC " ++ show d 
  show (JNC d) = "JNC " ++ show d
  show (JO d) = "JO " ++ show d
  show (JNO d) = "JNO " ++ show d
  show (JMP d) = "JMP " ++ show d
  show RET = "RET"
  show (INT n) = "INT " ++ show n ++ "h"
  show (ENTER n m) = "ENTER " ++ show n ++ ", " ++ show m 
  show LEAVE = "LEAVE"
  show PUSHA = "PUSHA"
  show POPA = "POPA"
  show (INC d) = "INC " ++ show d
  show (JZ d) = "JZ " ++ show d
  show (JNZ d)= "JNZ " ++ show d
  show (JS d) = "JS " ++ show d
  show (JNS d) = "JNS " ++ show d
  show (LiteralInst s) = s

-- The assembler program
type AsmHeader = [HeaderDecl]
type DataSection = [ConstDecl]
type CodeSection = [ModDecl]      -- Una lista de procedimientos
type BssSection  = [GlobalVarDecl] -- Not required if vars are placed in the stack

data AsmProg = AsmProg AsmHeader DataSection CodeSection BssSection

instance Show AsmProg where
  show p = show $ ppAsmProg p

ppAsmProg (AsmProg hs ds cs bs) = ppHeader hs $$ space $$
                                  ppDataSection ds $$ space $$
                                  ppCodeSection cs $$ space $$
                                  ppBssSection bs

ppHeader = foldr (($$) . ppHeaderDecl) empty

ppHeaderDecl :: HeaderDecl -> Doc
ppHeaderDecl = text . show 

ppDataSection ds = text "SECTION .data" $$
                   foldr (($$) . ppDataDecl) empty ds
ppDataDecl :: ConstDecl -> Doc
ppDataDecl = text . show 

ppBssSection bs = text "SECTION .bss" $$
                  foldr (($$) . ppBssDecl) empty bs
ppBssDecl:: GlobalVarDecl -> Doc
ppBssDecl = text . show
ppCodeSection cs = text "SECTION .text" $$
                   foldr (\p d -> ppModDecl p $$ space $$ d) empty cs

		   		   

-- The initial program
initAsmProg::CompOptions -> Program -> AsmProg
initAsmProg opts p = AsmProg (initHeader opts p) (initData opts p) (initCode opts p) (initBss opts p)

initHeader _ _ = [USE32, GLOBAL "_start", CPU CPU386]

-- Decision de diseño: las constantes de todos los procedimientos son declaradas de forma
-- global. (Puede ser que nunca se llame a un proc y sus constantes nunca se usen)
initData _ (Program mainProc) = (initDataDecls mainProc) ++ overflowDecls

initDataDecls p = let getDecls p = decl p ++ foldr ((++) . getDecls) [] (filter isMod (decl p))
                      consts = filter isConst (decl p)
                      ds = concatMap initDataDecls (filter isMod (decl p)) 
                  in map toAsmConst consts ++ ds

overflowDecls = [ConstStrDecl "_overflow_msg" "Programa abortado: se produjo un error de overflow$"]

-- Notar que la declaracion de la constante debe estar normalizada
toAsmConst::Declaration -> ConstDecl
toAsmConst (Const id (CInt n _) _) = ConstIntDecl id n
toAsmConst (Const id (CString s _) _) = ConstStrDecl id s
toAsmConst (Const id (CBool b _) _ ) = ConstBoolDecl id b
                                    
initCode opts (Program mainProc)= [ModDecl "_start" [([], CALL (LabDir $ modId mainProc)),                -- Llamar proc principal
                                 ([], MOV (RegDir EAX) (ImmDir 1)), -- Salir
                                 ([], MOV (RegDir EBX) (ImmDir 0)),
                                 ([], INT 80)]] ++ [writeNum]
initBss _ _ = []

-- The state monad
-- Nota: FreeRegs y UnsafeRegs se refieren a un estado global, ExpScope es un estado local
-- Posible optimizacion: llevar el valor actual de los registros, en caso de que se conozca,
-- para evitar asignaciones innecesarias.
type GlobalRegs = [Reg]       -- Registros libres globalmente
type PushedRegs = [Reg]     -- Registros cuyo valor fue temporalmente desplazado a la pila
                            -- Solo me importa saber cuales fueron desplazados en el scope local
type FreeExpRegs = [Reg]    -- Registros que una expresion esta autorizada a utilizar 
type ResReg = Maybe Reg     -- Registro usado por una expresion para poner su resultado
type LabelNum = Int         -- Proximo numero de label a usar
type Labels = [String]      -- Poner o no poner label en la proxima instruccion
data MParams = MParams {mId::String, 
                        mParams::Parameters}deriving Show
type ModParams = [MParams]
data LocalVar = LocalVar {lvarId::String,
                          lvarSize::CPUSize,
                          lvarIndex::Int} deriving Show
type LocalVars = [[LocalVar]] -- Las variables accesibles desde el proc, junto con su tamaño y direccion en la pila

data AsmState = AsmState {getGlobalRegs::GlobalRegs, 
                          getPushedRegs::PushedRegs, 
			  getExpRegs::[(FreeExpRegs, ResReg)],
                          getLabelNum::LabelNum, 
                          getLabels::Labels, 
                          getLocalVars::LocalVars, 
                          getModParams::ModParams,
                          getMod::Declaration,
                          compOptions::CompOptions,
                          getAsmProg::AsmProg} 
--instance Show AsmState where
--      show s = "freeregs = " ++ show(getGlobalRegs s) ++ 
--               "\nunsafe=" ++ show (getPushedRegs s) ++ 
--               "\nuntouch=" ++ show(getResRegs s) ++
--               "\nexpscope=" ++ show(getExpRegs s)

initState::CompOptions -> Program -> AsmState
initState opts p = AsmState{getGlobalRegs = allRegs,
                            getPushedRegs = [],
                            getExpRegs = [],
			    getLabelNum = 0,
                            getLabels = [],
                            getLocalVars = [],
                            getModParams = initModParams p,
                            compOptions = opts,
                            getAsmProg = initAsmProg opts p}

-- Funciones tremendamente utiles
setGlobalRegs fr s = s{getGlobalRegs = fr}
setMod m s = s{getMod = m}
setPushedRegs us s = s{getPushedRegs = us}
setExpRegs sc s = s{getExpRegs = sc}   
setLabelNum ln s = s{getLabelNum = ln}
setLabels ls s = s{getLabels = ls}
setLocalVars lv s = s{getLocalVars=lv}
setModParams mp s = s{getModParams = mp}
setAsmProg p s= s{getAsmProg = p}

type Asm = State AsmState


done::Asm ()
done = return ()


-- Estado inicial
allRegs = [AL, BL, CL, DL, AH, BH, CH, DH,
           AX, BX, CX, DX, SP, BP, SI, DI,
           EAX, EBX, ECX, EDX, ESP, EBP, ESI, EDI,
           CS, DS, ES, FS, GS, SS]

initModParams::Program -> ModParams
initModParams p = initModParams' (mainProc p)

initModParams'::Declaration -> ModParams
initModParams' m = let lmods = filter isMod (decl m)
                   in MParams (modId m) (params m) : concatMap initModParams' lmods      

isData32Reg r = elem r [EAX,EBX,ECX,EDX]

-- Opciones que afectan la generacion de codigo
data CompOptions = CompOptions {checkOverflow::Bool} deriving Show

-- El numero a imprimir esta en EAX
writeNum = ModDecl "__write_num_" [([], PUSH (RegDir EBP)),
				   ([], MOV (RegDir EBP) (RegDir ESP)),
                                   ([], MOV (RegDir ECX) (ImmDir 0)),
                                   ([], MOV (RegDir SI) (StrDir "")),
                                   ([], ADD (RegDir EAX) (ImmDir 0)),
                                   ([], JNS (LabDir "_begin_")),
                                   ([], MOV (RegDir EBX) (ImmDir (-1))),
                                   ([], IMUL (RegDir EBX)),
                                   ([], MOV (RegDir SI) (StrDir "-")),
				   ("_begin_:\n", MOV (RegDir EBX) (ImmDir 0)),
                                   ([], MOV (RegDir EDX) (ImmDir 0)),
                                   ([], MOV (RegDir EDI) (ImmDir 10)),
                                   ([], IDIV (RegDir EDI)),
                                   ([], MOV (RegDir BL) (RegDir DL)),
                                   ([], ADD (RegDir BL) (StrDir "0")),
				   ([], INC (RegDir ECX)),
                                   ([], INC (RegDir ECX)),
				   ([], PUSH (RegDir BX)),
				   ([], CMP (RegDir EAX) (ImmDir 0)),
				   ([], JNZ (LabDir "_begin_")),
                                   ([], PUSH (RegDir SI)),
                                   ([], INC (RegDir ECX)),
                                   ([], INC (RegDir ECX)),
				   ([], MOV (RegDir EDX) (RegDir ECX)),
				   ([], MOV (RegDir EAX) (ImmDir 4)),
				   ([], MOV (RegDir EBX) (ImmDir 1)),
				   ([], MOV (RegDir ECX) (RegDir ESP)),
				   ([], INT 80),
				   ([], MOV (RegDir ESP) (RegDir EBP)),
				   ([], POP (RegDir EBP)),
				   ([], RET)]
				   


