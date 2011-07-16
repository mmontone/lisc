module Main where
import System.Environment
import Grammar
import CodeGen
import IO
import Control.Monad.State
import Control.Monad.Error
import I386Types
import System.Cmd
import System.Directory


data Options = Options {asmOnly::Bool,            -- Solo genera assembler
                        compileOnly::Bool,        -- No hace linking
                        allowWarnings::Bool,      -- Acepta que aparezcan warnings 
                        outName::String,          -- Nombre del archivo compilado
                        giveHelp::Bool,           -- Mostrar ayuda
                        compOpts::CompOptions}    -- Opciones que afectan al codigo generado

defaultOptions = Options False False True "./a.out" False (CompOptions False)

askParams = do putStrLn "LISC (Compilador de Lenguaje Imperativo Simple)"
               putStrLn "Para obtener ayuda: lisc -h"

readParams::Options -> [String] -> Either String (String, Options)
readParams op ("-co":ss) = let copts = compOpts op
                           in  readParams op{compOpts = copts{checkOverflow = True}} ss
readParams op ("-s": ss) = readParams op{asmOnly = True} ss
readParams op ("-c":ss) = readParams op{compileOnly = True} ss
readParams op ("-st":ss) = readParams op{allowWarnings = False} ss
readParams op ("-o":name:ss) = readParams op{outName = name} ss
readParams op ["-h"] = return ("", op{giveHelp = True})
readParams op [fileName] = return (fileName,op)
readParams op (s:ss) = fail $  "Parametro '" ++ s ++ "' no reconocido"
                   

showHelp = "Ayuda de LISC (Compilador de Lenguaje Imperativo Simple)\n" ++
           "SINTAXIS: lisc [opciones] archivo\n" ++
           "Opciones:\n" ++
           "-h  : Acceder a esta ayuda\n" ++
           "-s  : Generar solo codigo assembler\n" ++
           "-c  : Solo compilar (no hacer el linking)\n" ++
           "-st : Compilar de forma estricta (no se permiten warnings)\n" ++
           "-co : Chequear overflow en tiempo de ejecucion\n" ++
           "-o archivo : Nombre del ejecutable a generar\n"

 
genAsm fileName op =  do res <- parseFile fileName
                         let asmFile = fst (break (=='.') fileName) ++ ".s~~"
                         case res of
                           Left (p,[]) -> genAsm2 p asmFile op
                           Left (p,ws) -> do putStrLn $ showWarnings ws
                                             if allowWarnings op then genAsm2 p asmFile op
                                                                 else putStrLn "No se compila: hay warnings"
                           Right err   -> putStrLn err

genAsm2 p asmFile op = do  h <- openFile asmFile WriteMode
                           let cOpts = compOpts op
                           let out = getAsmProg $ snd $ runState (genCode p) (initState cOpts p)
                           hPrint h out
                           hClose h
                           let newAsmFile = fst (break (=='.') asmFile) ++ ".asm"
                           if asmOnly op then renameFile asmFile newAsmFile else compile asmFile op

compile asmFile op = do system $ "nasm -f elf " ++ asmFile
                        let objFile = fst (break (=='.') asmFile) ++ ".o"
                        removeFile asmFile
                        if (compileOnly op) then return () else link objFile op

link objFile op = do system $ "ld -s -o " ++ outName op ++ " " ++ objFile
                     removeFile objFile

main = do args <- getArgs
          if null args 
             then askParams
             else do let mparams = readParams defaultOptions args
                     case mparams of
                       Right (fileName, ops) -> if giveHelp ops then putStrLn showHelp
                                                                else genAsm fileName ops
                       Left err              -> putStrLn err