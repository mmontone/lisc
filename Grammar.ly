> {
> module Grammar where
> import Types
> import qualified CheckDup
> import qualified CheckDecl
> import qualified CheckTypes
> import Char
> import Optimizations
> import qualified CheckIdentsUse
> import Namemangling
> }

> %name lis
> %tokentype { Token }

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { TEOF }

> %token
>       program         { TProgram   }
>       id              { TId $$     }
>       proc            { TProc      }
>       fun             { TFun       }
>       return          { TRet       }
>       cint            { TCInt $$   }
>       cbool           { TCBool $$  }
>       cstring         { TCString $$}
       cfloat          { TCFloat $$ }
>       type            { TType $$   }
>       if              { TIf        } 
>       then            { TThen      }
>       else            { TElse      }
>       while           { TWhile     }
>       do              { TDo        }
>       write           { TWrite     }
>	writeln		{ TWriteLn   }
>       writenum        { TWriteNum  }
>       begin           { TBegin     }
>       end             { TEnd       }
>       '&&'            { TAnd       }
>       '||'            { TOr        }
>       not             { TNot       }
>       '='             { TAssign    }
>       '=='            { TEq        }
>       '/='            { TNEq       }
>       '<'             { TLt        }
>       '<='            { TLE        }
>       '>'             { TGt        }
>       '>='            { TGE        }
>       '+'             { TAdd       }
>       '-'             { TSub       }
>       '*'             { TTimes     }
>       '/'             { TDiv       }
>       ':'             { TE         }
>       '('             { TOB        }
>       ')'             { TCB        }
>       ';'             { TSep       }
>       ','             { TSepP      }
                      
> %nonassoc '>' '<' '>=' '<=' '==' '/=' ':='
> %right '='
> %left '&&' '||' ';'
> %left not
> %left '+' '-'
> %left '*' '/'
> %right ':='

> %%

> PROGRAM::     {Program}
>               :program id DECLARATIONS  CODEBLOCK      {% \s l -> returnP (Program (Proc $2 [] $3 $4 l)) s l}

> DECLARATIONS::{Declarations}
>               :{- empty -}		      {[]}
>               |DECLARATION		      {[$1]}
>               |DECLARATION ';' DECLARATIONS {$1 : $3}


> DECLARATION:: {Declaration}
>               :id ':' EXP                                        {% \s l -> returnP (Const $1 $3 l) s l}
>               |type  id                                          {% \s l -> returnP (Var $2 $1 l) s l}
>               |proc id '(' PARAMETERS ')' DECLARATIONS CODEBLOCK {% \s l -> returnP (Proc $2 $4 $6 $7 l) s l}
>               |fun  id '(' PARAMETERS ')' ':' type DECLARATIONS CODEBLOCK {% \s l -> returnP  (Fun $2 $4 $7 $8 $9 l) s l}

> PARAMETERS:: {Parameters}
>              : {- empty -}                    {[]}
>              | PARAMETER			{[$1]}
>              | PARAMETER ';' PARAMETERS       {$1:$3 }

> PARAMETER:: {Parameter}
>              : type id   {% \s l -> returnP (Param $2 $1 l) s l}                      

> CODEBLOCK::      {Statements}
>              : begin STATEMENTS end          {$2}

> STATEMENTS::  {Statements}
>               :{- empty -}               {[]}
>               |STATEMENT	           {[$1]}
>               |STATEMENT ';' STATEMENTS  {$1 : $3}

> CODESTAT :: {Statements}
> CODESTAT:  STATEMENT {[$1]}
>          | CODEBLOCK {$1}

> STATEMENT::   {Statement}
>               :id '=' EXP                             {% \s l -> returnP (Assign $1 $3 l) s l}
>               |if EXP then CODESTAT else CODESTAT     {% \s l -> returnP (IfThenElse $2 $4 $6 l) s l}
>               |if EXP then CODESTAT                   {% \s l -> returnP (IfThen $2 $4 l) s l}
}
>               |while EXP do CODESTAT                  {% \s l -> returnP (WhileDo $2 $4 l) s l}
>               |id '(' EXPS ')'                        {% \s l -> returnP (Call $1 $3 l) s l}
>               |write id                               {% \s l -> returnP (Write (Id $2 l) l) s l}
>               |write cstring                          {% \s l -> returnP (Write (CString $2 l) l) s l}
>               |writenum EXP                           {% \s l -> returnP (WriteNum $2 l) s l}
>               |writeln                                {% \s l -> returnP (WriteLn l) s l}
>		|return EXP				{% \s l -> returnP (Return $2 l) s l}

> EXP:: {Exp}
>               :cbool                  {% \s l -> returnP (CBool $1 l) s l}
>               |EXP '&&' EXP           {% \s l -> returnP (And $1 $3 l) s l}
>               |EXP '||' EXP           {% \s l -> returnP (Or $1 $3 l) s l}
>               |not EXP                {% \s l -> returnP (Not $2 l) s l}
>               |EXP '==' EXP           {% \s l -> returnP (Eq $1 $3 l) s l}
>               |EXP '/=' EXP           {% \s l -> returnP (NEq $1 $3 l) s l}
>               |EXP '<'  EXP           {% \s l -> returnP (Lt $1 $3 l) s l}
>               |EXP '<=' EXP           {% \s l -> returnP (LEq $1 $3 l) s l}
>               |EXP '>'  EXP           {% \s l -> returnP (Gt $1 $3 l) s l}
>               |EXP '>=' EXP           {% \s l -> returnP (GEq $1 $3 l) s l}
>               |cint                   {% \s l -> returnP (CInt $1 l) s l}
               |cfloat                 {% \s l -> returnP (CFloat $1 l) s l}
>               |EXP '+'  EXP           {% \s l -> returnP (Add $1 $3 l) s l}
>               |EXP '-'  EXP           {% \s l -> returnP (Sub $1 $3 l) s l}
>               |EXP '*'  EXP           {% \s l -> returnP (Mul $1 $3 l) s l}
>               |EXP '/'  EXP           {% \s l -> returnP (Div $1 $3 l) s l}
>               |'-' EXP                {% \s l -> returnP (Sub (CInt 0 l) $2 l) s l}
>               |cstring                {% \s l -> returnP (CString $1 l) s l}
>               |id                     {% \s l -> returnP (Id $1 l) s l}
>               |id '(' EXPS ')'        {% \s l -> returnP (Func $1 $3 l) s l}
>               |'(' EXP ')'            {% \s l -> returnP $2 s l}

> EXPS:: {Exps}
>               :{- empty -}						{[]}
>               |EXP						{[$1]}
>               |EXP ',' EXPS                              {$1 : $3}

> {
> type Warnings = [String]

> data ParseResult a
>       = ParseOk a Warnings
>       | ParseFail String

> type P a = String -> LineNumber -> Warnings -> ParseResult a

> getLineNum :: P Int
> getLineNum = \s line w -> ParseOk line []

> getWarnings :: P Warnings
> getWarnings = \s l w -> ParseOk w []

> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s l w -> case m s l w of
>                         ParseFail msg -> ParseFail msg
>                         ParseOk a w -> k a s l w

> returnP :: a -> P a
> returnP a = \s l w -> ParseOk a w

> happyError :: P a
> happyError = getLineNum `thenP` \line ->
>              getWarnings `thenP` \ws -> 
>              failP $ (showWarnings ws) ++ "Error de parsing en linea " ++ show line ++ "\n"

> lexError :: String -> P a
> lexError msg = getLineNum `thenP` \line ->
>                getWarnings `thenP` \ws -> 
>                failP $ (showWarnings ws) ++ "Error lexico en linea " ++ show line ++ ": " ++ msg ++ "\n"

> failP :: String -> P a
> failP err = \s l w -> ParseFail err

> lexer :: (Token -> P a) -> P a
> lexer cont []           = cont TEOF []
> lexer cont ('\'':cs)    = let (_, cs') = break (== '\n') cs in \line -> lexer cont cs' line
> lexer cont ('\n':cs)    = \line -> lexer cont cs (line + 1)
> lexer cont (c:cs)       | isSpace c = lexer cont cs
>                         | isAlpha c = lexAlpha cont (c:cs)
>                         | isDigit c = lexNum   cont (c:cs)
> lexer cont ('.':cs)     = lexNum cont ('.':cs)
> lexer cont ('"':cs)     = lexString cont cs
> lexer cont ('+':cs)     = cont TAdd    cs
> lexer cont ('-':cs)     = cont TSub    cs
> lexer cont ('*':cs)     = cont TTimes  cs
> lexer cont ('=':'=':cs) = cont TEq     cs
> lexer cont ('=':cs)     = cont TAssign cs
> lexer cont (':':cs)     = cont TE      cs
> lexer cont ('/':'=':cs) = cont TNEq    cs
> lexer cont ('/':cs)     = cont TDiv    cs
> lexer cont ('<':'=':cs) = cont TLE     cs
> lexer cont ('<':cs)     = cont TLt     cs
> lexer cont ('>':'=':cs) = cont TGE     cs
> lexer cont ('>':cs)     = cont TGt     cs
> lexer cont ('&':'&':cs) = cont TAnd    cs
> lexer cont ('(':cs)     = cont TOB     cs
> lexer cont (')':cs)     = cont TCB     cs
> lexer cont (';':cs)     = cont TSep    cs
> lexer cont (',':cs)     = cont TSepP   cs
> lexer cont ('|':'|':cs) = cont TOr     cs
> lexer cont cs           = lexError ("simbolo no esperado '" ++ [head cs] ++ "'") cs 

> lexNum cont cs = let (num,rest) = lexNum1 cs
>		   in cont (TCInt (read num)) rest
>                  where lexNum1  cs = span isDigit cs

                   in case rest of                    
                       ('.':r) -> let (num',rest') = lexNum1 r
                                  in case num' of 
                                       [] -> lexError "numero real mal escrito" cs
                                       n' -> cont (TCFloat (read ('0':num ++ '.':n'))) rest'
                       (s:r)   -> if s `elem` [' ',';',')','\n',',','+','-','*','/'] 
                                  then 
                                  else lexError "numero mal escrito" cs
                       []      -> cont (TCInt (read num)) rest

> lexString cont cs =
>        \l w -> let (str,x:rest) = span (\c -> (c /= '"') && (c /= '\n')) cs
>                    w' = w ++ [("Warning: en la linea " ++ show l ++ " string terminado con fin de linea")]
>                in case x of
>                     '"'  -> cont (TCString str) rest l w 
>                     '\n' -> cont (TCString str) rest l w' 
                                 
> lexAlpha cont cs = case span (\c ->isAlpha c || isDigit c) cs of
>                     ("program",rest) -> cont TProgram        rest
>                     ("True",   rest) -> cont (TCBool True)   rest
>                     ("False",  rest) -> cont (TCBool False)  rest
>                     ("if",     rest) -> cont TIf             rest
>                     ("then",   rest) -> cont TThen           rest
>                     ("else",   rest) -> cont TElse           rest
>                     ("while",  rest) -> cont TWhile          rest
>                     ("do",     rest) -> cont TDo             rest
>                     ("begin",  rest) -> cont TBegin          rest
>                     ("end",    rest) -> cont TEnd            rest
>                     ("proc",   rest) -> cont TProc           rest
>                     ("fun",    rest) -> cont TFun            rest
>                     ("return", rest) -> cont TRet            rest
>                     ("bool",   rest) -> cont (TType Boolean) rest
>                     ("int",    rest) -> cont (TType Number)  rest
>                     ("string", rest) -> cont (TType Str)     rest
>                     ("real",   rest) -> cont (TType Real)    rest
>                     ("writeln",  rest) -> cont TWriteLn      rest
>                     ("writenum",  rest) -> cont TWriteNum    rest
>                     ("write",  rest) -> cont TWrite          rest
>                     ("not",    rest) -> cont TNot            rest
>                     (id,       rest) -> cont (TId id)        rest
>                     (s,r)            -> lexError ("simbolo no esperado '" ++  s ++ "'") cs 

> basicParse :: String -> (Program,Warnings)
> basicParse s = case lis s 1 [] of
>                  ParseOk p w -> (p,w)
>                  ParseFail msg -> error msg

> parse :: String -> Either (Program,Warnings) String
> parse s =  case lis s 1 [] of
>                  ParseOk p ws -> let  ws' = CheckIdentsUse.check p
>                                  in CheckDup.check p `ifNoErr`
>                                     CheckDecl.check p `ifNoErr`
>                                     CheckTypes.check p `noErr`
>	                              Left (namemangling(declStr(optimize p)),ws'++ ws )
>                  ParseFail msg -> Right msg

 
> showWarnings = (concatMap (\s -> s ++ "\n")). filter (/="")

> parseFile :: FilePath -> IO (Either (Program,Warnings) String)
> parseFile f = do s <- readFile f
>                  return $ parse s 
            
> }
