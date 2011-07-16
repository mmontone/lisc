USE32
GLOBAL _start
CPU 386
 
SECTION .data
_overflow_msg: db 'Programa abortado: se produjo un error de overflow$'
_overflow_msg_size: dd 51
 
SECTION .text
_start:
CALL _main___neg_
MOV EAX, 1
MOV EBX, 0
INT 80h
 
__write_num_:
PUSH EBP
MOV EBP, ESP
MOV ECX, 0
MOV SI, ''
ADD EAX, 0
JNS _begin_
MOV EBX, -1
IMUL EBX
MOV SI, '-'
_begin_:
MOV EBX, 0
MOV EDX, 0
MOV EDI, 10
IDIV EDI
MOV BL, DL
ADD BL, '0'
INC ECX
INC ECX
PUSH BX
CMP EAX, 0
JNZ _begin_
PUSH SI
INC ECX
INC ECX
MOV EDX, ECX
MOV EAX, 4
MOV EBX, 1
MOV ECX, ESP
INT 80h
MOV ESP, EBP
POP EBP
RET
 
_main___neg_:
ENTER 4, 1
MOV DWORD [EBP - 8], 20
PUSHA
MOV EAX, DWORD [EBP - 8]
ADD EAX, 0
JS _label_0
MOV EDX, 0
JMP _label_1
_label_0:
MOV EDX, -1
_label_1:
MOV EAX, 0
MOV ECX, 10
SUB EAX, ECX
MOV EBX, EAX
POP EAX
IDIV EBX
CALL __write_num_
POPA
PUSH 10
MOV EAX, 4
MOV EBX, 1
MOV ECX, ESP
MOV EDX, 1
INT 80h
INC ESP
LEAVE
RET
 
_overflow_error:
MOV EAX, 4
MOV EBX, 1
MOV ECX, _overflow_msg
MOV EDX, DWORD [_overflow_msg_size]
INT 80h
MOV EAX, 1
MOV EBX, 1
INT 80h
 
 
SECTION .bss
