// ["push" "argument" "1"]
@2
D=M
@1
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "pointer" "1"]
@SP
M=M-1
@SP
A=M
D=M
@4
M=D
// ["push" "constant" "0"]
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "that" "0"]
@4
D=M
@0
D=D+A
@STASH
M=D
@SP
M=M-1
@SP
A=M
D=M
@STASH
A=M
M=D
// ["push" "constant" "1"]
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "that" "1"]
@4
D=M
@1
D=D+A
@STASH
M=D
@SP
M=M-1
@SP
A=M
D=M
@STASH
A=M
M=D
// ["push" "argument" "0"]
@2
D=M
@0
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["push" "constant" "2"]
@2
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["sub"]
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
@SP
A=M
D=M-D
M=D
@SP
M=M+1
// ["pop" "argument" "0"]
@2
D=M
@0
D=D+A
@STASH
M=D
@SP
M=M-1
@SP
A=M
D=M
@STASH
A=M
M=D
// ["label" "MAIN_LOOP_START"]
(MAIN_LOOP_START)
// ["push" "argument" "0"]
@2
D=M
@0
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["if-goto" "COMPUTE_ELEMENT"]
@SP
M=M-1
@SP
A=M
D=M
@COMPUTE_ELEMENT
D;JNE
// ["goto" "END_PROGRAM"]
@END_PROGRAM
0;JMP
// ["label" "COMPUTE_ELEMENT"]
(COMPUTE_ELEMENT)
// ["push" "that" "0"]
@4
D=M
@0
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["push" "that" "1"]
@4
D=M
@1
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["add"]
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
@SP
A=M
D=M+D
M=D
@SP
M=M+1
// ["pop" "that" "2"]
@4
D=M
@2
D=D+A
@STASH
M=D
@SP
M=M-1
@SP
A=M
D=M
@STASH
A=M
M=D
// ["push" "pointer" "1"]
@4
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["push" "constant" "1"]
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["add"]
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
@SP
A=M
D=M+D
M=D
@SP
M=M+1
// ["pop" "pointer" "1"]
@SP
M=M-1
@SP
A=M
D=M
@4
M=D
// ["push" "argument" "0"]
@2
D=M
@0
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["push" "constant" "1"]
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["sub"]
@SP
M=M-1
@SP
A=M
D=M
@SP
M=M-1
@SP
A=M
D=M-D
M=D
@SP
M=M+1
// ["pop" "argument" "0"]
@2
D=M
@0
D=D+A
@STASH
M=D
@SP
M=M-1
@SP
A=M
D=M
@STASH
A=M
M=D
// ["goto" "MAIN_LOOP_START"]
@MAIN_LOOP_START
0;JMP
// ["label" "END_PROGRAM"]
(END_PROGRAM)
(END_LOOP_SwW1Gwu9s-2hkU-Gs8zmA)
@END_LOOP_SwW1Gwu9s-2hkU-Gs8zmA
0;JMP