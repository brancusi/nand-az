// ["push" "constant" "0"]
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "local" "0"]
@1
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
// ["label" "LOOP_START"]
(LOOP_START)
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
// ["push" "local" "0"]
@1
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
// ["pop" "local" "0"]
@1
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
// ["if-goto" "LOOP_START"]
@SP
M=M-1
@SP
A=M
D=M
@LOOP_START
D;JNE
// ["push" "local" "0"]
@1
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
(END_LOOP_BqQARzdUIpXVyByALxOaI)
@END_LOOP_BqQARzdUIpXVyByALxOaI
0;JMP