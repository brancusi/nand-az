// ["function" "SimpleFunction.test" "2"]
(SimpleFunction.test)
@1
D=M
@0
D=D+A
A=D
M=0
A=A+1
M=0
A=A+1
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
// ["push" "local" "1"]
@1
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
// ["not"]
@SP
M=M-1
@SP
A=M
D=M
D=!D
M=D
@SP
M=M+1
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
// ["return"]
//Stash endframe
@LCL
D=M
@endFrame
M=D
@endFrame
D=M
@5
D=D-A
A=D
D=M
//Stash retAddr
@retAddr
M=D
//Pop the stack to args which will be new stack head
@SP
M=M-1
@SP
A=M
D=M
@ARG
A=M
M=D
//Update to Args
@ARG
D=M
@SP
M=D+1
//rehydrate that
@endFrame
D=M
@1
A=D-A
D=M
@4
M=D
//rehydrate this
@endFrame
D=M
@2
A=D-A
D=M
@3
M=D
//rehydrate argument
@endFrame
D=M
@3
A=D-A
D=M
@2
M=D
//rehydrate local
@endFrame
D=M
@4
A=D-A
D=M
@1
M=D
//Return
@retAddr
A=M
0;JMP
(END_LOOP_baraI3wL1hcSNn1W6Nr4u)
@END_LOOP_baraI3wL1hcSNn1W6Nr4u
0;JMP