// ["function" "SimpleFunction.test" "2"]

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

(END_LOOP_Oh3ZGBSDGEXgAiiTn-fqy)
@END_LOOP_Oh3ZGBSDGEXgAiiTn-fqy
0;JMP