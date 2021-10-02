// ["push" "constant" "111"]
@111
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["push" "constant" "333"]
@333
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["push" "constant" "888"]
@888
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "static" "8"]
@SP
M=M-1
@SP
A=M
D=M
@STATIC.8
M=D
// ["pop" "static" "3"]
@SP
M=M-1
@SP
A=M
D=M
@STATIC.3
M=D
// ["pop" "static" "1"]
@SP
M=M-1
@SP
A=M
D=M
@STATIC.1
M=D
// ["push" "static" "3"]
@STATIC.3
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["push" "static" "1"]
@STATIC.1
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
// ["push" "static" "8"]
@STATIC.8
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
(END_LOOP_B9FA5lHgBoQhkFF7RlwnQ)
@END_LOOP_B9FA5lHgBoQhkFF7RlwnQ
0;JMP