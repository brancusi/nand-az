// ["push" "constant" "10"]
@10
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
// ["push" "constant" "21"]
@21
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["push" "constant" "22"]
@22
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "argument" "2"]
@2
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
// ["pop" "argument" "1"]
@2
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
// ["push" "constant" "36"]
@36
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "this" "6"]
@3
D=M
@6
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
// ["push" "constant" "42"]
@42
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["push" "constant" "45"]
@45
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "that" "5"]
@4
D=M
@5
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
// ["push" "constant" "510"]
@510
D=A
@SP
A=M
M=D
@SP
M=M+1
// ["pop" "temp" "6"]
@SP
M=M-1
@SP
A=M
D=M
@11
M=D
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
// ["push" "that" "5"]
@4
D=M
@5
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
// ["push" "this" "6"]
@3
D=M
@6
D=D+A
A=D
D=M
@SP
A=M
M=D
@SP
M=M+1
// ["push" "this" "6"]
@3
D=M
@6
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
// ["push" "temp" "6"]
@11
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
(END_LOOP_7-sZ4AkXNoIH8XUjaH5Hs)
@END_LOOP_7-sZ4AkXNoIH8XUjaH5Hs
0;JMP