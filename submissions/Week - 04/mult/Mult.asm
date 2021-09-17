// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.

// Put your code here.

(RESET)
@R2
M=0

@MULTCOUNTER
M=0

@ACC
M=0

// Check for zeros
@R0
D=M
@ZERO
D;JEQ

@R1
D=M
@ZERO
D;JEQ

// The multiplication loop
(MULTIPLY)
@R0
D=M

// Update the accumulator
@ACC
M=D+M

// Check where we are in the iteration
(CHECKINC)
@MULTCOUNTER
M=M+1
D=M

// Subtract the inc from the multiplicant if we are still less than, then we need to loop again
@R1
D=D-M

@MULTIPLY
D;JLT

// We've reache the end let's store the result
@ACC
D=M
@R2
M=D

// Jump to end loop
@END
0;JMP

(END)
@END
0;JMP

(ZERO)
@R2
M=0
@END
0;JMP