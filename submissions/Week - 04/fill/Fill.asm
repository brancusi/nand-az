// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.   

(START)
    @24576
    D=M
    @CURRENTKEY
    M=D

    @PROCESSBLACK
    D;JGT

    @PROCESSWHITE
    0;JMP

(PROCESSBLACK)
    @COLORMODE
    D=M

    @BLACKEN
    D;JLT

    @COLORMODE
    M=-1
    
    @16383
    D=A
    @CURRENTSCREEN
    M=D

(BLACKEN)
    @CURRENTSCREEN
    D=M
    @24575
    D=D-A
    @START
    D;JEQ

    @CURRENTSCREEN
    M=M+1
    A=M
    M=-1
    
    @START
    0;JMP

(PROCESSWHITE)
    @COLORMODE
    D=M

    @WHITEN
    D;JGT

    @COLORMODE
    M=1
    
    @16383
    D=A
    @CURRENTSCREEN
    M=D

(WHITEN)
    @CURRENTSCREEN
    D=M
    @24575
    D=A-D
    @START
    D;JLT

    @CURRENTSCREEN
    M=M+1
    A=M
    M=0

    @START
    0;JMP



// Keyboard - 24576
// Screen - 16384
