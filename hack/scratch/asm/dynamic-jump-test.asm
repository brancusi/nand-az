// Get top nums off stack
(BLOCK_1)
    // Set up next block after this comp completes
    
    @BLOCK_3
    D=A
    @NEXT_BLOCK
    M=D

    @20
    D=A
    @20
    D=D-A

    @EQ_AND_STACK
    0;JMP

// EQ check
(EQ_AND_STACK)
    @EQ
    D;JEQ

    @NEQ
    0;JMP

(BLOCK_3)
    @40
    D=A
    @2
    M=D

    @END
    0;JMP


// Safety jump over if not explicitly requested
    @EQ_END
    0;JMP
(EQ)
    @SP
    M=-1

    @NEXT_BLOCK
    A=M
    0;JMP
(EQ_END)

// Safety jump over if not explicitly requested
    @NEQ_END
    0;JMP
(NEQ)
    @SP
    M=0

    @NEXT_BLOCK
    A=M
    0;JMP
(NEQ_END)

(END)
    @END
    0;JMP