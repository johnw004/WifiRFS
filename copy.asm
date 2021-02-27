
.Select_WRFS
        LDY     #myFSno
        
.Select_FS
        LDA     #$8F
        LDX     #$12
        JMP     osbyte
        
.Call_Osfile0
        LDA     #$00
        PHA
        BEQ     Call_Osfile2
        
.Call_Osfile                    \ A=5 to check file and attributes, A=$FF to load file
        PHA
        LDY     #$00
.Load_Copy_Block
        LDA     Copy_Block1,Y
        STA     heap+$70,Y
        INY
        CPY     #$0A
        BNE     Load_Copy_Block

.Call_Osfile2
        LDX     #$70
        LDY     #HI(heap)
        PLA
        JMP     osfile
        
.Copy_Block1
        EQUW    heap+$61
        EQUW    dload,$FFFF
        EQUD    0
        
.Do_COPY
        JSR     Get_Filename_From_Line
        BCS     Leave_Copy2
        
        LDA     #$0D
        
        DEX
.Copy_Loop
        STA     heap+$60,X
        LDA     heap+$1F,X
        DEX
        BNE     Copy_Loop
        
        LDY     #$04
        JSR     Select_FS
        
        LDA     #$00
        TAY
        JSR     osargs
        CMP     #$04
        BEQ     DFS_Present
        
        JSR     Print_String2
        EQUS    $0D,"No DFS",$0D,$EA
        JMP     Leave_Claim
        
.DFS_Present
        LDA     #$05
        JSR     Call_Osfile      \ check file
        CMP     #$01
        BEQ     Copy_Found
        
        JSR     Not_Found
        LDY     #$03
.Copy_Dir_String
        LDA     Dir_String,Y
        STA     $100,Y
        DEY
        BPL     Copy_Dir_String
        
        LDX     #$00
        LDY     #$01
        JSR     oscli
        
.Leave_Copy2
        JMP     Leave_Claim
        
.Dir_String
        EQUS    "*.",$0D

.Copy_Found
        LDA     #$84
        JSR     osbyte
        STY     scratch
        LDA     #$85
        LDX     #$06
        JSR     osbyte
        STY     scratch+1
        
        LDX     heap+$7B          \ msb of file length
        LDA     heap+$7A
        BNE     Dont_Dec_Msb      \ checking end of file
        DEX
.Dont_Dec_Msb
        TXA
        CLC
        ADC     #HI(dload)
        CMP     scratch
        BCC     Copy_Size_Ok
        CMP     scratch+1
        BCS     Copy_Size_Error
        
        JSR     Print_String2
        EQUS    22,6,$0D,$0D,$EA
        
.Copy_Size_Ok
        LDA     #$FF
        JSR     Call_Osfile
        
        LDX     #$03
.Copy_Save_Block
        LDA     Copy_Block1+2,X
        LDY     heap+$7A,X
        STA     heap+$7A,X
        CPX     #$02
        BCS     Dont_Add_Length
        TYA
        ADC     heap+$7A,X
.Dont_Add_Length
        STA     heap+$7E,X
        DEX
        BPL     Copy_Save_Block
        JSR     Select_WRFS
        JSR     Call_Osfile0
        
.Leave_Copy
        JSR     Select_WRFS
        JSR     osnewl
        JMP     Leave_Claim
        
.Copy_Size_Error
        JSR     Print_String2
        EQUS    $0D,"File too large",$0D,$EA
        JMP     Leave_Copy
        
        
        
        
        