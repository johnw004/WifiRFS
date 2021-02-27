
.Write_To_Ram
        PHA
        SEC
        BCS     RW_To_Ram

.Read_From_Ram
        CLC
        
.RW_To_Ram
        PHP
        LDX     ramptr
        LDA     ramptr+1
        SEC
        SBC     #roffset
        STA     pagereg
        PLP  
        BCS     W_To_Ram
        
        LDA     pageram,X
        BCC     RW_Ram_Finish
        
.W_To_Ram
        PLA
        STA     pageram,X
        
.RW_Ram_Finish
        LDX     #heappage
        STX     pagereg
        RTS                          \-7
        
.Inc_Ramptr
        INC     ramptr
        BNE     Inc_Ramptr_End
        INC     ramptr+1
.Inc_Ramptr_End
        RTS
       
       
.Modify_Header                         \ enter with * at start of header in ramptr
        LDY     #$00
        STY     crc16
        STY     crc16+1        

.Modify_Loop
        JSR     Inc_Ramptr
        DEY
        BEQ     Write_Ramend
        JSR     Read_From_Ram
        JSR     Do_Crc
        TAX
        BNE     Modify_Loop
        TYA
        BPL     Modify_Loop
        LDY     #$0E
        BNE     Modify_Loop          \-8
 
.Write_Ramend
        LDY     #$00
.Ramend_Loop
        LDA     ramend,Y
        JSR     Do_Crc
        JSR     Write_To_Ram
        JSR     Inc_Ramptr
        INY
        CPY     #$02
        BNE     Ramend_Loop
        
        LDA     #$00
        JSR     Do_Crc
        JSR     Do_Crc
        TAY                        \ make Y=0
        JSR     Inc_Ramptr
        JSR     Inc_Ramptr
        
.Write_Crc
        LDA     crc16,Y
        JSR     Write_To_Ram
        JSR     Inc_Ramptr
        INY
        CPY     #$02
        BNE     Write_Crc
        
        LDA     ramptr
        SEC
        SBC     ramstart
        STA     hlength
        RTS
        
.Dont_Defrag
        JSR     Print_String2
        EQUS    $0D,"No dead space",$0D,$EA
        
.Dont_Defrag2
        JMP     Leave_Claim

.Do_DEFRAG
        JSR     Load_Rwcode
        
        LDA     #21
        JSR     osasci
        JSR     Info_Code
        LDA     #6
        JSR     osasci

        LDA    $100
        ORA    $101
        BEQ    Dont_Defrag
        
        JSR    Print_String2
        EQUS   $0D,"    ",$EA
        JSR    Print_Dec3
        JSR    Print_String2
        EQUS   " Deleted",$0D,"  &",$EA
        LDA    delsp+1
        JSR    printhex
        LDA    delsp
        JSR    printhex
        JSR    Print_String2
        EQUS   " Dead Space",$0D,$0D, "Defrag now? y/N",$0D,$EA
        JSR    Yes_No
        BVS    Dont_Defrag2
        BCS    Dont_Defrag2

        LDA     #$FF
        STA     rfsptr
        STA     rfsptr+1
        STA     ramend
        STA     ramend+1
        
        STA     ramptr
        STA     ramptr+1
        JSR     Write_To_Ram          \ put end marker in ram

.Defrag_Start_Loop
        SEC
        LDA     rfsptr
        STA     fend
        SBC     #$06
        STA     rfsptr
        LDA     rfsptr+1  
        STA     fend+1
        SBC     #$00
        STA     rfsptr+1
        JSR     Update_Bank
        LDY     #$00
        
.Defrag_Loop1
        TYA
        PHA
        JSR     Inc_And_Read_Byte
        TAX
        PLA
        TAY
        TXA
        STA     fstart,Y
        INY
        CPY     #$05
        BNE     Defrag_Loop1
        
        LDA     fstart
        AND     fstart+1
        CMP     #$FF
        BNE     Defrag_Continue
        JMP     Defrag_1_Done
        
.Defrag_Continue
        LDA     fstart+1
        STA     rfsptr        
        LDA     fstart
        STA     rfsptr+1
        JSR     Update_Bank
        
        LDA     delflag
        BPL     Defrag_Start_Loop         \ file deleted so skip
        
        SEC
        LDA     fend
        SBC     fstart+1
        STA     scratch
        LDA     fend+1
        SBC     fstart
        STA     scratch+1                 \ scratch has length of file
        
        SEC
        LDA     ramend
        SBC     scratch
        STA     ramstart
        STA     ramptr
        LDA     ramend+1
        SBC     scratch+1
        STA     ramstart+1                \ set up start of file in ram
        STA     ramptr+1                  \ and set pointer to start
        
        LDA     #'*'
        JSR     Write_To_Ram
        
.Defrag_Copy_Across
        JSR     Inc_Ramptr
        LDA     ramptr
        CMP     ramend
        BNE     Copy_Across_Continue
        LDA     ramptr+1
        CMP     ramend+1
        BEQ     Copy_Across_Done
        
.Copy_Across_Continue
        JSR     Inc_And_Read_Byte
        JSR     Write_To_Ram
        JMP     Defrag_Copy_Across
        
.Copy_Across_Done
        LDA     ramstart
        STA     ramptr
        LDA     ramstart+1
        STA     ramptr+1                   \ point ramptr at first header
        JSR     Modify_Header
        
        LDA     ramend                   \Calculate start of final header, end address - LO(length) - header length - 8
        STA     ramptr
        LDA     ramend+1
        STA     ramptr+1
       
        
        LDA     dlength+1                   \ if lsb of length is 0, subtract extra $100
        BNE     Dont_Sub_100
        DEC     ramptr+1
.Dont_Sub_100
        JSR     Sub_Ptr                     \ subtract length of last block
        LDA     hlength
        JSR     Sub_Ptr                     \ subtract length of header
        LDA     #$08
        JSR     Sub_Ptr                     \ sbtract 8 for checksum of last block and 6 extra bytes of info

        JSR     Modify_Header
        
        LDA     ramend                  \ find location to write start of file
        STA     ramptr
        LDA     ramend+1
        STA     ramptr+1
        LDA     #$05
        JSR     Sub_Ptr
        
        LDY     #$01
.Ramstart_Loop
        LDA     ramstart,Y
        STA     ramend,Y                 \ update ramend
        JSR     Write_To_Ram             \ and write ramstart to file
        JSR     Inc_Ramptr
        DEY
        BPL     Ramstart_Loop
        
        LDA     fstart+1                \ set new start of file
        STA     rfsptr
        LDA     fstart
        STA     rfsptr+1
        JMP     Defrag_Start_Loop

.Defrag_1_Done
        JSR     Format_Main             \ copy to ram done, now erase rfs
        
        LDA     ramend
        STA     ramptr
        STA     rfsptr
        LDA     ramend+1
        STA     ramptr+1
        STA     rfsptr+1
        JSR     Update_Bank
        
.Copy_Back_Loop
        JSR     Read_From_Ram
        JSR     Write_Byte
        JSR     Inc_Ramptr
        LDA     ramptr
        AND     ramptr+1
        CMP     #$FF
        BNE     Copy_Back_Loop

        JSR     Find_Start
        JMP     Format_Leave

        
.Sub_Ptr
        STA     scratch
        SEC
        LDA     ramptr
        SBC     scratch
        STA     ramptr
        LDA     ramptr+1
        SBC     #$00
        STA     ramptr+1
        RTS
        
        
        