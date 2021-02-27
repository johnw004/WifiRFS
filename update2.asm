\ enters here after rom has been downloaded
        JMP     Update2-$8000+dload
        
\ returns here after updating

.Return_From_Update
        LDA     #heappage
        STA     pagereg
        
        LDX     $F4
        LDA     #$82
        STA     $2A0,X
        
        JSR     Set_Vecs               \ rom now updated so can use its routine 
        
        PLP                            \ from PHP at start of format code
                
        LDA     #$05
        JSR     Call_Osfile3-$8000+dload
        CMP     #$01
        BEQ     Dont_Save_Boot
                
        LDA     #$00
        JSR     Call_Osfile3-$8000+dload

.Dont_Save_Boot
        LDY     #$00
        STY     rcheck
        
        JSR     Print_String2
        EQUS    $0D,"Update Done",$0D,$EA
        
        LDA     Update_Flag-$8000+dload
        BEQ     Leave_Update2

        JSR     Print_String2
        EQUS    "If this replaced a different ROM",$0D,"you will need to FORMAT",$0D,$EA
        
        JMP     Recovery_Return-$8000+dload
        
.Leave_Update2
        PLA
        TAY
        LDX     $F4
        PLA
        LDA     #$00
        RTS

.Update_Flag
        EQUB    $00                   \ will be set to non zero by recovery code
        
.Update2
        LDY     #$00
        STY     crc16
        STY     crc16+1
        
.Find_Etag
        LDA     pageram,Y
        INY
        BEQ     Etag_Done
        CMP     #'"'
        BNE     Find_Etag

.Etag_Loop
        LDA     pageram,Y
        INY
        BEQ     Etag_Done
        CMP     #'"'
        BEQ     Etag_Done
        JSR     Do_Crc-$8000+dload     \ might be a change so use routine in ram
        CLC
        BCC     Etag_Loop
        
.Etag_Done
        LDA     crc16
        STA     etagram
        LDA     crc16+1
        STA     etagram+1

        LDA     #heappage
        STA     pagereg
        
        LDA     etagram
        ORA     etagram+1
        BEQ     Go_For_Update
        
        LDA     etagram
        CMP     etagrom
        BNE     Go_For_Update
        LDA     etagram+1
        CMP     etagrom+1
        BNE     Go_For_Update
        
        JSR     Print_String2                     \ no change so can use existing rom routines
        EQUS    $0D,"No difference. Upload anyway? y/N",$0D,$EA
        JSR     Yes_No
        BVS     Leave_Update2
        BCS     Leave_Update2

.Go_For_Update                               \ rom has changed so need to update rw code and format code
        LDA     #rwpage
        STA     pagereg
        
        LDY     #$00                          \ remove recovery entry from start of rom
        STY     dload
        STY     dload+1
        STY     dload+2
        
        
.Rw_Load_Loop2
        LDA     Rw_Start-$8000+dload,Y
        STA     rwcode,Y
        INY
        CPY     #Rw_End-Rw_Start
        BNE     Rw_Load_Loop2
        
        LDA     #formatpage
        STA     pagereg
        LDY     #$00
.Format_Loop3
        LDA     Format_Start-$8000+dload,Y 
        STA     formatcode,Y
        INY
        CPY     #Format_End-Format_Start
        BNE     Format_Loop3
        
        LDX     #$04
        JMP     formatcode

.Boot_File_Start
        EQUS    "CH.",$22,"START",$22,$0D
.Boot_File_End

.Boot_File_Name
        EQUS    "!BOOT",$0D
        
.Boot_File_Block
        EQUW    Boot_File_Name-$8000+dload
        EQUW    $FFFF,$FFFF
        EQUW    $FFFF,$FFFF
        EQUW    Boot_File_Start-$8000+dload,$FFFF
        EQUW    Boot_File_End-$8000+dload,$FFFF
        
.Call_Osfile3
        LDX     #LO(Boot_File_Block-$8000+dload)
        LDY     #HI(Boot_File_Block-$8000+dload)
        JMP     osfile
        
.Electron_Wifi
        EQUS    "Electron Wifi",$00
        
.Cannot_Find
        EQUS    $0D,"Cannot find wifi board",$0D,$00

.Recovery_Jmp
        JMP     Recovery-$8000+dload
        
.Recovery
        PHP
        SEI
        LDA     $F4
        PHA
        LDX     #$02
        
.Find_Wifi
        LDA     #$0F
        STA     Update_Flag-$8000+dload
        STA     $FE05
        STX     $F4
        STX     $FE05
        LDY     #$00
        
.Find_Wifi2
        LDA     Electron_Wifi-$8000+dload,Y
        BEQ     Wifi_Slot_Found
        EOR     $8009,Y
        INY
        AND     #$DF
        BEQ     Find_Wifi2
        
        DEX
        DEX
        BPL     Find_Wifi
        
        PLA
        STA     $F4
        STA     $FE05
        PLP
        LDY     #$00
        
.Cannot_Find_Loop
        LDA     Cannot_Find-$8000+dload,Y
        BEQ     Cannot_Find_Done
        JSR     osasci
        INY
        BNE     Cannot_Find_Loop

.Cannot_Find_Done
        RTS
        
.Wifi_Slot_Found
        INX
        STX     $F4
        JMP     Go_For_Update-$8000+dload
        
        
.Recovery_Return
        PLA
        STA     $F4
        STA     $FE05
        PLP
        
        LDY     #$02
.Reinstate_Jmp_Code
        LDA     Recovery_Jmp-$8000+dload,Y
        STA     dload,Y
        DEY
        BPL     Reinstate_Jmp_Code
        
        RTS
        
        


        
        
        
        