\ routines to read and write data to/from RFS

.Write_Header
        STA     rfssrc
        LDA     #HI(heap)
        STA     rfssrc+1        \ start address points to heap+1 or heap+$21
        LDX     hdrlen
        DEX
        DEX
        DEX
        STX     wrnum           \ number of bytes, subtract 1 for * and 2 for crc
        LDA     tubeflag        \ save tubeflag
        PHA
        LDA     #$00            \ writing header so dont read from tube
        STA     tubeflag
        LDA     #'*'            \ sync byte for block header
        JSR     Write_Block_With_Sync
        PLA
        STA     tubeflag        \ restore tubeflag
        RTS

\entry with sync byte in accumulator
.Write_Block_With_Sync
        JSR     Update_And_Write_Byte        

.Write_Block
        LDA     rfsptr+1
        JSR     Update_Bank    
        LDY     #$00
        STY     crc16
        STY     crc16+1
.Block_Loop
        BIT     tubeflag
        BVC     Read_Mem
        LDA     $FCE5
        BVS     Rfssrc_OK
        
.Read_Mem
        LDY     #$00
        LDA     (rfssrc),Y
        INC     rfssrc
        BNE     Rfssrc_OK
        INC     rfssrc+1
.Rfssrc_OK
        JSR     Do_Crc
        JSR     Write_Byte
        DEC     wrnum
        BNE     Block_Loop
        LDA     crc16
        JSR     Write_Byte
        LDA     crc16+1
        JMP     Write_Byte
        
.Do_Crc
        PHA
        LDX     #$08
        EOR     crc16
.Rotlp
        ASL     crc16+1
        ROL     A
        BCC     Clear
        STA     crcsave
        LDA     crc16+1
        EOR     #$21
        STA     crc16+1
        LDA     crcsave
        EOR     #$10
.Clear
        DEX
        BNE     Rotlp
        STA     crc16
        PLA
        RTS
        
.Update_And_Write_Byte
        PHA
        LDA     rfsptr+1
        JSR     Update_Bank
        PLA
.Write_Byte
        LDY    rfsptr
        STY    zrfsptr
        TAY
        LDA    rfsptr+1
        AND    #$3F
        ORA    #$80
        STA    zrfsptr+1
        LDA    #rwpage
        STA    pagereg
        TYA
        JSR    rwcode       \ entry for write byte

        LDA    #heappage
        STA    pagereg
        INC    rfsptr
        BNE    Rfsptr_OK
        INC    rfsptr+1
        LDA    rfsptr+1
        JSR    Update_Bank
        LDA    #'.'
        JSR    osasci
        
.Rfsptr_OK
        RTS
        
.Read_Byte_From_F6
        LDY    ROMptr
        LDA    ROMptr+1
        BNE    Do_Byte
        
.Inc_And_Read_Byte
        INC    rfsptr
        BNE    Read_Byte
        INC    rfsptr+1
        
.Update_And_Read_Byte
        LDA    rfsptr+1
        JSR    Update_Bank

.Read_Byte
        LDY    rfsptr
        LDA    rfsptr+1
        
.Do_Byte
        CPY    #$FF
        BNE    Do_Byte2
        CMP    #$FF
        BNE    Do_Byte2           \ if reading end byte &FFFF return with '+'
        LDA    #'+'
        RTS

.Do_Byte2
        STY    zrfsptr
        AND    #$3F
        ORA    #$80
        STA    zrfsptr+1
        LDA    #rwpage
        STA    pagereg
        JSR    rwcode+3     \ entry for read byte
        
.Read_Done
        LDY    #heappage
        STY    pagereg
        TAY                 \ to set zero flag
        RTS
        
.Update_Bank                \ enter with msb of address in A
        PHA
        LDX    #rwpage
        STX    pagereg
   
        LDX    #$00
        ROL    A
        BCC    Update_Mfa     \ if msb = 0, mfa = 0, else 8
        LDX    #$08
.Update_Mfa
        STX    mfatable
        ROL    A              \ if bit 6 was 0, bank = lower bank of pair, else higher
        LDA    $F4
        AND    #$06
        ADC    #$00
        STA    banktable
        
        LDA    #heappage
        STA    pagereg
        PLA
        RTS
        
.Check_Delete
        TAY                   \ a has existing delete flag, v is set, c clear if delete, c set if osfile 5
        LDX    #$00
.Delete_Loop
        LDA    heap+1,X
        ORA    heap+$40,X     \ end of filename reached, names match
        BEQ    Delete_Found
        LDA    heap+1,X
        EOR    heap+$40,X
        AND    #$DF
        BNE    Delete_End     \ filenames dont match
        INX
        BNE    Delete_Loop
        
.This_Is_Osfile5FF
        LDA    save_a
        CMP    #$05
        BNE    End_Osfile5
        LDY    #$02
        
.Copy_Attribs
        LDA    heap+$4E,Y
        STA    (save_y),Y
        INY
        CPY    #$0C
        BNE    Copy_Attribs
        BEQ    End_Osfile5
        
.Delete_Found
        TYA
        BEQ    Already_Deleted
        BCS    This_Is_Osfile5FF
        LDA    #$00          \ write delete flag
        JSR    Write_Byte

.End_Osfile5
        CLV                  \ clear v to indicate found
.Already_Deleted
        PLA                  \ pull return address to not return to new_cat
        PLA
        PLA
        PHP                  \ push v for return
                
.New_Cat_End
        PLP
.Delete_End
        RTS
        
.New_Cat 
        PHP                  \ save catalogue flag, c, and delete flag v
        LDA     #heappage
        STA     pagereg
        JSR    Check_Start   \ check rstart still valid
        
        LDA    rstart
        STA    rfsptr
        LDA    rstart+1
        STA    rfsptr+1
        
        LDA    #$00
        STA    $100
        STA    $101          \ clear deleted count
        STA    delsp
        STA    delsp+1       \ clear deleted space count

.Find_Loop1
        JSR    Update_And_Read_Byte
        CMP    #'*'
        BEQ    Find_Continue
        PLP
        RTS
        
.Find_Continue
        JSR    Inc_And_Read_Byte
        STA    heap+$40
        
        LDX    #$01
        
.Find_Filename
        STX    zp
        JSR    Inc_And_Read_Byte
        LDX    zp
        STA    heap+$40,X
        INX
        CMP    #$00
        BNE    Find_Filename
        
        LDX    #$00
.Find_Attribs
        STX    zp
        JSR    Inc_And_Read_Byte
        LDX    zp
        STA    heap+$50,X
        INX
        CPX    #15
        BNE    Find_Attribs
        
        SEC
        LDA    heap+$5D
        STA    scratch           \ prepare scratch to calculate size of file
        SBC    #$05
        STA    rfsptr
        LDA    heap+$5E
        STA    scratch+1
        SBC    #$00
        STA    rfsptr+1          \ point at start pointer
        
        JSR    Update_And_Read_Byte
        PHA
        JSR    Inc_And_Read_Byte
        SEC
        SBC    scratch
        STA    scratch
        PLA
        SBC    scratch+1
        STA    scratch+1         \ scratch now has negative size of file in RFS
        
        JSR    Inc_And_Read_Byte
        STA    heap+$59
        JSR    Inc_And_Read_Byte
        STA    heap+$58

        JSR    Inc_And_Read_Byte    \ if 0 file is deleted
        PHA
        BNE    Dont_Inc_Deleted
        INC    $100
        BNE    Add_Space
        INC    $101
        
.Add_Space
        SEC
        LDA    delsp             \ add size to deleted sum by subtracting negative value 
        SBC    scratch
        STA    delsp
        LDA    delsp+1
        SBC    scratch+1
        STA    delsp+1
        
.Dont_Inc_Deleted
        PLA
        PLP
        PHP                      \ if v set, delete called
        BVC    Not_Delete

        JSR    Check_Delete
        BVS    Dont_Print_Attribs

.Not_Delete
        TAX
        BEQ    Dont_Print_Attribs
        PLP
        PHP
        BCS    Dont_Print_Spaces  \ if doing INFO don't print spaces
        JSR    Print_String2      \ if doing catalogue print 3 spaces 
        EQUS   "   ",$EA
        
.Dont_Print_Spaces        
        LDX    #$00
.Find_Loop2
        LDA    heap+$40,X
        JSR    osasci
        BEQ    Print_Spaces
        INX
        BNE    Find_Loop2
        
.Print_Spaces
        LDA    #' '
        JSR    osasci
        INX
        CPX    #11
        BCC    Print_Spaces       \ if doing info, column width is 12 (11 + space in attribs loop)
        PLP
        PHP
        BCS    Print_Attribs
        
        CPX    #17
        BNE    Print_Spaces       \ if doing cat expand to column width of 17+3

        BEQ    Dont_Print_Attribs

.Print_Attribs
        LDY    #$02
        
.Attribs_Loop
        LDA    #' '
        JSR    osasci
        
        LDX    Attribs_List,Y
.Attribs_Loop2
        LDA    heap+$50,X
        JSR    printhex
        DEX
        TXA
        CMP    Attribs_List+1,Y
        BNE    Attribs_Loop2
        DEY
        BPL    Attribs_Loop
        
        JSR    osnewl
        
.Dont_Print_Attribs
        LDA    heap+$5D
        STA    rfsptr
        LDA    heap+$5E
        STA    rfsptr+1
        JMP    Find_Loop1
        
.Attribs_List
        EQUB   $09,$07,$03,$FF
        
        
.printhex           pha                     \ save accu
                    lsr a                   \ shift high nibble to low
                    lsr a
                    lsr a
                    lsr a
                    jsr printhex_l1         \ print nibble
                    pla                     \ restore value
.printhex_l1        and #&0F                \ remove high nibble
                    cmp #&0A                \ test for hex digit
                    bcc printhex_l2         \ if not then continue
                    adc #6                  \ add 6 for hex letter
.printhex_l2        adc #&30                \ add &30 for ascii value
                    jsr osasci              \ print the digit and return
                    rts
                    
                    
      
        
        
        
        
        