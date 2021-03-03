\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike

\ high level routines to read and write data to/from RFS

.Write_Header                          \ the header for the first block will be at heap+1, and the last block at heap+$21
        STA     rfssrc                 \ A indicates which one 
        LDA     #HI(heap)
        STA     rfssrc+1               \ start address points to heap+1 or heap+$21
        LDX     hdrlen                 \ header length from get_filename in vectors.asm  
        DEX
        DEX
        DEX
        STX     wrnum                  \ number of bytes, subtract 1 for * and 2 for crc
        LDA     tubeflag               \ save tubeflag
        PHA
        LDA     #$00                   \ writing header so dont read from tube
        STA     tubeflag
        LDA     #'*'                   \ sync byte for block header
        JSR     Write_Block_With_Sync
        PLA
        STA     tubeflag               \ restore tubeflag
        RTS


.Write_Block_With_Sync                 \entry with sync byte in accumulator
        JSR     Update_And_Write_Byte        

.Write_Block                           \ rfssrc points to data in memory
        LDA     rfsptr+1
        JSR     Update_Bank            \ update the bank variables before starting   
        LDY     #$00
        STY     crc16
        STY     crc16+1
.Block_Loop
        BIT     tubeflag               \ read from memory or tube
        BVC     Read_Mem
        LDA     $FCE5
        BVS     Rfssrc_OK
        
.Read_Mem
        LDY     #$00
        LDA     (rfssrc),Y
        INC     rfssrc                 \ inc memory pointer
        BNE     Rfssrc_OK
        INC     rfssrc+1
.Rfssrc_OK
        JSR     Do_Crc                 \ update the crc
        JSR     Write_Byte             \ write a byte
        DEC     wrnum
        BNE     Block_Loop             \ until end of block
        LDA     crc16
        JSR     Write_Byte
        LDA     crc16+1
        JMP     Write_Byte             \ write crc at end of block
        
.Do_Crc                                \ crc routine by JG Harston
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
        JSR     Update_Bank             \ update mfa and bank
        PLA
.Write_Byte
        LDY    rfsptr
        STY    zrfsptr
        TAY
        LDA    rfsptr+1
        AND    #$3F
        ORA    #$80
        STA    zrfsptr+1                \ set zrfsptr to address in sideways ROM ($8000-$BFFF)
        LDA    #rwpage
        STA    pagereg                  \ switch paged ram to rwcode
        TYA
        JSR    rwcode                   \ entry for write byte

        LDA    #heappage                \ restore paged ram to heap
        STA    pagereg
        INC    rfsptr                   \ inc rfs pointer
        BNE    Rfsptr_OK
        INC    rfsptr+1
        LDA    rfsptr+1                 \ if msb changed update mfa and bank
        JSR    Update_Bank
        LDA    #'.'
        JSR    osasci
        
.Rfsptr_OK
        RTS
        
.Read_Byte_From_F6                      \ entry to use $F6/7 as pointers
        LDY    ROMptr
        LDA    ROMptr+1
        BNE    Do_Byte
        
.Inc_And_Read_Byte                      \ entry to inc rfs pointer before reading
        INC    rfsptr
        BNE    Read_Byte
        INC    rfsptr+1
        
.Update_And_Read_Byte                   \ entry to update mfa and bank before reading
        LDA    rfsptr+1
        JSR    Update_Bank

.Read_Byte
        LDY    rfsptr
        LDA    rfsptr+1
        
.Do_Byte
        CPY    #$FF
        BNE    Do_Byte2
        CMP    #$FF
        BNE    Do_Byte2                 \ if reading end byte &FFFF return with '+'
        LDA    #'+'
        RTS

.Do_Byte2
        STY    zrfsptr
        AND    #$3F
        ORA    #$80
        STA    zrfsptr+1                \ set zrfsptr to address in sideways ROM ($8000-$BFFF)
        LDA    #rwpage
        STA    pagereg                  \ set ram page to rw code
        JSR    rwcode+3                 \ entry for read byte
        
.Read_Done
        LDY    #heappage                \ restore paged ram to heap
        STY    pagereg
        TAY                             \ to set zero flag
        RTS
        
\ The 64K EEPROM on the wifi board is split into 4 x 16K banks. Bit 14 of the address is driven by the LSB of the ROM select. 
\ Bit 15 is driven by the mfa signal from the uart on the board. (See Roland's explanation)

\ In order to address the board correctly these two signals need to correspond to the bits in the high byte of the rfs pointer 
\ at all times. Every time it is changed, or potentially changed, this routine must be called. They are stored in the locations
\ mfatable and banktable in the rw page of ram to be read by the low level rw routines when they are called.
        
.Update_Bank                            \ enter with msb of address in A
        PHA
        LDX    #rwpage                  \ set ram to rw page
        STX    pagereg
   
        LDX    #$00
        ROL    A
        BCC    Update_Mfa               \ if bit 15 (bit 7 of msb) = 0, mfa = 0, else 8
        LDX    #$08
.Update_Mfa
        STX    mfatable
        ROL    A                        \ if bit 14 (bit 6 of msb) was 0, bank = lower bank of pair, else higher
        LDA    $F4
        AND    #$06                     \ mask out bit 0 of ROM bank
        ADC    #$00                     \ add in bit 14 (bit 6 of msb) of address
        STA    banktable
        
        LDA    #heappage                \ reset heap page
        STA    pagereg
        PLA
        RTS
        
.Check_Delete                           \ also does filename matching for osfile and osfind
        TAY                             \ A has existing delete flag
        LDX    #$00
.Delete_Loop
        LDA    heap+1,X                 \ compare search name in heap+1 with file name in heap+$40
        ORA    heap+$40,X  
        BEQ    Delete_Found             \ end of both filenames reached, names match
        LDA    heap+1,X
        EOR    heap+$40,X
        AND    #$DF
        BNE    Delete_End               \ filenames dont match
        INX
        BNE    Delete_Loop
        
.This_Is_Osfile5FF
        LDA    save_a                   \ this is osfile or osfind
        CMP    #$05
        BNE    End_Osfile5
        LDY    #$02
        
.Copy_Attribs
        LDA    heap+$4E,Y               \ if osfile 5 copy file attributes at heap+$50 to parameter block in (X,Y) + 2
        STA    (save_y),Y
        INY
        CPY    #$0C                     \ copy 10 bytes, 4 byte load address, 4 byteexecution address, 2 byte length
        BNE    Copy_Attribs
        BEQ    End_Osfile5
        
.Delete_Found
        TYA                              \ filenames match
        BEQ    Already_Deleted           \ ignore if already deleted
        BCS    This_Is_Osfile5FF         \ if C set this is osfile or osfind
        LDA    #$00                      \ if C clear this is delete so write delete flag
        JSR    Write_Byte

.End_Osfile5
        CLV                              \ clear V to indicate file found
.Already_Deleted
        PLA                              \ pull return address to return to calling address and not new_cat
        PLA
        PLA                              \ pull entry flags
        PHP                              \ push V for return
                
.New_Cat_End
        PLP
.Delete_End
        RTS

\ New_Cat performs 4 functions associated with finding files, depending on C and V flags

\  V clear, C clear:     Display catalogue
\  V clear, C set  :     Display file info
\  V set,   C clear:     Delete file
\  V set,   C set  :     Find file for osfile and osfind and return file attributes for osfile 5

.Bit_New_Cat
        BIT     AnRTS                    \ set V flag for delete and osf entries
        
.New_Cat 
        PHP                              \ save V and C flags
        LDA     #heappage
        STA     pagereg                  \ set ram to heap page
        JSR    Check_Start               \ check rstart still valid
 
        LDY    #$01

.Cat_Loop1
        LDA    rstart,Y
        STA    rfsptr,Y                  \ set pointer to start of files
        LDA    #$00
        STA    $100,Y                    \ clear deleted count
        STA    delsp,Y                   \ clear dead space count
        DEY
        BPL    Cat_Loop1

.Find_Loop1
        JSR    Update_And_Read_Byte
        CMP    #'*'
        BEQ    Find_Continue             \ if byte is not a "*" end has been reached
        PLP
        RTS
        
.Find_Continue
        JSR    Inc_And_Read_Byte         \ start reading filename from header into heap+$40
        STA    heap+$40
        
        LDX    #$01
        
.Find_Filename
        STX    zp
        JSR    Inc_And_Read_Byte
        LDX    zp
        STA    heap+$40,X
        INX
        CMP    #$00
        BNE    Find_Filename             \ 0 indicates end of filename
        
        LDX    #$00
.Find_Attribs                            \ read the file attributes into heap+$50
        STX    zp
        JSR    Inc_And_Read_Byte
        LDX    zp
        STA    heap+$50,X
        INX
        CPX    #15
        BNE    Find_Attribs
        
        SEC
        LDA    heap+$5D                  \ locations 13 and 14 ($D and $E) in attributes point to end of file
        STA    scratch                   \ prepare scratch to calculate size of file
        SBC    #$05                      \ start is held 5 bytes before
        STA    rfsptr
        LDA    heap+$5E
        STA    scratch+1
        SBC    #$00
        STA    rfsptr+1                  \ point at start pointer
        
        JSR    Update_And_Read_Byte      \ read start 
        PHA
        JSR    Inc_And_Read_Byte
        SEC
        SBC    scratch
        STA    scratch
        PLA
        SBC    scratch+1                 \ subtract end
        STA    scratch+1                 \ scratch now has negative size of file in RFS
        
        JSR    Inc_And_Read_Byte         \ read data length and store at locations 8 and 9 in the attributes
        STA    heap+$59
        JSR    Inc_And_Read_Byte
        STA    heap+$58

        JSR    Inc_And_Read_Byte         \ if 0 file is deleted
        PHA                              \ push deleted flag
        BNE    Dont_Inc_Deleted
        INC    $100
        BNE    Add_Space
        INC    $101                      \ inc deleted count
        
.Add_Space
        SEC
        LDA    delsp                     \ add size in scratch to deleted sum by subtracting negative value 
        SBC    scratch
        STA    delsp
        LDA    delsp+1
        SBC    scratch+1
        STA    delsp+1
        
.Dont_Inc_Deleted
        PLA                              \ pull deleted flag
        PLP
        PHP                
        BVC    Not_Delete

        JSR    Check_Delete              \ if V set, delete or osfile/osfind called
        BVS    Dont_Print_Attribs        \ if file not found skip to end

.Not_Delete
        TAX
        BEQ    Dont_Print_Attribs        \ if file deleted skip to end
        PLP
        PHP
        BCS    Dont_Print_Spaces         \ if doing INFO don't print spaces
        JSR    Print_String2             \ if doing catalogue print 3 spaces 
        EQUS   "   ",$EA
        
.Dont_Print_Spaces        
        LDX    #$00
.Find_Loop2
        LDA    heap+$40,X                \ print filename
        JSR    osasci
        BEQ    Print_Spaces
        INX
        BNE    Find_Loop2
        
.Print_Spaces
        LDA    #' '                      \ print spaces to next column
        JSR    osasci
        INX
        CPX    #11
        BCC    Print_Spaces              \ if doing info, column width is 12 (11 + space in attribs loop)
        PLP
        PHP
        BCS    Print_Attribs
        
        CPX    #17
        BNE    Print_Spaces              \ if doing cat expand to column width of 20 (17+3 at beginning)

        BEQ    Dont_Print_Attribs

.Print_Attribs
        LDY    #$02
        
.Attribs_Loop
        LDA    #' '
        JSR    osasci
        
        LDX    Attribs_List,Y            \ load end location of attribute (msb)
.Attribs_Loop2
        LDA    heap+$50,X                \ get attrib byte and print hex
        JSR    printhex
        DEX                              \ dec pointer
        TXA
        CMP    Attribs_List+1,Y          \ compare pointer with end of attrib
        BNE    Attribs_Loop2
        DEY                              \ if not end loop to next attrib
        BPL    Attribs_Loop
        
        JSR    osnewl
        
.Dont_Print_Attribs
        LDA    heap+$5D                  \ load pointer with start of next file
        STA    rfsptr
        LDA    heap+$5E
        STA    rfsptr+1
        JMP    Find_Loop1
        
.Attribs_List
        EQUB   $09,$07,$03,$FF           \ end and start offsets into attribute list, length at 8,9, exec at 4-7, load at 0-3
        
        
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
                    
                    
      
        
        
        
        
        