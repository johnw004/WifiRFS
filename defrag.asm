\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike

\ Routines to defragment the filing system

\ During defragmentation a shadow copy of the filing system will be created in the paged ram.
\ The addresses wil be 3 pages lower, ie. from $FCFF downwards corresponding to $FFFF downwards so as not to overwrite the
\ heap, format and rw pages.
\ 
\ Sarting from the end of the rfs at $FFFF, each non-deleted file will be copied over to ram and the headers modified
\ to account for the new addresses.
\
\ Once the copy has been made the whole filing system will be erased, then the files will be copied back from the ram.
\
\
.Write_To_Ram
        PHA                          \ save A for writing
        SEC
        BCS     RW_To_Ram

.Read_From_Ram
        CLC
        
.RW_To_Ram
        PHP
        LDX     ramptr               \ load X with the lsb of the ram address
        LDA     ramptr+1
        SEC
        SBC     #roffset
        STA     pagereg              \ load A with the msb offset as described above
        PLP  
        BCS     W_To_Ram
        
        LDA     pageram,X            \ load 1 byte from the ram
        BCC     RW_Ram_Finish
        
.W_To_Ram
        PLA                          \ retrieve A
        STA     pageram,X            \ save 1 byte to the ram
        
.RW_Ram_Finish
        LDX     #heappage
        STX     pagereg
        RTS
        
.Inc_Ramptr                          \ inc pointer to paged ram
        INC     ramptr
        BNE     Inc_Ramptr_End
        INC     ramptr+1
.Inc_Ramptr_End
        RTS
       
       
.Modify_Header                         \ modify header of file in ram 
        LDY     #$00                   \ enter with ramptr pointing to * at start of header
        STY     crc16
        STY     crc16+1                \ clear crc

.Modify_Loop
        JSR     Inc_Ramptr
        DEY
        BEQ     Write_Ramend           \ if Y is zero, address of next file has been reached and needs modifying
        JSR     Read_From_Ram          \ read byte from ram
        JSR     Do_Crc                 \ add to crc, in write.asm
        TAX
        BNE     Modify_Loop            \ if byte is 0....
        TYA
        BPL     Modify_Loop            \ and Y is negative....
        LDY     #$0E                   \ set it to count the number of bytes to be copied after the filename
        BNE     Modify_Loop
 
.Write_Ramend
        LDY     #$00    
.Ramend_Loop
        LDA     ramend,Y               \ copy end of file, ie. start of next file, into header (2 bytes)
        JSR     Do_Crc                 \ do crc, in write.asm
        JSR     Write_To_Ram           \ write byte
        JSR     Inc_Ramptr
        INY
        CPY     #$02
        BNE     Ramend_Loop
        
        LDA     #$00                   \ the next two bytes will stay as 0
        JSR     Do_Crc                 \ so just add to crc, in write.asm
        JSR     Do_Crc
        TAY                            \ make Y=0
        JSR     Inc_Ramptr             \ inc pointer past 0 bytes
        JSR     Inc_Ramptr
        
.Write_Crc
        LDA     crc16,Y                \ write 2 bytes of crc to header
        JSR     Write_To_Ram
        JSR     Inc_Ramptr
        INY
        CPY     #$02
        BNE     Write_Crc
        
        LDA     ramptr                 \ the low byte of the start address can now be subtracted from the pointer
        SEC                            \ to give the length of the header
        SBC     ramstart               \ this is only valid for the first header, but it is not needed after the last header
        STA     hlength
        RTS
        
.Dont_Defrag
        JSR     Print_String2
        EQUS    $0D,"No dead space",$0D,$EA
        
.Dont_Defrag2
        JMP     Leave_Claim

.Do_DEFRAG                             \ *DEFRAG command
        JSR     Load_Rwcode
        
        LDA     #21
        JSR     osasci                 \ VDU 21, disable screen output
        JSR     Info_Code              \ do INFO to get number of deleted files in $100/1 and dead space in delsp
        LDA     #6
        JSR     osasci                 \ enable screen output

        LDA    $100
        ORA    $101
        BEQ    Dont_Defrag             \ if no deleted files print message and leave
        
        JSR    Print_String2           \ print deleted files and dead space
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
        JSR    Yes_No                   \ give option to continue
        BVS    Dont_Defrag2
        BCS    Dont_Defrag2

        LDA     #$FF                   \ start at end $FFFF
        STA     rfsptr
        STA     rfsptr+1               \ set rfs pointer
        STA     ramend 
        STA     ramend+1               \ and ram end
        
        STA     ramptr
        STA     ramptr+1
        JSR     Write_To_Ram           \ put end marker in ram, value does not matter

.Defrag_Start_Loop
        SEC
        LDA     rfsptr
        STA     fend                   \ set fend to end of file in rfs
        SBC     #$06
        STA     rfsptr                 \ set rfs pointer to start of WRFS 6 byte appendix 
        LDA     rfsptr+1  
        STA     fend+1
        SBC     #$00
        STA     rfsptr+1
        JSR     Update_Bank            \ update the mfa and bank values
        LDY     #$00
        
.Defrag_Loop1
        TYA                            \ copy the start address, data length and delete flag into heap
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
        BNE     Defrag_Continue        \ if the new start address is $FFFF the start of the files has been reached so go
        JMP     Defrag_1_Done          \ to next stage
        
.Defrag_Continue
        LDA     fstart+1               \ set rfs pointer to start of file, fstart is bigendian
        STA     rfsptr        
        LDA     fstart
        STA     rfsptr+1
        JSR     Update_Bank             \ update the mfa and bank values
        
        LDA     delflag
        BPL     Defrag_Start_Loop         \ file deleted so skip
        
        SEC
        LDA     fend                      \ subtract start in rfs from end in rfs
        SBC     fstart+1                  \ fstart is bigendian
        STA     scratch
        LDA     fend+1
        SBC     fstart
        STA     scratch+1                 \ scratch has length of file
        
        SEC
        LDA     ramend                   \ subtract length from end of file in ram    
        SBC     scratch
        STA     ramstart                 \ set start of file in ram
        STA     ramptr
        LDA     ramend+1
        SBC     scratch+1
        STA     ramstart+1                
        STA     ramptr+1                  \ and set pointer to start
        
        LDA     #'*'
        JSR     Write_To_Ram              \ write synchronising byte at start of file in ram
        
.Defrag_Copy_Across
        JSR     Inc_Ramptr                \ inc ram pointer
        LDA     ramptr
        CMP     ramend
        BNE     Copy_Across_Continue
        LDA     ramptr+1
        CMP     ramend+1
        BEQ     Copy_Across_Done          \ if ram pointer = end of file, copy across done
        
.Copy_Across_Continue
        JSR     Inc_And_Read_Byte         \ read byte from rfs
        JSR     Write_To_Ram              \ write to ram
        JMP     Defrag_Copy_Across        \ loop
        
.Copy_Across_Done
        LDA     ramstart
        STA     ramptr
        LDA     ramstart+1
        STA     ramptr+1                   \ point ramptr at start of first header
        JSR     Modify_Header              \ modify header of first block
        
        LDA     ramend                   \Calculate start of final header, end address - LO(length) - header length - 8
        STA     ramptr 
        LDA     ramend+1
        STA     ramptr+1
       
        
        LDA     dlength+1                   \ if lsb of bigendian length is 0, subtract extra $100
        BNE     Dont_Sub_100
        DEC     ramptr+1
.Dont_Sub_100
        JSR     Sub_Ptr                     \ subtract length of last block
        LDA     hlength
        JSR     Sub_Ptr                     \ subtract length of header
        LDA     #$08
        JSR     Sub_Ptr                     \ subtract 8 for checksum of last block and 6 extra bytes of WRFS info
                                            \ ramptr now points to start of last header
        JSR     Modify_Header               \ modify header of last block
        
        LDA     ramend                      \ now need to alter start address of file in WRFS info
        STA     ramptr                      \ this is 5 bytes before the end of the file
        LDA     ramend+1
        STA     ramptr+1
        LDA     #$05
        JSR     Sub_Ptr                     \ ram pointer now points to location in file to write ramstart
        
        LDY     #$01
.Ramstart_Loop
        LDA     ramstart,Y
        STA     ramend,Y                 \ copy ramstart to ramend for the next file to copy
        JSR     Write_To_Ram             \ and write ramstart to file
        JSR     Inc_Ramptr
        DEY
        BPL     Ramstart_Loop
        
        LDA     fstart+1                \ copy bigendian start of file in rfs to rfs pointer
        STA     rfsptr
        LDA     fstart
        STA     rfsptr+1
        JMP     Defrag_Start_Loop

.Defrag_1_Done
        JSR     Format_Main             \ copy to ram done, now erase rfs
        
        LDA     ramend                  \ ramend now holds start of first file
        STA     ramptr                  \ copy to ram pointer
        STA     rfsptr                  \ and rfs pointer
        LDA     ramend+1
        STA     ramptr+1
        STA     rfsptr+1
        JSR     Update_Bank             \ update mfa and bank
        
.Copy_Back_Loop
        JSR     Read_From_Ram           \ read byte from ram
        JSR     Write_Byte              \ write to rfs
        JSR     Inc_Ramptr
        LDA     ramptr
        AND     ramptr+1
        CMP     #$FF
        BNE     Copy_Back_Loop          \ when ramptr = $FFFF, end reached

        JSR     Find_Start              \ set RFS start
        JMP     Format_Leave

        
.Sub_Ptr                                \ subtract A from ram pointer
        STA     scratch
        SEC
        LDA     ramptr
        SBC     scratch
        STA     ramptr
        LDA     ramptr+1
        SBC     #$00
        STA     ramptr+1
        RTS
        
        
        