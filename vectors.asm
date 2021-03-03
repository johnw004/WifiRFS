\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike

\ New filing system vectors

\------------
.New_Osargs
        CMP     #$00                           \ if A=0 and Y=0 return with filing system numbef 3 (RFS)
        BNE     Try_Args
        CPY     #$00
        BNE     Try_Args
        LDA     #$03
        RTS
        
.Try_Args
        CPY     #$03                           \ if file handle <> 3 leave
        BNE     Leave_Args
        
        CMP     #$00
        BNE     Try_Args2
        
        LDA     $39E                           \ if A=0 this is PTR#
        STA     $00,X                          \ pointer to next byte in buffer = low byte
        LDA     $3C6
        STA     $01,X                          \ block number = high byte
        LDA     #$00                           \ restore A
        
.Leave_Args
        RTS
        
.Try_Args2
        CMP     #$02
        BNE     Leave_Args
        
        STX     save_x                         \ if A=2 this is EXT#
        JSR     Load_Rwcode                    \ load rw code
        
        SEC
        LDA     $3CB                           \ start of next file in header block
        SBC     #$03                           \ data length is 3 bytes before next file
        STA     rfsptr
        LDA     $3CC
        STA     rfsptr+1
        
        JSR     Update_And_Read_Byte           \ read data length and store in memory
        LDX     save_x
        STA     $01,X
        JSR     Inc_And_Read_Byte
        LDX     save_x
        STA     $00,X
        
        LDY     #$03
        LDA     #$02
        RTS

\----------
.New_Osfsc
		JSR     Save_axy
        
        CMP     #$05
        BEQ     Do_Cat                         \ if A=5 it is *. or *CAT
        
        CMP     #$03
        BNE     Leave_Osfsc
        
        LDA     #$02                           \ if A=3 it is unrecognised * command
        STA     save_a                         \ convert to A=2, "*/" and pass to RFS to run it
        
.Leave_Osfsc
        LDY     #LO(osfscvec)+1        
        JMP     Leave_Default_Vecs

.Do_Cat                                  
        LDY     #$00                           \ print TITLESTR
        JSR     Print_String
        
        LDY     #13
        LDA     #' '
.Cat_Spaces
        JSR     osasci
        DEY
        BNE     Cat_Spaces

        JSR     Print_String2
        EQUS    "Boot Option: EXEC",$0D,$0D,$EA
        
        CLC
        CLV
        JSR     New_Cat                       \ in write.asm
        JMP     Leave_Osfile

\----------
.New_Osfind
        JSR     Save_axy
        CMP     #$00
        BEQ     Default_Osfind                \ pass A=0, close file
        STX     wrzp
        STY     wrzp+1
        LDY     #$00
        JSR     Get_Filename2                 \ get filename from X,Y
        SEC
        JSR     Bit_New_Cat                   \ in write.asm
        
        
        BVC     Default_Osfind                \ check if file exists and is not deleted
        LDA     #$00                          \ if file is deleted set A=0 and leave
        JMP     Leave_Osfile2                 \ leave with A=0

.Default_Osfind
        LDY     #LO(osfindvec)+1        
        JMP     Leave_Default_Vecs            \ file exists so pass call to default RFS routine

.Osfile_5FF
        PHA                                   \ find file for osfile 5 and $FF, save A
        JSR     Get_Filename
        SEC
        BIT     AnRTS
        JSR     New_Cat                       \ in write.asm
        PLA
        TAY                                   \ retrieve A
        BMI     Osfile_FF                     \ if negative this was osfile $FF
        
        LDA     #$01
        BVC     Osfile_5_Found                \ if file found set A=1, else set A=0
        LDA     #$00
.Osfile_5_Found
        JMP     Leave_Osfile2
        
.Osfile_FF
        BVC     Osfile_FF_Found
        JSR     Not_Found                     \ if file not found print and leave with A=0
        LDA     #$00
        JMP     Leave_Osfile2
        
.Osfile_FF_Found
        JSR     Opt_10                        \ OPT 1,0 to suppress printing
        JSR     Default_Osfile                \ do default osfile to load file
        LDA     #$01                          \ set A=1 to show file found
        RTS
        
.Osfile_Delete
        JSR     Get_Filename                  \ get filename
        CLC
        JSR     Bit_New_Cat                   \ in write.asm
        JMP     Leave_Osfile1

.Save_axy
		STA     save_a
        STY     save_x                        \ x and y are wrong way round in electron.asm!!
        STX     save_y
        RTS

\----------
.New_Osfile
        JSR     Save_axy
        CMP     #$06
        BEQ     Osfile_Delete
        CMP     #$05
        BEQ     Osfile_5FF
        CMP     #$FF
        BEQ     Osfile_5FF
        TAY
        BEQ     Osfile_Write

.Default_Osfile
        LDY     #LO(osfvec)+1
        
.Leave_Default_Vecs                          \ Y points to higher byte of vector           
        LDA     vectabaddr                   \ get location of default vec table in OS
        STA     wrzp
        LDA     vectabaddr+1
        STA     wrzp+1
        LDA     (wrzp),Y                     \ get msb of contents of table offset by Y 
        PHA                                  \ push MSB
        DEY
        LDA     (wrzp),Y                     \ get LSB
        PHA                                  \ push LSB
        PHP                                  \ push status register, not needed but need to adjust stack
        LDX     save_y
        LDY     save_x
        LDA     save_a
        RTI                                  \ jump to default routine

.Get_Filename_From_Line
        INY                                  \ increment command line pointer until not space
        LDA     (line),Y
        CMP     #' '
        BEQ     Get_Filename_From_Line
        BCC     Syntax_Error                 \ if nothing print syntax error
        LDA     line
        STA     wrzp
        LDA     line+1
        STA     wrzp+1                       \ get command line pointer into filename pointer
        BNE     Get_Filename2
        
.Syntax_Error
        JSR     Print_String2
        EQUS    $0D,"Syntax",$0D,$EA
        SEC
        RTS
        
.Get_Filename                                 \ Create two header blocks at heap+1 and heap+$21, 
                                              \ one for the first block and one for the last block (if any)        
        LDY     #$01                          \ get pointer to filename
        LDA     (save_y),Y
        STA     wrzp+1
        DEY
        LDA     (save_y),Y
        STA     wrzp
        
.Get_Filename2                                \ entry for alternative filename pointers
        LDA     #heappage
        STA     pagereg                       \ set ram to heap page
        LDX     #$01
.Param_Loop1
        LDA     (wrzp),Y                      \ filename could end in cr or space
        CMP     #$21
        BCC     Param_Loop2
        CPX     #$08                          \ limit to 7 characters
        BEQ     Param_Loop2
        STA     heap,X                        \ store in heap+1 and heap+$21
        STA     heap+$20,X
        INY
        INX
        BNE     Param_Loop1
        
.Param_Loop2
        TXA                                   \ X has length of filename
        CLC
        ADC     #$14                          \ 20 more bytes in header
        STA     hdrlen                        \ store header length
        LDA     #$00
        STA     heap,X                        \ write 0 at end of filename in header
        STA     heap+$20,X
        INX
        RTS

.Osfile_Write
        JSR     Get_Filename
        TXA                                   \ save X during delete
        PHA
        CLC
        JSR     Bit_New_Cat                   \ delete previous version
        PLA
        TAX                                   \ retrieve X

\ Get load and execution address
        LDY     #$02                          \ first 8 bytes after file name pointer in parameter block
.Param_Loop3                                  \ are load and execution addresses
        LDA     (save_y),Y     
        STA     heap,X                        \ store in header after filename
        STA     heap+$20,X
        INY
        INX
        CPY     #$0A
        BNE     Param_Loop3
        
\ Get start address
        LDA     $27A
        STA     tubeflag                      \ FF if tube present, 00 if not
        LDA     (save_y),Y                    \ next four bytes in parameter block are start address of program
        STA     sdata                         \ we need to calculate data length so store two bytes of start address
        INY
        LDA     (save_y),Y
        STA     sdata+1
        INY
        LDA     (save_y),Y                    \ read 2 msb's of start address
        INY
        AND     (save_y),Y                    \ if both $FF this is i/o address
        EOR     #$FF                          \ make 00
        BNE     Not_IO_Address
        STA     tubeflag                      \ write 00 to tubeflag
        
.Not_IO_Address
        INY
        
\ Get length
        SEC
        LDA     (save_y),Y                    \ get the next two bytes of the parameter block
        SBC     sdata                         \ this is the end address so subtract the start address to get the length
        STA     ldata
        INY
        LDA     (save_y),Y
        SBC     sdata+1
        STA     ldata+1                       \ store the length
        
\ Get number of blocks
        TAY                                   \ use the msb of the length as the number of 256 byte blocks
        LDA     ldata                         \ if length is x00, no need to increment number of blocks
        BEQ     Nblocks_OK                    \ eg. $200 is two blocks, $369 is 4 blocks             
        INY
.Nblocks_OK
\ Get block number of first and last blocks
        STY     nblocks
        DEY
        TYA                                   \ block number of last block will be number of blocks-1 
        INY
        STA     heap+$20,X
        LDA     #$00
        STA     heap,X                        \ block number of first block will be 0
        INX
        STA     heap,X                        \ msb's of block numbers will be 0
        STA     heap+$20,X
        INX
        
\ Get block length of first and last blocks
        LDA     #$00                          \ initially set length of $100
        STA     heap,X
        STA     heap+$20,X
        LDA     #$01
        STA     heap+1,X
        STA     heap+$21,X
        
        LDA     ldata                         \ if length = x00, block lengths are OK
        BEQ     Block_lengths_OK
        STA     heap+$20,X                    \ otherwise length of last block will be lsb of data length
        DEC     heap+$21,X                    \ adjust msb of length of last block to 0
                         
        CPY     #$01                          \ if more than one block, first block OK
        BNE     Block_lengths_OK
        STA     heap,X                        \ if only 1 block, length of first block will be lsb of data length
        DEC     heap+1,X                      \ and msb of block length will be 0
.Block_lengths_OK
        INX
        INX
        LDA     #$80
        STA     heap+$20,X                    \ block flag of last block is $80
        CPY     #$01
        BEQ     Flag_OK
        LDA     #$00                          \ if >1 block, block flag of first block will be 0
.Flag_OK
        STA     heap,X
        INX
        
                                              \ Calculate length of file in RFS
        LDA     #$00
        STA     scratch+1
        LDA     hdrlen                        \ first add in headers
        DEY
        BEQ     Only_1_header
        ASL     A                             \ if > 1 block there will be 2 headers
.Only_1_header
        CLC
        ADC     #$06                          \ add 6 bytes for *, start location, data length and delete flag
        ADC     ldata                         \ add the length of the data
        STA     scratch
        LDA     scratch+1
        ADC     ldata+1
        STA     scratch+1

        LDY     #$02                          \ add 2 bytes for checksum for each block
.Nblocks_loop
        CLC
        LDA     nblocks
        ADC     scratch
        STA     scratch
        BCC     Scratch_Ok1
        INC     scratch+1
.Scratch_Ok1
        DEY
        BNE     Nblocks_loop
        
        LDY     nblocks
        CPY     #$03                           \ if more than 2 blocks add 1 byte for the # symbol in intermediate blocks
        BCC     Less_than_3_blocks
        DEY
        DEY
        TYA
        CLC
        ADC     scratch
        STA     scratch
        BCC     Scratch_Ok2
        INC     scratch+1
.Scratch_Ok2                                   \ scratch now holds length of file in RFS

.Less_than_3_blocks
        LDA     rstart                         \ put previous start (end of this file) in header
        STA     heap,X
        STA     heap+$20,X
        INX
        LDA     rstart+1
        STA     heap,X
        STA     heap+$20,X
        INX
        LDA     #$00                           \ then put two zeroes in header
        STA     heap,X
        STA     heap+$20,X
        INX
        STA     heap,X
        STA     heap+$20,X
        INX
                                
        SEC                                    \ calc start address for this file
        LDA     rstart
        SBC     scratch                        \ subtract length from end of file
        STA     rfsptr                         \ temp store in rfsptr while checking space
        LDA     rstart+1
        SBC     scratch+1
        STA     rfsptr+1
        CMP     #HI(Data)-$40                  \ compare start of file with start of rfs ($9000 in ROM, $5000 in RFS)
        BCS     Space_OK

        JSR     Print_String2

        EQUS    $0D," Not Enough Space",$0D,$EA
        
        JMP     Leave_Osfile
        
.Space_OK
        LDA     rfsptr        
        STA     rstart                         \ update start address
        EOR     #$55
        STA     rcheck                         \ and update check value
        
        LDA     rfsptr+1
        STA     rstart+1
        EOR     #$AA
        STA     rcheck+1
        
        BIT     tubeflag
        BVC     Dont_Setup_Tube
        
 .Claim_Tube
        LDA     #tubeID                        \ claim tube interface
        JSR     &406
        BCC     Claim_Tube
        LDX     #LO(sdata)
        LDY     #HI(sdata)
        LDA     #$00
        JSR     &406                           \ set up tube for read operation
               
.Dont_Setup_Tube        
        LDA     #$01                           \ set up to transfer first header
        JSR     Write_Header                   \ in write.asm
        LDA     sdata                          \ set up to write first block
        STA     rfssrc                         \ by setting rfssrc to start address of data
        LDA     sdata+1
        STA     rfssrc+1
        LDA     #$00
        DEC     nblocks
        PHP
        BNE     More_Than_1_Block
        LDA     ldata                          \ if only one block use data length as length of block
.More_Than_1_Block
        STA     wrnum
        JSR     Write_Block                    \ in write.asm
        PLP
        BEQ     Wrfs_End                       \ if only one block finish now
        
.Wrfs_Loop
        DEC     nblocks
        BEQ     Do_Last_Block
        
        LDA     #'#'                           \ if not last block write "#" synchroniser
        JSR     Write_Block_With_Sync          \ in write.asm
        JMP     Wrfs_Loop                      \ repeat
        
.Do_Last_Block
        LDA     rfssrc                         \ save memory pointer rfssrc because it will be modified during header
        PHA
        LDA     rfssrc+1
        PHA
        LDA     #$21                           \ set up to transfer last header
        JSR     Write_Header                   \ in write.asm
        PLA
        STA     rfssrc+1                       \ retrieve memory pointer rfssrc
        PLA
        STA     rfssrc
        LDA     ldata
        STA     wrnum                          \ length of last block = lsb of data length
        JSR     Write_Block                    \ in write.asm

.Wrfs_End
        BIT     tubeflag
        BVC     Dont_Release_Tube
        LDX     #LO(sdata)
        LDY     #HI(sdata)
        LDA     #tubeID
        AND     #&BF                           \ clear bit 6 of ID to release tube
        JSR     &406                           \ release tube

.Dont_Release_Tube                             \ append 6 byte WRFS suffix to file
        LDA     #'*'                           \ "*" as end synchronisation for RFS
        JSR     Write_Byte
        LDA     rstart+1                       \ bigendian start of rfs file
        JSR     Write_Byte
        LDA     rstart
        JSR     Write_Byte
        LDA     ldata+1                        \ bigendian data length
        JSR     Write_Byte
        LDA     ldata
        JSR     Write_Byte
        LDA     #$FF                           \ initial delete flag, will be set to 0 when deleted
        JSR     Write_Byte

.Leave_Osfile
        JSR     osnewl
        
.Leave_Osfile1
        LDA     save_a
        
.Leave_Osfile2
        LDY     save_x
        LDX     save_y
        RTS
 