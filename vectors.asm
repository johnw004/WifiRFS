        
.New_Osargs
        CMP     #$00
        BNE     Try_Args
        CPY     #$00
        BNE     Try_Args
        LDA     #$03
        RTS
        
.Try_Args
        CPY     #$03
        BNE     Leave_Args
        
        CMP     #$00
        BNE     Try_Args2
        
        LDA     $39E
        STA     $00,X
        LDA     $3C6
        STA     $01,X
        LDA     #$00
        
.Leave_Args
        RTS
        
.Try_Args2
        CMP     #$02
        BNE     Leave_Args
        
        STX     save_x
        JSR     Load_Rwcode
        
        SEC
        LDA     $3CB
        SBC     #$03
        STA     rfsptr
        LDA     $3CC
        STA     rfsptr+1
        
        JSR     Update_And_Read_Byte
        LDX     save_x
        STA     $01,X
        JSR     Inc_And_Read_Byte
        LDX     save_x
        STA     $00,X
        
        LDY     #$03
        LDA     #$02
        RTS


.New_Osfsc
		JSR     Save_axy
        
        CMP     #$05
        BEQ     Do_Cat
        
        CMP     #$03
        BNE     Leave_Osfsc
        
        LDA     #$02
        STA     save_a
        
.Leave_Osfsc
        LDY     #LO(osfscvec)+1        
        JMP     Leave_Default_Vecs

.Do_Cat
        LDY     #$00
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
        JSR     New_Cat
        JMP     Leave_Osfile

.New_Osfind
        JSR     Save_axy
        CMP     #$00
        BEQ     Default_Osfind               \ pass A=0, close file
        STX     wrzp
        STY     wrzp+1
        LDY     #$00
        JSR     Get_Filename2
        SEC
        JSR     Bit_New_Cat
        
        
        BVC     Default_Osfind               \ check if file exists and is not deleted
        LDA     #$00
        JMP     Leave_Osfile2                \ leave with A=0

.Default_Osfind
        LDY     #LO(osfindvec)+1        
        JMP     Leave_Default_Vecs            \ pass call to default RFS routine

.Osfile_5FF
        PHA
        JSR     Get_Filename
        SEC
        BIT     AnRTS
        JSR     New_Cat
        PLA
        TAY
        BMI     Osfile_FF
        
        LDA     #$01
        BVC     Osfile_5_Found
        LDA     #$00
.Osfile_5_Found
        JMP     Leave_Osfile2
        
.Osfile_FF
        BVC     Osfile_FF_Found
        JSR     Not_Found
        LDA     #$00
        JMP     Leave_Osfile2
        
.Osfile_FF_Found
        JSR     Opt_10
        JSR     Default_Osfile
        LDA     #$01
        RTS
        
.Osfile_Delete
        JSR     Get_Filename
        CLC
        JSR     Bit_New_Cat
        JMP     Leave_Osfile1

.Save_axy
		STA     save_a
        STY     save_x             \ x and y are wrong way round in electron.asm!!
        STX     save_y
        RTS
        
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
        
.Leave_Default_Vecs
        LDA     vectabaddr
        STA     wrzp
        LDA     vectabaddr+1
        STA     wrzp+1
        LDA     (wrzp),Y
        PHA
        DEY
        LDA     (wrzp),Y
        PHA
        PHP
        LDX     save_y
        LDY     save_x
        LDA     save_a
        RTI

.Get_Filename_From_Line
        INY
        LDA     (line),Y
        CMP     #' '
        BEQ     Get_Filename_From_Line
        BCC     Syntax_Error
        LDA     line
        STA     wrzp
        LDA     line+1
        STA     wrzp+1
        BNE     Get_Filename2
        
.Syntax_Error
        JSR     Print_String2
        EQUS    $0D,"Syntax",$0D,$EA
        SEC
        RTS
        
.Get_Filename        
\ Create two header blocks at heap+1 and heap+$21, one for the first block and one for the last block (if any)        
        LDY     #$01                \ get filename
        LDA     (save_y),Y
        STA     wrzp+1
        DEY
        LDA     (save_y),Y
        STA     wrzp
        
.Get_Filename2
        LDA     #heappage
        STA     pagereg
        LDX     #$01
.Param_Loop1
        LDA     (wrzp),Y           \ filename could end in cr or space
        CMP     #$21
        BCC     Param_Loop2
        CPX     #$08               \ limit to 7 characters
        BEQ     Param_Loop2
        STA     heap,X
        STA     heap+$20,X
        INY
        INX
        BNE     Param_Loop1
.Param_Loop2
        TXA
        CLC
        ADC     #$14
        STA     hdrlen             \ store header length
        LDA     #$00
        STA     heap,X
        STA     heap+$20,X
        INX
        RTS

.Osfile_Write
        JSR     Get_Filename
        TXA
        PHA
        CLC
        JSR     Bit_New_Cat            \ delete previous version
        PLA
        TAX

\ Get load and execution address
        LDY     #$02
.Param_Loop3
        LDA     (save_y),Y     
        STA     heap,X
        STA     heap+$20,X
        INY
        INX
        CPY     #$0A
        BNE     Param_Loop3
        
\ Get start address
        LDA     $27A
        STA     tubeflag           \ FF if tube present, 00 if not
        LDA     (save_y),Y
        STA     sdata
        INY
        LDA     (save_y),Y
        STA     sdata+1
        INY
        LDA     (save_y),Y
        INY
        AND     (save_y),Y
        EOR     #$FF              \ FF if IO address, make 00
        BNE     Not_IO_Address
        STA     tubeflag          \ write 00 to tubeflag
        
.Not_IO_Address
        INY
        
\ Get length
        SEC
        LDA     (save_y),Y
        SBC     sdata
        STA     ldata
        INY
        LDA     (save_y),Y
        SBC     sdata+1
        STA     ldata+1
        
\ Get number of blocks
        TAY
        LDA     ldata                        \ if length is x00, no need to increment number of blocks
        BEQ     Nblocks_OK
        INY
.Nblocks_OK
\ Get block number of first and last blocks
        STY     nblocks
        DEY
        TYA
        INY
        STA     heap+$20,X
        LDA     #$00
        STA     heap,X
        INX
        STA     heap,X
        STA     heap+$20,X
        INX
        
\ Get block length of first and last blocks
        LDA     #$00           \ set length of $100
        STA     heap,X
        STA     heap+$20,X
        LDA     #$01
        STA     heap+1,X
        STA     heap+$21,X
        
        LDA     ldata          \ if length = x00, block lengths are OK
        BEQ     Block_lengths_OK
        STA     heap+$20,X
        DEC     heap+$21,X       \ adjust length of last block
                         
        CPY     #$01            \ if more than one block, first block OK
        BNE     Block_lengths_OK
        STA     heap,X          \ adjust length of first block
        DEC     heap+1,X
.Block_lengths_OK
        INX
        INX
        LDA     #$80
        STA     heap+$20,X
        CPY     #$01
        BEQ     Flag_OK
        LDA     #$00            \ if only one block set flag to $80
.Flag_OK
        STA     heap,X
        INX
        
\ Calculate length in RFS
        LDA     #$00
        STA     scratch+1
        LDA     hdrlen
        DEY
        BEQ     Only_1_header   \ first add in headers
        ASL     A               \ if > 1 block there will be 2 headers
.Only_1_header
        CLC
        ADC     #$06            \ add 6 bytes for *, start location delete flag and length info
        ADC     ldata           \ add the length of the data
        STA     scratch
        LDA     scratch+1
        ADC     ldata+1
        STA     scratch+1

        LDY     #$02            \ add 2 bytes for checksum for each block
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
        CPY     #$03            \ if more than 2 blocks add 1 byte for the # symbol in intermediate blocks
        BCC     Less_than_3_blocks
        DEY
        DEY
        TYA
        CLC
        ADC     scratch
        STA     scratch
        BCC     Scratch_Ok2
        INC     scratch+1
.Scratch_Ok2                 \ scratch now holds length of file

.Less_than_3_blocks
        LDA     rstart          \ put previous start in header
        STA     heap,X
        STA     heap+$20,X
        INX
        LDA     rstart+1
        STA     heap,X
        STA     heap+$20,X
        INX
        LDA     #$00
        STA     heap,X
        STA     heap+$20,X
        INX
        STA     heap,X
        STA     heap+$20,X
        INX
                                
        SEC                     \ calc start address for this file
        LDA     rstart
        SBC     scratch
        STA     rfsptr          \ temp store in rfsptr while checking space
        LDA     rstart+1
        SBC     scratch+1
        STA     rfsptr+1
        CMP     #HI(Data)-$40 
        BCS     Space_OK

        JSR     Print_String2

        EQUS    $0D," Not Enough Space",$0D,$EA
        
        JMP     Leave_Osfile
        
.Space_OK
        LDA     rfsptr        
        STA     rstart          \ update start address
        EOR     #$55
        STA     rcheck          \ and check value
        
        LDA     rfsptr+1
        STA     rstart+1
        EOR     #$AA
        STA     rcheck+1
        
        BIT     tubeflag
        BVC     Dont_Setup_Tube
        
 .Claim_Tube
        LDA     #tubeID                \ claim tube interface
        JSR     &406
        BCC     Claim_Tube
        LDX     #LO(sdata)
        LDY     #HI(sdata)
        LDA     #$00
        JSR     &406                   \ set up tube for read operation
               
.Dont_Setup_Tube        
        LDA     #$01            \ set up to transfer first header
        JSR     Write_Header
        LDA     sdata           \ set up to write first block
        STA     rfssrc
        LDA     sdata+1
        STA     rfssrc+1
        LDA     #$00
        DEC     nblocks
        PHP
        BNE     More_Than_1_Block
        LDA     ldata           \ if only one block use data length
.More_Than_1_Block
        STA     wrnum
        JSR     Write_Block
        PLP
        BEQ     Wrfs_End        \ if only one block finish now
        
.Wrfs_Loop
        DEC     nblocks
        BEQ     Do_Last_Block
        
        LDA     #'#'
        JSR     Write_Block_With_Sync
        JMP     Wrfs_Loop
        
.Do_Last_Block
        LDA     rfssrc
        PHA
        LDA     rfssrc+1
        PHA
        LDA     #$21
        JSR     Write_Header
        PLA
        STA     rfssrc+1
        PLA
        STA     rfssrc
        LDA     ldata
        STA     wrnum
        JSR     Write_Block

.Wrfs_End
        BIT     tubeflag
        BVC     Dont_Release_Tube
        LDX     #LO(sdata)
        LDY     #HI(sdata)
        LDA     #tubeID
        AND     #&BF                   \ clear bit 6 of ID to release tube
        JSR     &406                   \ release tube

.Dont_Release_Tube
        LDA     #'*'
        JSR     Write_Byte
        LDA     rstart+1
        JSR     Write_Byte
        LDA     rstart
        JSR     Write_Byte
        LDA     ldata+1
        JSR     Write_Byte
        LDA     ldata
        JSR     Write_Byte
        LDA     #$FF
        JSR     Write_Byte

.Leave_Osfile
        JSR     osnewl
        
.Leave_Osfile1
        LDA     save_a
        
.Leave_Osfile2
        LDY     save_x
        LDX     save_y
        RTS
 