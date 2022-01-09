\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike

\ As well as the source for the WRFS ROM this code includes RAM based routines for updating and recovery.
\ Those sections follow this code at an offset of $1000 and will not be copied into the ROM.

\ The filing system uses memory starting at $9000 in the program ROM up to $BFFF in the second "hidden" ROM
\ This is mapped virtually as $5000 to $FFFF.
\ $FFFF is always the end of the last file and other files grow downwards in memory
\ This means that RFS will always see the latest version of a file first because it always searches from the beginning

\ Files are conventional RFS files with the addition of 6 extra bytes at the end of each:
\ 1.  A "*" to enable the RFS to verify the end of the file
\ 2/3 A bigendian pointer to the start of the file
\ 4/5 The bigendian length of the file data, ie, end address - start address
\ 6.  Delete flag $FF, set to $00 when the file is deleted


\ 6-jan-2022 changes to restore and defrag routines to remove any files that might have been corrupted by a different ROM.  

include "electron.asm"

 
.romstart

        JMP     Recovery-$8000+dload      \ this is to allow the code to run for installation/recovery 
                                          \ and these 3 bytes will be replaced by nulls in the ROM        
        JMP     ServiceEntry
        
        EQUB    $82

        EQUB    LO(COPYR)

        EQUB    01

.TITLESTR
        EQUS    "Wifi RFS ",$00,"0.2"

.COPYR
        EQUS    $00,"(C)2021 John Wike",$00

.HELPSTR
        EQUS    $0D,"  "
.COMMSTR
        EQUS    "WROM",$80,$0D,$0D,$00
.COMMSTR2
        EQUS    "  SAVE",$0D,"  "
.COPYSTR
        EQUS    "!WCOPY",$82,"  <fsp>",$0D,"  "    \ ! is a place marker for the command routine

        EQUS    "!DELETE",$84," <fsp>",$0D,"  "    \ and negative values are pointers to command table offset by $80

        EQUS    "!INFO",$86,$0D,"  "               \ both will be ignored by the print routine

        EQUS    "!FREE",$88,$0D,"  "

        EQUS    "!FORMAT",$8A,$0D,"  "
        
        EQUS    "!DEFRAG",$8C,$0D,"  "
        
        EQUS    "!UPDATE",$8E,$0D,"  "
        
        EQUS    "!MANUAL",$90,$0D,"!",$00
        
        
       
.Comm_Table
        EQUW    Do_WROM-1                         \ command pointers
        EQUW    Do_COPY-1
        EQUW    Do_DELETE-1
        EQUW    Do_INFO-1
        EQUW    Do_FREE-1
        EQUW    Do_FORMAT-1
        EQUW    Do_DEFRAG-1                       \ in defrag.asm
        EQUW    Do_UPDATE-1                       \ in update.asm
        EQUW    Do_MANUAL-1                       \ in update.asm

.Boot_Command
        EQUS    "*E.!BOOT",$0D
        
.AddPTR                                          \ add the accumulator to the ROM pointer ($F6/F7) for the
        CLC                                      \ RFS services
        ADC     ROMptr
        STA     ROMptr
        BCC     AddPTR_End
        INC     ROMptr+1
        LDA     ROMptr+1
        JSR     Update_Bank                      \ update the mfa and bank values for ROM access, in write.asm
.AddPTR_End
        RTS
        
.ServiceEntry
        CLD
        PHA
        CMP     #$03
        BEQ     Do_3
        CMP     #$12
        BNE     Not_12
        TYA                                      \ service $12 initialise filing system
        PHA
        CPY     #myFSno                          \ is it my FS number?
        BNE     Leave_Not_Claim2
        BEQ     Continue_03
        
.Do_3
        TYA                                      \ service 03 Auto Boot 
        PHA
                                                 \ check if W is pressed
        LDA     #$7A			                 \ OSBYTE &7A - keyboard scan from 16
        JSR     osbyte
        CPX     #$21 			                 \ Was W pressed?
        BEQ     Acknowledge_W

        INX				                
        BEQ     Continue_03                      \ no key pressed so continue
        
.Leave_Not_Claim2                                \ different key pressed so leave
        JMP     Leave_Not_Claim
        
.Acknowledge_W
        LDY     #$00                             \ osbyte $78 acknowledge key press
        LDA     #$78
        JSR     osbyte
        
.Continue_03 
        JSR     Wrom_Setup                       \ set up wrom vectors
        PLA
        PHA
        TAY                                      \ was Y zero on entry?
        BNE     Leave_03                 
        
        LDX     #LO(Boot_Command)                \ if so shift is pressed so auto boot
        LDY     #HI(Boot_Command)
        JSR     oscli                            \ pass boot command to oscli
                
.Leave_03
        JMP     Leave_Claim
        
.Not_12
        CMP     #$0E
        BEQ     Serv14
        JMP     Not_E

.Serv14_Slow                                     \ use conventional (slow) RFS routines
        LDA     ROMptr+1
        JSR     Update_Bank                      \ update mfa and bank for ROM access, in write.asm
        JSR     Read_Byte_From_F6                \ read byte using ROM pointers $F6,$F7
        TAY
        LDA     #$01                             \ inc ROM pointers
        JSR     AddPTR
        PLA
        LDX     $F4
        LDA     #$00
        RTS
        
.Serv14                                          \ read byte from RFS
        LDA     serROM
        EOR     #15
        CMP     $F4                              \ is it my ROM?
        BEQ     Its_Me
        PLA
        RTS

.Its_Me
        LDA     $E2
        AND     #$08
        BNE     Serv14_Slow                      \ if cataloguing use normal RFS
        
        LDA     $C2
        CMP     #$04
        BNE     Serv14_Slow                      \ if not fetching data use normal RFS
        
        LDA     $27A
        BPL     Serv14_Loop0                     \ if no tube continue with C set from CMP
        LDA     $B2
        AND     $B3
        CMP     #$FF                             \ if i/o address ($FFFFxxxx) set C
        
.Serv14_Loop0
        BCC     Not_Seq                          \ if C clear this is a tube address
        LDA     $B1
        EOR     #$0A                             \ if the i/o address is in page $A00 this is 
        BEQ     Serv14_Slow                      \ sequential access so use normal RFS
        
                                                 \ this is a data fetch so use the      
.Not_Seq                                         \ "fast RFS" routines developed by JG Harston
        PHP                                      \ save tube flag, C clear = tube, set = io
        LDA     ROMptr+1
        JSR     Update_Bank                      \ update mfa and bank for ROM access, in write.asm
.Serv14_Loop1
        LDY     #$00       
        
.Serv14_Loop        
        STY     count14
        JSR     Read_Byte_From_F6                \ read byte, in write.asm
        LDY     count14
        PLP                                      \ get tube flag
        PHP
        BCS     Serv14_IO        
        STA     $FCE5                            \ read from tube
        BCC     Serv14_Tube
        
.Serv14_IO
        STA     ($B0),Y                          \ read from memory
.Serv14_Tube
        LDA     #$01
        JSR     AddPTR                           \ inc pointer

        INY
        CPY     $3C8
        BNE     Serv14_Loop                      \ continue until block length reached
        
        LDA     #$02                             \ step past crc at end of block
        JSR     AddPTR
        LDA     $3C9
        BEQ     Serv14_Finished                  \ if high byte of block length is 0, then finished
        
        INC     $B1                              \ inc high byte of memory pointer
        JSR     Read_Byte_From_F6                \ read byte, in write.asm
        CMP     #'#'
        BNE     Serv14_Last                      \ if it is not "#" this is the start of the last block header
        
        LDA     #$01
.Serv14_Next
        JSR     AddPTR
        BCC     Serv14_Loop1                     \ inc the pointer past "#" and repeat
        
.Serv14_Last                                     \ last block header
        LDA     #$01
        JSR     AddPTR
        JSR     Read_Byte_From_F6                \ in write.asm
        BNE     Serv14_Last                      \ advance pointer to 0 at end of filename
        LDA     #11
        JSR     AddPTR                           \ advance pointer to block length (lsb of data length)
        JSR     Read_Byte_From_F6                \ read length, in write.asm
        STA     $3C8                             \ set length in memory
        LDA     #$00
        STA     $3C9                             \ set msb to 0
        LDA     #09
        BNE     Serv14_Next                      \ advance to start of data block
        
.Serv14_Finished        
        PLP                                      \ get flag off stack
        STA     $BE                              \ set crc result to 0
        STA     $BF
        STA     $C2                              \ set read done flag
        
        LDA     #$80
        STA     $BD                              \ set last byte flag
        STA     $3CA                             \ set last block flag
        JMP     Leave_Claim1

        
.Print_String
        LDA     TITLESTR,Y                       \ print the 0 terminated title and help strings depending on offset in Y
        BEQ     Leave_Print
        BMI     Dont_Print                       \ ignore the negative bytes and "!" bytes
        CMP     #'!'
        BEQ     Dont_Print
        JSR     osasci
        
.Dont_Print
        INY
        BNE     Print_String
        
.Leave_Print
.AnRTS
        RTS
        
.Not_E
        CMP     #$0D
        BNE     Not_D

\ service 13
        TYA                                      \ initiate read from RFS
        EOR     #15
        CMP     $F4                              \ is it my ROM?
        BCC     Not_4
        JSR     Check_WROM                       \ is WROM active?
        BNE     Not_4
        LDA     #heappage                        \ set the heap page in paged RAM
        STA     pagereg
        LDA     $F4
        EOR     #15                              \ take ownership of RFS
        STA     serROM
        JSR     Check_Start                      \ ensure the internal RFS pointer is valid
        LDA     rstart
        STA     ROMptr                           \ load the ROM pointer at $F6,$F7 with the internal pointer
        LDA     rstart+1
        STA     ROMptr+1
        JSR     Update_Bank                      \ update the mfa and bank for the ROM, in write.asm
        JMP     Leave_Claim1
        
.Print_String2                                   \ print the inline text following the JSR, ending with $EA
        PLA
        STA     wrzp                             \ pull return address off stack and store in zero page
        PLA
        STA     wrzp+1
.String_Loop
        LDY     #$00
        INC     wrzp                             \ inc the zero page pointer
        BNE     String_Loop2
        INC     wrzp+1
.String_Loop2
        LDA     (wrzp),Y                         \ get next character in string
        BMI     End_String                       \ leave if negative
        JSR     osasci                           \ print the character 
        CLC
        BCC     String_Loop
.End_String
        JMP     (wrzp)                           \ JMP to termination character
                                                 \ that is why it needs to be $EA (NOP)
.Not_D
        CMP     #$09
        BNE     Not_9
        
\ service 9                                      \ help call
        TYA
        PHA
        LDX     #$00                             \ pointer to COMMSTR
        LDA     (line),Y                         \ get next character in command line
        CMP     #$20
        BCS     Help_Loop
        JSR     osnewl                           \ if next character is a cr print TITLESTR and version
        LDY     #$00
        JSR     Print_String
        INY
        JSR     Print_String
        LDY     #HELPSTR-TITLESTR                \ then print "WROM"
        JSR     Print_String
        BEQ     Leave_Not_Claim
        
.Help_Loop
        LDA     (line),Y                         \ compare command line entry with "WROM"
        AND     #$DF
        CMP     COMMSTR,X
        BNE     Leave_Not_Claim
        INY
        INX
        CPX     #$04                             \ if 4 characters have matched continue
        BNE     Help_Loop
        
        LDY     #COMMSTR2-TITLESTR               \ print help string
        JSR     Print_String
        BEQ     Leave_Claim
        
.Leave_Not_Claim
        PLA                                      \ leave service without claiming it
        TAY
        LDX     $F4
.Not_4
        PLA
        RTS

.Info_Code
        SEC
        CLV
        JMP     New_Cat                          \ in write.asm

.Do_INFO                                         \ *INFO command
        JSR     Info_Code
        
.Leave_Claim
        PLA                                      \ leave service and claim with A=0
        TAY
.Leave_Claim1
        LDX     $F4
        
.Leave_Claim2
        PLA
        LDA     #$00
        RTS
        
.Not_9
        CMP     #$04
        BNE     Not_4
        
\ service 4
        TYA
        PHA
        LDX     #$00                             \ search command line for matching commands
.Comm_Loop
        LDA     COMMSTR,X
        BEQ     Leave_Not_Claim                  \ if 0 no matching command has been found
        BMI     Select_Command                   \ if negative a matching command has been found
        
        LDA     (line),Y
        AND     #$DF
        CMP     COMMSTR,X
        BNE     Comm_Loop1                       \ characters dont match so try next command
        
        INY
        INX
        BNE     Comm_Loop
        
.Select_Command
        DEY
        TAX
        LDA     Comm_Table-$7F,X                 \ get pointers from table offset by $80
        PHA                                      \ push on stack
        LDA     Comm_Table-$80,X
        PHA
        LDA     #heappage
        STA     pagereg
        RTS                                      \ "RTS" to routine
        
.Check_WROM
        LDA     osfvec+1                         \ determine whether vectors are set for this rom
        CMP     #$FF
        BNE     Leave_Check_WROM                 \ is osfile vector extended?
        
        LDA     extvectab+ext_osfile+2
        CMP     $F4                              \ is it extended to this ROM?
.Leave_Check_WROM        
        RTS                                      \ zero if they are

.Comm_Loop1
        INX
        LDA     COMMSTR,X
        BPL     Comm_Loop1                       \ advance until a negative value
        CMP     #$80
        BEQ     Point_To_Copy                    \ if it is $80 (end of WROM) need to advance to COPYSTR
        CMP     #$82
        BNE     Allow_WROM_Commands              \ commands WROM and WCOPY are allowed even if WROM not already selected
        JSR     Check_WROM                       \ otherwise check if WROM selected
        BNE     Leave_Not_Claim                  \ if not, leave without claiming
        BEQ     Allow_WROM_Commands

.Point_To_Copy
        LDX     #COPYSTR-COMMSTR-1

.Allow_WROM_Commands
        PLA
        PHA
        TAY                                      \ return y to start 
        
.Comm_Loop2
        INX
        LDA     COMMSTR,X
        CMP     #'!'
        BNE     Comm_Loop2                       \ advance pointer until "!" found
        INX
        BNE     Comm_Loop                        \ try next command
        
.Do_DELETE                                       \ *DELETE command
        JSR     Get_Filename_From_Line           \ in vectors.asm
        BCS     Leave_Delete                     \ leave if syntax error
        JSR     Bit_New_Cat                      \ with c clear, in write.asm
        BVC     Leave_Delete
        JSR     Not_Found                        \ if v set file not found
        
.Leave_Delete
        JMP     Leave_Claim

.Not_Found
        JSR     Print_String2
        EQUS    $0D,"Not Found",$0D,$0D,$EA
        RTS
        
.Hex_Dec                                         \ convert hex value in $100/$101 to decimal value in $102 to $104
        LDA     #$00
        STA     $102
        STA     $103
        STA     $104
        LDX     #16
        SED
.Hex_Dec_Loop
        ASL     $100
        ROL     $101
        LDA     $102
        ADC     $102
        STA     $102
        
        LDA     $103
        ADC     $103
        STA     $103
        
        LDA     $104
        ADC     $104
        STA     $104
        DEX
        BNE     Hex_Dec_Loop
        CLD
        RTS

.Print_Dec5                                      \ print 5 decimal digits from hex in $100/1       
        JSR     Hex_Dec
        LDA     $104
        JSR     printhex_l1
        LDA     $103
        JSR     printhex
        LDA     $102
        JMP     printhex
        
.Print_Dec3                                      \ print 3 decimal digits from hex in $100/1
        JSR     Hex_Dec
        LDA     $103
        JSR     printhex_l1
        LDA     $102
        JMP     printhex
        
        
.Do_FREE                                         \ *FREE command
        JSR     Check_Start                      \ check start pointer is correct
        LDA     rstart
        SEC
        SBC     #LO(Data)                        \ subtract start of filing system
        STA     $100
        LDA     rstart+1
        SBC     #HI(Data)-$40                    \ difference between actual location ($9000 in ROM) and virtual ($5000 in WRFS)
        STA     $101                             \ stored in $100/1
        LDY     #$00
        JSR     Print_String                     \ print title
        JSR     Print_String2

        EQUS    $0D,"   Free Space: &",$EA
        
        LDA     $101                             \ print hex value
        JSR     printhex
        LDA     $100
        JSR     printhex
        LDA     #' '
        JSR     osasci
        JSR     osasci
        JSR     Print_Dec5                       \ print 5 digit decimal value
        JSR     osnewl
        
.Format_Leave
        JSR     osnewl
        JMP     Leave_Claim
        
.Yes_No
        JSR     osrdch                           \ wait for "y", "N", Return or Escape to be pressed
        BCS     Yes_No_Escape
        CLV
        CMP     #'y'
        BEQ     Yes_No_Yes
        CMP     #$0D
        BEQ     Yes_No_Leave
        AND     #$DF
        CMP     #'N'
        BEQ     Yes_No_Leave
        BNE     Yes_No
        
.Yes_No_Escape                                   \ acknowledge Escape press               
        LDA     #$7E
        JSR     osbyte
        JSR     Print_String2
        
        EQUS    $0D,$0D,"Escape",$0D,$EA
        
        BIT     AnRTS

.Yes_No_Yes
        CLC
        
.Yes_No_Leave
        RTS                                      \ V set if escape, C set if no, C clear if yes

        
.Do_FORMAT                                       \ *FORMAT command
        JSR     Print_String2

        EQUS    "Are you sure? y/N",$EA

        JSR     Yes_No
        BVS     Format_Leave
        BCS     Format_Leave

        JSR     osnewl
        JSR     Format_Main
        JSR     Format_Cleanup
        JMP     Format_Leave
        
.Format_Main
        LDX     #$03                            \ X=3 means erase the filing system area
        JSR     Format_Setup                    \ load format code
        JSR     formatcode                      \ call format code
        LDA     #heappage                       \ reset the paged ram to the heap page
        STA     pagereg
        RTS

.Format_Setup                                   \ load format code into page $FD of paged ram
        LDA     #formatpage
        STA     pagereg
        LDY     #$00
.Format_Loop2
        LDA     Format_Start,Y
        STA     formatcode,Y
        INY
        CPY     #Format_End-Format_Start
        BNE     Format_Loop2
        RTS
        
        
.Format_Cleanup                                \ reset the start pointer and the check pointer
        LDA     #$FF
        STA     rstart
        STA     rstart+1
        LDA     #$FF EOR $55
        STA     rcheck
        LDA     #$FF EOR $AA
        STA     rcheck+1
        RTS

.Do_WROM                                       \ *WROM command
        JSR     Wrom_Setup
        JMP     Leave_Claim

.Wrom_Setup                                    \ because setup needs to be called from several places make it a subroutine
        STA     rcheck                         \ change the check value to force rechecking of the start pointer
        
        LDA     #$8D                           \ select RFS (WRFS is an extension of RFS)
        JSR     osbyte
        
        JSR     Set_Vecs                       \ set the WRFS vectors
        
        LDA     #$8F
        LDX     #$0F
        JMP     osbyte                         \ signal a change of filing system to other ROMs

.Vec_Tab
        EQUB    LO(osfvec)
        EQUB    LO(argsvec)
        EQUB    LO(osfindvec)
        EQUB    LO(osfscvec)

.Vec_Tab1
        EQUB    ext_osfile
        EQUB    ext_args
        EQUB    ext_osfind
        EQUB    ext_osfsc

.Vec_Tab2
        EQUB    LO(New_Osfile)        
        EQUB    LO(New_Osargs)
        EQUB    LO(New_Osfind)
        EQUB    LO(New_Osfsc)

.Vec_Tab3
        EQUB    HI(New_Osfile)        
        EQUB    HI(New_Osargs)
        EQUB    HI(New_Osfind)
        EQUB    HI(New_Osfsc)
        
.Set_Vecs                                    \ set up extended vectors
        LDX     #$00
.Set_Vecs_Loop
        LDY     Vec_Tab,X                    \ get location of vector in page $200
        LDA     #$FF
        STA     $201,Y
        LDA     Vec_Tab1,X
        STA     $200,Y                       \ save extended vector ($FFxx) in page $200
        TAY
        LDA     Vec_Tab2,X                   \ store new vector values in extended vector area
        STA     extvectab,Y
        LDA     Vec_Tab3,X
        STA     extvectab+1,Y
        LDA     $F4
        STA     extvectab+2,Y                \ store this rom number in extended vector area
        INX
        CPX     #Vec_Tab1-Vec_Tab
        BNE     Set_Vecs_Loop                \ repeat until all done
        
.Opt_10
        LDA     $E3                          \ *OPT 1,0 clears bits 7,6,3,2 of &E3
        AND     #$33
        STA     $E3
        RTS
                
.Load_Rwcode                                 \ load rw code into ram page $FF
        LDA     #rwpage
        STA     pagereg
        
        LDY     #$00
.Rw_Load_Loop
        LDA     Rw_Start,Y
        STA     rwcode,Y
        INY
        CPY     #Rw_End-Rw_Start
        BNE     Rw_Load_Loop
        
        LDA     #heappage
        STA     pagereg                      \ reset ram page to heap
        RTS

.Check_Start                                 \ load rw code and check if start pointer has been corrupted                   
        JSR     Load_Rwcode                
        LDA     rstart                       \ by comparing it with the EOR'ed values in rcheck
        EOR     #$55
        CMP     rcheck
        BNE     Find_Start
        LDA     rstart+1
        EOR     #$AA
        CMP     rcheck+1
        BNE     Find_Start
        RTS        

.Find_Start                                  \ update rstart pointer
        TXA
        PHA
        TYA
        PHA
        LDA     #$FF                         \ begin at end $FFFF
        STA     rfsptr
        STA     rfsptr+1

.Find_Start_Loop
        SEC                                  \ pointer to start of file is 5 bytes before end
        LDA     rfsptr
        STA     rstart
        SBC     #$05
        STA     rfsptr
        LDA     rfsptr+1
        STA     rstart+1
        SBC     #$00
        STA     rfsptr+1                     \ subtract 5 from end of file
        
        JSR     Update_And_Read_Byte         \ in write.asm
        PHA
        JSR     Inc_And_Read_Byte
        TAY
        PLA                                  \ read bigendian start pointer, high byte in A, low byte in Y
        CMP     #$FF
        BEQ     High_Byte_Is_FF              \ if both are $FF there are no more files and the start has been reached

.Low_Byte_Not_FF
        STA     rfsptr+1
        STY     rfsptr
        BCC     Find_Start_Loop              \ if not $FFFF loop to next file
        
.High_Byte_Is_FF
        CPY     #$FF
        BNE     Low_Byte_Not_FF

        LDA     rstart                       \ start reached so set rcheck value by EORing with rstart
        EOR     #$55
        STA     rcheck
        LDA     rstart+1
        EOR     #$AA
        STA     rcheck+1
        PLA
        TAY
        PLA
        TAX
        RTS

       

include "write.asm"
include "defrag.asm"
include "update.asm"
include "copy.asm"
include "vectors.asm"

.Format_Start
incbin "format.bin"
.Format_End

.Rw_Start
incbin "rw.bin"
.Rw_End

\ fill out rom with $FF to $9000 

     FOR n, Rw_End,romstart+$FFF,1
        EQUB $FF
     NEXT
     
\ THIS IS THE END OF THE ROM
\ The rest of the code will be run from RAM during update/recovery

include "update2.asm"

.romend

PRINT "Rw_End is at ",~(Rw_End)," versus limit of &8FF8"
\CRC of Etag will be stored at &8FF8, and 6 bytes are needed at the end as a buffer in case RFS fills up
    
SAVE "wrfs",romstart,romend
        