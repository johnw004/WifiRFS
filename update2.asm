\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike

\ second part of update sequence.
\ this part is not put in ROM and runs in RAM at an offset of $1000 from the start of the ROM code

\ 7-jan-2022 change to call defrag after recovery if other rom has corrupted bottom of rfs


\ enters here after rom has been downloaded
        JMP     Update2-$8000+dload
        
\ returns here after updating

.Return_From_Update
        LDA     #heappage                   \ set ram to heap page
        STA     pagereg
        
        LDX     $F4
        LDA     #$82
        STA     $2A0,X                      \ set rom type so OS knows there is a rom here
        
        JSR     Set_Vecs                    \ rom now updated so can use its routine to set the WRFS vectors,  
        
        PLP                                 \ from PHP at start of format code
                
        LDA     Corrupt_Flag-$8000+dload
        BNE     Dont_Save_Boot              \ dont try osfiles if rfs was corrupted
        
        LDA     #$05
        JSR     Call_Osfile3-$8000+dload    \ check if there is an existing !BOOT file
        CMP     #$01
        BEQ     Dont_Save_Boot
                
        LDA     #$00                        \ if not write one
        JSR     Call_Osfile3-$8000+dload

.Dont_Save_Boot
        LDY     #$00
        STY     rcheck
        
        JSR     Print_String2
        EQUS    $0D,"Update/Recovery Done",$0D,$EA
        
        LDA     Update_Flag-$8000+dload 
        BEQ     Leave_Update2               \ if the update flag is not 0 this is a recovery/install

        JMP     Recovery_Return-$8000+dload \ return to recovery routine
        
.Update_Flag
        EQUB    $00                         \ will be set to non zero by recovery code
        
.Corrupt_Flag
        EQUB    0                           \ will be set non zero if rom corrupted
        
.Update2
        LDY     wflagrom
        BPL     Update2a
        LDY     #0

.Update2a
        TYA
        BEQ     old_mfa
        LDA     #8
        
.old_mfa
        STA     wflagram                      \ set main flag for mfa version - 0 = old, 8 = new
        STA     Format_Start-$8000+dload+3    \ set format flag for mfa version
        STA     Rw_Start-$8000+dload+6        \ set rw flag for mfa version
        
        LDY     #$00                        \ clear the crc registers
        STY     crc16
        STY     crc16+1

\ The HTML header includes a quoted string called the E-Tag. This string changes every time a new version of the file
\ is saved. After the WGET operation the E-Tag is present in page 00 of the paged ram. I will store the crc of the E-Tag
\ and compare it with the crc of the downloaded E-Tag. If they are different this is a new version.

.Find_Etag
        LDA     pageram,Y                   \ search ram for quote
        INY
        BEQ     Etag_Done                   \ if none leave
        CMP     #'"'
        BNE     Find_Etag

.Etag_Loop
        LDA     pageram,Y                   \ read E-Tag
        INY
        BEQ     Etag_Done
        CMP     #'"'
        BEQ     Etag_Done                   \ finish at second quote
        JSR     Do_Crc-$8000+dload          \ in write.asm, but might be a change so use routine in ram
        CLC
        BCC     Etag_Loop
        
.Leave_Update2
        PLA                                 \ leaving update so claim service command
        TAY
        LDX     $F4
        PLA
        LDA     #$00
        RTS

.Etag_Done
        LDA     crc16                       \ save the crc in the ROM image
        STA     etagram
        LDA     crc16+1
        STA     etagram+1

        LDA     #heappage                   \ Etag read so set the paged ram to the heap
        STA     pagereg
        
        LDA     Update_Flag-$8000+dload
        BNE     Go_For_Update               \ if this is a recovery, automatically update

        LDA     etagram
        ORA     etagram+1
        BEQ     Go_For_Update
        
        LDA     etagram
        CMP     etagrom
        BNE     Go_For_Update               \ if etags are different, automatically update
        LDA     etagram+1
        CMP     etagrom+1
        BNE     Go_For_Update

        JSR     Print_String2               \ no change so give option, can use existing rom routines
        EQUS    $0D,"No difference. Upload anyway? y/N",$0D,$EA
        JSR     Yes_No
        BVS     Leave_Update2
        BCS     Leave_Update2

.Go_For_Update                              \ rom may have changed so need to update rw code and format code
        LDA     #rwpage
        STA     pagereg                     \ set rw page in ram      
        
        LDY     #$00                        \ remove recovery entry from start of rom
        STY     dload
        STY     dload+1
        STY     dload+2
        
        
.Rw_Load_Loop2
        LDA     Rw_Start-$8000+dload,Y      \ load rw code
        STA     rwcode,Y
        INY
        CPY     #Rw_End-Rw_Start
        BNE     Rw_Load_Loop2
        
        LDA     #formatpage
        STA     pagereg                     \ set format page and load format code
        LDY     #$00
.Format_Loop3
        LDA     Format_Start-$8000+dload,Y 
        STA     formatcode,Y
        INY
        CPY     #Format_End-Format_Start
        BNE     Format_Loop3
        
        LDX     #$04                        \ X=4 means erase program area of rom and not filing system
        JMP     formatcode

.Boot_File_Start
        EQUS    "CH.",$22,"START",$22,$0D   \ contents of !BOOT file
.Boot_File_End

.Boot_File_Name
        EQUS    "!BOOT",$0D                 \ cr terminated filename string for !BOOT
        
.Boot_File_Block
        EQUW    Boot_File_Name-$8000+dload   \ osfile parameter block
        EQUW    $FFFF,$FFFF
        EQUW    $FFFF,$FFFF
        EQUW    Boot_File_Start-$8000+dload,$FFFF
        EQUW    Boot_File_End-$8000+dload,$FFFF
        
.Call_Osfile3                                    \ called with A=5 to check if file exists, or 0 to write file
        LDX     #LO(Boot_File_Block-$8000+dload)
        LDY     #HI(Boot_File_Block-$8000+dload)
        JMP     osfile
        
.Electron_Wifi
        EQUS    "Electron Wifi",$00
        
.Cannot_Find
        EQUS    $0D,"Cannot find wifi board",$0D,$00

.Recovery_Jmp
        JMP     Recovery-$8000+dload
        
.Recovery                                     \ jump here from start of code
        PHP
        SEI                                   \ set interrupts
        LDA     $F4                           \ store ROM slot
        PHA
        LDX     #$02                          \ search for Electron Wifi ROM in slots 2 and 0
        
.Find_Wifi
        LDA     #$0F
        STA     Update_Flag-$8000+dload       \ set update flag non zero
        STA     $FE05                         \ deselect BASIC ROM
        
        STX     $F4
        STX     $FE05
        LDY     #$00
        
.Find_Wifi2
        LDA     Electron_Wifi-$8000+dload,Y   \ search for Electron Wifi title string
        BEQ     Wifi_Slot_Found
        EOR     $8009,Y
        INY
        AND     #$DF
        BEQ     Find_Wifi2
        
        DEX
        DEX
        BPL     Find_Wifi
        
        PLA                                   \ not in slot 2 or 0 so leave
        STA     $F4
        STA     $FE05                         \ reset calling slot
        PLP                                   \ clear interrupts
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
        LDY     &BFFE
        INX                                   \ wifi found so inc slot number
        STX     $F4                           \ store in $F4
        STX     $FE05
        LDA     $9000
        AND     $9001
        CMP     #$FF                          \ check that start of rfs area has not been overwritten
        BEQ     Not_Corrupted
        
        DEC     Corrupt_Flag-$8000+dload      \ set corrupt flag
        
.Not_Corrupted
        JMP     Update2a-$8000+dload           \ jump to Update2
        
        
.Recovery_Return                              \ returning from recovery
        LDA     Corrupt_Flag-$8000+dload
        BEQ     Not_Corrupted2
        JSR     Print_String2
        EQUS    $0D,"Corrupted by previous ROM so",$0D,"repairing filing system.",$0D
        EQUS    "This may result in some file loss",$0D,$EA

        JSR     Recovery_Defrag-$8000+dload   \ call defrag 
        JSR     Find_Start                    \ make sure start is set 

.Not_Corrupted2
        PLA
        STA     $F4                           \ reset calling slot
        STA     $FE05
        PLP                                   \ clear interrupts
        
        LDY     #$02                          \ put the JMP code back at the start so it could be run again
.Reinstate_Jmp_Code
        LDA     Recovery_Jmp-$8000+dload,Y
        STA     dload,Y
        DEY
        BPL     Reinstate_Jmp_Code

        LDA     #0
        STA     Update_Flag-$8000+dload       \ reset flags to zero
        STA     Corrupt_Flag-$8000+dload

        
        RTS
  
.Recovery_Defrag
        PHA
        PHA                                   \ adjust stack for service call
        SEC                                   \ Set C flag to show defrag called from restore
        JMP     Force_Defrag
      
        


        
        
        
        