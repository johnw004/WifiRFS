\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike
\

\ low level read and write routines for the EEPROM

\ Workspace
include "electron.asm"

\ Bank 0:   &0000 - &3FFF       mfa = 1     bank = 0  the first two banks are the "hidden" banks used by the filing system
\ Bank 1:   &4000 - &7FFF       mfa = 1     bank = 1
\ Bank 2:   &8000 - &BFFF       mfa = 0     bank = 0  this is the bank that the Electron Wifi ROM occupies
\ Bank 3:   &C000 - &FFFF       mfa = 0     bank = 1  the first sector $8000 to $9000 is program area, the rest is filing system

\ UART registers
uart_thr = uart+8
uart_dll = uart+8
uart_dlm = uart+9
uart_afr = uart+10
uart_lcr = uart+11
uart_mcr = uart+12


            org rwcode                          \ code will be copied to $FD00 and run from there
.rw            
            
                                                \ entry to write byte
            JMP     Write_Byte                  \note that this is assembled independently so these are local references
            
            JMP     Read_Byte                   \ entry to read byte

.rwflag     EQUB    0                           \ modified to 8 for new mfa during install


                                                \ entry point from format code during update
            LDA     $F4                         \ so this always points to Bank 3
            STA     banktable
            LDA     rwflag
            STA     mfatable
            
            LDA     #0
            STA     zrfsptr
            STA     rfssrc
            LDA     #$80
            STA     zrfsptr+1                   \ start location in EEPROM is $8000
            LDA     #HI(dload)
            STA     rfssrc+1                    \ source is the download area
            
.Update_Loop
            LDY     #$00
            LDA     (rfssrc),Y                  \ read byte from ram
            JSR     Write_Byte                  \ write byte to eeprom
            INC     rfssrc                      \ inc ram pointer
            BNE     Src_Ok
            INC     rfssrc+1
            LDA     #'.'
            JSR     osasci                      \ print "."to indicate progress
            
.Src_Ok
            INC     zrfsptr
            BNE     Ptr_Ok
            INC     zrfsptr+1                   \ inc eeprom pointer
            
.Ptr_Ok
            LDA     zrfsptr+1
            CMP     #$90
            BNE     Update_Loop                 \ loop until $9000 reached
            JSR     osnewl
            
            JMP     dload+$1003                  \ return to update code in update2.asm
            
.Read_Byte                 \ enter with EPROM address in zrfsptr, mfa in mfatable and bank in banktable
            PHP
            SEI                                  \ set interrupt flag
            LDY     mfatable
            STY     uart_mcr                     \ set bit 15 of eeprom
            LDY     banktable
            LDA     #$0F
            STA     $FE05
            STY     $FE05                        \ set bit 14 of eeprom
            LDY     #$00
            LDA     (zrfsptr),Y                  \ read byte from eeprom
            
            LDY     rwflag
            STY     uart_mcr                     \ reset Bank 3 to return to program
            LDY     $F4
            STY     $FE05
            PLP                                  \ clear interrupt flag
            RTS
             
\ entry to write byte, enter with EPROM address in zrfsptr, mfa in mfatable and bank in banktable
.Write_Byte      
                                
            PHP
            SEI                                   \ set interrupt flag
            PHA                 \ save A
            
            LDA     #$08        \ set Bank 1
            STA     uart_mcr
            
            LDA     #$0F        
            STA     $FE05
            
            LDA     $F4
            ORA     #$01        \ note to self: this line may not be necessary
            STA     $FE05
            LDY     #$AA        
            STY     $9555       \ write $AA to $9555 ($5555 in bank 1)
            
            AND     #$06        \ mask lsb of slot number
            STA     $FE05
            LDY     #$55
            STY     $AAAA       \ write $55 to $AAAA ($2AAA in bank 0)
            STY     uart_thr
            
            ORA     #$01        \ set lsb of slot number
            STA     $FE05
            LDY     #$A0
            STY     $9555       \ write $A0 to $9555 ($5555 in bank 1)
            
            LDY     mfatable
            STY     uart_mcr    \ set bit 15 of eeprom
            LDY     banktable   \ set bit 14 of eeprom
            STY     $FE05
            LDY     #$00
            PLA                  \ restore A
            STA     (zrfsptr),Y  \ write to EEPROM
            
            LDY     rwflag
            STY     uart_mcr     \ reset Bank 3 to return to program
            LDY     $F4
            STY     $FE05
            
.Wait
            LDA     $8000        \ read byte from eeprom
            EOR     $8000        \ compare with previous read
            AND     #$40
            BNE     Wait         \ wait until bit 6 is the same
            PLP                  \ clear interrupt flag
            RTS
                 
            
            


.rw_end

SAVE "rw.bin", rw, rw_end
