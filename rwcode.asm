
\ low level read and write routines for the EEPROM

\ Workspace
include "electron.asm"

\ UART registers
uart_thr = uart+8
uart_dll = uart+8
uart_dlm = uart+9
uart_afr = uart+10
uart_lcr = uart+11
uart_mcr = uart+12


            org rwcode
.rw            
            
\ entry to write byte
            JMP     Write_Byte
            
\ entry to read byte
            JMP     Read_Byte
            
\ entry point from format code during update
            LDA     $F4
            STA     banktable
            LDA     #$00
            STA     mfatable
            STA     zrfsptr
            STA     rfssrc
            LDA     #$80
            STA     zrfsptr+1
            LDA     #HI(dload)
            STA     rfssrc+1
            
.Update_Loop
            LDY     #$00
            LDA     (rfssrc),Y
            JSR     Write_Byte
            INC     rfssrc
            BNE     Src_Ok
            INC     rfssrc+1
            LDA     #'.'
            JSR     osasci
            
.Src_Ok
            INC     zrfsptr
            BNE     Ptr_Ok
            INC     zrfsptr+1
            
.Ptr_Ok
            LDA     zrfsptr+1
            CMP     #$90
            BNE     Update_Loop
            JSR     osnewl
            
            JMP     dload+$1003                  \ return to update code in ram
            
.Read_Byte                 \ enter with EPROM address in zrfsptr, mfa in mfatable and bank in banktable
            PHP
            SEI
            LDY     mfatable
            STY     uart_mcr
            LDY     banktable
            LDA     #$0F
            STA     $FE05
            STY     $FE05
            LDY     #$00
            LDA     (zrfsptr),Y
            
            STY     uart_mcr       \ reset mfa and bank to return to ROM
            LDY     $F4
            STY     $FE05
            PLP
            RTS
             
\ entry to write byte, enter with EPROM address in zrfsptr, mfa in mfatable and bank in banktable
.Write_Byte      
                                \ for banks 0 and 1 mfa = 8
            PHP
            SEI
            PHA
            
            LDA     #$08 
            STA     uart_mcr
            
            LDA     #$0F        \  bank = 1
            STA     $FE05
            
            LDA     $F4
            ORA     #$01
            STA     $FE05
            LDY     #$AA        
            STY     $9555       \ write $AA to $9555 ($5555 in bank 1)
            
            AND     #$06
            STA     $FE05
            LDY     #$55
            STY     $AAAA       \ write $55 to $AAAA ($2AAA in bank 0)
            STY     uart_thr
            
            ORA     #$01
            STA     $FE05
            LDY     #$A0
            STY     $9555       \ write $A0 to $9555 ($5555 in bank 1)
            
            LDY     mfatable
            STY     uart_mcr
            LDY     banktable
            STY     $FE05
            LDY     #$00
            PLA
            STA     (zrfsptr),Y
            
            STY     uart_mcr     \ reset mfa and bank to calling ROM
            LDY     $F4
            STY     $FE05
            
.Wait
            LDA     $8000
            EOR     $8000
            AND     #$40
            BNE     Wait
            PLP
            RTS
                 
            
            


.rw_end

SAVE "rw.bin", rw, rw_end
