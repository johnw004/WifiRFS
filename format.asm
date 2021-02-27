\ Flash routine to reprogram the EEPROM.
\ (c) 2020 Roland Leurs
\ Version 1.0 14-08-2020
\
\ This code will be included in the EEPROM but is copied to the main memory before it is executed. You can copy it
\ by executing the WiFi driver function &FE where the Y register contains the high byte of the data to be "burned"
\ into the EEPROM and the X register contains the bank number within the EEPROM. Always the full 16kB will be "burned".
\
\ Bank 0:   &0000 - &3FFF       mfa = 1     bank = 0
\ Bank 1:   &4000 - &7FFF       mfa = 1     bank = 1
\ Bank 2:   &8000 - &BFFF       mfa = 0     bank = 0
\ Bank 3:   &C000 - &FFFF       mfa = 0     bank = 1
\ 
\ After the programming has finished the Electron will be hard reset because one of the ROMs will have changed and
\ the operating system needs to be reinitialized. Luckily, a reboot on the Electron does not take as much time as
\ rebooting a PC ;-)

\ modified to just erase file system portions of the EPROM, ie, $9000 to $BFFF of Bank 1 and $8000 to $BFFF of Banks 2 and 3

\ Workspace
include "electron.asm"
\mfatabl  = zp+8                \ 4 bytes
\banktabl = zp+12               \ 4 bytes

\ UART registers
uart_thr = uart+8
uart_dll = uart+8
uart_dlm = uart+9
uart_afr = uart+10
uart_lcr = uart+11
uart_mcr = uart+12

            org formatcode
.Format_Start            
            JMP     Format
            
.Jmp_To_Rwpage
            STA     pagereg    \ this will jump to rw page location 6

.mfatabl
            EQUB    8,8,0,0
            
.banktabl
            EQUB    0,1,0,1
            
.Format
            PHP
            sei                 \ no more interrupts from here
            

\ Blank the bank (bank number is in X register)
\ Enter with X=3 to format rfs
\ Enter with X=4 to erase program ready for update

            CPX     #$03
            BEQ     Blank_Loop
            LDX     #$03
            STX     save_x
            BNE     Erase_Prog
.Blank_Loop
            STX     save_x
            CPX     #$02
            BEQ     Dont_Erase_Main_Rom
            BCS     Dont_Erase_Program
            
.Erase_Prog
            jsr prepare_erase   \ prepare the erase operation
            sta &8000           \ erase first sector
            jsr wait_star            \ wait for completion
            ldx save_x
            LDA     #rwpage
            cpx #$03
            beq Jmp_To_Rwpage
            
.Dont_Erase_Program
            jsr prepare_erase   \ prepare the erase operation
            sta &9000           \ erase first sector
            jsr wait_star            \ wait for completion
            jsr prepare_erase   \ prepare the erase operation
            sta &A000           \ erase first sector
            jsr wait_star            \ wait for completion
            jsr prepare_erase   \ prepare the erase operation
            sta &B000           \ erase first sector
            jsr wait_star            \ wait for completion
            
.Dont_Erase_Main_Rom
            LDX     save_x
            DEX
            BPL     Blank_Loop
            jsr osnewl          \ print a new line 
            
            LDX     #$03
            JSR     setbank     \ return to main RFS program bank

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\            LDA     $F4         \ during testing
\            STA     $FE05       \ reset calling ROM
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
            PLP
            RTS


\ End of routine
.wait_star
            jsr wait            \ wait for completion
            lda #'.'            \ print a * as progress indicator
            jmp oswrch

\ Prepare the erase operation for a sector in bank X. After returning from this subroutine
\ immediatly write to the sector address.
.prepare_erase
            lda #&AA            \ initialize the Sector-Erase command sequence
            jsr write5555
            lda #&55
            jsr write2AAA
            lda #&80
            jsr write5555
            lda #&AA
            jsr write5555
            lda #&55
            jsr write2AAA
            ldx save_x          \ reload bank number in X
            jsr setbank         \ set MFA and bank for the bank number in X
            lda #&30            \ start the erase operation
            sta uart_thr
            rts                 \ ready with preparation

\ Wait until an operation has finished. I use the "toggle bit" method; this means that during the
\ erase or program operation bit 6 will be toggled at every read cycle.
.wait       lda &8000           \ load data
            eor $8000
            and #&40            \ clear all bits except bit 6
            bne wait        \ continue with next wait cycle if they are not equal
.waitend    rts                 \ return to calling routine

\ Select the active bank for writing a byte to. The bank number is in the X register
.setbank    pha                 \ save A (it contains the byte that should be written)
            lda mfatabl,x      \ load MFA value
            sta uart_mcr        \ write to UART (this sets A15 of the EEPROM)
            lda #&0F
            sta &FE05
            lda $F4
            and #$06
            ora banktabl,x     \ load sideway bank number
            sta &FE05           \ write to ULA
            pla                 \ restore A
            rts                 \ return to calling routine

\ Write a value to &5555 of the EEPROM (not the 6502 address!)
.write5555  ldx #1              \ &5555 is in bank 1
            jsr setbank         \ select bank 1
            sta &9555           \ write to the right address (this is the 6502 address in SWR space)
            ldx save_x          \ restore X register
            rts                 \ return to calling routine

\ Write a value to &2AAA of the EEPROM (not the 6502 address!)
.write2AAA  ldx #0              \ &2AAA is in bank 0
            jsr setbank         \ select bank 0
            sta &AAAA           \ write to the right address (this is the 6502 address in SWR space)
            ldx save_x          \ restore X register
            rts                 \ return to calling routine


.Format_end

SAVE "format.bin", Format_Start, Format_end
