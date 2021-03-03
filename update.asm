\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike

\ Update and Manual commands

.Update_String
        EQUS    "*WGET http://johnwike.co.uk/electron/wrfs ",$00
        
.Manual_String
        EQUS    "*WGET -X http://johnwike.co.uk/electron/wrfsman.txt",$0D,$00

.Do_MANUAL                                     \ *MANUAL command
        LDX     #Manual_String-Update_String
        JSR     Load_Wget                      \ load manual string into oscli buffer  
        JSR     Print_String2
        EQUB    12,14,$EA                      \ clear screen and set page scrolling
        
        JSR     Do_Wget                        \ print manual
        JMP     Leave_Claim
        
.Load_Wget
        LDY     #$00                           \ load string into oscli buffer
.Load_Wget_Loop
        LDA     Update_String,X                \ X points to string
        BEQ     Load_Wget_End
        STA     wgetbfr,Y
        INX
        INY
        BNE     Load_Wget_Loop
        
.Load_Wget_End
        RTS

.Do_UPDATE                                     \ *UPDATE command
        JSR     Load_Rwcode \ * Note to self: this may not be necessary

        LDX     #$00
        JSR     Load_Wget                      \ load update string into oscli buffer
        
        LDA     #HI(dload)                     \ append download address to oscli string
        JSR     savehex
        LDA     #LO(dload)
        JSR     savehex
        LDA     #$0D
        STA     wgetbfr,X                      \ put cr at end of oscli string

        JSR     Do_Wget                        \ download file    

        JMP     dload+$1000                    \ jump to rest of update routine in ram (update2)
        
.Do_Wget
        LDX     #LO(wgetbfr)
        LDY     #HI(wgetbfr)
        JMP     oscli
        
.savehex            pha                     \ save accu
                    lsr a                   \ shift high nibble to low
                    lsr a
                    lsr a
                    lsr a
                    jsr savehex_l1          \ print nibble
                    pla                     \ restore value
.savehex_l1         and #&0F                \ remove high nibble
                    cmp #&0A                \ test for hex digit
                    bcc savehex_l2          \ if not then continue
                    adc #6                  \ add 6 for hex letter
.savehex_l2         adc #&30                \ add &30 for ascii value
                    STA wgetbfr,X           \ save the digit and return
                    INX
                    rts
        
        
        
        
        
        