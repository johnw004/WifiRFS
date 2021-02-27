
.Update_String
        EQUS    "*WGET http://johnwike.co.uk/electron/wrfs ",$00
        
.Manual_String
        EQUS    "*WGET -X http://johnwike.co.uk/electron/wrfsman.txt",$0D,$00

.Do_MANUAL
        LDX     #Manual_String-Update_String
        JSR     Load_Wget
        JSR     Print_String2
        EQUB    12,14,$EA
        
        JSR     Do_Wget
        JMP     Leave_Claim
        
.Load_Wget
        LDY     #$00
.Load_Wget_Loop
        LDA     Update_String,X
        BEQ     Load_Wget_End
        STA     wgetbfr,Y
        INX
        INY
        BNE     Load_Wget_Loop
        
.Load_Wget_End
        RTS

.Do_UPDATE
        JSR     Load_Rwcode

        LDX     #$00
        JSR     Load_Wget
        
        LDA     #HI(dload)
        JSR     savehex
        LDA     #LO(dload)
        JSR     savehex
        LDA     #$0D
        STA     wgetbfr,X

        JSR     Do_Wget        

        JMP     dload+$1000
        
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
        
        
        
        
        
        