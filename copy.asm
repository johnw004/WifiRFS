\ ROM Filing System for Electron Wifi Board
\ (C) 2021 John Wike
\
\ Routines to copy file from dfs
\
.Select_WRFS
        LDY     #myFSno          \ set Y to select WRFS
        
.Select_FS
        LDA     #$8F             \ select a filing system
        LDX     #$12
        JMP     osbyte
        
.Call_Osfile0
        LDA     #$00             \ call osfile 0, write file
        PHA
        BEQ     Call_Osfile2
        
.Call_Osfile                    \ A=5 to check file and attributes, A=$FF to load file
        PHA
        LDY     #$00
.Load_Copy_Block
        LDA     Copy_Block1,Y
        STA     heap+$70,Y
        INY
        CPY     #$0A
        BNE     Load_Copy_Block

.Call_Osfile2
        LDX     #$70
        LDY     #HI(heap)
        PLA
        JMP     osfile
        
.Copy_Block1
        EQUW    heap+$61
        EQUW    dload,$FFFF
        EQUD    0
        
.Do_COPY                                \ *WCOPY command
        JSR     Get_Filename_From_Line  \ get filename from command line
        BCS     Leave_Copy2             \ leave if error
        
        LDA     #$0D                    \ start off with cr to put after filename

        DEX                             \ dec X to point at 0 at end of filename
.Copy_Loop
        STA     heap+$60,X              \ copy filename ending in cr to heap+$61
        LDA     heap+$1F,X
        DEX
        BNE     Copy_Loop
        
        LDY     #$04
        JSR     Select_FS               \ select dfs
        
        LDA     #$00
        TAY
        JSR     osargs                  \ get current filing system, should be 4, dfs
        CMP     #$04
        BEQ     DFS_Present
        
        JSR     Print_String2           \ if not, print message and leave
        EQUS    $0D,"No DFS",$0D,$EA
        JMP     Leave_Claim
        
.DFS_Present
        LDA     #$05
        JSR     Call_Osfile            \ call osfile 5 to check file is present
        CMP     #$01                   \ if present A=1
        BEQ     Copy_Found
        
        JSR     Not_Found              \ if not found, print message.....
        LDY     #$03
.Copy_Dir_String
        LDA     Dir_String,Y           \ copy "*." to $100....
        STA     $100,Y
        DEY
        BPL     Copy_Dir_String
         
        LDX     #$00                   \ set X and Y to point to $100
        LDY     #$01
        JSR     oscli                  \ do oscli to do a *. of the dfs drive
                                       \ leave with dfs selected
.Leave_Copy2
        JMP     Leave_Claim 
        
.Dir_String
        EQUS    "*.",$0D

.Copy_Found
        LDA     #$84                   \ osbyte $84, get HIMEM
        JSR     osbyte
        STY     scratch                \ put msb of current HIMEM in scratch
        LDA     #$85                   \ osbyte $85, HIMEM in selected mode
        LDX     #$06
        JSR     osbyte
        STY     scratch+1              \ put msb of HIMEM in MODE 6 in scratch+1
        
        LDX     heap+$7B               \ msb of file length
        LDA     heap+$7A
        BNE     Dont_Dec_Msb           \ if lsb of file length is 0, dec msb
        DEX
.Dont_Dec_Msb
        TXA
        CLC
        ADC     #HI(dload)             \ add high byte of length to high byte of  download location
        CMP     scratch                \ if < current HIMEM...
        BCC     Copy_Size_Ok           \ ok to proceed
        CMP     scratch+1              \ if >= than HIMEM in mode 6.....
        BCS     Copy_Size_Error        \ do size error
        
        JSR     Print_String2          \ change to mode 6
        EQUS    22,6,$0D,$0D,$EA
        
.Copy_Size_Ok
        LDA     #$FF                   \ osfile $FF to load file from dfs
        JSR     Call_Osfile
        
        LDX     #$03                   \ setup the parameters in heap+$70 to save the file
.Copy_Save_Block
        LDA     Copy_Block1+2,X        \ A reads the start address of data from Copy_Block1
        LDY     heap+$7A,X             \ Y reads the length of the file from the load operation
        STA     heap+$7A,X             \ save the start address
        CPX     #$02
        BCS     Dont_Add_Length        \ if X >= 2 copy the 2 msb's of the start address
        TYA                            \ if X < 2 add the length byte to the start byte to get the end address
        ADC     heap+$7A,X
.Dont_Add_Length
        STA     heap+$7E,X             \ save the end address
        DEX                            \ note: we can get away with adding the bytes in the wrong order (msb then lsb)
        BPL     Copy_Save_Block        \ because the lsb of the start address is always 0 from Copy_Block1 so it 
        JSR     Select_WRFS            \ will not affect the msb
        JSR     Call_Osfile0           \ call osfile 0 to save the file in WRFS
        
.Leave_Copy
        JSR     Select_WRFS            \ ensure WRFS selected after copy size error
        JSR     osnewl
        JMP     Leave_Claim
        
.Copy_Size_Error
        JSR     Print_String2
        EQUS    $0D,"File too large",$0D,$EA
        JMP     Leave_Copy
        
        
        
        
        