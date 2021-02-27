\ Electron Wifi Sideway ROM
\ Settings, definitions and constants

\ (C)Roland Leurs 2020
\ Version 1.00 May 2020

\ Modified by John Wike, February 2021, for Wifi RFS system

            __ELECTRON__ = 1
            __ATOM__ = 0

			uart = &FC30            \ Base address for the 16C2552 UART B-port

            pagereg = &FCFF
            pageram = &FD00
			timer = &140            \ Count down timer, 3 bytes
			time_out = timer + 4    \ Time-out setting, 1 byte

            errorspace = &100       \ Some volatile memory area for generating error messages
\            heap      = &900        \ Some volatile memory area for tempory storage          uses paged RAM in WRFS
			strbuf    = &A00        \ Some volatile memory area for string buffer
\           flashcode = &900        \ may overlap with heap and string buffer                 uses paged RAM in WRFS

            osrdch = &FFE0
            oswrch = &FFEE
            osasci = &FFE3
            osword = $FFF1
            osbyte = &FFF4
            osnewl = &FFE7
            oscli  = &FFF7
            switch = &FE05
            shadow = &F4

            uptvec = &222           \ User Print Vector
            netprt = &D90           \ Network printer name or ip (32 char)
            uptype = &DB0           \ User Printer Type
            uptsav = &DB1           \ Save old uptvec

            line = &F2              \ address for command line pointer
            zp = &B0                \ workspace            

			save_a = zp+2           \ only used in driver, outside driver is may be used for "local" work
			save_y = zp+3           \ only used in driver, outside driver is may be used for "local" work
			save_x = zp+4           \ only used in driver, outside driver is may be used for "local" work
            pr24pad = zp+5
			paramblok = save_y

			data_counter = zp+6
            blocksize = zp+6
            load_addr = zp+9

            baudrate = zp+6         \ must be the same as blocksize because of MUL10 routine
            parity   = zp+9
            databits = zp+10
            stopbits = zp+11

			buffer_ptr = zp+9       \ buffer_ptr and data_pointer must be adjescent!
			data_pointer = zp+11    \ a.k.a. data length
            size = zp+11            \ indeed, same as data_pointer
            needle = zp+12          \ may overlap with data_pointer, 2 bytes
            datalen = zp+13         \ data length counter, 2 bytes
            crc = zp+15             \ calculated crc, 2 bytes
            servercrc = zp+17       \ received crc, 2 bytes
						
			mux_status  = &90
			mux_channel = &91     \ need 5 bytes!

\ extras for rfs
        wrzp        = zp+5      \ 2 bytes  pointer
        sdata       = zp+7      \ 2 bytes  start address of data
        ldata       = zp+9      \ 2 bytes  length of data
        hdrlen      = zp+11     \ 1 byte   length of header
        nblocks     = zp+12     \ 1 byte   number of blocks
        scratch     = zp+13     \ 2 bytes  scratch area for calcs
        
        crc16       = zp+15     \ 2 bytes  crc accumulator
        crcsave     = zp+17     \ 1 byte   temp store during crc
        wrnum       = zp+18     \ 1 byte   number of bytes to write
        zrfsptr     = $A2       \ zp+19     \ 2 bytes  modified copy of rfsptr
        count14     = $A4       \ counter during service 14
        tubeflag    = $A6       \ $00 if no tube
        
        rfssrc      = zp+19     \ 2 bytes  data pointer in memory for writing to RFS
        rfsdest     = zp+21     \ 2 bytes  data pointer in RFS for writing
        
        
        serROM      = $F5       \ current active RFS ROM
        ROMptr      = $F6       \ OS pointer into RFS
        osfvec      = $212                 \ filing system vectors and low bytes of extended vectors
        ext_osfile  = (LO(osfvec)/2)*3
        argsvec     = $214
        ext_args    = (LO(argsvec)/2)*3
        osfscvec    = $21E
        ext_osfsc   = (LO(osfscvec)/2)*3
        extvectab   = $D9F                 \ base for storage of extended vectors
        vectabaddr  = $FFB7                \ pointer to default vector table in OS
        
        osfile      = $FFDD
        osargs      = $FFDA
        osfind      = $FFCE
        
        tubeID      = $F6                  \ ID to use to claim/release tube
        myFSno      = $33                  \ filing system handle to initialise WRFS
        
        heap        = pageram              \ these three use paged ram
        formatcode  = pageram
        rwcode      = pageram
        
        heappage    = $FE                  \ paged ram pages to use
        formatpage  = $FD
        rwpage      = $FF
        rampage     = $FC                  \ last page of ram copy of rfs during defrag
        roffset     = $FF-rampage          \ offset to apply to high byte of rfs address to get ram copy page

\ variables held in heap page
        rstart      = heap+$FE             \ 2 bytes  pointer to start of RFS
        rfsptr      = rstart-2             \ &FC 2 bytes  moving pointer into RFS during writes   
        rcheck      = rfsptr-2             \ &FA 2 bytes  check value for rstart pointer 
                                           \ empty byte
        delsp       = rcheck-3             \ &F7 2 bytes  amount of deleted space
        tempptr     = delsp-2              \ &F5 2 bytes
        
        \ pointers for use during defrag
        ramptr      = tempptr-2            \ &F3 2 bytes  moving pointer in ram
        ramend      = ramptr-2             \ &F1 2 bytes  end of current file in ram + 1
        ramstart    = ramend-2             \ &EF 2 bytes  start of current file in ram
        ramhdr      = ramstart-2           \ &ED 2 bytes  start of second header in ram
        delflag     = ramhdr-1             \ &EC 1 byte   delete flag                     copied to/from end of file in rfs
        dlength     = delflag-2            \ &EA 2 bytes  bigendian data length of file   copied to/from end of file in rfs
        fstart      = dlength-2            \ &E8 2 bytes  bigendian start of file in rfs  copied to/from end of file in rfs
        fend        = fstart-2             \ &E6 2 bytes  end of file in rfs
        hlength     = fend-1               \ &E5 1 byte   length of header

\ variables held in rw code page
        mfatable    = rwcode+$FF \ 1 byte  mfa value for rfs access, 8 for banks 0 and 1, 0 for banks 2 (wifi) and 3 (wrfs)
        banktable   = mfatable-1 \ 1 byte  bank value for rfs access, bit 0 of sideways rom latch in $F4
        
        dload       = $2000      \ download location for update
        wgetbfr     = $100
        
        etagram     = dload+$FF8 \ crc of etag of downloaded file
        etagrom     = $8FF8      \ crc of previous etag
                

        ORG &8000
        Data        = $9000     \ start of first page of RFS ($5000 in RFS memory map)


