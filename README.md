# WifiRFS
Extension to Roland's Elk WIFI board to provide a 44K writeable ROM Filing System
WiFi RFS.

---------------------------------------
A Rom Filing System (RFS) extension to
Roland Leurs' Electron WiFi board.
---------------------------------------

This is an expansion of the built-in
Electron RFS to provide the following
extra commands:

SAVE, WCOPY, DELETE, INFO, FREE
FORMAT, DEFRAG, UPDATE, MANUAL

In addition, the RFS vectors are 
expanded to give more facilities,
including the BASIC keywords EXT# and
PTR#.

-----------------
Hardware Overview
-----------------
The system uses the three unused
sideways ROM slots (SWR) on the Wifi
board (2 of which are normally hidden)
to give a 4K program ROM and 44K of RFS
space.

Three pages of the paged RAM on the 
board are used (&FD-&FF), one as heap 
and two to hold the format and 
read/write code whilst the ROM 
is switched out.

Additionally, up to 176 pages 
(&FC down to &4D as necessary) are 
used as a scratch area during DEFRAG.

Using the WCOPY and UPDATE commands 
(see later) will result in main memory
from &2000 upwards being overwritten.

-------------------
* Commands overview
-------------------

*WROM
-----
Select the Wifi RFS (WRFS).
This will select the Electron RFS
and then modify some of the 
Filing System vectors.

*WCOPY
------
Copy the specified file from the 
current disc in the DFS to WRFS. 
If the file cannot be found, the *. 
command will be run and the DFS will be
left active to allow the disc to be
changed. When ready *WCOPY can be
re-run without re-selecting *WROM.

*DELETE
-------
Deleting a file will not remove it but
will mark it deleted. There will thus
be a build-up of dead space.

When a new copy of a file is SAVE'd
the old copy will be deleted and
increase the dead space.

*DEFRAG
-------
Indicates how much dead space there is
and gives the option of removing it.

*UPDATE
-------
Download the latest copy of the ROM.
If it is a new version it will be
automatically copied to the ROM.
Otherwise the option will be given.

This command will prevent the UPDATE 
command in the Electron Wifi ROM
working. To allow it, select another
Filing System first.

*MANUAL
-------
Displays this manual!


Other commands are similar to other 
filing systems.

---------
Auto Boot
---------
If WRFS is not the default FS at boot, 
ie. there is one in a higher ROM slot,
it can be run by holding the W key and
BREAK.

--------------------------
Filing System (FS) Vectors
--------------------------
This is not the place for a description
of the Electron's FS vectors. For that
see the "Advanced User Guide for the
Acorn Electron".
This is just an overview of the 
additions to the RFS vectors in WRFS.

OSFILE
     0 - Save a file
     5 - Read the catalogue entry of a
         file
     6 - Delete a file (but see the
           entry for *DELETE above)

OSARGS with file handle 3
     0 - Read sequential file pointer
            (PTR# in BASIC)
     2 - Read length of sequential file
            (EXT# in BASIC)

OSFSC
     3 - Unrecognised * command.
            Run the command if it is 
            a file on the WRFS
     5 - This runs a *. or *CAT command 
            but it formats the output 
            differently to RFS.

OSBYTE
     with A = &8F, X = &12, Y = &33
     - initialise WRFS filing system.
     Although the filing system number
     will still be 3 like the RFS.
     
-----------
Boot Option
-----------
There is only one Boot option - EXEC.

When a *UPDATE is used, if there isn't
already a !BOOT file on WRFS, it will
create a default one with the contents

CH.START

You can then create a file called 
START to run BASIC programs or
command loaders.

---------------------
When things go South!
---------------------
- or for a new install.

If necessary the ROM can be 
installed/recovered by WGETing the
bootloader:

WGET 
http://johnwike.co.uk/electron/wrfs 2000

then CALL &2000

If it is a fresh install over an 
existing different ROM, a *FORMAT will 
be required.


                John Wike, February 2021
