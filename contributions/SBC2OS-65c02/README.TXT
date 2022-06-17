https://sbc.rictor.org/sbcos.html

Here are my SBC-2 Operating System Source Files.

They include:

sbc.asm   - The build file that includes all the other files in the proper order
            (you would assemble this file with TASS)

upload.asm - XMODEM/CRC & Intel-Hex File Upload support
acia.asm - 65c51 init and IO support for monitor
sbcos.asm - monitor & minassembler
reset.asm - reset and IRQ initialization

Compiled using TASS using the following command line:
TASS /c /lsbc.lbl sbc.asm sbc.bin sbcos.lst

The basic idea is to place the upload tables, page aligned, first @ $F700; then let 
all of the code follow sequentially until done.   

The compiler list output file from TASS is called:
sbc25.lst

The label file contains all the labels and their addresses and is called:
sbc.lbl

There is 1 compiled file included:
sbc25.rom  -  the raw binary object file


Current object code occupies $F700-$FFFF


To modify for another target platform, just update the ACIA1.ASM file to 
support these functions:

ACIA1_init - called from reset.asm - initialize the serial terminal device

ACIA1_Input - called from sbc2os.asm - wait for character from terminal, return in Acc

ACIA1_Scan - called from  sbc2os.asm - test for character from terminal - 
             no wait - return with Carry clear for no character, set for character
             last character will be in Acc if carry set - no need to call ACIA1_Input

ACIA1_Output - called from sbc2os.asm - send char in Acc to terminal


In sbc2os.asm, line 40-42, are the calls used from the OS to the terminal.

One note, in the upload.asm file, there are two direct writes to the ACIA1 data register (ACIA1dat).
These are located at lines 363 and 369.  This is in the IntelHex section that ROss Archer contributed.
I'm not sure if a "jsr output" can be substituted or not.

ACIA1dat is defined in the ACIA1.asm file.

This version generates the CRC tables used by xmodem and intel-hex downloaders.  it saves 512 bytes of 
ROM by placing the tables in RAM.  Lines 90 & 91 of upload.asm contain the pointers to the tables.  Put 
these in RAM where they will not be overwritten by the code being downloaded.  They should be page 
aligned for optimal use.

The SBC-2OS command syntax is as follows:

Syntax = {} required, [] optional, HHHH hex address, DD hex data

[HHHH][ HHHH]{Return} - Hex dump address(s)(up to 16 if no address entered)
[HHHH]{.HHHH}{Return} - Hex dump range of addresses (16 per line)
[HHHH]{:DD}[ DD]{Return} - Change data bytes
[HHHH]{G}{Return} - Execute a program (use RTS to return to monitor)
{HHHH.HHHH>HHHH{I}{Return} - move range at 2nd HHHH down to 1st to 3rd HHHH
{HHHH.HHHH>HHHH{M}{Return} - Move range at 1st HHHH thru 2nd to 3rd HHHH
[HHHH][ HHHH]{Q}{Return} - Text dump address(s)
[HHHH]{.HHHH}{Q}{Return} - Text dump range of addresses (16 per line)
{R}{Return} - Print register contents from memory locations - used after BRK if needed
{U}{Return} - Upload File (Xmodem/CRC or Intel Hex)
{V}{Return} - Monitor Version



Good luck!

Daryl

