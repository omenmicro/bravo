echo off
REM This batch file will build a 32k binary image file using the TASS assembler.
REM
REM The input file is called "sbc.asm" It calls all of the other
REM source files in the proper order.
REM
REM A list of labels is saved in file "sbc.lbl" 
REM A compiler listing is saved in file "sbc.lst"
REM The binary object file is named "sbc25.rom"
type build.bat
echo on

tass /c /lsbc.lbl sbc.asm sbc25.rom sbc25.lst

pause
REM Done.