; ----------------- assembly instructions ---------------------------- 
;
; this is a subroutine library only
; it must be included in an executable source file
;
;
;*** I/O Locations *******************************
; define the i/o address of the ACIA1 chip
;*** 6551 CIA ************************
ACIA1dat       =     $83fc
ACIA1sta       =     $83fd
ACIA1cmd       =     $83fe
ACIA1ctl       =     $83ff
;
;***********************************************************************
; 6551 I/O Support Routines
;
ACIA1_init
ACIA1portset   lda   #$1F               ; 19.2K/8/1
               sta   ACIA1ctl           ; control reg 
               lda   #$0B               ; N parity/echo off/rx int off/ dtr active low
               sta   ACIA1cmd           ; command reg 
               rts                      ; done
;
; input chr from ACIA1 (waiting)
;

ACIA1_Input
               lda   ACIA1Sta           ; Serial port status             
               and   #$08               ; is recvr full
               beq   ACIA1_Input        ; no char to get
               Lda   ACIA1dat           ; get chr
               RTS                      ;
;
; non-waiting get character routine 
;

ACIA1_Scan     clc
               lda   ACIA1Sta           ; Serial port status
               and   #$08               ; mask rcvr full bit
               beq   ACIA1_scan2
               Lda   ACIA1dat           ; get chr
	         sec
ACIA1_scan2    rts
;
; output to OutPut Port
;

ACIA1_Output   PHA                      ; save registers
ACIA1_Out1     lda   ACIA1Sta           ; serial port status
               and   #$10               ; is tx buffer empty
               beq   ACIA1_Out1         ; no
               PLA                      ; get chr
               sta   ACIA1dat           ; put character to Port
               RTS                      ; done
;
;end of file
