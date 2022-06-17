;---------------------------------------------------------------------
;  SBC Firmware V5.1, 5-30-05, by Daryl Rictor
;
; ----------------- assembly instructions ---------------------------- 
;               *=   $E800                ; start of operating system
Start_OS       jmp   MonitorBoot         ; easy access to monitor program
;
;
;*********************************************************************       
;  local Zero-page variables
;
Prompt         =     $32               ; 1 byte   
linecnt        =     $33               ; 1 byte
Modejmp        =     $34               ; 1 byte
Hexdigcnt      =     $35               ; 1 byte
OPCtxtptr      =     $36               ; 1 byte
Memchr         =     $37               ; 1 byte
Startaddr      =     $38               ; 2 bytes
Startaddr_H    =     $39
Addrptr        =     $3a               ; 2 bytes
Addrptr_H       =    $3b
Hexdigits      =     $3c               ; 2 bytes
Hexdigits_H    =     $3d
Memptr         =     $3e               ; 2 bytes
Memptr_H       =     $3f
;
; Local Non-Zero Page Variables
;
buffer         =     $0300             ; keybd input buffer (127 chrs max)
PCH            =     $03e0             ; hold program counter (need PCH next to PCL for Printreg routine)
PCL            =     $03e1             ;  ""
ACC            =     $03e2             ; hold Accumulator (A)
XREG           =     $03e3             ; hold X register
YREG           =     $03e4             ; hold Y register
SPTR           =     $03e5             ; hold stack pointer
PREG           =     $03e6             ; hold status register (P)
;
; Monitor Input/Output to system IO device
;
output         =     ACIA1_Output
Input_Chr      =     ACIA1_Input
Scan_Input     =     ACIA1_Scan

;               
; *************************************************************************
; kernal commands
; *************************************************************************
; PrintRegCR   - subroutine prints a CR, the register contents, CR, then returns
; PrintReg     - same as PrintRegCR without leading CR
; Print2Byte   - prints AAXX hex digits
; Print1Byte   - prints AA hex digits
; PrintDig     - prints A hex nibble (low 4 bits)
; Print_CR     - prints a CR (ASCII 13)and LF (ASCII 10)
; PrintXSP     - prints # of spaces in X Reg
; Print2SP     - prints 2 spaces
; Print1SP     - prints 1 space
; Input_assem  - Alternate input prompt for Assember
; Input        - print <CR> and prompt then get a line of input, store at buffer
; Input_Chr    - get one byte from input port, waits for input
; Scan_Input   - Checks for an input character (no waiting)
; Output       - send one byte to the output port
; Bell         - send ctrl-g (bell) to output port
; Delay        - delay loop
; *************************************************************************
;
RegData        .byte" PC=  A=  X=  Y=  S=  P= (NVRBDIZC)="
;
PrintReg       Jsr   Print_CR          ; Lead with a CR
               ldx   #$ff              ;
               ldy   #$ff              ;
Printreg1      iny                     ;
               lda   Regdata,y         ;
               jsr   Output            ;
               cmp   #$3D              ; "="
               bne   Printreg1         ;
Printreg2      inx                     ;
               cpx   #$07              ;
               beq   Printreg3         ; done with first 6
               lda   PCH,x             ;  
               jsr   Print1Byte        ;
               cpx   #$00              ;
               bne   Printreg1         ;
               bra   Printreg2         ;
Printreg3      dex                     ;
               lda   PCH,x             ; get Preg
               ldx   #$08              ; 
Printreg4      rol                     ;
               tay                     ;
               lda   #$31              ;
               bcs   Printreg5         ;
               dec                     ;
Printreg5      jsr   Output            ;
               tya                     ;
               dex                     ;
               bne   Printreg4         ;
; fall into the print CR routine
Print_CR       PHA                     ; Save Acc
               LDA   #$0D              ; "cr"
               JSR   OUTPUT            ; send it
               LDA   #$0A              ; "lf"
               JSR   OUTPUT            ; send it
               PLA                     ; Restore Acc
               RTS                     ; 

Print2Byte     JSR   Print1Byte        ;  prints AAXX hex digits
               TXA                     ;
Print1Byte     PHA                     ;  prints AA hex digits
               LSR                     ;  MOVE UPPER NIBBLE TO LOWER
               LSR                     ;
               LSR                     ;
               LSR                     ;
               JSR   PrintDig          ;
               PLA                     ;
PrintDig       PHY                     ;  prints A hex nibble (low 4 bits)
               AND   #$0F              ;
               TAY                     ;
               LDA   Hexdigdata,Y      ;
               PLY                     ;
               jmp   output            ;
PrintXSP1      JSR   Print1SP          ;
               dex                     ;
PrintXSP       cpx   #$00              ;
               bne   PrintXSP1         ;
               rts                     ;
Print2SP       jsr   Print1SP          ; print 2 SPACES
Print1SP       LDA   #$20              ; print 1 SPACE
               JMP   OUTPUT            ;
;
Input_Assem    lda   #$21              ; Assembler Prompt "!"
               .byte $2c               ; mask out next line to bypass 
Input          lda   #$3E              ; Monitor Prompt ">"
               sta   Prompt            ; save prompt chr 
Input1         jsr   Print_CR          ; New Line
               lda   Prompt            ; get prompt
               jsr   Output            ; Print Prompt
               ldy   #$ff              ; pointer
InputWait      jsr   Input_Chr         ; get a character
               cmp   #$20              ; is ctrl char?
               BCS   InputSave         ; no, echo chr 
               cmp   #$0d              ; cr
               Beq   InputDone         ; done
               cmp   #$1B              ; esc
               beq   Input1            ; cancel and new line
               cmp   #$08              ; bs
               beq   backspace         ;
		   cmp   #$09		   ; TAB key
		   beq   tabkey		   ;
               cmp   #$02              ; Ctrl-B
               bne   InputWait         ; Ignore other codes
               brk                     ; Force a keyboard Break cmd
backspace      cpy   #$ff              ;
               beq   InputWait         ; nothing to do
               dey                     ; remove last char
               Lda   #$08              ; backup one space
               jsr   Output            ;
               Lda   #$20              ; Print space (destructive BS)
               jsr   Output            ;
               Lda   #$08              ; backup one space
               jsr   Output            ;
               BRA   InputWait         ; ready for next key
tabkey	   lda   #$20		   ; convert tab to space
		   iny			   ; move cursor
               bmi   InputTooLong	   ; line too long?
               sta   Buffer,y		   ; no, save space in buffer
		   jsr   output		   ; print the space too
               tya   			   ; test to see if tab is on multiple of 8
		   and   #$07		   ; mask remainder of cursor/8
               bne   tabkey		   ; not done, add another space
		   bra   InputWait	   ; done. 
InputSave      CMP   #$61              ;   ucase
               BCC   InputSave1        ;
               SBC   #$20              ;
InputSave1     INY                     ;
               BMI   InputTooLong      ; get next char (up to 127)
               STA   Buffer,y          ;
               JSR   Output            ; OutputCharacter
               BRA   InputWait         ;
InputDone      INY                     ;
InputTooLong   LDA   #$0d              ; force CR at end of 128 characters 
               sta   Buffer,y          ;
               JSR   Output            ;
;               lda   #$0a              ; lf Char   
;               JSR   Output            ;
               RTS                     ;
;
bell           LDA  #$07               ; Ctrl G Bell
               jmp  Output             ; 
;
Delay          PHA                     ; use A to execute a delay loop
delay1         DEC                     ;
               BNE   delay1            ;
               PLA                     ;
               DEC                     ;
               BNE   Delay             ;
GRTS           RTS                     ;
;
;
BRKroutine     sta   ACC               ; save A    Monitor"s break handler
               stx   Xreg              ; save X
               sty   Yreg              ; save Y
               pla                     ; 
               sta   Preg              ; save P
               pla                     ; PCL
               plx                     ; PCH
               sec                     ;
               sbc   #$02              ;
               sta   PCL               ; backup to BRK cmd
               bcs   Brk2              ;
               dex                     ;
Brk2           stx   PCH               ; save PC
               TSX                     ; get stack pointer
               stx   SPtr              ; save stack pointer
               jsr   Bell              ; Beep speaker
               jsr   PrintReg          ; dump register contents 
               ldx   #$FF              ; 
               txs                     ; clear stack
               cli                     ; enable interrupts again
               jmp   Monitor           ; start the monitor

;*************************************************************************
;     
;  Monitor Program 
;
;**************************************************************************
MonitorBoot    
               jsr   bell              ; beep ready
               JSR   Version           ;
SYSjmp                                 ; Added for EhBASIC
Monitor        LDX   #$FF              ; 
               TXS			   ;  Init the stack
               JSR   input             ;  line input
               LDA   #$00              ;
               TAY                     ;  set to 1st character in line
               sta   LineCnt           ; normal list vs range list 
Mon01          STA   Memchr            ;
Mon02          STZ   Hexdigits         ;  holds parsed hex
               STZ   Hexdigits+1       ;
               JSR   ParseHexDig       ;  Get any Hex chars
               LDX   #CmdCount         ;  get # of cmds currently used
Mon08          CMP   CmdAscii,X        ;  is non hex cmd chr?
               BEQ   Mon09             ;  yes x= cmd number
               DEX                     ;
               BPL   Mon08             ;
               BRA   Monitor           ;  no
Mon09          PHX                     ;  save command
               PHY                     ;  Save input line pointer
               TXA                     ;
               ASL                     ;  ptr * 2
               TAX                     ;  
               JSR   Mon10             ;  Execute cmd
               PLY                     ;
               PLX                     ;
               BEQ   Monitor           ;  done
               LDA   Cmdseccode,X      ;  
               BMI   Mon02             ;
               BRA   Mon01             ;
Mon10          JMP   (Cmdjmptbl,X)     ;
;--------------- Routines used by the Monitor commands ----------------------
ParseHexDig    STZ   Hexdigcnt         ;  cntr
               BRA   ParseHex05        ;
ParseHex03     TXA                     ;  parse hex dig
               LDX   #$04              ;  
ParseHex04     ASL   Hexdigits         ;
               ROL   Hexdigits+1       ;
               DEX                     ;
               BNE   ParseHex04        ;
               TSB   Hexdigits         ;
               DEC   Hexdigcnt         ;
ParseHex05     LDA   buffer,Y          ;
               LDX   #$0F              ;   is hex chr?
               INY                     ;
ParseHex07     CMP   Hexdigdata,X      ;
               BEQ   ParseHex03        ;   yes
               DEX                     ;
               BPL   ParseHex07        ;
               RTS                     ; Stored in HexDigits if HexDigCnt <> 0
;
;Help_cmd       lda   #<Helptxt         ;  lower byte - Menu of Commands
;               sta   addrptr           ;
;               lda   #>Helptxt         ;  upper byte
;               sta   addrptr+1         ;
;               bra   Help_cmd3         ;
;Help_Cmd4      cmp   #$7e              ;  "~"
;               beq   Help_Cmd1         ;
;               jsr   Output            ;
;               bra   Help_cmd2         ;
;Help_cmd1      jsr   Print_CR          ;     
;Help_cmd2      jsr   Inc_addrptr       ;
;Help_cmd3      lda   (addrptr)         ;
;               bne   Help_cmd4         ;
;               rts                     ;
Version        jsr   Print_CR          ; 
               ldx   #$FF              ; set txt pointer
               lda   #$0d              ; 
PortReadyMsg   inx                     ;
               JSR   Output            ; put character to Port
               lda   porttxt,x         ; get message text
               bne   PortReadyMsg      ; 
               rts                     ;
;
Excute_cmd     jsr   exe1              ;
               ldx   #$FF              ; reset stack
               txs                     ;
               jmp   Monitor           ;
exe1           JMP   (Hexdigits)       ;
;
DOT_cmd        LDX   Hexdigits         ; move address to addrptr
               LDA   Hexdigits+1       ;
               STX   Addrptr           ;
               STA   Addrptr+1         ;
               inc   LineCnt           ; range list command
               RTS                     ;
;
CR_cmd         CPY   #$01              ;
               BNE   SP_cmd            ;
               LDA   Addrptr           ; CR alone - move addrptr to hexdigits
               ORA   #$0F              ;  to simulate entering an address
               STA   Hexdigits         ; *** change 07 to 0f for 16 byte/line
               LDA   Addrptr+1         ;
               STA   Hexdigits+1       ;
               BRA   SP_cmd2           ;
SP_cmd         LDA   Hexdigcnt         ; Space command entry
               BEQ   SP_cmd5           ; any digits to process? no - done
               LDX   Memchr            ; yes - is sec cmd code 0 ? yes - 
               BEQ   SP_cmd1           ; yes - 
               DEX                     ; Is sec cmd = 1?       
               BEQ   SP_cmd3           ;       yes - is sec cmd code 1 ?
               LDA   Hexdigits         ;             no - ":" cmd processed
               STA   (Addrptr)         ;
               JMP   Inc_addrptr       ; set to next address and return
SP_cmd1        JSR   DOT_cmd           ; sec dig = 0  move address to addrptr
               BRA   SP_cmd3           ;
SP_cmd2        LDA   Addrptr           ; CR cmd entry 
               BIT   #$0F              ; *** changed 07 to 0F for 16 bytes/line
               BEQ   SP_cmd3           ; if 16, print new line
               cpy   #$00              ; if TXT cmd, don"t print the - or spaces between chrs
               beq   TXT_cmd1          ;
               BIT   #$07              ; if 8, print -
               BEQ   SP_cmd33          ;
               BRA   SP_cmd4           ; else print next byte
SP_cmd3        JSR   Print_CR          ; "." cmd - display address and data 
               jsr   Scan_Input        ; see if brk requested
               bcs   SP_brk            ; if so, stop 
               LDA   Addrptr+1         ; print address
               LDX   Addrptr           ;
               JSR   Print2Byte        ;
SP_cmd33       LDA   #$20              ; " " print 1 - 16 bytes of data
               JSR   OUTPUT            ;
               LDA   #$2D              ; "-"
               JSR   OUTPUT            ;
SP_cmd4        LDA   #$20              ; " " 
               JSR   OUTPUT            ;
               cpy   #$00              ;
               beq   TXT_Cmd1          ;
               LDA   (Addrptr)         ;
               JSR   Print1Byte        ;
SP_cmd44       SEC                     ;  checks if range done
               LDA   Addrptr           ;
               SBC   Hexdigits         ;
               LDA   Addrptr+1         ;
               SBC   Hexdigits+1       ;
               jsr   Inc_addrptr       ;
               BCC   SP_cmd2           ; loop until range done
SP_brk         STZ   Memchr            ; reset sec cmd code
SP_cmd5        RTS                     ; done or no digits to process
;
TXT_Cmd        PHY                     ;
               ldy   #$00              ;
               jsr   SP_cmd            ;
               PLY                     ;
               RTS                     ;
TXT_cmd1       LDA   (Addrptr)         ;
               AND   #$7F              ;
               CMP   #$7F              ;
               BEQ   TXT_Cmd2          ;
               CMP   #$20              ; " "
               BCS   TXT_Cmd3          ;
TXT_Cmd2       LDA   #$2E              ; "." use "." if not printable char
TXT_Cmd3       JSR   OUTPUT            ;
               BRA   SP_cmd44          ;
;
Inc_addrptr    INC   Addrptr           ;  increments addrptr
               BNE   Inc_addr1         ;
               INC   Addrptr+1         ;
Inc_addr1      RTS                     ;
;
Insert_cmd     lda   Linecnt           ;  "I" cmd code
               beq   Insert_3          ; abort if no . cmd entered
               sec                     ;
               lda   Hexdigits         ;
               sbc   addrptr           ;
               tax                     ;
               lda   Hexdigits+1       ;
               sbc   addrptr+1         ;
               tay                     ;
               bcc   Insert_3          ;
               clc                     ;
               txa                     ;
               adc   memptr            ;
               sta   hexdigits         ;
               tya                     ;
               adc   memptr+1          ;
               sta   hexdigits+1       ;
Insert_0       LDA   (memptr)          ;
               STA   (Hexdigits)       ;
               lda   #$FF              ;
               DEC   Hexdigits         ;  
               cmp   Hexdigits         ;  
               BNE   Insert_1          ;
               DEC   Hexdigits+1       ;
Insert_1       dec   Memptr            ;  
               cmp   Memptr            ;
               bne   Insert_2          ;
               dec   Memptr+1          ;
Insert_2       SEC                     ;  
               LDA   memptr            ;
               SBC   Addrptr           ;
               LDA   memptr+1          ;
               SBC   Addrptr+1         ;
               bcc   Insert_3          ;
               jsr   Scan_Input        ; see if brk requested
               bcc   Insert_0          ; if so, stop List
Insert_3       RTS                     ;
;
Move_cmd       lda   Linecnt           ; *** any changes to this routine affect EEPROM_WR too!!!
               bne   Move_cmd3         ; abort if no . cmd was used
Move_brk       RTS                     ;
Move_cmd1      INC   Addrptr           ;  increments addrptr
               BNE   Move_cmd2         ;
               INC   Addrptr+1         ;
Move_cmd2      inc   Hexdigits         ;  "M" cmd code
               bne   Move_cmd3         ;
               inc   Hexdigits+1       ;
Move_cmd3      SEC                     ;  checks if range done
               LDA   Memptr            ;
               SBC   Addrptr           ;
               LDA   Memptr+1          ;
               SBC   Addrptr+1         ;
               BCC   Move_brk          ;  exit if range done
               jsr   Scan_Input        ; see if brk requested
               bcs   Move_brk          ; 
               LDA   (Addrptr)         ;  Moves one byte
               STA   (Hexdigits)       ;
               BRA   Move_cmd1         ; (zapped after move from eeprom_wr)

;
Dest_cmd       LDX   Hexdigits         ;  ">" cmd code
               LDA   Hexdigits+1       ;
               STX   Memptr            ;  move address to memptr
               STA   Memptr+1          ;
               RTS                     ;  
                                       ;

;
;-----------DATA TABLES ------------------------------------------------
;
Hexdigdata     .byte "0123456789ABCDEF";hex char table 
;     
CmdCount       =$0c                    ; number of commands to scan for
CmdAscii       .byte $0D               ; 0 enter    cmd codes
               .byte $20               ; 1 SPACE
               .byte $2E               ; 2 .
               .byte $3A               ; 3 :
               .byte $3E               ; 4 >  
;               .byte $3f               ; 5 ? - Help
               .byte $47               ; 7 g - Go
               .byte $49               ; 8 i - Insert
               .byte $4D               ; A m - Move
               .byte $51               ; B q - Query memory (text dump)
               .byte $52               ; C r - Registers
  	       .byte $55	       ; F U - Uploader
               .byte $56               ;10 v - Version
;     
Cmdjmptbl      .word CR_cmd            ; 0  enter   cmd jmp table
               .word SP_cmd            ; 1   space
               .word DOT_cmd           ; 2    .
               .word DOT_cmd           ; 3    :
               .word Dest_cmd          ; 4    >  
;               .word Help_Cmd          ; 5    ?
               .word Excute_cmd        ; 7    g
               .word Insert_Cmd        ; 8    i
               .word Move_cmd          ; A    m
               .word TXT_cmd           ; B    q
               .word Printreg          ; C    r
               .word xmodem            ; F    u
               .word Version           ;10    v
;     
Cmdseccode     .byte $00               ; 0   enter       secondary command table
               .byte $FF               ; 1   sp
               .byte $01               ; 2   .
               .byte $02               ; 3   :
               .byte $00               ; 4   > 
;               .byte $00               ; 5   ?
               .byte $00               ; 7   g
               .byte $00               ; 8   i
               .byte $00               ; A   m
               .byte $00               ; B   q
               .byte $00               ; C   r
               .byte $00               ; F   u
               .byte $00               ;10   v
;
;     
;
;
;HelpTxt        .byte "~Current commands are :~"
;               .byte "Syntax = {} required, [] optional, HHHH hex address, DD hex data~"
;               .byte "~"
;               .byte "[HHHH][ HHHH]{Return} - Hex dump address(s)(up to 16 if no address entered)~"
;               .byte "[HHHH]{.HHHH}{Return} - Hex dump range of addresses (16 per line)~"
;               .byte "[HHHH]{:DD}[ DD]{Return} - Change data bytes~"
;               .byte "[HHHH]{G}{Return} - Execute a program (use RTS to return to monitor)~"
;               .byte "{HHHH.HHHH>HHHH{I}{Return} - move range at 2nd HHHH down to 1st to 3rd HHHH~"
;               .byte "{HHHH.HHHH>HHHH{M}{Return} - Move range at 1st HHHH thru 2nd to 3rd HHHH~"
;               .byte "[HHHH][ HHHH]{Q}{Return} - Text dump address(s)~"
;               .byte "[HHHH]{.HHHH}{Q}{Return} - Text dump range of addresses (16 per line)~"
;               .byte "{R}{Return} - Print register contents from memory locations~"
;               .byte "{U}{Return} - Upload File (Xmodem/CRC or Intel Hex)~"
;               .byte "{V}{Return} - Monitor Version~"
;               .byte "{?}{Return} - Print menu of commands~~"
;               .byte $00
;
Porttxt        .byte "65C02 Monitor v5.1.1 (7-8-13) Ready"
               .byte  $0d, $0a
;               .byte "(Press ? for help)"
               .byte $00
;
; *** VERSION Notes ***
; 3.5 added the text dump command, "q"
; 4.0 reorganized structure, added RAM vectors for chrin, scan_in, and chrout
; 4.1 fixed set time routine so 20-23 is correct    
; 4.2 RST, IRQ, NMI, BRK all jmp ind to 02xx page to allow user prog to control
; 4.3 added status register bits to printreg routine
; 4.4 refined set time to reduce unneeded sec"s and branches, disp time added CR,
;     and added zeromem to the reset routine, ensuring a reset starts fresh every time!
;     continued to re-organize - moved monitor"s brk handler into mon area.
; 4.5 nop out the jsr scan_input in the eeprom write routine to prevent BRK"s
; 4.6 added version printout when entering assember to show ? prompt
; 4.7 added Lee Davison's Enhanced Basic to ROM Image 
; 4.9 Added all of the WDC opcodes to the disassembler and mini-assembler
; 5.0 Added TAB key support to the input routine, expands tabs to spaces
; 5.1 Added jump table at the start of the monitor to commonly used routines
; 5.1.1 - stripped down version 
;end of file
