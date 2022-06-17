; upload.asm   (hooked into SbcOS at $fa00-feff)
; By Daryl Rictor & Ross Archer  Aug 2002
;
; 21st century code for 20th century CPUs (tm?)
; 
; A simple file transfer program to allow upload from a serial
; port to the SBC.  It integrates both x-modem/CRC transfer protocol 
; and Intel Hex formatted files. Primary is XMODEM-CRC, due to its
; superior reliability.  Fallback to Intel Hex is automagical following
; receipt of the first Hexfile d/l character, so the selection is 
; transparent to the user.
;
; Files uploaded via XMODEM-CRC must be
; in .o64 format -- the first two bytes are the load address in
; little-endian format:  
;  FIRST BLOCK
;     offset(0) = lo(load start address),
;     offset(1) = hi(load start address)
;     offset(2) = data byte (0)
;     offset(n) = data byte (n-2)
;
; Subsequent blocks
;     offset(n) = data byte (n)
;
; The TASS assembler and most Commodore 64-based tools generate this
; data format automatically and you can transfer their .obj/.o64 output
; file directly.  
;   
; The only time you need to do anything special is if you have 
; a raw memory image file (say you want to load a data
; table into memory). For XMODEM you'll have to 
; "insert" the start address bytes to the front of the file.
; Otherwise, XMODEM would have no idea where to start putting
; the data.
;
; The "fallback" format is Intel Hex.  As address information is included
; at the start of each line of an Intel Hex file, there is no need for a special
; "first block". As soon as the receiver sees an Intel Hex
; character ':' coming in, it aborts the XMODEM-CRC upload attempt and
; tries to accept Intel Hex instead.  This is the format used natively
; by a lot of generic tools such as TASM.
; Note there is no "fallback fallback."  Once it quits CRC and 
; thinks you're sending it Intel Hex, you either have to finish the download 
; or press CTRL-C to abort.
;
; By having support for both formats under the same "U"pload command,
; it enables seamless switching between either kind of toolchain with
; no special user intervention.  This seemed like a Good Thing (tm).
;
; Note: testing shows that no end-of-line delay is required for Intel Hex
; uploads, but in case your circumstances differ and you encounter
; error indications from a download (especially if you decided to run the
; controller under 1 Mhz), adding a 10-50 mS delay after each line is 
; harmless and will ensure no problems even at low clock speeds
;
;
; Style conventions being tried on this file for possible future adoption:
; 1. Constants known at assembly time are ALL CAPS
; 2. Variables are all lower-case, with underscores used as the word separator
; 3. Labels are PascalStyleLikeThis to distinguish from constants and variables
; 4. Old labels from external modules are left alone.  We may want
;    to adopt these conventions and retrofit old source later.
; 5. Op-codes are lower-case
; 6. Comments are free-style but ought to line up with similar adjacent comments

; zero page variables (Its ok to stomp on the monitor's zp vars)
;
;
crc		=	$38		; CRC lo byte
crch		=	$39		; CRC hi byte
ptr		=	$3a		; data pointer
ptrh		=	$3b		;   "    "
blkno		=	$3c		; block number
retry		=	$3d		; retry counter
retry2	=	$3e		; 2nd counter
bflag		=	$3f		; block flag 
reclen    	=   	$39        	; record length in bytes
chksum    	=   	crc        	; record checksum accumulator
start_lo  	=   	$3b
start_hi  	=   	$3c
rectype   	=   	$3d
dlfail    	=   	$3e     	; flag for upload failure
temp      	=   	$3f     	; save hex value
strptr      =     $40
strptrh	=     $41		; temporary string pointer (not preserved across calls)

;
;  tables and constants
;
crclo    	=	$7a00      	; Two 256-byte tables for quick lookup
crchi    	= 	$7b00      	; (should be page-aligned for speed)
Rbuff		=	$0300      	; temp 128 byte receive buffer 
					; (uses the Monitor's input buffer)
SOH		=	$01		; start block
EOT		=	$04		; end of text marker
ACK		=	$06		; good block acknowleged
NAK		=	$15		; bad block acknowleged
CAN		=	$18		; cancel (not standard, not supported)
CR		=	13
LF		=	10
ESC         =     27          ; ESC to exit
                                        ;
;		*= $f700		; lo CRC lookup table
;crclo
; .byte $00,$21,$42,$63,$84,$A5,$C6,$E7,$08,$29,$4A,$6B,$8C,$AD,$CE,$EF
; .byte $31,$10,$73,$52,$B5,$94,$F7,$D6,$39,$18,$7B,$5A,$BD,$9C,$FF,$DE
; .byte $62,$43,$20,$01,$E6,$C7,$A4,$85,$6A,$4B,$28,$09,$EE,$CF,$AC,$8D
; .byte $53,$72,$11,$30,$D7,$F6,$95,$B4,$5B,$7A,$19,$38,$DF,$FE,$9D,$BC
; .byte $C4,$E5,$86,$A7,$40,$61,$02,$23,$CC,$ED,$8E,$AF,$48,$69,$0A,$2B
; .byte $F5,$D4,$B7,$96,$71,$50,$33,$12,$FD,$DC,$BF,$9E,$79,$58,$3B,$1A
; .byte $A6,$87,$E4,$C5,$22,$03,$60,$41,$AE,$8F,$EC,$CD,$2A,$0B,$68,$49
; .byte $97,$B6,$D5,$F4,$13,$32,$51,$70,$9F,$BE,$DD,$FC,$1B,$3A,$59,$78
; .byte $88,$A9,$CA,$EB,$0C,$2D,$4E,$6F,$80,$A1,$C2,$E3,$04,$25,$46,$67
; .byte $B9,$98,$FB,$DA,$3D,$1C,$7F,$5E,$B1,$90,$F3,$D2,$35,$14,$77,$56
; .byte $EA,$CB,$A8,$89,$6E,$4F,$2C,$0D,$E2,$C3,$A0,$81,$66,$47,$24,$05
; .byte $DB,$FA,$99,$B8,$5F,$7E,$1D,$3C,$D3,$F2,$91,$B0,$57,$76,$15,$34
; .byte $4C,$6D,$0E,$2F,$C8,$E9,$8A,$AB,$44,$65,$06,$27,$C0,$E1,$82,$A3
; .byte $7D,$5C,$3F,$1E,$F9,$D8,$BB,$9A,$75,$54,$37,$16,$F1,$D0,$B3,$92
; .byte $2E,$0F,$6C,$4D,$AA,$8B,$E8,$C9,$26,$07,$64,$45,$A2,$83,$E0,$C1
; .byte $1F,$3E,$5D,$7C,$9B,$BA,$D9,$F8,$17,$36,$55,$74,$93,$B2,$D1,$F0 
;
;		*= $f800		; hi CRC lookup table
;crchi
; .byte $00,$10,$20,$30,$40,$50,$60,$70,$81,$91,$A1,$B1,$C1,$D1,$E1,$F1
; .byte $12,$02,$32,$22,$52,$42,$72,$62,$93,$83,$B3,$A3,$D3,$C3,$F3,$E3
; .byte $24,$34,$04,$14,$64,$74,$44,$54,$A5,$B5,$85,$95,$E5,$F5,$C5,$D5
; .byte $36,$26,$16,$06,$76,$66,$56,$46,$B7,$A7,$97,$87,$F7,$E7,$D7,$C7
; .byte $48,$58,$68,$78,$08,$18,$28,$38,$C9,$D9,$E9,$F9,$89,$99,$A9,$B9
; .byte $5A,$4A,$7A,$6A,$1A,$0A,$3A,$2A,$DB,$CB,$FB,$EB,$9B,$8B,$BB,$AB
; .byte $6C,$7C,$4C,$5C,$2C,$3C,$0C,$1C,$ED,$FD,$CD,$DD,$AD,$BD,$8D,$9D
; .byte $7E,$6E,$5E,$4E,$3E,$2E,$1E,$0E,$FF,$EF,$DF,$CF,$BF,$AF,$9F,$8F
; .byte $91,$81,$B1,$A1,$D1,$C1,$F1,$E1,$10,$00,$30,$20,$50,$40,$70,$60
; .byte $83,$93,$A3,$B3,$C3,$D3,$E3,$F3,$02,$12,$22,$32,$42,$52,$62,$72
; .byte $B5,$A5,$95,$85,$F5,$E5,$D5,$C5,$34,$24,$14,$04,$74,$64,$54,$44
; .byte $A7,$B7,$87,$97,$E7,$F7,$C7,$D7,$26,$36,$06,$16,$66,$76,$46,$56
; .byte $D9,$C9,$F9,$E9,$99,$89,$B9,$A9,$58,$48,$78,$68,$18,$08,$38,$28
; .byte $CB,$DB,$EB,$FB,$8B,$9B,$AB,$BB,$4A,$5A,$6A,$7A,$0A,$1A,$2A,$3A
; .byte $FD,$ED,$DD,$CD,$BD,$AD,$9D,$8D,$7C,$6C,$5C,$4C,$3C,$2C,$1C,$0C
; .byte $EF,$FF,$CF,$DF,$AF,$BF,$8F,$9F,$6E,$7E,$4E,$5E,$2E,$3E,$0E,$1E 
;
;
; Xmodem/CRC upload routine
; By Daryl Rictor, July 31, 2002
;
;
; v0.3  tested good minus CRC
; v0.4  CRC fixed!!! init to $0000 rather than $FFFF as stated   
; v0.5  added CRC tables vs. generation at run time
; v 1.0 recode for use with SBC2
; v 1.1 added block 1 masking (block 257 would be corrupted)

		*= $f900
					;
XModem		jsr	MAKECRCTABLE
		jsr   	prtMsg		; send prompt and info
		lda   	#$01
		sta	blkno		; set block # to 1
		sta	bflag		; set flag to get address from block 1
StartCrc	lda	#"C"		; "C" start with CRC mode
		jsr	output		; send it
		lda	#$FF	
		sta	retry2		; set loop counter for ~3 sec delay
		lda   	#$00
                sta	crc
		sta	crch		; init CRC value	
		jsr	GetByte		; wait for input
                bcs     GotByte         ; byte received, process it
		bcc	StartCrc	; resend "C"

StartBlk	lda	#$FF		; 
		sta	retry2		; set loop counter for ~3 sec delay
		lda   	#$00		;
		sta	crc		;
		sta	crch		; init CRC value	
		jsr	GetByte		; get first byte of block
		bcc	StartBlk	; timed out, keep waiting...
GotByte         cmp     #ESC            ; quitting?
                bne     GotByte3
                brk
GotByte3        cmp     #SOH            ; start of block?
		beq	BegBlk		; yes
		cmp	#":"		; Intel-Hex format - jump to its handler below
		bne	GotByte1	; 
		jmp	HexUpLd		; 
GotByte1	cmp	#EOT		;
		bne	BadCrc		; Not SOH, ":", EOT, so flush buffer & send NAK	
		jmp	Done		; EOT - all done!
BegBlk		ldx	#$00
GetBlk		lda	#$ff		; 3 sec window to receive characters
		sta 	retry2		;
GetBlk1		jsr	GetByte		; get next character
		bcc	BadCrc		; chr rcv error, flush and send NAK
GetBlk2		sta	Rbuff,x		; good char, save it in the rcv buffer
		inx			; inc buffer pointer	
		cpx	#$84		; <01> <FE> <128 bytes> <CRCH> <CRCL>
		bne	GetBlk		; get 132 characters
		ldx	#$00		;
		lda	Rbuff,x		; get block # from buffer
		cmp	blkno		; compare to expected block #	
		beq	GoodBlk1	; matched!
		lda	#>MsgCrcBadBlkno
		ldx	#<MsgCrcBadBlkno
		jsr	PrintStrAX	; Unexpected block number - abort	
		jsr	Flush		; mismatched - flush buffer and then do BRK
		brk			; unexpected block # - fatal error
GoodBlk1	eor	#$ff		; 1's comp of block #
		inx			;
		cmp	Rbuff,x		; compare with expected 1's comp of block #
		beq	GoodBlk2 	; matched!
		lda	#>MsgCrcBadBlkno
		ldx	#<MsgCrcBadBlkno
		jsr	PrintStrAX	; Unexpected block number - abort	
		jsr 	Flush		; mismatched - flush buffer and then do BRK
		brk			; bad 1's comp of block#	
GoodBlk2	ldy	#$02		; 
CalcCrc		lda	Rbuff,y		; calculate the CRC for the 128 bytes of data	
		jsr	UpdCrc		; could inline sub here for speed
		iny			;
		cpy	#$82		; 128 bytes
		bne	CalcCrc		;
		lda	Rbuff,y		; get hi CRC from buffer
		cmp	crch		; compare to calculated hi CRC
		bne	BadCrc		; bad crc, send NAK
		iny			;
		lda	Rbuff,y		; get lo CRC from buffer
		cmp	crc		; compare to calculated lo CRC
		beq	GoodCrc		; good CRC
BadCrc		jsr	Flush		; flush the input port
		lda	#NAK		;
		jsr	output		; send NAK to resend block
		jmp	StartBlk	; start over, get the block again			
GoodCrc		ldx	#$02		;
		lda	blkno		; get the block number
		cmp	#$01		; 1st block?
		bne	CopyBlk		; no, copy all 128 bytes
		lda	bflag		; is it really block 1, not block 257, 513 etc.
		beq	CopyBlk		; no, copy all 128 bytes
		lda	Rbuff,x		; get target address from 1st 2 bytes of blk 1
		sta	ptr		; save lo address
		inx			;
		lda	Rbuff,x		; get hi address
		sta	ptr+1		; save it
		inx			; point to first byte of data
		dec	bflag		; set the flag so we won't get another address		
CopyBlk		ldy	#$00		; set offset to zero
CopyBlk3	lda	Rbuff,x		; get data byte from buffer
		sta	(ptr),y		; save to target
		inc	ptr		; point to next address
		bne	CopyBlk4	; did it step over page boundry?
		inc	ptr+1		; adjust high address for page crossing
CopyBlk4	inx			; point to next data byte
		cpx	#$82		; is it the last byte
		bne	CopyBlk3	; no, get the next one
IncBlk		inc	blkno		; done.  Inc the block #
		lda	#ACK		; send ACK
		jsr	output
		jmp	StartBlk	; get next block
Done		lda	#ACK		; last block, send ACK and exit.
		jsr	output
		;
		lda	#>MsgCrcDone
		ldx	#<MsgCrcDone
		jsr	PrintStrAX	;
		rts			;
;
; subroutines
;
;					;
GetByte		lda	#$00		; wait for chr input and cycle timing loop
		sta	retry		; set low value of timing loop
StartCrcLp	jsr	Scan_Input	; get chr from serial port, don't wait 
		bcs	GetByte1	; got one, so exit
		dec   	retry		; no character received, so dec counter
		bne	StartCrcLp	;
		dec	retry2		; dec hi byte of counter
		bne	StartCrcLp	; look for character again
		clc			; if loop times out, CLC, else SEC and return
GetByte1	rts			; with character in "A"

PrtMsg		ldx	#$00		; PRINT starting message
PrtMsg1		lda   	Msg,x
		beq	PrtMsg2
		jsr   	output
		inx
                jmp     PrtMsg1
PrtMsg2		rts
Msg             .byte   "Begin XMODEM/CRC transfer."
                .byte   CR,LF
                .byte   "Press <Esc> to abort... "
                .byte   0
;					;
Flush		lda	#$70		; flush receive buffer
		sta	retry2		; flush until empty for ~1 sec.
Flush1		jsr	GetByte		; read the port
		bcs	Flush
		rts	
;
;  CRC subroutines
;
UpdCrc		eor 	crc+1 		; Quick CRC computation with lookup tables
         	tax		 
         	lda 	crc
         	eor 	CRCHI,X
         	sta 	crc+1
         	lda 	CRCLO,X
         	sta 	crc
         	rts

MAKECRCTABLE
		ldx 	#$00
		txa
zeroloop	sta 	crclo,x
		sta 	crchi,x
		inx
		bne	zeroloop

		ldx	#$00
fetch		txa
		eor	crchi,x
		sta	crchi,x
		ldy	#$08
fetch1		asl	crclo,x
		rol	crchi,x
		bcc	fetch2
		lda	crchi,x
		eor	#$10
		sta	crchi,x
		lda	crclo,x
		eor	#$21
		sta	crclo,x
fetch2		dey
		bne	fetch1
		inx
		bne	fetch
		rts
;
;
;****************************************************
;
; Intel-hex 6502 upload program
; Ross Archer, 25 July 2002
;
; 
HexUpLd 	lda     #CR
		jsr	output
		lda	#LF
		jsr	output
		lda    	#0
        	sta	dlfail          ;Start by assuming no D/L failure
	  	beq	IHex		
HdwRecs 	jsr     GetSer          ; Wait for start of record mark ':'
        	cmp     #":"
        	bne     HdwRecs         ; not found yet
        	; Start of record marker has been found
IHex    	jsr     GetHex          ; Get the record length
        	sta     reclen          ; save it
       	 	sta     chksum          ; and save first byte of checksum
        	jsr     GetHex          ; Get the high part of start address
        	sta     start_hi
        	clc
        	adc     chksum          ; Add in the checksum       
        	sta     chksum          ; 
        	jsr     GetHex          ; Get the low part of the start address
        	sta     start_lo
        	clc
        	adc     chksum
        	sta     chksum  
        	jsr     GetHex          ; Get the record type
        	sta     rectype         ; & save it
        	clc
        	adc     chksum
        	sta     chksum   
        	lda     rectype
        	bne     HdEr1           ; end-of-record
        	ldx     reclen          ; number of data bytes to write to memory
        	ldy     #0              ; start offset at 0
HdLp1   	jsr     GetHex          ; Get the first/next/last data byte
        	sta     (start_lo),y    ; Save it to RAM
        	clc
        	adc     chksum
        	sta     chksum          ; 
        	iny                     ; update data pointer
        	dex                     ; decrement count
        	bne     HdLp1
        	jsr     GetHex          ; get the checksum
        	clc
        	adc     chksum
        	bne     HdDlF1          ; If failed, report it
        	; Another successful record has been processed
        	lda     #"#"            ; Character indicating record OK = '#'

        	sta	ACIA1dat        ; write it out but don't wait for output 

        	jmp     HdwRecs         ; get next record     
HdDlF1  	lda     #"F"            ; Character indicating record failure = 'F'
        	sta     dlfail          ; upload failed if non-zero

        	sta	ACIA1dat        ; write it to transmit buffer register

        	jmp     HdwRecs         ; wait for next record start
HdEr1   	cmp     #1              ; Check for end-of-record type
        	beq     HdEr2
		lda	#>MsgUnknownRecType
		ldx	#<MsgUnknownRecType
                jsr     PrintStrAX      ; Warn user of unknown record type
		lda     rectype         ; Get it
        	sta     dlfail          ; non-zero --> upload has failed
        	jsr     Print1Byte      ; print it
		lda     #CR		; but we'll let it finish so as not to 
        	jsr     output		; falsely start a new d/l from existing 
        	lda     #LF		; file that may still be coming in for 
        	jsr     output          ; quite some time yet.
		jmp	HdwRecs
		; We've reached the end-of-record record
HdEr2   	jsr     GetHex          ; get the checksum 
        	clc
        	adc     chksum          ; Add previous checksum accumulator value
        	beq     HdEr3           ; checksum = 0 means we're OK!
		lda	#>MsgBadRecChksum
		ldx	#<MsgBadRecChksum
                jmp     PrintStrAX
HdEr3   	lda     dlfail
        	beq     HdErOK
        	;A upload failure has occurred
		lda	#>MsgUploadFail
		ldx	#<MsgUploadFail
                jmp     PrintStrAX
HdErOK  	lda	#>MsgUploadOK
		ldx	#<MsgUploadOK
                jsr     PrintStrAX
		; Eat final characters so monitor doesn't cope with it
	  	jsr     Flush		; flush the input buffer
HdErNX  	rts
;
;  subroutines
;
                     
GetSer  	jsr	scan_input	; get input from Serial Port	    
                cmp     #ESC            ; check for abort 
        	bne     GSerXit         ; return character if not
                brk
GSerXit 	rts

GetHex  	lda     #$00
	  	sta     temp
        	jsr     GetNibl
        	asl     a
        	asl     a
        	asl     a
       	 	asl     a       	; This is the upper nibble
        	sta     temp
GetNibl 	jsr     GetSer
					; Convert the ASCII nibble to numeric value from 0-F:
	        cmp     #"9"+1  	; See if it's 0-9 or 'A'..'F' (no lowercase yet)
       	 	bcc     MkNnh   	; If we borrowed, we lost the carry so 0..9
        	sbc     #7+1    	; Subtract off extra 7 (sbc subtracts off one less)
        	; If we fall through, carry is set unlike direct entry at MkNnh
MkNnh   	sbc     #"0"-1  	; subtract off '0' (if carry clear coming in)
        	and     #$0F    	; no upper nibble no matter what
        	ora     temp
        	rts             	; return with the nibble received



;Print the string starting at (AX) until we encounter a NULL
;string can be in RAM or ROM.  It's limited to <= 255 bytes.
;
PrintStrAX      sta     strptr+1
		stx	strptr
		tya
		pha
		ldy	#0
PrintStrAXL1    lda     (strptr),y
                beq     PrintStrAXX1      ; quit if NULL
    		jsr	output
		iny
                bne     PrintStrAXL1      ; quit if > 255
PrintStrAXX1    pla
		tay
		rts   
; CRC messages
MsgCrcBadBlkno  .byte	CR,LF,CR,LF
		.byte  	"Unexpected block number received"
 		.byte	" Aborting"
		.byte 	CR,LF
		.byte 	0
MsgCrcDone	.byte 	CR,LF
                .byte   "XMODEM-CRC download is complete"
		.byte 	0


; Checksum messages
;					
MsgUnknownRecType  
		.byte   CR,LF,CR,LF
      		.byte   "Unknown record type $"
		.byte	0		; null-terminate every string
MsgBadRecChksum .byte   CR,LF,CR,LF
                .byte   "Bad record checksum!"
        	.byte   0		; Null-terminate  
MsgUploadFail   .byte   CR,LF,CR,LF
                .byte   "Upload Failed",CR,LF
                .byte   "Aborting!"
                .byte   0               ; null-terminate every string or crash'n'burn
MsgUploadOK	.byte   CR,LF,CR,LF
                .byte   "Upload Successful!"
        	.byte   0         	
;  Fin.
;
