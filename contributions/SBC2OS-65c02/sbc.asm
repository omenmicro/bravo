;      *= $8002			; create exact 32k bin image

;
; prefill 32k block from $8002-$ffff with 'FF'
;
;      .rept 2047
;         .byte  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ;
;      .next 
;      .byte  $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff  ;

;
; compile the sections of the OS
;

	.include upload.asm	   ; Xmodem and Intel Hex upload    
      	.include ACIA1.asm	   ;        ACIA init (19200,n,8,1)

 	.include sbcos.asm         ; $E800  OS
 
	.include reset.asm         ; $FF00  Reset & IRQ handler

