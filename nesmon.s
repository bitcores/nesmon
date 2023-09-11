.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte $02               ; 2x 16KB PRG code
  .byte $00               ; 0x  8KB CHR data
  .byte $A9, $D0        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr NMI
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr RESET
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; Main code segment for the program
.segment "CODE"


;  The WOZ Monitor for the Apple 1
;  Written by Steve Wozniak in 1976


; Page 0 Variables

XAML            = $24           ;  Last "opened" location Low
XAMH            = $25           ;  Last "opened" location High
STL             = $26           ;  Store address Low
STH             = $27           ;  Store address High
L               = $28           ;  Hex value parsing Low
H               = $29           ;  Hex value parsing High
YSAV            = $2A           ;  Used to see if hex value is given
MODE            = $2B           ;  $00=XAM, $7F=STOR, $AE=BLOCK XAM

VSCROLLL        = $2C           ;  vertical scroll value low
VSCROLLH        = $2D           ;  vertical scroll value high
VSCROLLY        = $2E           ;  vertical scroll y value
buttons         = $2F           ;  current buttons value
lastbuttons     = $30           ;  last buttons value
YIN             = $31           ;  Y-index of Input buffer
YOUT            = $32           ;  Y-index of Display buffer


; Other Variables

IN              = $0300         ;  Input buffer to $031E ; $0200 is OAM by convention
DSPBUF          = $0320         ;  Display buffer to $033E
KEYPTR          = $031E         ;  pointer for the keymap
INBUF           = $031F         ;  currently modified character

PPUCTRL         = $2000
PPUMASK         = $2001
PPUSTATUS       = $2002
OAMADDR         = $2003
OAMDATA         = $2004
PPUSCROLL       = $2005
PPUADDR         = $2006
PPUDATA         = $2007
OAMDMA          = $4014
JOYPAD1         = $4016

BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

               .export RESET

RESET:          SEI
                CLD             ; Clear decimal arithmetic mode.
                CLI
                LDX #$40        
                STX $4017	; disable APU frame IRQ
                LDX #$ff 	; Set up stack
                TXS 		;  Set stack pointer to $FF
                LDX #$00
                STX PPUCTRL	; disable NMI
                STX PPUMASK ; disable rendering
                STX $4010 	; disable DMC IRQs
                
;; first wait for vblank to make sure PPU is ready
                JSR VBWAIT

clear_ram:      LDA #$00
                STA $0000, x
                STA $0100, x
                STA $0300, x
                STA $0400, x
                STA $0500, x
                STA $0600, x
                STA $0700, x
                LDA #$FE
                STA $0200, x
                INX
                BNE clear_ram

;; second wait for vblank, PPU is ready after this
                JSR VBWAIT

                LDA #$00 	; Set SPR-RAM address to 0
                STA OAMADDR
                LDA #$02  ; Set OAMDMA address to $0200
                STA OAMDMA

;; clear out the nametable/attribute RAM
                LDA PPUSTATUS
                LDA #$20
                STA PPUADDR
                LDA #$00
                STA PPUADDR
                LDA #0
                LDY #4
                : LDX #0
                : STA PPUDATA
                INX
                BNE :-
                DEY
                BNE :--

load_palettes:  LDA PPUSTATUS
                LDA #$3f
                STA PPUADDR
                LDA #$00
                STA PPUADDR
                LDX #$00
                : LDA PALETTES, x
                STA PPUDATA
                INX
                CPX #$20
                BNE :-

                LDA #<TILEDATA
                STA $00
                LDA #>TILEDATA
                STA $01
                LDY #$00
                STY PPUMASK
                STY PPUADDR
                STY PPUADDR
                LDX #$04  ; store up to 4x 256B pages
                : LDA ($00),y
                STA PPUDATA
                INY
                BNE :-
                INC $01
                DEX
                BNE :-

;; finally enable rendering
                LDA #$00        ; set scroll
                STA PPUSCROLL
                STA PPUSCROLL
                LDA #%10000000	; Enable NMI
                STA PPUCTRL
                LDA #%00011010	; Enable sprites and background
                STA PPUMASK

;; ready to start mon
                
                LDY #$1F
                LDA #$23        ; set the VSCROLL start
                STA VSCROLLH
                LDA #$A0
                STA VSCROLLL
NOTCR:          CMP #$02        ; ESC?
                BEQ ESCAPE      ; Yes.
                                ; only advance text index if input was A button
                ;INY            ; Advance text index.
                CPY #$1E        ; compare with 30
                BMI NEXTCHAR    ; Auto ESC if > 30.
ESCAPE:         JSR CLEARLINE
                LDA #$02        ; "\" how about /
                STA INBUF
                JSR ECHO        ; Output it.
GETLINE:        JSR CLEARLINE   ;
                LDY #$01        ; Initialize text index.
BACKSPACE:      DEY             ; Back up text index.
                BMI GETLINE     ; Beyond start of line, reinitialize.
NEXTCHAR:       JSR READJOY     ; Read the joypad for button presses
                LDA buttons
                CMP lastbuttons
                BEQ NEXTCHAR    ; Loop until ready.
                JSR PROCESSJOY  ; process the input
                CMP #$8D
                BNE NOTCR       ; No.
                LDY #$FF        ; Reset text index.
                LDA #$00        ; For XAM mode.
                TAX             ; 0->X.
SETSTOR:        ASL             ; Leaves $7B if setting STOR mode.
                ASL
                ASL
                ASL
SETBLKM:        ROR
SETMODE:        STA MODE        ; $00=XAM $7B=STOR $AE=BLOK XAM
BLSKIP:         INY             ; Advance text index.
                STY YIN
NEXTITEM:       LDY YIN
                LDA IN,Y        ; Get character.
                CMP #$8D        ; CR?
                BEQ GETLINE     ; Yes, done this line.
                CMP #$01        ; "."?
                BCC BLSKIP      ; Skip delimiter.
                BEQ SETBLKM     ; Yes. Set STOR mode.
                CMP #$0D        ; ":"?
                BEQ SETSTOR     ; Yes. Set STOR mode.
                CMP #$1B        ; "R"?
                BEQ RUN         ; Yes. Run user program.
                STX L           ; $00-> L.
                STX H           ; and H.
                STY YSAV        ; Save Y for comparison.
NEXTHEX:        LDA IN,Y        ; Get character for hex test.
                SEC
                SBC #$03        ; Map digits to $0-9.
                CMP #$0A        ; Digit?
                BCC DIG         ; Yes.
                SBC #$06
                SEC
                ADC #$EE        ; Map letter "A"-"F" to $FA-FF.
                CMP #$FA        ; Hex letter?
                BCC NOTHEX      ; No, character not hex.
DIG:            ASL
                ASL             ; Hex digit to MSD of A.
                ASL
                ASL
                LDX #$04        ; Shift count.
HEXSHIFT:       ASL             ; Hex digit left, MSB to carry.
                ROL L           ; Rotate into LSD.
                ROL H           ;  Rotate into MSD’s.
                DEX             ; Done 4 shifts?
                BNE HEXSHIFT    ; No, loop.
                INY             ; Advance text index.
                STY YIN             
                BNE NEXTHEX     ; Always taken. Check next char for hex.
NOTHEX:         CPY YSAV        ; Check if L, H empty (no hex digits).
                BEQ ESCAPE      ; Yes, generate ESC sequence.
                BIT MODE        ; Test MODE byte.
                BVC NOTSTOR     ;  B6=0 STOR 1 for XAM & BLOCK XAM
                LDA L           ; LSD’s of hex data.
                STA (STL,X)     ; Store at current ‘store index’.
                INC STL         ; Increment store index.
                BNE NEXTITEM    ; Get next item. (no carry).
                INC STH         ; Add carry to ‘store index’ high order.
TONEXTITEM:     JMP NEXTITEM    ; Get next command item.
RUN:            JMP (XAML)      ; Run at current XAM index.
NOTSTOR:        BMI XAMNEXT     ; B7=0 for XAM, 1 for BLOCK XAM.
                LDX #$02        ; Byte count.
SETADR:         LDA L-1,X       ; Copy hex data to
                STA STL-1,X     ; ‘store index’.
                STA XAML-1,X    ; And to ‘XAM index’.
                DEX             ; Next of 2 bytes.
                BNE SETADR      ; Loop unless X=0.
NXTPRNT:        BNE PRDATA      ; NE means no address to print.
                JSR CLEARLINE   ; Output a CLEARLINE
                LDA XAMH        ; ‘Examine index’ high-order byte.
                JSR PRBYTE      ; Output it in hex format.
                LDA XAML        ; Low-order ‘examine index’ byte.
                JSR PRBYTE      ; Output it in hex format.
                LDA #$0D        ; ":".
                STA INBUF
                JSR ECHO        ; Output it.
PRDATA:         LDA #$00        ; Blank.
                STA INBUF
                JSR ECHO        ; Output it.
                LDA (XAML,X)    ; Get data byte at ‘examine index’.
                JSR PRBYTE      ; Output it in hex format.
XAMNEXT:        STX MODE        ; 0->MODE (XAM mode).
                LDA XAML
                CMP L           ; Compare ‘examine index’ to hex data.
                LDA XAMH
                SBC H
                BCS TONEXTITEM  ; Not less, so no more data to output.
                INC XAML
                BNE MOD8CHK     ; Increment ‘examine index’.
                INC XAMH
MOD8CHK:        LDA XAML        ; Check low-order ‘examine index’ byte
                AND #$07        ; For MOD 8=0
                BPL NXTPRNT     ; Always taken.
PRBYTE:         PHA             ; Save A for LSD.
                LSR
                LSR
                LSR             ; MSD to LSD position.
                LSR
                JSR PRHEX       ; Output hex digit.
                PLA             ; Restore A.
PRHEX:          AND #$0F        ; Mask LSD for hex print.
                CLC             
                ADC #$03        ; Add 3 to shift.
                STA INBUF
                CMP #$0D        ; Digit?
                BCC ECHO        ; Yes, output it.
                ADC #$06        ; Add offset for letter.
                STA INBUF
ECHO:           STY YIN
                LDY YOUT
                LDA INBUF       ; load the character from inbuf
                STA DSPBUF, Y   ; store the character in DSPBUF for output
                INY
                JSR VBWAIT
                LDA #$00
                STA INBUF
                STY YOUT
                LDY YIN
                RTS             ; Return.
                ;; ECHO needs to be used to push characters into the frame layout

                BRK             ; unused
                BRK             ; unused

VBWAIT:         BIT PPUSTATUS   ; wait for v-blank after ECHO
                BPL VBWAIT
                RTS

NMI:            ; push contents of flags, and registers onto stack
                PHP
                PHA
                TXA
                PHA
                TYA
                PHA
                
                CLC
                LDY #$00

                LDA PPUSTATUS
                LDA VSCROLLH    
                STA PPUADDR
                TYA             ; set accumulator to value Y
                ADC VSCROLLL    ; add the low vscroll to Y to get x offset
                STA PPUADDR
                
                :LDA DSPBUF, Y
                INY
                CMP #$8D
                BEQ :-
                STA PPUDATA
                CPY #$1E
                BNE :-

                LDA PPUSTATUS
                LDA #$00        ; set scroll
                STA PPUSCROLL
                LDA VSCROLLY
                STA PPUSCROLL

                ; restore contents of flags and registers from stack
                PLA
                TAY
                PLA
                TAX
                PLA
                PLP

                RTI

READJOY:        LDA #$00
                STA JOYPAD1
                LDA #$01
                STA JOYPAD1
                STA buttons
                LSR A
                STA JOYPAD1
                : LDA JOYPAD1
                LSR A
                ROL buttons
                BCC :-
                RTS

PROCESSJOY:     STA lastbuttons
                AND #BUTTON_DOWN
                BEQ JOYNOTDOWN
                INC KEYPTR
                JSR KEYSET                
                JMP JOYSAME
JOYNOTDOWN:     LDA lastbuttons
                AND #BUTTON_UP
                BEQ JOYNOTUP
                DEC KEYPTR
                JSR KEYSET
                JMP JOYSAME
JOYNOTUP:       LDA lastbuttons
                AND #BUTTON_A
                BEQ JOYNOTA
                LDA INBUF
                STA IN, Y
                INY
                STA DSPBUF, Y
                JMP JOYSAME
JOYNOTA:        LDA lastbuttons
                AND #BUTTON_B
                BEQ JOYNOTB
                LDA #$00
                STA DSPBUF, Y
                DEY
                LDA IN, Y
                STA INBUF
                JMP JOYBOUNDS
JOYNOTB:        LDA lastbuttons
                AND #BUTTON_START
                BEQ JOYNOTSTART
                LDA INBUF
                STA IN, Y
                INY
                LDA #$8D
                STA IN, Y
                JMP JOYSAME
JOYNOTSTART:    LDA lastbuttons
                AND #BUTTON_SELECT
                BEQ JOYSAME
                LDA #$02
                JMP JOYSAME
JOYBOUNDS:      CPY #$00
                BPL JOYSAME
JOYNEG:         LDA #$00
                TAY
JOYSAME:        RTS


CLEARLINE:      STY YIN   ; save the y value for IN
                ; increase the vscroll
                JSR INCVSCROLL
                CLC
                LDA #$08
                ADC VSCROLLY
                CMP #$F0
                BCC SKIPOVER
                LDA #$00
SKIPOVER:       STA VSCROLLY
                ; clear DSPBUF so it will write a blank line
                LDA #$00
                LDY #$20
                :STA DSPBUF, Y
                DEY
                BPL :-
                TAY             ; return "cursor" to the start
                STY YOUT
                STA KEYPTR      ; reset key pointer
                JSR KEYSET

                JSR VBWAIT      ; wait for v-blank
                
                LDY YIN   ; restore the y value for IN
                RTS

INCVSCROLL:     LDA VSCROLLH
                CMP #$23
                BNE INCVSCROLLL
                LDA VSCROLLL
                CMP #$A0
                BNE INCVSCROLLL
                LDA #$20
                STA VSCROLLH
                LDA #$00
                STA VSCROLLL
                RTS
INCVSCROLLL:    CLC
                LDA #$20
                ADC VSCROLLL
                STA VSCROLLL
                BCS INCVSCROLLH
                RTS
INCVSCROLLH:    INC VSCROLLH
                RTS

KEYSET:         TYA
                PHA
                LDY KEYPTR
                CPY #$14
                BCC LOADKEY
                LDA #$00
                CPY #$FE
                BCC PTRL
                LDA #$13
PTRL:           TAY
LOADKEY:        LDA KEYMAP, Y
                STA INBUF
                STY KEYPTR
                PLA
                TAY
                LDA INBUF
                STA DSPBUF, Y
                RTS

; Interrupt Vectors
KEYMAP:
  .byte $03, $04, $05, $06
  .byte $07, $08, $09, $0A
  .byte $0B, $0C, $14, $15
  .byte $16, $17, $18, $19
  .byte $01, $0D, $1B, $00

PALETTES:
  ; Background Palette
  .byte $0F, $00, $00, $00
  .byte $0F, $12, $00, $00
  .byte $0F, $00, $00, $00
  .byte $0F, $00, $00, $00

  ; Sprite Palette
  .byte $0F, $20, $17, $29
  .byte $0F, $07, $00, $00
  .byte $0F, $1A, $00, $00
  .byte $0F, $34, $00, $00

.segment "RODATA"
TILEDATA: .incbin "./chars.chr"