.include "keyboard.s"

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
YIN             = $30           ;  Y-index of Input buffer
YOUT            = $31           ;  Y-index of Display buffer
kbdetect        = $32           ;  00 if joypad, 01 if fami keyboard, 02 if keyboard host
kbinput         = $33
mseinput        = $34
kbreadp         = $35           ; pointer for reading from kbread buffer
kbread          = $36
mseread         = $3A
fkbtemp         = $3F


; Other Variables
IN              = $0300         ;  Input buffer to $031E ; $0200 is OAM by convention
DSP             = $0320         ;  Display buffer to $033E
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
JOYPAD2         = $4017


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
                STA $6000, x
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
                LDA #%00001010	; Enable sprites and background
                STA PPUMASK

;; initialize keyboard
JSR KEYBOARD::INIT

;; ready to start mon
                
NESMON:         LDY #$1F
                LDA #$23        ; set the VSCROLL start
                STA VSCROLLH
                LDA #$81
                STA VSCROLLL
NOTCR:          CMP #$88        ; Backspace?
                BEQ BACKSPACE   ; Yes.
                CMP #$9B        ; ESC?
                BEQ ESCAPE      ; Yes.
                INY            ; Advance text index.
                CPY #$1E        ; compare with 30
                BMI NEXTCHAR    ; Auto ESC if > 30.
ESCAPE:         LDA #'\'+$80    ; "\".
                JSR ECHO        ; Output it.
GETLINE:        JSR CLEARLINE   ;
                LDY #$01        ; Initialize text index.
BACKSPACE:      DEY             ; Back up text index.
                BMI GETLINE     ; Beyond start of line, reinitialize.
NEXTCHAR:       JSR KEYBOARD::KBDREADY ; Key ready?
                BPL NEXTCHAR    ; Loop until ready
                JSR KEYBOARD::KBDREAD ; Load character
                STA IN,Y
                JSR ECHO
                CMP #$8D
                BNE NOTCR       ; No.
                LDY #$FF        ; Reset text index.
                LDA #$00        ; For XAM mode.
                TAX             ; 0->X.
SETSTOR:        ASL             ; Leaves $7B if setting STOR mode.
SETMODE:        STA MODE        ; $00=XAM $7B=STOR $AE=BLOK XAM
BLSKIP:         INY             ; Advance text index.
NEXTITEM:       LDA IN,Y        ; Get character.
                CMP #$8D        ; CR?
                BEQ GETLINE     ; Yes, done this line.
                CMP #'.'+$80    ; "."?
                BCC BLSKIP      ; Skip delimiter.
                BEQ SETMODE     ; Yes. Set STOR mode.
                CMP #';'+$80    ; ":"? change once SHIFT mode added
                BEQ SETSTOR     ; Yes. Set STOR mode.
                CMP #'R'+$80        ; "R"?
                BEQ RUN         ; Yes. Run user program.
                STX L           ; $00-> L.
                STX H           ; and H.
                STY YSAV        ; Save Y for comparison.
NEXTHEX:        LDA IN,Y        ; Get character for hex test.
                EOR #$B0        ; Map digits to $0-9.
                CMP #$0A        ; Digit?
                BCC DIG         ; Yes.
                ADC #$88        ; Map letter "A"-"F" to $FA-FF.
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
                LDA #':'+$80        ; ":".
                JSR ECHO        ; Output it.
PRDATA:         LDA #$A0        ; Blank.
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
                ORA #$B0        ; Add "0"
                CMP #$BA        ; Digit?
                BCC ECHO        ; Yes, output it.
                ADC #$06        ; Add offset for letter.
ECHO:           STY YIN
                LDY YOUT
                STA DSP,Y
                INY
                JSR VBWAIT
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
                
                LDY #$00
                LDA PPUSTATUS
                LDA VSCROLLH    
                STA PPUADDR
                LDA VSCROLLL
                STA PPUADDR          
                :LDA DSP,Y
                INY
                CMP #$8D
                BEQ :-
                JSR KEYBOARD::CONVASCII
                STA PPUDATA
                CPY #$20
                BNE :-

                ; clear the next line as well before scrolling
                LDA PPUSTATUS
                CLC
                TYA
                ADC VSCROLLL
                TAY
                LDA VSCROLLH
                ADC #$00
                CMP #$23  ; check if we have to scroll to first line
                BCC :+
                CPY #$C0
                BCC :+
                LDA #$20  ; set first line
                LDY #$01
                :STA PPUADDR
                TYA
                STA PPUADDR
                LDY #$20
                LDA #$00
                :STA PPUDATA
                INY
                CPY #$40
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

CLEARLINE:      JSR VBWAIT      ; wait for v-blank
                STY YIN
                JSR INCVSCROLL
                CLC
                LDA #$08
                ADC VSCROLLY
                CMP #$F0
                BCC SKIPOVER
                LDA #$00
SKIPOVER:       STA VSCROLLY
                ; clear IN so it will write a blank line
                LDA #$00
                LDY #$41
                :DEY
                STA DSP,Y 
                BNE :-
                STY YOUT
                LDY YIN
                RTS

INCVSCROLL:     LDA VSCROLLH
                CMP #$23
                BNE INCVSCROLLL
                LDA VSCROLLL
                CMP #$A1
                BNE INCVSCROLLL
                LDA #$20
                STA VSCROLLH
                LDA #$01
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