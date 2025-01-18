.include "keyboard.s"

.segment "HEADER"
  ; .byte "NES", $1A        ; NES2.0 header identifier
  .byte $4E, $45, $53, $1A
  .byte $02                 ; 2x 16KB PRG code
  .byte $00                 ; 0x  8KB CHR data
  .byte $A9, $D8            ; mapper 0, vertical mirroring
  .byte $00, $00, $0F, $04  ; 8KB of Work RAM, 1KB of CHR RAM
  .byte $00, $00, $00, $23  ; default expansion device Family Basic Keyboard

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi_handler
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr RESET
  ;; External interrupt IRQ (unused)
  .addr irq_handler


; Main code segment for the program
.segment "CODE"


;  The WOZ Monitor for the Apple 1
;  Written by Steve Wozniak in 1976


; Page 0 Variables
RESETDETECT     = $20           ;  Four bytes $20-23 are used to detect a reset
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
READY           = $2F           ;  NMI ready flag
ROW             = $30           ;  row number
YIN             = $31           ;  Y-index of Input buffer
YOUT            = $32           ;  Y-index of Display buffer
kbdetect        = $33           ;  00 if joypad, 01 if fami keyboard, 02 if keyboard host
kbinput         = $34           ;  keyboard read byte
mseinput        = $35           ;  mouse read byte
kbreadp         = $36           ;  pointer for reading from kbread buffer
kbread          = $37           ;  keyboard buffered input
mseread         = $3B           ;  mouse read input
fkbtemp         = $3F
kbmodkey        = $40           ;  modifier key status



; Other Variables
IN              = $0300         ;  Input buffer to $031E ; $0200 is OAM by convention
DSP             = $0320         ;  Display buffer to $033E
nmipointer      = $07EE         ;  pointer for where nmi interrupt will jump to
irqpointer      = $07F7         ;  pointer for where irq interrupt will jump to
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
                LDX #$40        
                STX $4017       ; disable APU frame IRQ
                LDX #$FF        ; Set up stack
                TXS             ; Set stack pointer to $FF
                INX
                STX PPUCTRL     ; disable NMI
                STX PPUMASK     ; disable rendering
                STX $4010       ; disable DMC IRQs
                
;; first wait for vblank to make sure PPU is ready
                BIT PPUSTATUS
                : BIT PPUSTATUS
                BPL :-

;; VSCROLLY and ROW need to be zeroed regardless of reset or poweron
                STX VSCROLLY
                STX ROW
;; check if the four bytes in resetdetect are expected values
                :TXA
                CMP RESETDETECT,X
                BNE poweron
                INX
                CPX #$04
                BNE :-
;; if this is a reset, clear out IN and DSP buffers anyway
                LDX #$3F
                LDA #$00
                : STA $0300,X
                DEX
                BPL :-
                STA YIN        ;  and reset the y-index
                STA YOUT
                JMP interrupt_setup

poweron:
                LDX #$00
clear_ram:      LDA #$00
                STA $0000,X
                STA $0100,X
                STA $0300,X
                STA $0400,X
                STA $0500,X
                STA $0600,X
                STA $0700,X
                LDA #$FE
                STA $0200,X
                INX
                BNE clear_ram

;; set 4 bytes of resetdetect to known values
                : TXA
                STA RESETDETECT,X
                INX
                CPX #04
                BNE :-

;; set up the default nmi and irq interrupt addresses 
interrupt_setup:LDA #<NMI
                STA nmipointer+1
                LDA #>NMI
                STA nmipointer+2
                LDA #<IRQ
                STA irqpointer+1
                LDA #>IRQ
                STA irqpointer+2

;; second wait for vblank, PPU is ready after this
                : BIT PPUSTATUS
                BPL :-

;; load palettes within vblank to hide visible stripes
load_palettes:  LDA #$3F
                STA PPUADDR
                LDA #$00
                STA PPUADDR
                TAX
                : LDA PALETTES,X
                STA PPUDATA
                INX
                CPX #$20
                BNE :-

;; clear out the nametable/attribute RAM
                BIT PPUSTATUS
                LDA #$20
                STA PPUADDR
                LDA #$00
                STA PPUADDR
                LDY #$04
                : LDX #$00
                : STA PPUDATA
                INX
                BNE :-
                DEY
                BNE :--

;; load CHR-RAM data
                LDA #<TILEDATA
                STA VSCROLLL    ; these will be initialized later
                LDA #>TILEDATA
                STA VSCROLLH    ; so I can make use of them here
                LDY #$00
                STY PPUMASK
                STY PPUADDR
                STY PPUADDR
                LDX #$04        ; store up to 4x 256B pages
                : LDA (VSCROLLL),Y
                STA PPUDATA
                INY
                BNE :-
                INC VSCROLLH    ; without causing problems
                DEX
                BNE :-

;; finally enable rendering
                LDA #$00        ; set scroll
                STA PPUSCROLL
                STA PPUSCROLL
                LDA #%10000000  ; Enable NMI
                STA PPUCTRL

;; initialize keyboard
                JSR KEYBOARD::INIT

;; ready to start mon        
NESMON:         LDY #$20        ; set the VSCROLL start       
                STY VSCROLLH
                INY             ; start on second line for overscan reasons
                STY VSCROLLL
NOTCR:          CMP #$88        ; Backspace?
                BEQ BACKSPACE   ; Yes.
                CMP #$9B        ; ESC?
                BEQ ESCAPE      ; Yes.
                INY             ; Advance text index.
                CPY #$1E        ; compare with 30
                BMI NEXTCHAR    ; Auto ESC if > 30.
ESCAPE:         LDA #'\'+$80    ; "\".
                JSR ECHO        ; Output it.
GETLINE:        LDA #$8D        ; CR.
                JSR ECHO        ; Output it.
                LDY #$01        ; Initialize text index.
BACKSPACE:      DEY             ; Back up text index.
                BMI GETLINE     ; Beyond start of line, reinitialize.
NEXTCHAR:       JSR VBWAIT      ; Wait for NMI to read keyboard
                JSR KEYBOARD::READKBD ; Load character
		            BPL NEXTCHAR    ; Loop until ready
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
                BEQ SETMODE     ; Set BLOCK XAM mode.
                CMP #':'+$80    ; ":"?
                BEQ SETSTOR     ; Yes. Set STOR mode.
                CMP #'R'+$80    ; "R"?
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
                LDA #':'+$80    ; ":".
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
                ORA #'0'+$80    ; Add "0"
                CMP #$BA        ; Digit?
                BCC ECHO        ; Yes, output it.
                ADC #$06        ; Add offset for letter.
ECHO:           PHA             ; Back up A
                STY YIN
                LDY YOUT
                CMP #$8D        ; CR? go to new line
                BEQ :+

                JSR KEYBOARD::CONVASCII  ; convert the ascii here
                STA DSP,Y
                INY
                JSR VBWAIT

                CPY #$1E        ; when running a program that will print to the screen
                BMI :++         ; check if line reaches max length and move to a new
                  :JSR CLEARLINE ; line if necessary
                  LDY #$00
                :LDA #$20
                STA DSP,Y
                STY YOUT
                LDY YIN
                PLA             ; Restore A
                RTS             ; Return.
                ;; ECHO needs to be used to push characters into the frame layout


;; PPUSTATUS bit 7 is unreliable for vblank detection
;; use a flag in RAM instead, so the NMI handler knows it's safe to run
VBWAIT:         SEC              ; set NMI ready flag
                ROR READY
                : BIT READY      ; and wait until the NMI handler clears it
                BMI :-
                RTS

;; these are the jump addresses for nmi and irq interrupts
nmi_handler:
                BIT nmipointer
                BMI nmithree
                BVS nmitwo
                JMP (nmipointer+1)        ; default jump location
      nmitwo:   JMP (nmipointer+3)        ; if #0100,0000
      nmithree: BVS nmifour
                JMP (nmipointer+5)        ; if #1000,0000
      nmifour:  JMP (nmipointer+7)        ; if #1100,0000

irq_handler:
                BIT irqpointer
                BMI irqthree
                BVS irqtwo
                JMP (irqpointer+1)        ; default jump location
      irqtwo:   JMP (irqpointer+3)        ; if #0100,0000
      irqthree: BVS irqfour
                JMP (irqpointer+5)        ; if #1000,0000
      irqfour:  JMP (irqpointer+7)        ; if #1100,0000

NMI:            BIT READY        ; abort if not ready yet
                BPL EXITNMI
          
                PHA              ; push contents of flags, and registers onto stack
                TXA
                PHA
                TYA
                PHA

;; TakuikaNinja - allows for writes to the $0200 page to be reflected in OAM.
                LDA #$00 	; Set SPR-RAM address to 0
                STA OAMADDR
                LDA #$02        ; Set OAMDMA address to $0200
                STA OAMDMA
                LDA #%00011110  ; Enable sprites and background
                STA PPUMASK

;; transfer DSP contents to the PPU nametable
                BIT PPUSTATUS
                LDA VSCROLLH
                STA PPUADDR
                LDA VSCROLLL
                STA PPUADDR
                
                LDY #$00
                : LDA DSP,Y
                INY
                STA PPUDATA
                CPY #$1E
                BMI :-

;; clear an extra line after the input display so the vertical mirroring isn't apparent on the bottom row
                LDA VSCROLLH
                PHA
                LDA VSCROLLL
                PHA
                JSR INCVSCROLL

                BIT PPUSTATUS
                LDA VSCROLLH
                STA PPUADDR
                LDA VSCROLLL
                STA PPUADDR

                LDA #$00
                TAX
                LDY #$1E
                : STA PPUDATA
                DEY
                BNE :-

                PLA
                STA VSCROLLL
                PLA
                STA VSCROLLH

                BIT PPUSTATUS
                STX PPUSCROLL   ; set scroll
                LDA VSCROLLY
                STA PPUSCROLL
                LDA #%10000000  ; select nametable and keep NMI enabled
                STA PPUCTRL

                ; with all the PPU handling done, check the keyboard buffer
                JSR KEYBOARD::KBDREADY

                ; restore contents of flags and registers from stack
                PLA
                TAY
                PLA
                TAX
                PLA

                ASL READY       ; clear NMI ready flag
EXITNMI:
                RTI
IRQ:
                RTI

CLEARLINE:      PHA
                STY YIN         ; save the y value for IN
                ; increase the vscroll
                JSR INCVSCROLL

;; keep the vertical scroll fixed until we reach row $1d, to simulate the original Apple 1 terminal
;; this also ensures that the text display is mostly within the "action safe" area
;; see: https://www.nesdev.org/wiki/Overscan
                INC ROW
                LDA #$1B        ; reduce by 1 because we start on second row
                CMP ROW
                BCS :+
                STA ROW
                LDA #$08
                ADC VSCROLLY
                CMP #$F0
                BCC SKIPOVER
                LDA #$00
SKIPOVER:       STA VSCROLLY
                ; clear DSP so it will write a blank line
                : LDA #$00
                LDY #$20
                : STA DSP,Y
                DEY
                BPL :-
                TAY             ; return "cursor" to the start
                STY YOUT

                JSR VBWAIT      ; wait for v-blank
                LDY YIN         ; restore the y value for IN
                PLA
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
                BCC :+
INCVSCROLLH:    INC VSCROLLH
                : RTS

.segment "RODATA"

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

TILEDATA: .incbin "./chars.chr"
