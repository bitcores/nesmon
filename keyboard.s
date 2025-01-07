.segment "CODE"

    ; the point of this file is to handle keyboard initialization,
    ; reading input from the keyboard and converting input to and
    ; from ascii representation
    ; this should help ease support for existing software
.proc KEYBOARD

    INIT:   ; discover which keyboard mode to use and prepare it
    ;; detect keyboard present
    ;; first, try family basic keyboard
        LDA #$05
        STA $4016
        NOP
        NOP
        NOP
        LDA #$04
        STA $4016
        NOP
        NOP
        NOP
        LDA $4017
        AND #$1E
        CMP #$1E
        BNE KBMHOST
        LDA #$01
        JMP ENDINIT
;; next, try keyboard mouse host
    KBMHOST: JSR READKBMH
        LDA mseread
        AND #$16                ; sets 0 if not 0x16
        BEQ WAIT4INIT
        LDA #$02
        JMP ENDINIT

    WAIT4INIT:
        JSR VBWAIT              ; keep waiting until keyboard found
        JMP INIT

    ENDINIT:
        STA kbdetect
        RTS

    READKBMH:  PHP
        TYA
        PHA

        LDY #$00
        ; strobe $4016 once to signal start reading
        LDA #$00
        STA JOYPAD1
        NOP
        NOP
        NOP
        LDA #$01
        STA JOYPAD1
        NOP
        NOP
        NOP
        LSR A
        STA JOYPAD1
        ; loop this four times total for 4 bytes
        LOOPFOUR: LDA #$01
            STA mseinput
            JOY2LOOP: LDA JOYPAD2
                LSR A ; D0
                LSR A ; D1
                LSR A ; D2
                LSR A ; D3
                ROL kbinput
                LSR A ; D4
                ROL mseinput
                BCC JOY2LOOP

            STY $1D
            LDA kbinput
            AND #$80
            STA $1F             ; store up/down status
            CMP #$00 
            LDA kbinput
            BEQ :+
            AND #$7F            ; get usb code
            CMP #$78
            BCC :+
            JSR SETMODKEYS
            :TAY
            LDA KBMHOSTMAP, Y   ; load ascii code
            CMP #$00
            BEQ STOREASCII      ; if the code is invalid, don't restore
            CMP #';'            ; we are only looking to mod ; right now, create subroutine for more handling
            BNE :+
            PHA
            LDA kbmodkey
            AND #$22            ; is any shift key held?
            TAY
            PLA
            CPY #$00
            BEQ :+
            LDA #':'            ; could do math, just replace for now
            :ORA $1F            ; restore status
    STOREASCII:LDY $1D
            STA kbread, Y
            LDA mseinput
            STA mseread, Y
            INY
            CPY #$04
            BNE LOOPFOUR

        PLA
        TAY
        PLP
        RTS

;; set the status of the mod keys; shift, alt, ctrl
    SETMODKEYS:
        PHA                     ; save the keycode
        SEC
        SBC #$77                ; get an offset 1-8
        TAY
        SEC
        LDA #$00
        :ROL                    ; roll a bit into the right position
        DEY
        BNE :-
        PHA
        LDA $1F
        AND #$80
        BNE SUBMODKEY
        PLA
        ORA kbmodkey            ; if key down, set 1 in key position
        JMP RETMODKEY
    SUBMODKEY:
        PLA
        EOR kbmodkey            ; if key up, set 0 in key position
    RETMODKEY:
        STA kbmodkey
        PLA
    RTS

    ; return one key at a time, from buffer first or
    ; read keyboard input, return ascii in A
    ; maybe make a similar that returns usb codes
    KBDREADY:
    ; some key presses may already be buffered, we want to return those
    ; first if necessary before looking for new keys from the host
        STY $0D
        LDY kbreadp
        LBUF:
            CPY #$04
            BEQ REFBUFFER       ; no release key in buffer
            LDA kbread, Y
            AND #$80
            BNE BUFREADY
            INY
            JMP LBUF
    BUFREADY:
        STY kbreadp
        LDY $0D
        CMP #$00
        RTS 

    REFBUFFER:
        LDA #$00                ; reset pointer index
        STA kbreadp
        LDA kbdetect
        CMP #$01                ; is it family keyboard?
        BEQ REFFAMIKBD
        CMP #$02                ; is it keyboard mouse host?
        BEQ REFKBMH
        JSR INIT                ; neither? init the keyboard again ?
        JMP NOTREADY
    REFFAMIKBD:
        JSR CLEARKBUF
        JSR READFAMIKBD
        JMP NOTREADY
    REFKBMH:
        JSR READKBMH
    NOTREADY:
        LDA #$00                ; signal not ready this round
        LDY $0D                 ; restore Y
        RTS

    READKBD:
    ; we know there is something in the buffer, at least one character,
    ; and the pointer should be pointing at it already
    ; can be buffer the family keyboard keys in the same place?
        STY $0D
        LDY kbreadp             ; load the pointer
        LDA kbread, Y           ; load the character in butter
        INY                     ; advance the pointer
        STY kbreadp             ; store the pointer
        LDY $0D
        RTS

    CLEARKBUF:
    ; clear out the keyboard buffer
        TYA
        PHA
        LDY #$03
        LDA #$00
        :STA kbread, Y
            DEY
            BPL :-
        PLA
        TAY
        RTS

    READFAMIKBD: PHP            ; let's store all the keyboard data in $50 ->
        TYA
        PHA
        TXA
        PHA

        LDY #$00
        LDA #$05
        STA $4016               ; strobe family keyboard for report
        NOP
        NOP
        NOP
        LDA #$04
        STA $0C
    readkeyboard:
        STA $4016               
        NOP
        NOP
        NOP
        LDA $4017
        LSR
        CLC
        AND #$0F
        STA fkbtemp             ; store in fkbtemp
        CMP $50, Y              ; if same as history, ignore
        BEQ nextkeyboard  
        LDA $50, Y              ; we want to check if a key has been released
        ORA fkbtemp             ; get all 1 positions
        EOR fkbtemp             ; get the released positions

        LDX #$04
    parsekeyboard:DEX
        LSR
        BCC finishparse         ; if 0, skip
        PHA                     ; backup kb data
        TYA                     ; 
        PHA                     ; and Y
        ASL                     ; multiply by 4 to get word pos
        ASL
        STA $0F
        CLC
        TXA
        ADC $0F                 ; location of target character
        TAY
        LDA FBKBMAP, Y
        CMP #$00
        BEQ restorekbdata
        ORA #$80                ; convert to release code   
        LDY kbreadp         
        CPY #$04                ; do we have four new keys in buffer already?
        BEQ restorekbdata       ; we might lose key presses this way, rarely
        STA kbread, Y
        INY
        STY kbreadp
    restorekbdata:PLA           ; restore kb data
        TAY
        PLA
    finishparse:CPX #$00
        BNE parsekeyboard
    nextkeyboard:LDA fkbtemp 
        STA $50, Y              ; store new keyboard values
        LDA $0C
        EOR #$02                ; flipflop the 2nd bit to switch column/row
        STA $0C
        INY
        CPY #$12
        BCC readkeyboard

    exitkeyboard:
        LDA #$00
        STA kbreadp

        PLA
        TAX
        PLA
        TAY
        PLP

        RTS

    CONVASCII: ; ascii values map directly to charmap;
        ; return blank for anything below A0
        CMP #$88                ; check for backspace
        BNE :+
        LDA #$DF                ; print an underscore if backspace
        :CMP #$A0
        BCC :+
        CMP #$E0                ; return blank for anything above E0
        BCS :+
        SEC
        SBC #$A0                ; subtract A0 to get the character
        JMP RETCHR
        : LDA #$00
    RETCHR:
        RTS

.endproc

.segment "RODATA"

FBKBMAP: ; this maps the family keyboard to ascii values; ignore $00
    .byte $5D, $5B, $0D, $00, $00, $00, $00, $00 ; row 0
    .byte $3B, $3A, $40, $00, $5E, $2D, $2F, $5F ; row 1
    .byte $4B, $4C, $4F, $00, $30, $50, $2C, $2E ; row 2
    .byte $4A, $55, $49, $00, $38, $39, $4E, $4D ; row 3
    .byte $48, $47, $59, $00, $36, $37, $56, $42 ; row 4
    .byte $44, $52, $54, $00, $34, $35, $43, $46 ; row 5
    .byte $41, $53, $57, $00, $33, $45, $5A, $58 ; row 6
    .byte $00, $51, $1B, $00, $32, $31, $00, $00 ; row 7
    .byte $00, $00, $00, $00, $00, $08, $20, $00 ; row 8

KBMHOSTMAP: ; this maps the input values to the ascii values; ignore $00
            ; wozmon expects key releases to be returned +0x80
    .byte $00, $00, $00, $00, $41, $42, $43, $44 ; 00 - 07
    .byte $45, $46, $47, $48, $49, $4A, $4B, $4C ; 08 - 0F
    .byte $4D, $4E, $4F, $50, $51, $52, $53, $54 ; 10 - 17
    .byte $55, $56, $57, $58, $59, $5A, $31, $32 ; 18 - 1F
    .byte $33, $34, $35, $36, $37, $38, $39, $30 ; 20 - 27
    .byte $0D, $1B, $08, $00, $20, $2D, $3D, $5B ; 28 - 2F
    .byte $5D, $5C, $00, $3B, $27, $00, $2C, $2E ; 30 - 37
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 38 - 3F
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 40 - 47
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 48 - 4F
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 50 - 57
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 58 - 5F
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 60 - 67
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 68 - 6F
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 70 - 77
    .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 78 - 7F
