/*
        Random number code.
        
        ALL CODE BELOW IS FROM:
        http://www.devrs.com/gb/asmcode.php#random
*/
; ********************************

; *   Random Number Generation   *

; ********************************

;

;> Anyone have RGBDS (or other z80) random number code they'd like to share?

;

;  I think Luc on the GB dev web ring has some code on his page.

;

;  You can either calculate it as you go or use a lookup table.

;

;  Here are some examples for 8-bit random numbers. You should

; call one of these routines everytime a button is pressed to

; maximize randomness. Also, using the divider register ($fff4)

; helps increase randomness as well:





;* Random # - Calculate as you go *

; (Allocate 3 bytes of ram labeled 'Seed')

; Exit: A = 0-255, random number

SECTION "rng variables", WRAM0

        Seed: ds 2
        RandomPtr: ds 1

SECTION "rng code", ROM0

RandomNumber:

        ld      hl,Seed

        ld      a,[hli]

        sra     a

        sra     a

        sra     a

        xor     [hl]

        inc     hl

        rra

        rl      [hl]

        dec     hl

        rl      [hl]

        dec     hl

        rl      [hl]

        ld      a,[$fff4]          ; get divider register to increase randomness

        add     [hl]

        ret



;* Random # - Use lookup table *

; (Allocate 1 byte of ram labeled 'RandomPtr')

; Exit: A = 0-255, random number



RandomNumber_LUT:

        push    hl

        ld      a,[RandomPtr]

        inc     a

        ld      [RandomPtr],a

        ld      hl,RandTable

        add     a,l

        ld      l,a

        jr      nc,.skip

        inc     h

.skip:  ld      a,[hl]

        pop     hl

        ret



RandTable:

        db      $3B,$02,$B7,$6B,$08,$74,$1A,$5D,$21,$99,$95,$66,$D5,$59,$05,$42

        db      $F8,$03,$0F,$53,$7D,$8F,$57,$FB,$48,$26,$F2,$4A,$3D,$E4,$1D,$D9

        db      $9D,$DC,$2F,$F5,$92,$5C,$CC,$00,$73,$15,$BF,$B1,$BB,$EB,$9E,$2E

        db      $32,$FC,$4B,$CD,$A7,$E6,$C2,$10,$11,$80,$52,$B2,$DA,$77,$4F,$EC

        db      $13,$54,$64,$ED,$94,$8C,$C6,$9A,$19,$9F,$75,$FA,$AA,$8D,$FE,$91

        db      $01,$23,$07,$C1,$40,$18,$51,$76,$3C,$BD,$2A,$88,$2D,$F1,$8A,$72

        db      $F6,$98,$35,$97,$68,$93,$B3,$0C,$82,$4E,$CB,$39,$D8,$5F,$C7,$D4

        db      $CE,$AE,$6D,$A3,$7C,$6A,$B8,$A6,$6F,$5E,$E5,$1B,$F4,$B5,$3A,$14

        db      $78,$FD,$D0,$7A,$47,$2C,$A8,$1E,$EA,$2B,$9C,$86,$83,$E1,$7B,$71

        db      $F0,$FF,$D1,$C3,$DB,$0E,$46,$1C,$C9,$16,$61,$55,$AD,$36,$81,$F3

        db      $DF,$43,$C5,$B4,$AF,$79,$7F,$AC,$F9,$37,$E7,$0A,$22,$D3,$A0,$5A

        db      $06,$17,$EF,$67,$60,$87,$20,$56,$45,$D7,$6E,$58,$A9,$B0,$62,$BA

        db      $E3,$0D,$25,$09,$DE,$44,$49,$69,$9B,$65,$B9,$E0,$41,$A4,$6C,$CF

        db      $A1,$31,$D6,$29,$A2,$3F,$E2,$96,$34,$EE,$DD,$C0,$CA,$63,$33,$5B

        db      $70,$27,$F7,$1F,$BE,$12,$B6,$50,$BC,$4D,$28,$C8,$84,$30,$A5,$4C

        db      $AB,$E9,$8E,$E8,$7E,$C4,$89,$8B,$0B,$24,$85,$3E,$38,$04,$D2,$90


EXPORT RandomNumber
EXPORT RandomNumber_LUT