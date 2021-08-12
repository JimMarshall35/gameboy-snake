INCLUDE "hardware.inc"
def VBLANK_IE_BIT equ 0
SECTION "variables", WRAM0

	def SPRITE_HU equ	0
	def SPRITE_HD equ	1
	def SPRITE_HL equ	2
	def SPRITE_HR equ	3
	def SPRITE_L2R equ	13
	def SPRITE_U2D equ	12
	def SPRITE_R2D equ	8
	def SPRITE_R2U equ	11
	def SPRITE_L2D equ	9
	def SPRITE_L2U equ	10
	def SPRITE_TU equ	4
	def SPRITE_TD equ	5
	def SPRITE_TL equ	6
	def SPRITE_TR equ	7
 
	def UP equ 0
	def DOWN equ 1
	def LEFT equ 2 
	def RIGHT equ 3
	def STARTX equ 9
	def STARTY equ 9

	def SNAKE_MAX equ 100
	def SNAKE_SEGMENT_SIZE equ 4 ; some code segments have this hard coded as repeated inc hl's - check_self_collision does
	def OVERFLOWS_UNTIL_MOVE equ 3 ; no of overflows from the timer before the snake moves 
	/*
		struct Segment{     // size SNAKE_SEGMENT_SIZE ie 4
			char  x;
			char  y;
			char* tile_addr; // tile address in vram
		}
	*/
	snake_array: ds SNAKE_MAX * SNAKE_SEGMENT_SIZE
	last_tail: ds 2
	last_direction: ds 1
	move_direction: ds 1
	length: ds 1
	snake_changed: ds 1
	timer_overflow_counter: ds 1
	should_advance: ds 1

SECTION	"Vblank",ROM0[$0040]
	;call set_snake_tiles
	reti
	
SECTION	"stat",ROM0[$0048]
	reti

SECTION	"timer",ROM0[$0050]
	call timer_overflow

SECTION "serial", ROM0[$0058]
	reti

SECTION "joypad", ROM0[$0060]
	reti

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; Make room for the header



EntryPoint:
	; Shut down audio circuitry
	ld a, 0
	ld [rNR52], a

	; Do not turn the LCD off outside of VBlank
WaitVBlank:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank

	; Turn the LCD off
	ld a, 0
	ld [rLCDC], a

	; Copy the tile data
	ld de, Tiles
	ld hl, $9000
	ld bc, TilesEnd - Tiles
CopyTiles:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, CopyTiles

	; Copy the tilemap
	ld de, Tilemap
	ld hl, $9800
	ld bc, TilemapEnd - Tilemap
CopyTilemap:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, CopyTilemap

	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON
	ld [rLCDC], a

	;initialize snake to its starting state
	call initialize_snake

	; configure timer - enabled at ~4194Hz 16 overflows per second (4194 / 256)  
	ld hl, rTAC
	ld [hl], 0
	set 2, [hl]
	ld hl, timer_overflow_counter
	ld [hl], 0


	ld   hl,$0FF41    ;-STAT Register
wait:            ;\
	 
	bit  1,[hl]       ; Wait until Mode is 0 or 1
	jr   nz,wait    ;/


	; enable vblank interrupt
	ld hl, rIE
	set 0, [hl] 
	; enable timer interrupt
	set 2, [hl]
	
WaitVBlank1:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank1
	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ld [rBGP], a

	ei
	ld a, 0
	ld [should_advance], a
MainLoop:
	call poll_input
	; check if the timer interrupt has signalled its time to advance the snake
	ld a, [should_advance]
	cp a, 1
	; if it is, goto advance
	jp z, advance
	; if not goto mainloop
	jp MainLoop
advance:
	di
		call advance_snake
		; wait for vblank before setting the snakes new tiles
		WaitVBlank2:
		ld a, [rLY]
		cp 144
		jp c, WaitVBlank2
		call set_snake_tiles
	ei
	; set should_advance to false
	ld a, 0
	ld [should_advance], a
	; goto mainloop
	jp MainLoop

memset_snake:
	ld hl, snake_array
memset_loop:
	ld a, $ff
	ld [hli], a
	ld a, h
	cp a, HIGH(snake_array + (SNAKE_MAX * SNAKE_SEGMENT_SIZE))
	jp z, h_matches
	jp memset_loop
h_matches:
	ld a, l
	cp a, LOW(snake_array + (SNAKE_MAX * SNAKE_SEGMENT_SIZE))
	jp z, l_matches
	jp memset_loop
l_matches:
	ret

initialize_snake:
	;call memset_snake
	ld a, 6
	ld [length], a
	ld a, UP
	ld [move_direction], a

	ld a, UP
	ld [last_direction], a

	ld b, 0
	ld hl, snake_array
iloop:
	ld a, 4
	cp a, b
	jp z, loop_exit
	ld a, STARTX
	ld [hli], a ; set x
	ld a, STARTY
	add a, b
	ld [hli], a ; set y

	push hl
	push bc
	ld h, $98
	ld l, $00
	ld de, 0

	ld b, a  ; a is still set to the y coord - store in b
	ld c, STARTX

	call get_index

    add hl, de
    ld d, h
    ld e, l
    pop bc
	pop hl
	ld a, d
	ld [hli], a
	ld a, e
	ld [hli], a
	
	inc b
	jp iloop
loop_exit:
	ld a, 0
	ld [snake_changed], a
	
	ret

set_snake_tiles: ; set snake tiles in vram 
	; delete last tail
	ld hl, last_tail
	ld b, [hl]
	inc hl
	ld c, [hl]
	ld h, b
	ld l, c
	ld [hl], 0

	; prepare for loop
	ld a, [length]
	ld hl, snake_array + 2
	ld b, 0
	ld c, SNAKE_SEGMENT_SIZE
segment_loop: 
	dec a
	ld d, [hl]
	inc hl
	ld e, [hl]
	dec hl
	ld b, a
	;push af ; caching a into b faster than push / pop of af
	ld a, 1
	ld [de], a
	;pop af
	ld a, b
	ld b, 0
	add hl, bc
	cp a, 0
	jp nz, segment_loop
segment_loop_end:
	; the snake has been drawn - set the snake_changed flag to false
	ld a, 0
	ld [snake_changed], a
set_snake_end:	
	reti



clear_screen: 
	; turn off lcd
	ld a, 0
	ld [rLCDC], a
	; clear from 2nd row
	ld hl, $9820
cls_loop:
	ld a, 0
	ld [hli], a
	ld a, h
	cp a, $9a
	jp nz, cls_loop
	ld a, l
	cp a, $5f
	jp nz, cls_loop
	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON
	ld [rLCDC], a
	ret



get_index:
	push af
rowsloop:
	/*
	store y coord in b and x in c
	de is loaded with offset from
	start of tile map vram, add to $9800
	to get the memory address the tile
	should go in
	*/
	ld a, $20     ; $20 (ie 32) is size of tilemap row in vram
	add   a, e    ; A = A+L
    ld    e, a    ; L = A+L
    adc   a, d    ; A = A+L+H+carry
    sub   e       ; A = H+carry
    ld    d, a    ; H = H+carry
    dec b
    ld a, b
    cp a, 0
	jp z, rowsloopend
	jp rowsloop
rowsloopend:
	ld a, c
	add   a, e    ; A = A+L
    ld    e, a    ; L = A+L
    adc   a, d    ; A = A+L+H+carry
    sub   e       ; A = H+carry
    ld    d, a    ; H = H+carry
    pop af
    ret



; timer ISR
timer_overflow:
	push af
	push hl
	push bc
	push de
		; OVERFLOWS_UNTIL_MOVE - advance snake will be called when
		; the timer overflows this many times. then the overflow counter
		; will be set back to 0. Timer is ~4194Hz
		ld a, [timer_overflow_counter]
		inc a
		ld [timer_overflow_counter], a
		cp a, OVERFLOWS_UNTIL_MOVE
		jp nz, timer_overflow_end
		ld a, 0
		ld [timer_overflow_counter], a
		ld a, [should_advance]
		ld a, 1
		ld [should_advance], a
timer_overflow_end:
	pop de
	pop bc
	pop hl
	pop af
	reti

advance_snake:
	di
	ld hl, snake_changed
	ld a, [hl]
	cp a, 0
	jp nz, advance_snake_end

	ld a, [move_direction]
	ld [last_direction], a
	cp a, UP
	jp z, up
	cp a, DOWN
	jp z, down
	cp a, LEFT
	jp z, left
	cp a, RIGHT
	jp z, right
	jp advance_snake_end
	/*
		store the new head position in bc (y, x).
		if out of bounds goto advance snake end (for now)
	*/
up:
	ld hl, snake_array
	ld a, [hli]
	ld c, a
	ld a, [hl]
	sub a, 1
	cp a, 0
	jp z, dead
	ld b, a
	jp adv_snake_loop_setup
down:
	ld hl, snake_array
	ld a, [hli]
	ld c, a
	ld a, [hl]
	dec hl
	add a, 1
	cp a, 18
	jp z, dead
	ld b, a
	jp adv_snake_loop_setup
left:
	ld hl, snake_array
	ld a, [hli]
	sub a, 1
	cp a, -1
	jp z, dead
	ld c, a
	ld a, [hl]
	ld b, a
	jp adv_snake_loop_setup
right:
	ld hl, snake_array
	ld a, [hli]
	add a, 1
	cp a, $14
	jp z, dead
	ld c, a
	ld a, [hl]
	ld b, a
adv_snake_loop_setup:
	
	ld a, 0
	ld hl, snake_array
adv_snake_loop:
	; store old x,y position in de
	ld d, [hl]
	inc hl
	ld e, [hl]
	dec hl
	; set new position (from bc)
	push af
		ld a, c
		ld [hli], a
		ld a, b
		ld [hli], a
	pop af

	push de ; de holds old position
		push hl ; hl holds ptr into the array, pointing the the vram pointer of this iteration
			; get new vram addr in de
			ld de, 0
			call get_index
			ld hl, $9800
			add hl, de
			ld d, h
			ld e, l
		pop hl
		push af
			ld a, [hli]
			ld [last_tail], a
			ld a, [hl]
			ld [last_tail + 1], a
			dec hl 
			; set new vram ptr
			ld a, d
			ld [hli], a
			ld a, e
			ld [hli], a 
		pop af
	pop de ; de holds old x,y pos again
	; swap de w/ bc
	ld c, d
	ld b, e ; bc now holds old position
	push hl
		; get length in d for compare
		ld hl, length
		ld d, [hl]
	pop hl
	inc a
	cp a, d
	jp nz, adv_snake_loop
	; set snake_changed flag
	ld hl, snake_changed
	ld [hl], 1
advance_snake_end:
	ld hl, snake_array
	ld c, [hl]
	inc hl
	ld b, [hl]
	call check_self_collision
	cp a, 1
	jp z, dead
	ret
dead:
	
	call initialize_snake
	call clear_screen
	ret

check_self_collision:
	;  y in b and x in c
	push de
	push hl

		ld hl, snake_array + SNAKE_SEGMENT_SIZE*2 ; ptr to 2 tiles after head
		ld d, 0
		ld a, [length]
		sub a, 2
		ld e, a
self_collision_loop:
		
		; check head against this segment
		ld a, [hli]
		cp a, c
		jp z, same_x
		
mid_loop:
		inc hl
		; increment ptr
		inc hl
		inc hl
		inc d
		ld a, d
		cp a, e
		jp nz, self_collision_loop

		jp end

same_x:
		ld a, [hl]
		cp a, b
		jp z, same_y
		jp mid_loop
same_y:
		ld a, 1
		jp end
no_collision:
		ld a, 0
		jp end
end:
	pop hl
	pop de
	ret

poll_input:
	
	ld a, [rP1]
	and %11101111
	ld [rP1], a
	ld a, [rP1]
	ld a, [rP1]

	
	bit 0, a
	jp z, right_pressed
	bit 1, a
	jp z, left_pressed
	bit 2, a
	jp z, up_pressed
	bit 3, a
	jp z, down_pressed

	jp poll_input_end
up_pressed:
	ld a, [last_direction]
	cp a, UP
	jp z, poll_input_end
	cp a, DOWN
	jp z, poll_input_end
	ld a, UP
	ld [move_direction], a
	jp poll_input_end
down_pressed:
	ld hl, move_direction
	ld a, [last_direction]
	cp a, UP
	jp z, poll_input_end
	cp a, DOWN
	jp z, poll_input_end
	ld a, DOWN
	ld [move_direction], a
	jp poll_input_end
left_pressed:
	ld hl, move_direction
	ld a, [last_direction]
	cp a, LEFT
	jp z, poll_input_end
	cp a, RIGHT
	jp z, poll_input_end
	ld a, LEFT
	ld [move_direction], a
	jp poll_input_end
right_pressed:
	ld hl, move_direction
	ld a, [last_direction]
	cp a, LEFT
	jp z, poll_input_end
	cp a, RIGHT
	jp z, poll_input_end
	ld a, RIGHT
	ld [move_direction], a

poll_input_end:
	ret

SECTION "Tile data", ROM0

Tiles:
	DB $00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00
	DB $18,$18,$24,$3C,$42,$7E,$A5,$FF
	DB $A5,$FF,$A5,$FF,$81,$FF,$42,$7E
	DB $42,$7E,$81,$FF,$A5,$FF,$A5,$FF
	DB $A5,$FF,$42,$7E,$24,$3C,$18,$18
	DB $1E,$1E,$21,$3F,$5C,$7F,$80,$FF
	DB $80,$FF,$5C,$7F,$21,$3F,$1E,$1E
	DB $78,$78,$84,$FC,$3A,$FE,$01,$FF
	DB $01,$FF,$3A,$FE,$84,$FC,$78,$78
	DB $42,$7E,$42,$7E,$42,$7E,$42,$7E
	DB $42,$7E,$42,$7E,$24,$3C,$18,$18
	DB $18,$18,$24,$3C,$42,$7E,$42,$7E
	DB $42,$7E,$42,$7E,$42,$7E,$42,$7E
	DB $00,$00,$FC,$FC,$02,$FE,$01,$FF
	DB $01,$FF,$02,$FE,$FC,$FC,$00,$00
	DB $00,$00,$3F,$3F,$40,$7F,$80,$FF
	DB $80,$FF,$40,$7F,$3F,$3F,$00,$00
	DB $00,$00,$1F,$1F,$20,$3F,$40,$7F
	DB $40,$7F,$40,$7F,$43,$7F,$42,$7E
	DB $00,$00,$F8,$F8,$04,$FC,$02,$FE
	DB $02,$FE,$02,$FE,$C2,$FE,$42,$7E
	DB $42,$7E,$C2,$FE,$02,$FE,$02,$FE
	DB $02,$FE,$04,$FC,$F8,$F8,$00,$00
	DB $42,$7E,$43,$7F,$40,$7F,$40,$7F
	DB $40,$7F,$20,$3F,$1F,$1F,$00,$00
	DB $42,$7E,$42,$7E,$42,$7E,$42,$7E
	DB $42,$7E,$42,$7E,$42,$7E,$42,$7E
	DB $00,$00,$FF,$FF,$00,$FF,$00,$FF
	DB $00,$FF,$00,$FF,$FF,$FF,$00,$00
TilesEnd:

SECTION "Tilemap", ROM0

Tilemap:
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0
TilemapEnd:
