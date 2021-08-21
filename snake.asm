INCLUDE "hardware.inc"
INCLUDE "gbt_player.inc"

EXPORT  song_data
def VBLANK_IE_BIT equ 0
SECTION "variables", WRAM0
	def PLAYFIELD_HEIGHT equ 18
	def PLAYFIELD_WIDTH equ 20
	def SPRITE_HU equ	1
	def SPRITE_HD equ	2
	def SPRITE_HL equ	3
	def SPRITE_HR equ	4
	def SPRITE_L2R equ	14
	def SPRITE_U2D equ	13

	def SPRITE_R2D equ	10
	def SPRITE_R2U equ	11
	def SPRITE_L2D equ	9
	def SPRITE_L2U equ	12

	def SPRITE_TU equ	5
	def SPRITE_TD equ	6
	def SPRITE_TL equ	7
	def SPRITE_TR equ	8
	def SPRITE_FOOD equ 16
	def SPRITE_CHERRY equ 15
 
	def UP equ 0
	def DOWN equ 1
	def LEFT equ 2 
	def RIGHT equ 3
	def STARTX equ 9
	def STARTY equ 9

	def SNAKE_MAX equ 150
	def SNAKE_SEGMENT_SIZE equ 5 ; some code segments have this hard coded as repeated inc hl's - check_self_collision does
	def OVERFLOWS_UNTIL_MOVE equ 2 ; no of overflows from the timer before the snake moves 

	def START_SIZE equ 6

	def VBLANKS_UNTIL_SCROLL equ 6

	def BLANK_TILE_INDEX equ 47
	def EMPTY_BG_FRAMES_START_OFFSET equ 43 * 16
	def BLANK_TILE_START_OFFSET equ BLANK_TILE_INDEX * 16
	def NUM_BG_FRAMES equ 4

	def SCORE_HIGH_DIGIT_VRAM equ $9801
	def SCORE_LOW_DIGIT_VRAM equ $9802

	def HEX_DIGITS_START equ 48
	/*
		struct Segment{     // size SNAKE_SEGMENT_SIZE ie 5
			char  x;
			char  y;
			char* tile_addr; // tile address in vram
			char  tile_index;
		}
	*/
	snake_array: ds SNAKE_MAX * SNAKE_SEGMENT_SIZE
	last_tail: ds 2
	last_direction: ds 1
	move_direction: ds 1
	length: ds 1
	timer_overflow_counter: ds 1
	should_advance: ds 1
	food: ds 4 ; x, y, vram_address
	new_tile: ds 1
	last_tile: ds 1
	snake_loop_counter: ds 1
	vblank_counter: ds 1
	on_bg_frame: ds 1
	new_score_flag: ds 1
	/* 
		struct Cherry{
			char x;
			char y;
			char* tile_addr;
			char  advance_countdown;
		}
	*/
	def CHERRY_X_OFFSET equ 0
	def CHERRY_Y_OFFSET equ 1
	def CHERRY_ADDR_OFFSET equ 2
	def CHERRY_COUNTDOWN_OFFSET equ  4
	def CHERRY_LIFETIME equ 20 ; cherry lasts 10 advances of the snake
	def CHERRY_SIZE equ 5
	cherry: ds CHERRY_SIZE
	cherry_flag: ds 1 ; is the cherry active

	; random number generation
	Seed: ds 2
	RandomPtr: ds 1



SECTION	"Vblank",ROM0[$0040]
	call scroll_background
	;reti
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
	;ld a, 0
	;ld [rNR52], a

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

WaitVBlank1:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank1
	; During the first (blank) frame, initialize display registers
	ld a, %11100100
	ld [rBGP], a

	ld a, 0
	ld [vblank_counter], a
	ld [on_bg_frame], a
	ld [should_advance], a
	ld [new_score_flag], a


	; enable vblank interrupt
	ld hl, rIE
	set 0, [hl] 
	ei

	ld      de,song_data
    ld      bc,BANK(song_data)
    
    ld      a,$05
    call    gbt_play ; Play song
    ld a, 2
    call gbt_loop
title_screen:
	halt
	call    gbt_update ; Update player
	; check if start has been pressed
	ld a, [rP1]
	and %11011111
	ld [rP1], a
	ld a, [rP1]
	ld a, [rP1]
	bit 3, a
	; if it has been, break from title screen loop
	jp z, title_screen_end
	jp title_screen
title_screen_end:
	call gbt_stop

	

WaitVBlank5:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank5

	; set the bg tile to the first frame 
	ld d, 0
	ld e, 16
	ld hl, Tiles + BLANK_TILE_START_OFFSET
	ld bc, $9000
	call memcpy_tile
	; clear screen
	call clear_screen

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


	; disable vblank interrupt
	ld hl, rIE
	res 0, [hl] 
	; enable timer interrupt
	set 2, [hl]

	

	
	;ei
	
MainLoop:
	call poll_input
	; check if the timer interrupt has signalled its time to advance the snake
	ld a, [should_advance]
	cp a, 1
	; if it is, goto advance
	jp z, advance
	; if not goto mainloop
	halt
	jp MainLoop
advance:
	di
		call advance_snake
		; wait for vblank before setting the snakes new tiles
		WaitVBlank2:
		ld a, [rLY]
		cp 144
		jp c, WaitVBlank2
		call vram_set
	ei
	; set should_advance to false
	ld a, 0
	ld [should_advance], a
	; goto mainloop
	jp MainLoop

set_score:
	push bc
	push af
		ld b, HEX_DIGITS_START
		and %00001111
		add a, b
		ld [SCORE_LOW_DIGIT_VRAM], a
	pop af
		sra a
		sra a
		sra a
		sra a
		add a, b
		ld [SCORE_HIGH_DIGIT_VRAM], a
	pop bc
	ret


scroll_background:
	push af
	push hl
	push bc
	push de
		ld a, [vblank_counter]
		inc a
		ld [vblank_counter], a
		cp a, VBLANKS_UNTIL_SCROLL
		jp nz, not_scroll
scroll:
		ld a, 0
		ld [vblank_counter], a
		ld a, [on_bg_frame]
		inc a
		ld [on_bg_frame], a
		cp a, NUM_BG_FRAMES
		jp nz, no_overflow
frame_overflow:
		ld a, 0
		ld [on_bg_frame], a
no_overflow:
		ld hl, Tiles + EMPTY_BG_FRAMES_START_OFFSET
		ld b, a
		ld d, 0
		ld e, 16
tile_scroll_mul_loop:
		ld a, b
		cp a, 0
		jp z, tile_scroll_mul_loop_end
		add hl, de
		dec b
		jp tile_scroll_mul_loop
tile_scroll_mul_loop_end:
		ld bc, $9000
		call memcpy_tile
not_scroll:
	pop de
	pop bc
	pop hl
	pop af
	reti

memcpy_tile:
bg_tile_memcpy_loop:
	ld a, [hli]
	ld [bc], a
	inc bc
	dec e
	ld a, e
	cp a, 0
	jp nz, bg_tile_memcpy_loop
	ret

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


set_cherry:
	push bc
		call RandomNumber_LUT
		and %00001111
		inc a
		ld [cherry + CHERRY_X_OFFSET], a 

		call RandomNumber_LUT
		and %00001111
		inc a
		ld [cherry + CHERRY_Y_OFFSET], a 

		ld b, a
		ld a, [cherry + CHERRY_X_OFFSET]
		ld c, a

		call get_vram_from_xy
		ld h, d
		ld l, e
		ld a, h
		ld [cherry + CHERRY_ADDR_OFFSET], a
		ld a, l
		ld [cherry + CHERRY_ADDR_OFFSET + 1], a

		ld a, CHERRY_LIFETIME
		ld [cherry + CHERRY_COUNTDOWN_OFFSET], a
	pop bc
	ret

set_pellet:
	push bc
		call RandomNumber_LUT
		and %00001111
		inc a
		ld [food], a 

		call RandomNumber_LUT
		and %00001111
		inc a
		ld [food + 1], a 

		ld b, a
		ld a, [food]
		ld c, a

		call get_vram_from_xy
		ld h, d
		ld l, e
		ld a, h
		ld [food + 2], a
		ld a, l
		ld [food + 3], a
	pop bc
	ret

initialize_snake:
	ld a, 0
	ld [cherry_flag], a
	call set_score
	call set_pellet
	;call memset_snake
	ld a, START_SIZE
	ld [length], a
	ld a, 1
	ld [new_score_flag], a
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
		ld b, a  ; a is still set to the y coord - store in b
		ld c, STARTX
		call get_vram_from_xy
    pop bc
	pop hl
	; set vram pointer
	ld a, d
	ld [hli], a
	ld a, e
	ld [hli], a

	; set tile index
	ld a, SPRITE_U2D
	ld [hli], a
	
	inc b
	jp iloop
loop_exit:	
	ret

vram_set: ; set snake tiles in vram 
	ld a, [cherry_flag]
	cp a, 1
	jp nz, cherry_flag_not_set
	ld a, [cherry + CHERRY_ADDR_OFFSET]
	ld h, a
	ld a, [cherry + CHERRY_ADDR_OFFSET + 1]
	ld l, a
	ld [hl], SPRITE_CHERRY
	jp cherry_flag_set
cherry_flag_not_set:
	ld a, [cherry + CHERRY_ADDR_OFFSET]
	ld h, a
	ld a, [cherry + CHERRY_ADDR_OFFSET + 1]
	ld l, a
	ld [hl], BLANK_TILE_INDEX
cherry_flag_set:

	ld a, [new_score_flag]
	cp a, 1
	jp nz, new_score_flag_not_set
new_score_flag_set:
	ld a, [length]
	sub a, START_SIZE
	call set_score
	ld a, 0
	ld [new_score_flag], a
new_score_flag_not_set:
	
	; delete last tail
	ld hl, last_tail
	ld b, [hl]
	inc hl
	ld c, [hl]
	ld h, b
	ld l, c
	ld [hl], BLANK_TILE_INDEX

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
	inc hl
	; hl now pointing to tile index
	;dec hl
	ld b, a
	;push af ; caching a into b faster than push / pop of af
	ld a, [hl]
	ld [de], a
	dec hl
	dec hl
	;pop af
	ld a, b
	ld b, 0
	add hl, bc
	cp a, 0
	jp nz, segment_loop
segment_loop_end:
	ld a, [food + 2]
	ld h, a
	ld a, [food + 3]
	ld l, a
	ld [hl], SPRITE_FOOD
	ret



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



get_vram_from_xy:
	push af
	ld de, 0
rowsloop:
	/*
	store y coord in b and x in c
	de is loaded with the address in vram
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

    ld hl, $9800
    add hl, de
    ld d, h
    ld e, l
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


get_segment_tile:
	; this direction in b, last direction in c
	; returns the tile in a.
	; When the snake advances, this is used to pick
	; what the tile after the snakes head will be
	ld a, c
	cp a, UP
	jp z, last_up
	cp a, DOWN
	jp z, last_down
	cp a, LEFT
	jp z, last_left
	cp a, RIGHT
	jp z, last_right
last_up:
	ld a, b
	cp a, UP
	jp z, u2d
	cp a, LEFT
	jp z, u2l
	cp a, RIGHT
	jp u2r
last_down:
	ld a, b
	cp a, DOWN
	jp z, u2d
	cp a, LEFT
	jp z, d2l
	cp a, RIGHT
	jp d2r
last_left:
	ld a, b
	cp a, LEFT
	jp z, l2r
	cp a, UP
	jp z, l2u
	cp a, DOWN
	jp l2d
last_right:
	ld a, b
	cp a, RIGHT
	jp z, l2r
	cp a, UP
	jp z, r2u
	cp a, DOWN
	jp r2d

u2d:
	ld a, SPRITE_U2D
	jp get_segment_tile_end
l2r:
	ld a, SPRITE_L2R
	jp get_segment_tile_end
d2r:
l2u:
	ld a, SPRITE_L2U
	jp get_segment_tile_end
u2r:
l2d:
	ld a, SPRITE_L2D
	jp get_segment_tile_end

d2l:
r2u:
	ld a, SPRITE_R2U
	jp get_segment_tile_end
u2l:
r2d:
	ld a, SPRITE_R2D
	jp get_segment_tile_end
get_segment_tile_end:
	ret
update_cherry:
	ld a, [cherry + CHERRY_COUNTDOWN_OFFSET]
	dec a
	ld [cherry + CHERRY_COUNTDOWN_OFFSET], a
	cp a, 0
	jp nz, update_cherry_end
	ld a, 0
	ld [cherry_flag], a
update_cherry_end:
	ret

advance_snake:
	ld a, [cherry_flag]
	cp a, 1
	jp nz, cherry_flag_reset
	call update_cherry
cherry_flag_reset:
	ld a, [move_direction]

	;ld [last_direction], a
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
	ld b, UP
	ld a, [last_direction]
	ld c, a
	call get_segment_tile
	ld [new_tile], a

	ld hl, snake_array
	ld a, [hli]
	ld c, a
	ld a, [hl]
	sub a, 1
	cp a, 0
	jp z, dead
	ld b, a

	jp check_food_eaten
down:
	ld b, DOWN
	ld a, [last_direction]
	ld c, a
	call get_segment_tile
	ld [new_tile], a

	ld hl, snake_array
	ld a, [hli]
	ld c, a
	ld a, [hl]
	dec hl
	add a, 1
	cp a, PLAYFIELD_HEIGHT
	jp z, dead
	ld b, a
	jp check_food_eaten
left:
	ld b, LEFT
	ld a, [last_direction]
	ld c, a
	call get_segment_tile
	ld [new_tile], a

	ld hl, snake_array
	ld a, [hli]
	sub a, 1
	cp a, -1
	jp z, dead
	ld c, a
	ld a, [hl]
	ld b, a
	jp check_food_eaten
right:
	ld b, RIGHT
	ld a, [last_direction]
	ld c, a
	call get_segment_tile
	ld [new_tile], a

	ld hl, snake_array
	ld a, [hli]
	add a, 1
	cp a, PLAYFIELD_WIDTH
	jp z, dead
	ld c, a
	ld a, [hl]
	ld b, a
check_food_eaten:
	ld a, [move_direction]
	ld [last_direction], a
	ld a, [food]
	cp a, c
	jp z, x_food_same
	jp adv_snake_loop_setup
x_food_same:
	ld a, [food + 1]
	cp a, b
	jp z, y_food_same
	jp adv_snake_loop_setup
y_food_same:
	ld a, [length]
	inc a
	ld [length], a
	ld a, 1
	ld [new_score_flag], a
	call set_pellet
	ld a, [cherry_flag]
	cp a, 0
	jp nz, adv_snake_loop_setup

	call RandomNumber_LUT
	and %00001111
	cp a, 0
	jp nz, adv_snake_loop_setup
	
	ld a, 1
	ld [cherry_flag], a
	call set_cherry

adv_snake_loop_setup:
	ld a, 0
	ld [snake_loop_counter], a
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
			
			call get_vram_from_xy

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

			ld a, [length]
			ld a, [snake_loop_counter]
			cp a, 0
			jp z, is_head
			ld a, [hl]
			ld [last_tile], a
			push af
				; set tile index
				ld a, [new_tile]
				ld [hli], a
			pop af
			ld [new_tile], a
			jp not_head
is_head:
			ld a, [move_direction]
			cp a, UP
			jp z, set_head_u
			cp a, DOWN
			jp z, set_head_d
			cp a, LEFT
			jp z, set_head_l
			cp a, RIGHT
			jp set_head_r
set_head_u:
			ld a, SPRITE_HU
			ld [hli], a
			jp head_end
set_head_d:
			ld a, SPRITE_HD
			ld [hli], a
			jp head_end
set_head_r:
			ld a, SPRITE_HR
			ld [hli], a
			jp head_end
set_head_l:
			ld a, SPRITE_HL
			ld [hli], a
			jp head_end
not_head:
head_end:
		pop af
	pop de ; de holds old x,y pos again

	; swap de w/ bc

	ld c, d
	ld b, e ; bc now holds old position
	push af
		; get length in d for compare
		ld a, [length]
		ld d, a
	pop af
	inc a
	ld [snake_loop_counter], a
	cp a, d
	jp nz, adv_snake_loop
	call set_tail
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

set_tail:
	ld hl, snake_array
	ld a, [length]
	dec a
	dec a
	ld d, 0
	ld e, SNAKE_SEGMENT_SIZE
set_tail_mul_loop:
	add hl, de
	dec a
	cp a, 0
	jp nz, set_tail_mul_loop
	ld c, [hl]
	inc hl
	ld b, [hl]

	ld hl, snake_array
	ld a, [length]
	dec a
	ld d, 0
	ld e, SNAKE_SEGMENT_SIZE
set_tail_mul_loop2:
	add hl, de
	dec a
	cp a, 0
	jp nz, set_tail_mul_loop2

	push de
		ld a, [hli]
		ld e, a
		ld a, [hli]
		ld d, a
		inc hl
		inc hl
		inc b
		ld a, b
		cp a, d
		jp z, tail_up
		dec b
		dec b
		ld a, b
		cp a, d
		jp z, tail_down
		inc c
		ld a, c
		cp a, e
		jp z, tail_left
		dec c 
		dec c
		ld a, c
		cp a, e
		jp tail_right
tail_up:
	ld [hl], SPRITE_TU
	jp tail_end
tail_down:
	ld [hl], SPRITE_TD
	jp tail_end
tail_left:
	ld [hl], SPRITE_TL
	jp tail_end
tail_right:
	ld [hl], SPRITE_TR
tail_end:
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
	call RandomNumber_LUT
	ld a, [last_direction]
	cp a, UP
	jp z, poll_input_end
	cp a, DOWN
	jp z, poll_input_end
	ld a, UP
	ld [move_direction], a
	jp poll_input_end
down_pressed:
	call RandomNumber_LUT
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
	call RandomNumber_LUT
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
	call RandomNumber_LUT
	ld hl, move_direction
	ld a, [last_direction]
	cp a, LEFT
	jp z, poll_input_end
	cp a, RIGHT
	jp z, poll_input_end
	ld a, RIGHT
	ld [move_direction], a

poll_input_end:
	ld a, [rP1]
	and %11011111
	ld [rP1], a
	ld a, [rP1]
	ld a, [rP1]
	ret

SECTION "Tile data", ROM0

Tiles:
DB $0F,$00,$0F,$00,$0F,$00,$0F,$00
DB $F0,$00,$F0,$00,$F0,$00,$F0,$00
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
DB $00,$00,$FC,$FC,$06,$FE,$02,$FE
DB $02,$FE,$02,$FE,$C2,$FE,$42,$7E
DB $42,$7E,$C2,$FE,$02,$FE,$02,$FE
DB $02,$FE,$04,$FC,$F8,$F8,$00,$00
DB $42,$7E,$43,$7F,$40,$7F,$40,$7F
DB $40,$7F,$60,$7F,$3F,$3F,$00,$00
DB $42,$7E,$42,$7E,$42,$7E,$42,$7E
DB $42,$7E,$42,$7E,$42,$7E,$42,$7E
DB $00,$00,$FF,$FF,$00,$FF,$00,$FF
DB $00,$FF,$00,$FF,$FF,$FF,$00,$00
DB $1E,$1E,$34,$34,$64,$64,$46,$46
DB $FF,$FF,$FF,$99,$FF,$99,$77,$77
DB $18,$18,$10,$10,$38,$38,$54,$7C
DB $A2,$DE,$A2,$DE,$54,$6C,$38,$38
DB $42,$7E,$43,$7F,$40,$7F,$40,$7F
DB $40,$7F,$40,$7F,$43,$7F,$42,$7E
DB $42,$7E,$C2,$FE,$02,$FE,$02,$FE
DB $02,$FE,$02,$FE,$C2,$FE,$42,$7E
DB $FF,$FF,$81,$81,$81,$81,$F3,$F3
DB $92,$92,$92,$92,$82,$82,$FE,$FE
DB $FF,$FF,$99,$99,$F9,$F9,$99,$99
DB $98,$98,$9A,$9A,$9A,$9A,$FF,$FF
DB $E0,$E0,$20,$20,$20,$20,$20,$20
DB $20,$20,$A0,$A0,$A0,$A0,$E0,$E0
DB $FF,$FF,$93,$93,$93,$93,$93,$93
DB $83,$83,$AB,$AB,$AB,$AB,$FF,$FF
DB $FF,$FF,$18,$18,$5B,$5B,$18,$18
DB $18,$18,$5A,$5A,$5A,$5A,$FF,$FF
DB $FF,$FF,$30,$30,$B0,$B0,$33,$33
DB $F0,$F0,$3E,$3E,$30,$30,$FF,$FF
DB $FF,$FF,$49,$49,$49,$49,$C9,$C9
DB $41,$41,$49,$49,$49,$49,$FF,$FF
DB $FF,$FF,$89,$89,$A9,$A9,$89,$89
DB $89,$89,$A8,$A8,$A8,$A8,$FF,$FF
DB $F0,$F0,$90,$90,$90,$90,$90,$90
DB $DC,$DC,$44,$44,$44,$44,$FC,$FC
DB $7C,$7C,$C6,$C6,$92,$92,$B6,$B6
DB $E4,$E4,$EF,$EF,$81,$81,$FF,$FF
DB $3C,$3C,$66,$66,$C3,$C3,$99,$99
DB $99,$99,$C3,$C3,$66,$66,$3C,$3C
DB $38,$38,$68,$68,$48,$48,$68,$68
DB $28,$28,$28,$28,$28,$28,$38,$38
DB $42,$7E,$C3,$FF,$42,$FF,$42,$FF
DB $42,$FF,$42,$FF,$C3,$FF,$42,$7E
DB $00,$00,$FF,$FF,$04,$FF,$02,$FF
DB $02,$FF,$02,$FF,$C3,$FF,$42,$7E
DB $42,$7E,$C3,$FF,$40,$FF,$40,$FF
DB $40,$FF,$40,$FF,$C3,$FF,$42,$7E
DB $42,$7E,$43,$7F,$42,$7F,$42,$7F
DB $42,$7F,$42,$7F,$43,$7F,$42,$7E
DB $42,$7E,$C2,$FE,$42,$FE,$42,$FE
DB $42,$FE,$42,$FE,$C2,$FE,$42,$7E
DB $42,$7E,$7F,$7F,$C0,$FF,$00,$FF
DB $00,$FF,$C0,$FF,$7F,$7F,$42,$7E
DB $42,$7E,$42,$7E,$42,$7E,$43,$7F
DB $43,$7F,$42,$7E,$42,$7E,$42,$7E
DB $42,$7E,$FF,$FF,$00,$FF,$00,$FF
DB $00,$FF,$00,$FF,$FF,$FF,$42,$7E
DB $42,$7E,$C3,$FF,$40,$FF,$40,$FF
DB $40,$FF,$20,$FF,$FF,$FF,$00,$00
DB $42,$7E,$5F,$7F,$60,$7F,$40,$7F
DB $40,$7F,$40,$7F,$43,$7F,$42,$7E
DB $42,$7E,$C3,$FF,$40,$FF,$40,$FF
DB $40,$FF,$60,$FF,$DF,$FF,$42,$7E
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$24,$24,$18,$18,$18,$18
DB $0F,$00,$0F,$00,$0F,$00,$0F,$00
DB $F0,$00,$F0,$00,$F0,$00,$F0,$00
DB $87,$00,$87,$00,$87,$00,$78,$00
DB $78,$00,$78,$00,$78,$00,$87,$00
DB $C3,$00,$C3,$00,$3C,$00,$3C,$00
DB $3C,$00,$3C,$00,$C3,$00,$C3,$00
DB $E1,$00,$1E,$00,$1E,$00,$1E,$00
DB $1E,$00,$E1,$00,$E1,$00,$E1,$00
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $FF,$FF,$C3,$C3,$99,$99,$BD,$BD
DB $BD,$BD,$99,$99,$C3,$C3,$FF,$FF
DB $FF,$FF,$E7,$E7,$F7,$F7,$F7,$F7
DB $F7,$F7,$F7,$F7,$F7,$F7,$FF,$FF
DB $FF,$FF,$C3,$C3,$BB,$BB,$FB,$FB
DB $F3,$F3,$CF,$CF,$81,$81,$FF,$FF
DB $FF,$FF,$C3,$C3,$99,$99,$FD,$FD
DB $E1,$E1,$B9,$B9,$C3,$C3,$FF,$FF
DB $FF,$FF,$BF,$BF,$B7,$B7,$B7,$B7
DB $81,$81,$F7,$F7,$F7,$F7,$FF,$FF
DB $FF,$FF,$83,$83,$BF,$BF,$83,$83
DB $F3,$F3,$F3,$F3,$83,$83,$FF,$FF
DB $FF,$FF,$C3,$C3,$9D,$9D,$87,$87
DB $B3,$B3,$BB,$BB,$C3,$C3,$FF,$FF
DB $FF,$FF,$C1,$C1,$DD,$DD,$F9,$F9
DB $FB,$FB,$E3,$E3,$CF,$CF,$FF,$FF
DB $FF,$FF,$C3,$C3,$BD,$BD,$BD,$BD
DB $C3,$C3,$BD,$BD,$C3,$C3,$FF,$FF
DB $FF,$FF,$C1,$C1,$DD,$DD,$C1,$C1
DB $F9,$F9,$F3,$F3,$E7,$E7,$FF,$FF
DB $F7,$F7,$E3,$E3,$E9,$E9,$CD,$CD
DB $C0,$C0,$9C,$9C,$9D,$9D,$FF,$FF
DB $FF,$FF,$83,$83,$D9,$D9,$D9,$D9
DB $C3,$C3,$DD,$DD,$81,$81,$FF,$FF
DB $FF,$FF,$C3,$C3,$9D,$9D,$BF,$BF
DB $BF,$BF,$99,$99,$C3,$C3,$FF,$FF
DB $FF,$FF,$83,$83,$D9,$D9,$DD,$DD
DB $DD,$DD,$D9,$D9,$83,$83,$FF,$FF
DB $83,$83,$B9,$B9,$AD,$AD,$8F,$8F
DB $AD,$AD,$B9,$B9,$83,$83,$FF,$FF
DB $FF,$FF,$81,$81,$DD,$DD,$C7,$C7
DB $D7,$D7,$DF,$DF,$8F,$8F,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF


TilesEnd:

SECTION "Tilemap", ROM0

Tilemap:
DB $40,$30,$30,$40,$40,$40,$40,$40,$40,$40
DB $40,$40,$40,$40,$40,$40,$40,$40,$40,$40
DB $40,$40,$40,$40,$40,$40,$40,$40,$40,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$2A,$00,$00,$00,$00,$00,$01,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$01,$00,$00,$00,$00,$00
DB $0D,$01,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$0D,$00,$00,$06
DB $00,$00,$0D,$0D,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$09,$0E,$0B,$09
DB $0A,$0D,$09,$0A,$28,$0B,$09,$0E,$04,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$08,$27,$0E
DB $20,$1F,$26,$1F,$26,$1F,$29,$20,$1F,$04
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $09,$0E,$0B,$0D,$0C,$0B,$0D,$0D,$05,$05
DB $0C,$0E,$0A,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$0D,$00,$00,$02,$00,$00,$05,$02
DB $00,$00,$00,$00,$05,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$05,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$13,$14,$15
DB $16,$17,$18,$19,$1A,$1B,$0F,$00,$1C,$1D
DB $1C,$1E,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00

TilemapEnd:

/*
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