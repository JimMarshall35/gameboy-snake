/*
	Code for the snakes movement
*/

INCLUDE "hardware.inc"
INCLUDE "gbt_player.inc"
INCLUDE "sprite_defs.inc"
INCLUDE "snake_defs.inc"

EXPORT song_data
EXPORT should_advance
EXPORT advance_snake
EXPORT clear_screen
EXPORT timer_overflow_counter

EXPORT init_snake_vars
EXPORT vram_set
EXPORT initialize_snake
EXPORT BLANK_TILE_START_OFFSET
EXPORT timer_overflow
EXPORT get_vram_from_xy


def VBLANK_IE_BIT equ 0
SECTION "variables", WRAM0
	

	
	/*
	(imaginary c struct)
		struct Segment{     // size SNAKE_SEGMENT_SIZE ie 5
			char  x;
			char  y;
			char* tile_addr; // tile address in vram
			char  tile_index;
		}
	*/
	snake_array: ds SNAKE_MAX * SNAKE_SEGMENT_SIZE
	last_tail: ds 2
	length: ds 1
	timer_overflow_counter: ds 1
	should_advance: ds 1
	
	new_tile: ds 1
	last_tile: ds 1
	snake_loop_counter: ds 1
	
	new_score_flag: ds 1
	
SECTION "snake code", ROM0

init_snake_vars:
	ld a,0
	ld [should_advance], a
	ld [new_score_flag], a
	ret




initialize_snake:
	ld a, 0
	ld [cherry_flag], a
	call set_score
	call set_pellet
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
	call eaten_sound_effect
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
	call die_sound_effect
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







