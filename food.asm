/*
	variables for the snakes food and code for randomly spawning it.
*/

include "snake_defs.inc"
EXPORT set_cherry
EXPORT set_pellet
EXPORT food
EXPORT cherry
EXPORT cherry_flag

SECTION "food variables", WRAM0
	/* 
	(imaginary c struct)
		struct Cherry{
			char x;
			char y;
			char* tile_addr;
			char  advance_countdown;
		}
	*/
	food: ds 4 ; x, y, vram_address
	cherry: ds CHERRY_SIZE
	cherry_flag: ds 1 ; is the cherry active	

section "food code", ROM0

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