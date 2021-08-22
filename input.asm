/*
	variables for last direction and current direction, and code to poll input and set them
*/

INCLUDE "hardware.inc"
INCLUDE "snake_defs.inc"
EXPORT last_direction
EXPORT move_direction
EXPORT poll_input

SECTION "input variables", WRAM0
	last_direction: ds 1
	move_direction: ds 1

SECTION "input code", ROM0
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