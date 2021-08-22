/*
	code for the scrolling checkerboard effect on the title screen.
	The blank background tile is animated to make it look like the background is in two layers with one scrolling diagonally.
*/

INCLUDE "sprite_defs.inc"
EXPORT scroll_background
EXPORT memcpy_tile
EXPORT init_scroll_effect_vars


def VBLANKS_UNTIL_SCROLL equ 6
SECTION "scroll effect variables", WRAM0
	vblank_counter: ds 1
	on_bg_frame: ds 1
SECTION "scroll effect code", ROM0

init_scroll_effect_vars:
	ld a, 0
	ld [vblank_counter], a
	ld [on_bg_frame], a
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