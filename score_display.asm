/*
	function that prints a number as its string as a hex number 
*/

INCLUDE "sprite_defs.inc"

def SCORE_HIGH_DIGIT_VRAM equ $9807
def SCORE_LOW_DIGIT_VRAM equ $9808
def HI_SCORE_HIGH_DIGIT equ $9810
def HI_SCORE_LOW_DIGIT equ $9811

section "score code", ROM0
set_score:
	; pass the score to be set to the screen in a
	push bc
	push af
		ld b, HEX_DIGITS_START
		and %00001111
		add a, b
		ld [SCORE_LOW_DIGIT_VRAM], a
	pop af
		swap a
		and %00001111
		add a, b
		ld [SCORE_HIGH_DIGIT_VRAM], a
	pop bc
	ret
EXPORT set_score