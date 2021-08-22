
INCLUDE "hardware.inc"
EXPORT eaten_sound_effect
EXPORT die_sound_effect


SECTION "sound effects code", ROM0

eaten_sound_effect:
	; set sweep period
	ld a, $15
	ld [rNR10], a
	; set length, duty
	ld a, $96
	ld [rNR11], a
	; set volume, envelope
	ld a, $73
	ld [rNR12], a
	;set frequency
	ld a, $bb
	ld [rNR13], a
	; enable length for sound chip
	ld a, $85
	ld [rNR14], a
	ret

die_sound_effect:
	; set sweep period
	ld a, $4f
	ld [rNR10], a
	; set length, duty
	ld a, $96
	ld [rNR11], a
	; set volume, envelope
	ld a, $b7
	ld [rNR12], a
	;set frequency
	ld a, $bb
	ld [rNR13], a
	; enable length for sound chip
	ld a, $85
	ld [rNR14], a
	ret