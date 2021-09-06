
INCLUDE "hardware.inc"
EXPORT eaten_sound_effect
EXPORT die_sound_effect


SECTION "sound effects code", ROM0

eaten_sound_effect:
	; set sweep period
	ld a, $15             ; 21
	ld [rNR10], a
	; set length, duty
	ld a, $96             ; 150
	ld [rNR11], a
	; set volume, envelope
	ld a, $73	      ; 115
	ld [rNR12], a
	;set frequency
	ld a, $bb             ; 187
	ld [rNR13], a
	; enable length for sound chip
	ld a, $85             ; 133
	ld [rNR14], a
	ret

die_sound_effect:
	; set sweep period
	ld a, $4f              ; 79
	ld [rNR10], a
	; set length, duty
	ld a, $96              ; 150
	ld [rNR11], a
	; set volume, envelope
	ld a, $b7              ; 183
	ld [rNR12], a
	;set frequency
	ld a, $bb              ; 187
	ld [rNR13], a
	; enable length for sound chip
	ld a, $85              ; 133 
	ld [rNR14], a
	ret