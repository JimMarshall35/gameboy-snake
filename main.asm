/*
	Initialization code and main loop
*/

INCLUDE "hardware.inc"
INCLUDE "gbt_player.inc"

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

SECTION "main", ROM0

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
	ld de, title_screen_map
	ld hl, $9800
	ld bc, title_screen_map_end - title_screen_map
	call CopyTilemap

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

	call init_scroll_effect_vars
	call init_snake_vars


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

	; set the bg tile to the blank tile 
	ld d, 0
	ld e, 16
	ld hl, Tiles + BLANK_TILE_START_OFFSET
	ld bc, $9000
	call memcpy_tile
	; clear screen
	;call clear_screen
	ld de, game_screen_map
	ld hl, $9800
	ld bc, game_screen_map_end - game_screen_map

	call CopyTilemap
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

	ld a, $ff
	ld [rNR52], a

	ld a, %01110111
	ld [rNR50], a
	ld a, $ff
	ld [rNR51], a
	
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



CopyTilemap:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, CopyTilemap
	ret