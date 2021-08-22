def PLAYFIELD_HEIGHT equ 18
def PLAYFIELD_WIDTH equ 20


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


def CHERRY_X_OFFSET equ 0
def CHERRY_Y_OFFSET equ 1
def CHERRY_ADDR_OFFSET equ 2
def CHERRY_COUNTDOWN_OFFSET equ  4
def CHERRY_LIFETIME equ 20 ; cherry lasts 10 advances of the snake
def CHERRY_SIZE equ 5