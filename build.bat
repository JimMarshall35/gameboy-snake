rgbasm -L -o snake.o snake.asm
rgblink -o snake.gb snake.o
rgbfix -v -p 0xFF snake.gb
PAUSE