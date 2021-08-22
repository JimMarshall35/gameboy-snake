mod2gbt\mod2gbt template.mod song
rgbasm -ogbt_player.o gbt_player.asm
rgbasm -ogbt_player_bank1.o gbt_player_bank1.asm
rgbasm -osong.o song.asm
rgbasm -o snake.o snake.asm
rgbasm -o snake_tiles.o snake_tiles.asm
rgbasm -o rng.o rng.asm
rgbasm -o main.o main.asm
rgbasm -o titlescreen_scroll.o titlescreen_scroll.asm
rgbasm -o score_display.o score_display.asm
rgbasm -o sound_effects.o sound_effects.asm
rgbasm -o input.o input.asm
rgbasm -o food.o food.asm
rgblink -o snake.gb  -m gbt_test.map -n gbt_test.sym main.o snake.o gbt_player.o gbt_player_bank1.o song.o snake_tiles.o rng.o titlescreen_scroll.o score_display.o sound_effects.o input.o food.o
rgbfix -v -p 0xFF snake.gb
del *.o
pause
bgb.exe snake.gb