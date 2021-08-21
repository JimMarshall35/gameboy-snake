mod2gbt\mod2gbt template.mod song
rgbasm -ogbt_player.o gbt_player.asm
rgbasm -ogbt_player_bank1.o gbt_player_bank1.asm
rgbasm -osong.o song.asm
rgbasm -o snake.o snake.asm
rgblink -o snake.gb  -m gbt_test.map -n gbt_test.sym snake.o gbt_player.o gbt_player_bank1.o song.o
rgbfix -v -p 0xFF snake.gb
PAUSE
bgb.exe snake.gb