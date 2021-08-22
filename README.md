# gameboy-snake
![alt text](https://github.com/JimMarshall35/gameboy-snake/blob/main/titlescreen.png?raw=true)
![alt text](https://github.com/JimMarshall35/gameboy-snake/blob/main/screenshot.png?raw=true)






this is snake for the gameboy, basically complete but needs some embellishing. This repo includes the bgb emulator and tools for editing gameboy sprites - because they're both such small files.

You can move the snake around, eat pellets and get longer. You die if you crash into yourself or the edges of the screen.

The snake is part of the background tile layer to allow for a massive snake. The timer overflow interrupt is used to advance the snake in whatever direction its facing, which is whatever valid direction button was pressed last.

Made from the "Hello world" example here as a starting point: https://eldred.fr/gb-asm-tutorial/part1/hello_world.html

All code in snake.asm. 

RNG code taken from http://www.devrs.com/gb/asmcode.php#random

All artwork created by me, including fonts / numbers. The music is from the gbt-player example project.

To do:
-> make the correct tile be drawn for each part of the snake - done

-> have timed food pellets that you have to be quick to reach in time before they disappear - these could be sprites that swoop in and out from off the screen

-> add music - GTB player library added - plays the sample tune during the title screen (https://github.com/AntonioND/gbt-player)

-> add scoring, score display on the top row of tiles - done, sort of (score displayed in hex)

-> stop food from spawning in the snake (food can sometimes spawn in the exact same place it was just eaten from - making it look like the snake didn't eat the food)
