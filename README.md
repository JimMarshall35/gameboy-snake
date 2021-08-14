# gameboy-snake
this is snake for the gameboy, it's still a work in progress.

The basic game is complete, you can move the snake around, eat pellets and get longer. You die if you crash into yourself or the edges of the screen.

The snake is part of the background tile layer to allow for a massive snake. The timer overflow interrupt is used to advance the snake in whatever direction its facing, which is whatever valid direction button was pressed last.

Made from the "Hello world" example here as a starting point: https://eldred.fr/gb-asm-tutorial/part1/hello_world.html

To do:
-> make the correct tile be drawn for each part of the snake - mostly done -except for the tail

-> have timed food pellets that you have to be quick to reach in time before they disappear - these could be sprites that swoop in and out from off the screen

-> add music

-> add a menu screen

-> add scoring, score display on the top row of tiles

-> stop food from spawning in the snake (food can sometimes spawn in the exact same place it was just eaten from - making it look like the snake didn't eat the food)
