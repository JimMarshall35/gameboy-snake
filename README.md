# gameboy-snake
this is snake for the gameboy, it's still a work in progress.

The basic game is complete, you can move the snake around, eat pellets and get longer. You die if you crash into yourself or the edges of the screen.

The snake is drawn as part of the background tile layer to allow for a massive snake, and the timer overflow interrupt is used to advance the snake in whatever direction its facing. I've drawn different sprites for different parts of the snake but at the moment all segments of the snake are just drawn as the head.

Made from the "Hello world" example here as a starting point: https://eldred.fr/gb-asm-tutorial/part1/hello_world.html

To do:
-> make the correct tile for be drawn for each part of the snake
-> have timed food pellets that you have to be quick to reach in time before they disappear - these could be sprites that swoop in and out from off the screen
-> add music
-> add a menu screen
-> add scoring, score display on the top row of tiles
