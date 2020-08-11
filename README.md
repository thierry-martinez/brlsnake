# A small snake-like game for Braille display.

The game takes place in one 80-character line. The head of the snake
is initially in the middle of the line (just one dot), and the game
waits for an initial direction (use cursor keys). Then a first goal
appears randomly (just one other dot), and the snake has to be moved
towards this goal without crossing itself and without leaving the
borders of the line. Once the goal is eaten, the length of the snake
increases and another goal appears randomly.

## Usage

This game needs `dune`, a build system
for OCaml: `sudo apt install dune`.

The game can be executed by running the following command in the repository.
```
dune exec ./brlsnake.exe
```
