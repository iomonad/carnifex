<p align="center">
  <img src="https://raw.githubusercontent.com/iomonad/carnifex/master/.github/conway.gif"/><br>
<br>
<br>
</p>

# Carnifex [![HitCount](http://hits.dwyl.io/iomonad/carnifex.svg)](http://hits.dwyl.io/iomonad/carnifex)
> Conway's Game of Life automaton written in Common Lisp using SDL for graphic rendering.
## Installation
### Requierements
 - SBCL >= 1.3.21
 - Quicklisp
 - LibSDL >= 1.2.15-r9

## What is carnifex ?
Conway’s automaton, or *“Game Of Life”*, is a cellular automaton whose initial input configuration determines its evolution. The game is played on a virtually infinit grid of cells, each cell beeing either alive or dead. The life or death of a cell is determined by the state of its 8 neighbors according to a set of simple rules. Have a look to these links to get a better idea

The concept was originally discovered in the 1940s by Stanislaw Ulam and John von Neumann while they were contemporaries at Los Alamos National Laboratory. While studied by some throughout the 1950s and 1960s, it was not until the 1970s and *Conway's Game of Life*, a two-dimensional cellular automaton, that interest in the subject expanded beyond academia.

## Features
### Standard rules:
- Any live cell with fewer than two live neighbours dies, as if caused by under- population.
- Any live cell with two or three live neighbours lives on to the next generation.
- Any live cell with more than three live neighbours dies,as if by over-population.
- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
### Live interaction
- It's possible to skim through your grid using the `w`, `a`, `s` and `d` keys if the grid is larger than the display window. This behaviour is also available by draging the display with the mouse.
- It's possible to zoom the display in and out by using the `+` and `-` keys. This behaviour is also be available by using the mouse wheel.
- Pressing the `p` key pauses time. Pressing it again unpauses time. The game starts paused.
- It's possible to slow and speed up time by using the `<` and `>` keys. This behaviour must also be available by using the mouse wheel while holding the `shift` key.
- You can set a cell alive or dead by *clicking* on it. Thus when launching the program with an empty grid, one can create an initial state using the mouse.
- It's possible to reset the game to an empty grid by pressing the `r` key. Reseting the game also pauses it.
### Options
- Using `-i | --invert` invert the colorscheme of the grid, but also slowdown the rendering of the map.
- Coloring dead cells that were alive at least once to have a visal representation of the "fingerprint" of the intial state after stabilization. You can use this feature when `-t | --traces` is set.
### Missing features
- The *hashlife* algorithm
## Commons patterns
### Oscillators:
<p align="center">
  <img src="https://raw.githubusercontent.com/iomonad/carnifex/master/.github/oscillator.gif"/><br>
</p>

## Usage
```bash
./carnifex <dimx:int> <dimy:int> [-h --help -i --invert -t --traces -d --debug]
```

## License

The Carnifex project is licensed under the terms of the MIT license.

## Links

* [42.fr](http://www.42.fr/)
* [Quicklisp](https://www.quicklisp.org/beta/)
* [ClDoc](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
* [GoL Stanford](http://web.stanford.edu/~cdebs/GameOfLife/)
* [GoL NiH](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4905947/)
* [Wiki](https://en.wikipedia.org/wiki/Cellular_automaton)
