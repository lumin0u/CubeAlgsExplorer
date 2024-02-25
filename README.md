# CubeAlgsExplorer
## Overview
This project tries to be a sequel to Herbert Kociemba's Cube Explorer. The goal is to generate algorithms (maneuvers) to solve the rubik's cube from a given position or a set of positions. The point is that CubeAlgsExplorer sorts and filters algorithms based on its estimate on *the time one actually takes to do the maneuver*. This program uses Kociemba's optimal solver, and part of its source code in python was translated into OCaml to integrate it directly in the program.

## Notes
- The project is not yet finished and thus is still not working.
- The program needs a server running on port 8084 that solves cubes, which does not need to provide especially 'good' algorithms. I am using Kociemba's TwoPhaseSolver (https://github.com/hkociemba/RubiksCube-TwophaseSolver) as it is really fast to find solves of any size. I hope the server will one day be unnecessary, but that means translating a huge part of Kociemba's OptimalSolver code in OCaml.
- The program uses libraries to load/save csv files and to generate a gui which you can install using:
```bash
$ opam install csv
$ opam install lablgtk3
```
