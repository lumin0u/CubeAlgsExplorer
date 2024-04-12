# CubeAlgsExplorer
## Overview
This project tries to be a sequel to Herbert Kociemba's Cube Explorer. The goal is to generate algorithms (maneuvers) to solve the rubik's cube from a given position or a set of positions. The point is that CubeAlgsExplorer sorts and filters algorithms based on its estimate on *the time one actually takes to do the maneuver*.

## Notes
- The project is not yet finished and thus is still not working.
- The program needs a server running on port 8085 that gives, for each cube position aked, an heuristic of the number of moves needed to solve it (I hope one day i'll do without it).
- The program uses libraries to load/save csv files and to generate a gui which you can install using:
```bash
$ opam install csv
$ opam install lablgtk3
```
