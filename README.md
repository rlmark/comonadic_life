# Conway's Game of Life: Scala Edition, with Comonads

### Running the app

`sbt run` at the root of the project should allow you to view the Game of Life.

### Understanding the grid
The coordinate grid is oriented with the origin point being located at the rightmost bottom corner.

```
. . . . 4
. . . . 3
. . . . 2
. . . . 1
4 3 2 1 0
``` 
 
Items are placed on the grid via X and Y coordinates starting at their rightmost bottom corner. 

There may be some undesirable edge artifacts due to the lack of periodicity in the current grid representation. 
