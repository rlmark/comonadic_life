# Conway's Game of Life: Scala Edition, with Comonads

### Running the app 

`sbt run` at the root of the project should allow you to view the Game of Life.

### Understanding the grid
The coordinate grid is admittedly oddly numbered, with the origin point being located at the rightmost bottom corner. 
 
There may be some undesirable edge artifacts due to the lack of periodicity in the current grid representation. 
