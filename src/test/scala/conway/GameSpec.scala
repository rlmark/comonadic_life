package conway

import conway.data.GridZipper
import conway.Game.cellLifecycle
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.coflatMap._

class GameSpec extends FlatSpec with Matchers {

  def setGridState(width: Int, desiredActiveCells: List[Coordinates])(test: GridZipper[Int] => Unit): Unit = {
    val coordMap: Map[(Int, Int), Int] = desiredActiveCells.map(coordinates => coordinates -> 1).toMap
    val initialGridState: GridZipper[Int] = Main.buildGrid((coords) => coordMap.getOrElse(coords, 0), width)
    println("INITIAL GRID STATE")
    println(initialGridState.prettyPrint + "\n")
    test(initialGridState)
  }

  "cellLifecycle" should "keep board of all 0's the same" in setGridState(width = 3, List.empty){ grid =>
    val result = grid.coflatMap(cellLifecycle)
    println(result.prettyPrint)
    result.value.toList.flatMap(_.toList).sum shouldBe 0
  }
  it should "change a live cell to dead when no surrounding cells are alive" in setGridState(5, List((2,2))){grid =>
    val result = grid.coflatMap(cellLifecycle)
    println(result.prettyPrint)
    result.value.toList.flatMap(_.toList).sum shouldBe 0
  }
  it should "change dead cell to alive when surrounded by three live cells" in setGridState(5, List((2,2), (3,3), (3,1))){ grid =>
    val result = grid.coflatMap(cellLifecycle)
    println(result.prettyPrint)
    result.value.toList.flatMap(_.toList).sum shouldBe 2
  }
  it should "create a stable blinker with period of 2" in setGridState(5, List((2,2), (2,3), (2,1))){ grid =>
    val gen2 = grid.coflatMap(cellLifecycle)
    println(gen2.prettyPrint + "\n")
    val gen3 = gen2.coflatMap(cellLifecycle)
    println(gen3.prettyPrint)
    gen3 shouldBe grid
  }

}
