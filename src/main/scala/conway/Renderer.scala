package conway

import catlike.data.GridZipper
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._
import cats.effect.{Sync, Timer}
import cats.syntax.all._
import conway.Main.Coordinates

import scala.sys.process._
import scala.util.Try

class Console[F[_] : Sync] {
  def getVisualization: F[Visualization] = {
    for {
      _ <- Sync[F].delay(println("Pick a visualization style:"))
      _ <- Sync[F].delay(println("Enter 1 for Ocean, 2 for Forest"))
      s <- Sync[F].delay(scala.io.StdIn.readLine)
    } yield (
      if (s == "1") Visualization.Ocean else Visualization.Forest
      )
  }

  def getWidth: F[Int] = {
    for {
      _ <- Sync[F].delay(println("How wide do you want your board to be"))
      _ <- Sync[F].delay(println("Max is 100, min is 3"))
      s <- Sync[F].delay(scala.io.StdIn.readLine)
      int <- Sync[F].fromTry(Try(s.toInt))
      sanitizedInt <- {
        val greaterThan3 = math.max(int, 3)
        val lessThan100 = math.min(greaterThan3, 100)
        Sync[F].delay(lessThan100)
      }
    } yield (sanitizedInt)
  }

  def getUserShape: F[Map[Coordinates, Int]] = {
    for {
      _ <- Sync[F].delay(println("Select a shape"))
      _ <- Sync[F].delay(println("Enter 1 for Blinker, 2 for Glider"))
      s <- Sync[F].delay(scala.io.StdIn.readLine)
      int <- Sync[F].fromTry(Try(s.toInt))
    } yield (
      if (int == 1) Swarms.blinker
      else if (int == 2) Swarms.glider
      else Swarms.dieHard
      )
  }

  def getCoordinates: F[Coordinates] = {
    for {
      _ <- Sync[F].delay(println("Where do you want your shape?"))
      _ <- Sync[F].delay(println("Enter in format X,Y"))
      s <- Sync[F].delay(scala.io.StdIn.readLine)
      coordinates <- Sync[F].fromTry {
        Try {
          val intList: List[Int] = s.split(",").toList.map(_.toInt)
          val x :: y :: Nil = intList
          (x, y)
        }
      }
    } yield (coordinates)
  }

  import Main.InitOps
  def setShapeAtCoordinates(): F[Map[Coordinates, Int]] = {
    for {
      userShape <- getUserShape
      coordinates <- getCoordinates
    } yield userShape.at(coordinates)
  }

  def loop(acc : Map[Coordinates, Int]): F[Map[Coordinates, Int]] = {
    for {
      map <- setShapeAtCoordinates()
      _ <- Sync[F].delay(println( "Press 0 when done setting board, press any other Int to continue"))
      s <- Sync[F].delay(scala.io.StdIn.readLine)
      map2 <- if(s.toInt == 0) Sync[F].delay(acc ++ map) else loop(acc ++ map)
    } yield(map2)
  }
}
  class Renderer[F[_] : Timer : Sync](visualization: Visualization) {

    def clear: F[Int] = Sync[F].delay("clear".!)

    def cellRepresentation(value: Int): String = {
      val alive = visualization.alive
      val background = visualization.background
      if (value == 1) alive else background
    }

    def format(grid: GridZipper[Int]): String = {
      grid.map(i => cellRepresentation(i))
        .value
        .map(_.toList)
        .toList
        .map(_.mkString)
        .mkString("\n")
    }

    def renderFrame(gridZipper: GridZipper[Int]): F[Unit] = {
      for {
        _ <- clear
        _ <- Sync[F].delay(println(format(gridZipper)))
      } yield ()
    }
  }
