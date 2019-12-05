package conway

import cats.effect.Sync
import cats.syntax.all._

import scala.util.Try

class Console[F[_] : Sync] {
  private def readInt: F[Int] = {
    for {
      s <- Sync[F].delay(scala.io.StdIn.readLine)
      int <- Sync[F].fromTry(Try(Math.abs(s.trim.toInt))).handleErrorWith(_ =>
        Sync[F].delay(println("Input should be an Integer"))
          .flatMap(_ => readInt)
      )
    } yield int
  }

  def getVisualization: F[Visualization] = {
    for {
      _ <- Sync[F].delay(println("Pick a visualization style"))
      _ <- Sync[F].delay(println("Enter 1 for Ocean, 2 for Forest"))
      int <- readInt
      vis <- {
        val v: F[Visualization] = {
          if (int == 1) Sync[F].delay(Visualization.Ocean)
          else if (int == 2) Sync[F].delay(Visualization.Forest)
          else getVisualization
        }
        v
      }
    } yield vis
  }

  def getWidth: F[Int] = {
    for {
      _ <- Sync[F].delay(println("How wide do you want your board to be?"))
      _ <- Sync[F].delay(println("Max is 100, min is 3"))
      int <- readInt
      sanitizedInt <- {
        val greaterThan3 = math.max(int, 3)
        val lessThan100 = math.min(greaterThan3, 100)
        Sync[F].delay(lessThan100)
      }
    } yield sanitizedInt
  }

  def getUserShape: F[Map[Coordinates, Int]] = {
    def listPatterns: String = {
      Patterns.patterns.map(p => s"${p.value} for ${p.toString}").mkString("\n")
    }
    for {
      _ <- Sync[F].delay(println("Select a shape"))
      _ <- Sync[F].delay(println(
        s"Enter $listPatterns"))
      int <- readInt
    } yield Patterns(int).shape
  }

  def getCoordinates: F[Coordinates] = {
    for {
      _ <- Sync[F].delay(println("Where do you want your shape?"))
      _ <- Sync[F].delay(println("Enter in format X,Y"))
      s <- Sync[F].delay(scala.io.StdIn.readLine)
      coordinates <- Sync[F].fromTry {
        Try {
          val intList: List[Int] = s.split(",").toList.map(_.trim.toInt)
          val x :: y :: Nil = intList
          (x, y)
        }
      }.handleErrorWith(_ => getCoordinates)
    } yield coordinates
  }

  def setShapeAtCoordinates(): F[Map[Coordinates, Int]] = {
    for {
      userShape <- getUserShape
      coordinates <- getCoordinates
    } yield userShape.at(coordinates)
  }

  def placeUserShapes: F[Map[Coordinates, Int]] = {
    def loop(acc: Map[Coordinates, Int]): F[Map[Coordinates, Int]] = {
      for {
        currentShape <- setShapeAtCoordinates() // TODO, press any other key to continue
        _ <- Sync[F].delay(println("Press 0 when done setting board, press any other Int to continue"))
        int <- readInt
        coordinateMap <- if (int == 0) Sync[F].delay(acc ++ currentShape) else loop(acc ++ currentShape)
      } yield coordinateMap
    }

    loop(Map.empty)
  }
}
