package conway.data

import cats.laws.discipline.ComonadTests
import cats.tests.CatsSuite
import org.scalacheck.ScalacheckShapeless

class ZipperLawsSpec extends CatsSuite with ScalacheckShapeless {
  checkAll("Zipper.ComonadLaws", ComonadTests[Zipper].comonad[Int, Int, String])
  checkAll("GridZipper.ComonadLaws", ComonadTests[GridZipper].comonad[Int, Int, String])
}
