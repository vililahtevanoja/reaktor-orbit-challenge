package solver
import org.scalatest._
/**
  * Created on 2016-05-03.
  *
  * @author vili
  */
class SolverTests extends FlatSpec with Matchers {
  "Line from ground to up in space" should "not dissect sphere" in {
    val sphere = Sphere(CartesianCoordinates(0, 0, 0), 100)
    val start = Vec3(0, 0, 100)
    val end = Vec3(0, 0, 101)
    assert(!Solver.lineDissectsSphere(start, end, sphere))
  }
  //  test("Line from ground to up in space should not dissect sphere") {
  //    val sphere = Sphere(CartesianCoordinates(0, 0, 0), 100)
  //    val start = Vec3(0, 0, 100)
  //    val end = Vec3(0, 0, 101)
  //    assert(!Solver.lineDissectsSphere(start, end, sphere))
  //  }
}
