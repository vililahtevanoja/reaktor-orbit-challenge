case class Satellite(id: String, location: CartesianCoordinates)
case class Route(startLocation: CartesianCoordinates, endLocation: CartesianCoordinates)
case class Line(p1: CartesianCoordinates, p2: CartesianCoordinates)
case class Sphere(c: CartesianCoordinates, r: Double) {
  def normalized(that: Double) = Sphere(this.c, that / this.r)
}
case class CartesianCoordinates(x: Double, y: Double, z: Double) {
  def toVec3 = Vec3.tupled(CartesianCoordinates.unapply(this).get)
}
case class Vec3(x: Double, y: Double, z: Double) {
  def +(that: Vec3) = Vec3(this.x + that.x, this.y + that.y, this.z + that.z)
  def -(that: Vec3) = Vec3(this.x - that.x, this.y - that.y, this.z - that.z)
  def -(that: Double) = Vec3(this.x - that, this.y - that, this.z - that)
  def *(that: Vec3) = Vec3(this.x * that.x, this.y * that.y, this.z * that.z)
  def dot(v: Vec3) = (this.x * v.x) + (this.y * v.y) + (this.z * v.z)
  def length = math.sqrt((this.x*this.x) + (this.y*this.y) + (this.z+this.z))
  def unit = {
    val len = this.length
    new Vec3(this.x/len, this.y/len, this.z/len)
  }
  def fromEuclidean(e: CartesianCoordinates) = Vec3.tupled(CartesianCoordinates.unapply(e).get)
  def toEuclidean = CartesianCoordinates.tupled(Vec3.unapply(this).get)
}

object Solver extends App {
  lazy final val Globe = Sphere(CartesianCoordinates(0.0, 0.0, 0.0), 6371.0)
  override def main(args: Array[String]) = {
    val (satellites, route) = DataReader.readData("data.txt")
    //    println(satellites)
    //    println(route)
    //    println("")
    val res = solve(satellites, route)
    println(res.map(_.id))
  }

  def solve(satellites: List[Satellite], route: Route): List[Satellite] = {
    val starts = satellites.filterNot(x => lineDissectsSphere2(route.startLocation.toVec3, x.location.toVec3, Globe))
    val stops = satellites.filterNot(x => lineDissectsSphere2(route.endLocation.toVec3, x.location.toVec3, Globe))
    println(starts)
    println(stops)
    println("")
    stops
  }

  def lineDissectsSphere(start: Vec3, end: Vec3, sphere: Sphere): Boolean = {
    /*
    A sphere is given by its center (cx, cy, cz) and its radius, R.
    A line segment (ray) is given by its endpoints: P0 = (x0, y0, z0) and P1 = (x1, y1, z1).
    To find visible spheres, set P0 = viewer coordinates = (Vx, Vy, Vz) and let P1 run through all the points (x1, y1, 0) where (x1, y1) is in the display area.

    Set
      dx = x1 - x0
      dy = y1 - y0
      dz = z1 -  z0

      a = dx*dx + dy*dy + dz*dz;
      b = 2*dx*(x0-cx) +  2*dy*(y0-cy) +  2*dz*(z0-cz);
      c = cx*cx + cy*cy + cz*cz + x0*x0 + y0*y0 + z0*z0 + -2*(cx*x0 + cy*y0 + cz*z0) - R*R;

      discriminant(a, b, c) = b^2 - 4ac
      if  discriminant (a, b, c) < 0  -> no intersection
                                 = 0  -> ray is tangent to sphere
                                 > 0  -> ray intersects sphere in two points
     */
    val delta = end - start
    val a = (delta.x * delta.x) + (delta.y * delta.y) + (delta.z * delta.z)
    val b = (2 * delta.x*start.x) + (2 * delta.y * start.y) + (2 * delta.z * start.z)
    val t = (start.x * start.x) + (start.y * start.y) + (start.z * start.z)
    val rpow2 = (sphere.r * sphere.r)
    val c = (start.x * start.x) + (start.y * start.y) + (start.z * start.z) - ((sphere.r-0.001) * (sphere.r-0.001))
    val discr = (b*b) - (4*a*c)
    discr >= 0
  }

  def lineDissectsSphere2(start: Vec3, end: Vec3, sphere: Sphere): Boolean = {
    /*
    A = (x0-xc)^2 + (y0-yc)^2 + (z0-zc)^2 - R^2
    B = (x1-xc)^2 + (y1-yc)^2 + (z1-zc)^2 - A - C - R^2
    C = (x0-x1)^2 + (y0-y1)^2 + (z0-z1)^2
     */
    val A = math.pow(start.x, 2) + math.pow(start.y, 2) + math.pow(start.z, 2) - math.pow(sphere.r, 2)
    val delta = start - end
    val C = math.pow(delta.x, 2) + math.pow(delta.y, 2) + math.pow(delta.z, 2)
    val B = math.pow(end.x, 2) + math.pow(end.y, 2) + math.pow(end.z, 2) - A - C - math.pow(sphere.r, 2)
    val discr = math.pow(B, 2) - (4 * A * C)
    discr < 0
  }

  def vectorDissectsSphere(start: Vec3, v: Vec3, sphere: Sphere): Boolean = {
    /*
    A sphere is given by its center (cx, cy, cz) and its radius, R.
    A line segment (ray) is given by its endpoints: P0 = (x0, y0, z0) and P1 = (x1, y1, z1).
    To find visible spheres, set P0 = viewer coordinates = (Vx, Vy, Vz) and let P1 run through all the points (x1, y1, 0) where (x1, y1) is in the display area.

    Set
      dx = x1 - x0
      dy = y1 - y0
      dz = z1 -  z0

      a = dx*dx + dy*dy + dz*dz;
      b = 2*dx*(x0-cx) +  2*dy*(y0-cy) +  2*dz*(z0-cz);
      c = cx*cx + cy*cy + cz*cz + x0*x0 + y0*y0 + z0*z0 + -2*(cx*x0 + cy*y0 + cz*z0) - R*R;

      discriminant(a, b, c) = b^2 - 4ac
      if  discriminant (a, b, c) < 0  -> no intersection
                                 = 0  -> ray is tangent to sphere
                                 > 0  -> ray intersects sphere in two points
     */
    val l = v.unit
    val o = sphere.c.toVec3
    val c = start
    val d = (o-c)*(o-c)
    (l.dot(v) - d.length - sphere.r) >= 0
    /*
    ((l * (o - c))^2) - (||o - c||^2) - r^2
     */
  }
}
object DataReader {
  lazy final val Globe = Sphere(CartesianCoordinates(0.0, 0.0, 0.0), 6371.0)
  def readData(filename: String): (List[Satellite], Route) = {
    val dataLines = scala.io.Source.fromFile(filename, "UTF-8")
      .getLines()
      .filterNot(_.startsWith("#"))
      .toList
    val satellites = dataLines.filterNot(_.startsWith("ROUTE")).map(dataStringToSatellite(_))
    val route = dataStringToRoute(dataLines.last)
    (satellites, route)
  }

  def dataStringToSatellite(data: String): Satellite = {
    val vals = data.split(",")
    assert(vals.size == 4)
    val id = vals(0)
    val euclideanCoords = coordinatesToCartesianCoordinates(vals(1).toDouble, vals(2).toDouble , vals(3).toDouble)
    new Satellite(id, euclideanCoords)
  }

  //latitude of starting point,longitude of starting point,latitude of end point,longitude of end point
  def dataStringToRoute(data: String): Route = {
    val vals = data.split(",")
    val startCoords = coordinatesToCartesianCoordinates(vals(1).toDouble, vals(2).toDouble)
    val endCoords = coordinatesToCartesianCoordinates(vals(3).toDouble, vals(4).toDouble)
    new Route(startCoords, endCoords)
  }


  def coordinatesToCartesianCoordinates(lat: Double, lon: Double): CartesianCoordinates = {
    coordinatesToCartesianCoordinates(lat, lon, 0)
  }

  def coordinatesToCartesianCoordinates(lat: Double, lon: Double, alt: Double): CartesianCoordinates = {
    val latRad = lat * math.Pi / 180
    val lonRad = lon * math.Pi / 180
    val R = Globe.r + alt
    val x = -R * math.cos(latRad) * math.cos(lonRad)
    val y = R * math.sin(latRad)
    val z = R * math.cos(latRad) * math.sin(lonRad)
    new CartesianCoordinates(x, y, z)
  }
}
