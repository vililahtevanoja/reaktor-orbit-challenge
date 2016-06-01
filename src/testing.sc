val sphere = Sphere(CartesianCoordinates(0, 0, 0), 100)
val start = Vec3(0, 0, 101)
val end = Vec3(0, 0, 102)
assert(!Solver.lineDissectsSphere(start, end, sphere))
