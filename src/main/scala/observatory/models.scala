package observatory

case class Station(stn: Int, wban: Int)

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class UnitSquare(d00: Double, d01: Double, d10: Double, d11: Double)

case class UnitSquarePoints(d00: (Int, Int), d01: (Int, Int), d10: (Int, Int), d11: (Int, Int))
