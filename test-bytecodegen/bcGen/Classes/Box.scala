package bcGen.Classes

class Box[T, U](var v: T)(u: U) {
    def get: T = v
    def set(newV: T): Unit = {
        v = newV
    }
}

object testBox {
    val stringBox = new Box[String, Int]("Hello")(5)
    val doubleBox = new Box[Double, String](3.14)("Pi")
    val intBox = new Box[Int, Double](42)(2.71)
    def genericBox[T, U](v: T, u: U): Box[T, U] = 
        new Box[T, U](v)(u)
    @main def testBox1: Unit = {
        val intBox = new Box[Int, Double](10)(4.4)
        var sum = 0;
        var i = 0;
        val repeats = 1000000
        while (i < repeats) {
            sum += intBox.get
            i += 1
        }
        println(s"Sum of int boxes: $sum")
    }
    
}