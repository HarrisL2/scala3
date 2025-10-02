package bcGen.Classes

class Pair[K, V](var k: K, var v: V):
  def getKey: K = k
  
  def getValue: V = v
  
  def setKey(newKey: K): Unit =
    k = newKey
    
  def setValue(newValue: V): Unit =
    v = newValue

object testPair {
  val intRefPair = new Pair[Int, String](1, "one")
  val doubleLongPair = new Pair[Double, Long](3.14, 42L)
  val anyAnyPair = new Pair[Any, Any]("key", 232)
  @main def testPair1(): Unit = {
    println(s"Key: ${intRefPair.getKey}, Value: ${intRefPair.getValue}")
  }

  @main def testPair2(): Unit = {
    intRefPair.setKey(2)
    intRefPair.setValue("two")
    println(s"Updated Key: ${intRefPair.getKey}, Updated Value: ${intRefPair.getValue}")
  }

  @main def testPair3(): Unit = {
    println(s"Key: ${doubleLongPair.getKey}, Value: ${doubleLongPair.getValue}")
  }

  @main def testPair4(): Unit = {
    doubleLongPair.setKey(6.28)
    doubleLongPair.setValue(84L)
    println(s"Updated Key: ${doubleLongPair.getKey}, Updated Value: ${doubleLongPair.getValue}")
  }

  def passGenericPair[K, V](pair: Pair[K, V]): String = {
    s"Generic Pair - Key: ${pair.getKey}, Value: ${pair.getValue}" //InvokeReturnType, no InstrArgType
  }

  @main def testPair5(): Unit = {
    val v = passGenericPair[Int, String](intRefPair)
    println(v)
  }

  @main def testPair6(): Unit = {
    val v = passGenericPair[Double, Long](doubleLongPair)
    println(v)
  }

  @main def testPair7(): Unit = {
    println(s"Key: ${anyAnyPair.getKey}, Value: ${anyAnyPair.getValue}")
  }

  @main def testPair8(): Unit = {
    anyAnyPair.setKey(123)
    anyAnyPair.setValue("newValue")
    println(s"Updated Key: ${anyAnyPair.getKey}, Updated Value: ${anyAnyPair.getValue}")
  }

  @main def testPair9(): Unit = {
    val v = passGenericPair[Any, Any](anyAnyPair)
    println(v)
  }

}