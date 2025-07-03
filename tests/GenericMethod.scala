package bcGen

class GenericMethod {
  // non-generic method, no type hint
  def passInt(x: Int): Int = x
  // non-generic method, no type hint
  def passRef(x: Foo): Foo = x
  // @MethodTypeParameterCount: 1
  // @MethodParameterType: M0
  // @MethodReturnType: M0
  def identity[T](value: T): T = value
  // non-generic method, no type hint
  def identity2(value: Any): Any = value
  // @MethodTypeParameterCount: 1
  // @MethodParameterType: M0, M1
  // @MethodReturnType: M0
  def first[A, B](fst: A, snd: B): A = fst
}

object testGenericMethod1{
  val gm = new GenericMethod
  @main def main1(): Unit =
    val v = gm.identity[Int](42)
    println("identity[Int](42):" + v)

  @main def main2(): Unit = 
    val v = gm.identity2(2.2)
    println("identity2(2.2):" + v)
    
  @main def main3(): Unit =
    val v = gm.first[Double, Long](4.2, 42L)
    println("first[Double, Long](4.2, 42L):" + v)

  @main def main4(): Unit =
    val v = gm.identity[Foo](new Foo(1))
    println("identity[Foo](new Foo(1)):" + v)

  @main def main5(): Unit =
    val v = gm.passRef(new Foo(2))
    println("passRef(new Foo(2)):" + v)

  @main def main6(): Unit =
    val v = gm.identity[java.lang.Integer](java.lang.Integer.valueOf(8))
    println("identity[java.lang.Integer](java.lang.Integer.valueOf(8)):" + v)

  @main def main7(): Unit =
    val v = gm.identity2(java.lang.Integer.valueOf(87))
    println("identity2(java.lang.Integer.valueOf(87)):" + v)

  @main def main8(): Unit =
    val v = gm.passInt(88)
    println("passInt(88):" + v)

  def genericMethod1[A, B, C](a: A, b: B, c: C): Unit =
    val v1 = gm.identity[B](b)
    println("identity[B](b):" + v1)
    val v2 = gm.first[C, A](c, a)
    println("first[C, A](c, a):" + v2)

  @main def main9(): Unit =
    genericMethod1[Int, Char, Double](1, 'a', 3.14)
    // prints:
    // identity[B](b): a
    // first[C, A](c, a): 3.14
  
  @main def main10(): Unit =
    genericMethod1[Foo, Foo, Foo](new Foo(1), new Foo(2), new Foo(3))
    // prints:
    // identity[B](b): Printing Foo 2
    // first[C, A](c, a): Printing Foo 3

  def genericMethod2[A, B, C](a: A, b: B, c: C): Unit =
    val v1 = gm.identity2(c)
    println("identity2(c):" + v1)

  @main def main11(): Unit =
    genericMethod2[Int, Char, Double](1, 'a', 3.14)
    // prints:
    // identity2(c): 3.14

  @main def main12(): Unit =
    genericMethod2[Foo, Foo, Foo](new Foo(1), new Foo(2), new Foo(3))
    // prints:
    // identity2(c): Printing Foo 3
}

/*
object testGenericMethod {
  @main def testGenericMethodMain(): Unit = {
    println("test1:")
    /* prints:
       identity[Int](23)
       23
       identity2(2.2)
       2.2
       first[Char, Double]('c', 8.8)
       c
       identity[Foo](new Foo(1))
       Printing Foo 1
       passRef(new Foo(2))
       Printing Foo 2
       identity[java.lang.Integer](java.lang.Integer.valueOf(8))
       8
       identity2(java.lang.Integer.valueOf(87))
       87
       passInt(88)
       88
    */
    test1()
    println("test2[Int, Char, Double]\nU, X, Y: Int, Char, Double\nvalue: 7; fst: 'v'; snd: 9.9")
    /* prints:
       identity[Y](snd)
       9.9
       identity2(value)
       7
       identity[Any](value)
       7
       first[X, Y](fst, snd)
       v
       first[Any, Y](value, snd)
       7
     */
    test2[Int, Char, Double](7, 'v', 9.9)
    println("test2[Foo, Foo, Foo]\nU, X, Y:Foo, Foo, Foo\nvalue: new Foo(-1); fst: new Foo(-2); snd: new Foo(-3)")
    /* prints:
       identity[Y](snd)
       Printing Foo -3
       identity2(value)
       Printing Foo -1
       identity[Any](value)
       Printing Foo -1
       first[X, Y](fst, snd)
       Printing Foo -2
       first[Any, Y](value, snd)
       Printing Foo -1
     */
    test2[Foo, Foo, Foo](new Foo(-1), new Foo(-2), new Foo(-3))
  }
  //if (methodParameterType is generic (has generic methodParameterType type hint) &&
  //    reified type is primitive &&
  //    argument type before invoke is non-generic) {eunbox (before invoke)}
  // if (methodReturnType is generic (has methodReturnType type hint) &&
  //     reified type is primitive &&
  //     return type at call site is non-generic, InvokeReturnType is None (or 'L'), not 'M0', 'K0') {eBox (in big loop)}
  def test1() : Unit = {
    val gm = new GenericMethod
    // passing a primitive type as
    // type argument to a generic method
    // @InstructionTypeArguments: offset, I
    // special case: generic method return type which is a primitive type at the caller context
    println("identity[Int](23)")
    val v1 = gm.identity[Int](23) //need InstructionTypeArg: I
    println(v1)
    // passing to a non-generic method
    // with java.lang.Object as parameter
    // no hints needed
    println("identity2(2.2)")
    val v2 = gm.identity2(2.2)
    println(v2)
    // passing two different primitive types 
    // as type arguments to a generic method
    // @InstructionTypeArguments: offset, C D
    println("first[Char, Double]('c', 8.8)")
    val v3 = gm.first[Char, Double]('c', 8.8) //same as v1, need InstructionTypeArg: CD
    println(v3)
    // passing a scala class as
    // type argument to a generic method
    // @InstructionTypeArguments: offset, L
    println("identity[Foo](new Foo(1))")
    val v4 = gm.identity[Foo](new Foo(1)) //need InstructionTypeArg: L
    println(v4)
    // passing a scala class to a method
    // that takes Foo as parameter
    // no hints needed
    println("passRef(new Foo(2))")
    val v5 = gm.passRef(new Foo(2))
    println(v5)
    // passing a java class as
    // type argument to a generic method
    // @InstructionTypeArguments: offset, L
    println("identity[java.lang.Integer](java.lang.Integer.valueOf(8))")
    val v6 = gm.identity[java.lang.Integer](java.lang.Integer.valueOf(8))
    println(v6)
    // passing a java class to a 
    // non-generic method
    // with java.lang.Object as parameter
    // no hints needed
    println("identity2(java.lang.Integer.valueOf(87))")
    val v7 = gm.identity2(java.lang.Integer.valueOf(87))
    println(v7)
    // passing a scala int to a
    // non-generic method
    // no hints needed
    println("passInt(88)")
    val v8 = gm.passInt(88)
    println(v8)

  }
  //def test2$[U,X,Y](u:U,x:X,y:Y) = test2[U,X,Y](u,x,y)
  def test2[U, X, Y](value: U, fst: X, snd: Y): Any = {
    val gm = new GenericMethod
    // passing a type argument of current method
    // and a value
    // of that type to a generic method
    // @InstructionTypeArguments: offset, M2
    // @InvokeReturnType: offset, M2
    println("identity[Y](snd)")
    val v1 = gm.identity[Y](snd) //need InstructionTypeArg: M2, InvokeReturnType: M2?
    println(v1)
    // passing a value of generic type
    // to a non-generic method
    // no hints needed
    println("identity2(value)")
    val v2 = gm.identity2(value)
    println(v2)
    // passing a value of generic type
    // to a generic method with
    // type parameter Any(java.lang.Object)
    // @InstructionTypeArguments: offset, L
    // @InvokeReturnType: offset, L
    println("identity[Any](value)")
    val v3 = gm.identity[Any](value)
    println(v3)
    // passing two values of generic types
    // to a generic method
    // @InstructionTypeArguments: offset, M1 M2
    // @InvokeReturnType: offset, M1
    println("first[X, Y](fst, snd)")
    val v4 = gm.first[X, Y](fst, snd)
    println(v4)
    // passing two values, one a scala class
    // and the other a generic type
    // to a generic method
    // @InstructionTypeArguments: offset, L M2
    // @InvokeReturnType: offset, L
    println("first[Any, Y](value, snd)")
    val v5 = gm.first[Any, Y](value, snd)
    println(v5)
    // passing a value, storing it
    // into a supertype of the type parameter
    // @InstructionTypeArguments: offset, M0
    // @InvokeReturnType: offset, M0
    // @Casting: L
    val v6 : Any = gm.identity[U](value)
    val v7 : U = gm.identity[U](value)
    val v9 : Any = gm.identity[Int](99)
    val v8 : Any = v7
  }
}
*/

class Foo(id: Int) {
  //arbitrary scala class
  override def toString: String = "Printing Foo " + id
}
