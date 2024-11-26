import caps.*

class IO

class File

trait Abstract:
  type C >: CapSet <: CapSet^
  def f(file: File^{C^}): Unit

class Concrete1 extends Abstract:
  type C = CapSet
  def f(file: File) = ()

class Concrete2(io: IO^) extends Abstract:
  type C = CapSet^{io}
  def f(file: File^{io}) = ()

class Concrete3(io: IO^) extends Abstract:
  type C = CapSet^{io}
  def f(file: File) = () // error

trait Abstract2(io: IO^):
  type C >: CapSet <: CapSet^{io}
  def f(file: File^{C^}): Unit

class Concrete4(io: IO^) extends Abstract2(io):
  type C = CapSet
  def f(file: File) = ()

class Concrete5(io1: IO^, io2: IO^) extends Abstract2(io1):
  type C = CapSet^{io2} // error
  def f(file: File^{io2}) = ()