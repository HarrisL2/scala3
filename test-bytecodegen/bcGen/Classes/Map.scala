package bcGen.Classes

case class MapEntry[K, V](k: K, v: V, var next: MapEntry[K, V]):
  ???

class Map[K, V]:
  var head: MapEntry[K, V] = null
  
  def insert(k: K, v: V): Unit =
    ???
    
  def get(k: K): V =
    ???