/**
  * Created by lilisun on 3/19/19.
  */
object ProjectEuler225 {
  def tri(a:BigInt, b:BigInt, c:BigInt, n:Int):List[BigInt] = {
    var l = List(c,b,a)
    for(i <- 1 to n){
      l ::= (l.head + l.tail.head + l.tail.tail.head)
    }
    l
  }

  def main(args: Array[String]): Unit = {
    val l = tri(1,1,1,50)
    println(l)
    for(i <- BigInt(1) to l.head ) {
      println( l.count(_ % i == 0))
      println(i)
    }

  }
}
