import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  * Created by lilisun on 3/19/19.
  */
object ProjectEuler230 {
  def solver( na:BigInt, nb:BigInt,A:String, B:String, n:BigInt): Int ={
    if(n <= na) return A(n.toInt - 1) - '0'

    var l = List(nb, na)
    while(l.head < n){
      l ::= l.head + l.tail.head
    }
    val fab = l.reverse.toArray
    var p = n
    var i = fab.length - 1
    while(i > 1){
      if(p > fab(i-2)){
        p -= fab(i-2)
        i = i - 1
      }
      else{
        p = p
        i = i - 2
      }
    }
    i match {
      case 0 => A(p.toInt -1) - '0'
      case 1 => B(p.toInt -1) - '0'
    }
  }

  def main(args: Array[String]) {
    /*
    val q = scala.io.StdIn.readInt
    for(_ <- 1 to q){
      val line = scala.io.StdIn.readLine.trim
      val words = line.split(" ")
      val a = words(0)
      val b = words(1)
      val n = words(2).toInt
      println(fab(a,b,n))
    }*/
    val file=scala.io.Source.fromFile("testcase/ProjectEuler230/test1.txt")
    val lines = file.getLines
    val q = lines.next().trim.toInt
    for(_ <- 1 to q){
      val line = lines.next()
      val words = line.split(" ")
      val a = words(0)
      val b = words(1)
      val n = BigInt(words(2))
      val ans = solver(a.length, b.length, a, b, n)
      println(ans)
    }

    val A = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
    val B = "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
    val ans = for(i <- 0 to 17) yield solver(A.length, B.length, A,B, (127 + 19*i)*Math.round(Math.pow(7, i)))
    println(ans.mkString)
  }
}

