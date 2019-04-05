/**
  * Created by lilisun on 4/5/19.
  */
import scala.io.StdIn._
object fpHugeGCD {
    val plist = (1 to 10000).toList.filter(isPrime)
    def isPrime(n:Int):Boolean = n match {
      case 1 => false
      case 2 => true
      case 3 => true
      case _ =>(2 to Math.sqrt(n).toInt).forall( x => n % x != 0)
    }

    def g(plist:List[Int])(x:Int):List[Int] = ghelp(plist)(x, Nil)
    def ghelp(plist:List[Int])(x:Int, acc:List[Int]):List[Int] = x match {
      case 1 => acc
      case _ => if(x % plist.head == 0) ghelp(plist)( x / plist.head, plist.head::acc)
      else ghelp(plist.tail)( x, acc)
    }
    def pf(g:Int => List[Int])(A:Array[Int]):Array[Int] = {
      val ans = Array.fill(10000)(0)
      for{x <- A
          y <- g(x)}
        ans(y) += 1
      ans
    }
    def f(g:Int => List[Int])(A:Array[Int], B:Array[Int]):Int = {
      (pf(g)(A) zip pf(g)(B))
        .map(x => x._1 min x._2)
        .zipWithIndex
        .flatMap(x => List.fill(x._1)(x._2))
        .foldLeft(1){(mul, x) => pro(mul, x)}
    }

  def pro(x:Int, y:Int):Int = {
    (BigInt(x) * BigInt(y) % 1000000007).toInt
  }
    def submit():Unit = {
      readInt
      val A = readLine.trim.split(' ').map(_.toInt)
      readInt
      val B = readLine.trim.split(' ').map(_.toInt)
      println(f(g(plist))(A, B))
    }

  def main(args: Array[String]): Unit = {
    val A = Array(2,4,5,6,18,7,7)
    val B = Array(10000,1011, 9872)
    println(f(g(plist))(A, B))
    println(A.product)
    A.indexWhere(_ == 1)
  }
}


