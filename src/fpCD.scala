/**
  * Created by lilisun on 3/27/19.
  */
import  scala.io.StdIn.{readInt, readLine}
object fpCD {
  def solver(l:Array[Int]):Int = cdsum(gcd(l(0), l(1)))
  def gcd(a:Int, b:Int):Int = {
    println(a, b)
    a match {
      case 0 => b
      case _ => gcd(b % a, a)
    }
  }
  def cdsum(h:Int):Int = (1 to h).count(x => h % x == 0)
  def main(args: Array[String]) {
    //for(_ <- 1 to readInt) println(solver(readLine.trim.split(" ").map(_.toInt)))
    println(gcd(9,7))
  }
}
