/**
  * Created by lilisun on 3/27/19.
  */
import scala.io.StdIn.{readInt, readLine}
object fpSubsetSum {
  def f(A:Array[BigInt])(x:BigInt):Int = {
    if(A.head >= x) 1
    else if (A.last < x) -1
    else bs(x, A)(0, (A.length -1) / 2, A.length - 1) + 1 //
  }
  def bs(t:BigInt, A: Array[BigInt])(l:Int,m:Int, r:Int):Int = A(m) - t match {
      case x:BigInt if x ==0 => m
      case x:BigInt if x > 0 => A(m-1) - t match {
          case y:BigInt if y < 0 => m
          case _ => bs(t, A)(l, (m-1+l)/2,m-1)
        }
      case x:BigInt if x < 0 => A(m+1) - t match {
        case y :BigInt if y >= 0 => m + 1
        case _ => bs(t, A)(m+1,(m+1+r)/2,r)
      }
    }


  def cum(A: Array[Int]):Array[BigInt] = {
    val cA = A.map(x => BigInt(x))
    cA.indices.tail.foreach(i => cA(i) += cA(i-1))
    cA
  }


  def solve(func: BigInt => Int, n:Int):Unit = for(_ <- 1 to n) println(func(BigInt(readLine.trim)))
  def main(args: Array[String]): Unit = {
    readInt
    solve(f(cum(readLine.trim.split(" ").map(_.toInt).sortBy(x => -x))), readInt)
  }

}
