/**
  * Created by lilisun on 3/25/19.
  */
import scala.io.StdIn.readInt
object fpFib {
  val mod = 100000007
  def fib(n:Int):BigInt = fib(0,1,n)
  def fib(a0:BigInt, a1:BigInt, n:Int): BigInt = n match{
    case 0 => a0 % 100000007
    case _ => fib((a1)% 100000007, (a0+a1)% 100000007, n-1)
  }

  def all(a0:BigInt, a1:BigInt, n:Int, acc: List[BigInt]):List[BigInt] = n match {
    case 0 => a0  :: acc
    case _ => all(a1, (a0+ a1)%mod, n-1, a0::acc)
  }
  def main(args: Array[String]) {
    //n max = 10000
    val table = all(0,1,10000, Nil).reverse
    for(_ <- 1 to readInt) println(table(readInt))

  }
}
