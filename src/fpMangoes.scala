/**
  * Created by lilisun on 4/5/19.
  */
import scala.collection.mutable
import scala.io.StdIn.readLine
object fpMangoes {
    def solver(A:Array[Int], B:Array[Int])(N:Int, threshold: BigInt):Int = {
      if(f(A,B)(1) > threshold ) 0
      else if(f(A,B)(N) <= threshold ) N
      else binarySearch(A,B,threshold)(1,(N+1)/2, N)
    }
    def binarySearch(A:Array[Int], B:Array[Int], threshold:BigInt)(l:Int, mid:Int, r:Int):Int= {
      f(A,B)(mid) - threshold match {
        case x:BigInt if x == 0 => mid
        case x:BigInt if x > 0 => f(A,B)(mid-1) - threshold match {
            case y:BigInt if y <= 0 => mid -1
            case _ => binarySearch(A,B,threshold)(l,(l + mid -1)/2 ,mid - 1)
          }
        case x:BigInt if x < 0 => f(A,B)(mid + 1) - threshold match {
            case y: BigInt if y >= 0 => mid
            case _ => binarySearch(A,B,threshold)(mid+1,(mid + 1 + r)/2 , r)
          }
      }
    }
    def f(A:Array[Int], B:Array[Int])(n:Int):BigInt = {
      small(n)((A zip B.map(x => BigInt(x) * BigInt(n-1))).map(x => x._1 + x._2))
    }
    def small(k:Int)(A:Array[BigInt]):BigInt ={
      A.slice(k, A.length).foldLeft(init(mutable.PriorityQueue[BigInt]()(Ordering.by(x => x)),A.slice(0,k)))( (pq, x) => g(pq)(x)).sum
    }

    def g(pq: mutable.PriorityQueue[BigInt])(x:BigInt): mutable.PriorityQueue[BigInt] = {
      pq.enqueue(pq.dequeue min x)
      pq
    }
    def init(pq: mutable.PriorityQueue[BigInt], nums: Array[BigInt]):mutable.PriorityQueue[BigInt] = {
      nums.foreach(x => pq.enqueue(x))
      pq
    }

    def main(args: Array[String]) {
      val nm = readLine.split(" ").map(x => BigInt(x))
      val A = readLine.split(" ").map(_.toInt)
      val B = readLine.split(" ").map(_.toInt)
      println(solver(A, B)(nm(0).toInt, nm(1)))
    }
  }


