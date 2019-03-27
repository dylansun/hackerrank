/**
  * Created by lilisun on 3/19/19.
  */
object ProjectEuler229 {

  def f(n: Int, k:Int): IndexedSeq[Int] = {
    val r = Math.sqrt(n).toInt
    (for(i <- 1 to r) yield {
      for (j <- 1 to r if i*i + k *j *j <= n) yield {
        i*i + k *j *j
      }
    }).flatten
  }

  def count(n:Int):Int = (f(n,1) intersect f(n, 2) intersect f(n,3) intersect f(n,7)).size
  def main(args: Array[String]): Unit = {
    val n = scala.io.StdIn.readInt
    for(_ <- 1 to n){
      val N = scala.io.StdIn.readInt
      println(count(N))
    }
  }
}
