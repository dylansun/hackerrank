/**
  * Created by lilisun on 4/5/19.
  */
import scala.io.StdIn._
object fpPrisonTrans {
    class DSU(N:Int){
      val parent = (0 to N).toArray
      val rank = (0 to N).toArray
      def find(x:Int):Int = if(parent(x) != x) find(parent(x)) else parent(x)
      def union(x:Int, y:Int):Unit = {
        var xr = find(x)
        var yr = find(y)
        if(xr == yr) return
        if(rank(xr) < rank(yr)){
          val tmp = xr
          xr = yr
          yr = tmp
        }

        parent(yr) = xr
        if(rank(xr) == rank(yr))
          rank(xr) += 1
      }
    }

    def solver(N:Int, graph:Array[Array[Int]]):Int = {
      val dsu = new DSU(N)
      graph.foreach(x => dsu.union(x(0), x(1)))
      (1 to N).toArray.map(x => dsu.find(x)).groupBy(x => x).values.map(_.length)
        .foldLeft(0){(cost, x) => cost + f(x)}
    }
    def f(x:Int):Int = Math.sqrt(x).toInt match{
      case k:Int if k * k == x => k
      case k:Int  => k + 1
    }
    def main(args: Array[String]) {
      println(solver(readInt, Array.fill(readInt)(readLine.split(" ").map(_.toInt))))
    }
  }


