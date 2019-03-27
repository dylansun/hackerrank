/**
  * Created by lilisun on 3/24/19.
  */
object fpComputethePerimeterofaPolygon {
  def solverUnorder(arr:Array[Array[Int]]):Double = {
    simple(arr.head, arr.tail.toList, List(arr.head), 0)
  }

  def simple(start:Array[Int], todo: List[Array[Int]], remain: List[Array[Int]], acc :Double):Double = {
    (todo, remain) match {
      case (Nil, Nil )  => acc
      case (h::t, _  )   => simple(h,t,remain, acc + distSq(start, h))
      case (Nil, h::t)   => simple(h, Nil, t, acc + distSq(start, h))
    }
  }
  def solverOrdered(arr:List[Array[Int]]):Double = {
    help(arr.head, arr.tail, List(arr.head), 0)
  }

  def help(start:Array[Int], todo: List[Array[Int]], remain: List[Array[Int]], acc: Double):Double = {
    (todo, remain) match {
      case (Nil, Nil) => acc
      case (h::t, _) => if(start(1) > h(1)) help(start, t, h::remain, acc)
      else help(h, t, remain, acc + distSq(start, h))
      case (Nil, h::t) => help(h, Nil, t, acc + distSq(start, h))
    }
  }



  def distSq(p1:Array[Int], p2:Array[Int]):Double = {
    Math.sqrt((p1 zip p2).map(x => x._1 - x._2).map(x => x * x).sum)
  }

  def submit():Unit = {
    val n = scala.io.StdIn.readInt
    val arr = Array.fill(n,2)(0)
    for(i <- 0 until n){
      val line = scala.io.StdIn.readLine.trim.split(" ").map(_.toInt)
      arr(i)(0) = line(0)
      arr(i)(1) = line(1)
    }
    println(solverUnorder(arr).formatted("%.1f"))
  }

  def main(args: Array[String]) {
    val arr = Array(
      Array(458, 695)
    ,  Array(621, 483)
    ,Array(877, 469)
    ,Array(1035, 636)
    ,Array(1061, 825)
    ,Array(875 ,1023)
    ,Array(645 ,1033)
    ,Array(485 ,853)

    )
    println(solverUnorder(arr))
  }


}
