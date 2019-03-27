/**
  * Created by lilisun on 3/24/19.
  */
object fpComputetheAreaofaPolygon {
  def solverUnorder(arr:Array[Array[Int]]):Double = {
    simple(area,arr.head, arr.tail.toList, List(arr.head), 0)
  }

  def simple(func: (Array[Int], Array[Int])=> Double,start:Array[Int], todo: List[Array[Int]], remain: List[Array[Int]], acc :Double):Double = {
    (todo, remain) match {
      case (Nil, Nil )  => acc
      case (h::t, _  )   => simple(func,h,t,remain, acc + func(start, h))
      case (Nil, h::t)   => simple(func,h, Nil, t, acc - func(start, h))
    }
  }

  def area(p1:Array[Int], p2:Array[Int]):Double = {
    Math.abs(p2(0) - p1(0)) * (p1(1) + p2(1)) * 0.5
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
}
