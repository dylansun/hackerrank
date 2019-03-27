/**
  * Created by lilisun on 3/25/19.
  */
import scala.io.StdIn.{readInt, readLine}
object fpListGCD {
  case class PN(p:Int, n:Int){
    def -(pn:PN):PN = PN(p, n min pn.n)
  }
  def trans(list: List[Int]):List[PN] = PN(list.head, list.tail.head)::trans(list.tail.tail)
  def f(l1:List[PN], l2:List[PN]):List[PN] = f(l1, l2, Nil).reverse
  def f(l1:List[PN], l2:List[PN], acc:List[PN]):List[PN] = (l1, l2) match {
    case (Nil,_) => acc
    case (_, Nil) => acc
    case (h1::t1, h2::t2) =>
      {
        if(h1.p == h2.p) f(t1,t2,(h1-h2)::acc)
        else if(h1.p > h2.p) f(h1::t1, t2, acc)
        else f(t1, h2::t2, acc)
      }

  }
  def main(args: Array[String]): Unit = {
    println(List.fill(readInt)(trans(readLine.trim.split(" ").map(_.toInt).toList))
      .reduce(f).map(x => x.p + " "+x.n).mkString(" "))
  }
}
