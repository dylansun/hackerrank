/**
  * Created by lilisun on 3/25/19.
  */
object fpStringCompression {
  def solver(str:String):String = str match{
    case "" => ""
    case _ => solver(str.head, 1, str.tail.toList, Nil)
  }
  def solver(pre:Char, count:Int, todo: List[Char], acc:List[(Char, Int)]):String = todo match {
    case Nil => acc.reverse.map(f).mkString + f((pre , count))
    case `pre`::t => solver(pre, count+1, t, acc)
    case  h::t => solver(h, 1, t, (pre, count) :: acc )
  }
  def f(tuple:(Char, Int)):String = tuple match {
    case (ch, 1) => ch.toString
    case _ => tuple._1 + tuple._2.toString
  }
  def main(args: Array[String]) {
    println(solver(scala.io.StdIn.readLine.trim))
  }
}
