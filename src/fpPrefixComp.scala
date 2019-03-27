/**
  * Created by lilisun on 3/27/19.
  */
import scala.io.StdIn.readLine
object fpPrefixComp {
  def prefix(p:List[Char])(s1:List[Char], s2:List[Char]):List[(Int, String)] = {
    List((p.length, p.mkString),g(p)(s1), g(p)(s2))
  }

  def g(prefix: List[Char])(s:List[Char]):(Int, String) = {
    (s.length - prefix.length, s.slice(prefix.length, s.length).mkString)
  }

  def f(s1:List[Char], s2:List[Char], acc:List[Char] = Nil):List[Char] = (s1 ,s2) match {
    case (t::h,t1::h1) if t == t1 => f(h, h1, t::acc)
    case (_, _) => acc.reverse
  }
  def solver(s1:List[Char], s2:List[Char]):List[(Int, String)] = prefix(f(s1,s2))(s1,s2)
  def main(args: Array[String]): Unit = {
    println(solver(readLine.trim.toList, readLine.trim.toList).map(x => x._1 + " " + x._2).mkString("\n"))
  }
}
