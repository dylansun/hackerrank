/**
  * Created by lilisun on 3/25/19.
  */
import scala.io.StdIn.{readInt, readLine}
object fpSeqFulCol {
  case class Color(r:Int, g:Int, y:Int, b:Int){
    def +(char: Char):Color = char match {
      case 'R' => Color(r+1, g, y, b)
      case 'G' => Color(r, g+1, y, b)
      case 'Y' => Color(r,g, y+1, b)
      case 'B' => Color(r,g, y, b+1)
    }
  }

  def f(str:String):Boolean = f(str.toList, Color(0,0,0,0))
  def f(l:List[Char], color: Color):Boolean = {
    if(Math.abs(color.r - color.g) > 1 || Math.abs(color.y - color.b) > 1) false
    else
    l match {
      case Nil => color.r == color.g && color.y == color.b
      case h::t => f(t, color + h)
    }
  }
  def hehe(b:Boolean):String = if(b) "True" else "False"
  def main(args: Array[String]): Unit = {
    //for(_ <- 1 to readInt) println(hehe(f(readLine.trim)))
    val c = "a" * 5
    print(c)
  }
}
