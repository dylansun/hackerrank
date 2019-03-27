/**
  * Created by lilisun on 3/24/19.
  */
object fpSierpinskiTriangles {
  case class XY(x:Int, y:Int)
  def mid(p1:XY, p2:XY, bias:XY = XY(0,0)):XY = XY((p1.x + p2.x)/2+bias.x, (p1.y + p2.y)/2+bias.y)

  case class Triangle(top:XY, left:XY, right:XY)
  def getIter0 : List[List[String]] = {
    List.fill(32, 63)("_").zipWithIndex.map{
      case (row, i) => row.zipWithIndex.map{
            case (_, j) if (31-i to 31+i).toList.contains(j) => "1"
            case (x, _) => x
      }
    }
  }


  def getIter(n:Int,triangles: List[Triangle], iter:List[List[String]]):List[List[String]] = n match {
    case 0 => iter
    case _ => getIter(n-1, expand(triangles), update(triangles, iter))
  }

  def expand(triangles: List[Triangle]):List[Triangle] = triangles.flatMap(expandOne)
  def update(triangles: List[Triangle], iter:List[List[String]]):List[List[String]] = triangles match {
    case Nil => iter
    case h::t => update(t, updateOne(h, iter))
  }
  def expandOne(triangle: Triangle):List[Triangle] = {
    List(Triangle(triangle.top, mid(triangle.top, triangle.left), mid(triangle.top, triangle.right))
      ,Triangle(mid(triangle.top, triangle.left, XY(1,0)), triangle.left, mid(triangle.left, triangle.right, XY(0,-1)))
      ,Triangle(mid(triangle.top, triangle.right, XY(1,1)), mid(triangle.left, triangle.right, XY(0,1)), triangle.right)
    )
  }
  def updateOne(triangle: Triangle, iter:List[List[String]]):List[List[String]] = {
    iter.zipWithIndex.map{
      case (row, i) if i > (triangle.top.x + triangle.left.x) / 2 && i <= triangle.left.x => row.zipWithIndex.map{
        case (x,j) if (triangle.top.y - triangle.left.x + i to triangle.top.y +triangle.left.x - i).toList.contains(j) => "_"
        case (x,_) => x
      }
      case (row, _) => row
    }
  }

  def drawTriangles(n: Int) {
    //Draw the N'th iteration of the fractal as described
    // in the problem statement
    getIter(n, List(Triangle(XY(0,31), XY(31, 0), XY(31,62))), getIter0).foreach(x => println(x.mkString))
  }

  def main(args: Array[String]) {
    drawTriangles(2)
  }
}
