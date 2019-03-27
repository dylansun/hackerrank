/**
  * Created by lilisun on 3/25/19.
  */
object fpRecursiveTree {
  case class Point(x:Int, y:Int)
  case class Tree(peak:Point, height:Int)
  def getZero:List[List[String]] = List.fill(63, 100)("_")
  def getIter(n:Int, trees:List[Tree], iter:List[List[String]]):List[List[String]] = n match {
    case 0 => iter
    case _ => getIter(n-1, expand(trees), update(trees, iter))
  }

  def expand(trees: List[Tree]):List[Tree] = trees.flatMap(expandOne)
  def expandOne(tree: Tree):List[Tree] = {
      List(Tree(Point(tree.peak.x - 2* tree.height ,tree.peak.y - tree.height), tree.height /2),
           Tree(Point(tree.peak.x - 2* tree.height ,tree.peak.y + tree.height), tree.height /2))
  }
  def update(trees: List[Tree], iter:List[List[String]]):List[List[String]] = trees match {
    case Nil => iter
    case h::t => update(t, updateOne(h, iter))
  }
  def updateOne(tree: Tree, iter:List[List[String]]):List[List[String]]  = iter.zipWithIndex.map{
    case (row, i) if (tree.peak.x - tree.height + 1 to tree.peak.x).toList.contains(i) => row.zipWithIndex.map{
      case (x, j) if j == tree.peak.y => "1"
      case (x,_) => x
    }
    case (row, i) if(tree.peak.x - 2*tree.height + 1 until tree.peak.x).toList.contains(i) => row.zipWithIndex.map{
      case (x,j) if j == tree.peak.y - i + 1 + tree.peak.x - tree.height
                 || j == tree.peak.y + i - 1 - tree.peak.x + tree.height => "1"
      case (x,_) => x
    }
    case (row, _) => row
  }
  def main(args: Array[String]): Unit = {
    getIter(5, List(Tree(Point(62, 49), 16)), getZero).map(x => x.mkString).foreach(println)
    //val s = "_________________________________________________1__________________________________________________"
    //println(s.length, s.indexOf('1'))
  }
}
