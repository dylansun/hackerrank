/**
  * Created by lilisun on 3/25/19.
  */
object fpConvexHull {
  case class Point(x:Int, y:Int){
    def minus(p1:Point):Point = Point(x - p1.x, y - p1.y)
    def add(p1:Point):Point = Point(x + p1.x, y + p1.y)
    def +(p1:Point):Point = Point(x + p1.x, y + p1.y)
    def -(p1:Point):Point = Point(x - p1.x, y - p1.y)
  }
  def arr2Point(array: Array[Int]):Point = Point(array(0), array(1))

  def solver(arr:List[Point]):Double = {
    calAns(lr(Point(0,0),Point(0,0),trans(arr), Nil,Nil))
  }
  def calAns(_lr: (List[Point], List[Point])):Double = {
    distSq(_lr._1.head, _lr._2.head) + calAnshelper(_lr._1) + calAnshelper(_lr._2)
  }

  def calAnshelper(ls:List[Point]):Double = calAnshelper(ls:List[Point], 0)
  def calAnshelper(ls:List[Point], acc:Double):Double = ls match {
    case h0::h1::t => calAnshelper(h1::t, acc + distSq(h0, h1))
    case _ => acc
  }
  def trans(arr:List[Point]):List[Point] ={
    (arr zip List(arr.sortBy(x => (x.y, x.x)).head)
      .flatMap(x => List.fill(arr.length)(x)))
      .map(x => x._1 - x._2)
      .filterNot(_==Point(0,0))

  }

  def lr(left:Point,right:Point, todo: List[Point],ls:List[Point], rs:List[Point]):(List[Point], List[Point]) = {
    todo match {
      case Nil => (left::ls, right::rs)
      case h::t => {
        val nl = nextLeft(left, todo)
        val nr = nextRight(right, todo)
        if(nl == nr) (nl::left::ls, nr::right::rs)
        else lr(nl, nr, remove(nl, nr, todo.filterNot(x => x == nl || x == nr)), left::ls, right::rs)
      }
    }
  }

  def remove(left:Point, right: Point, todo:List[Point]):List[Point] = {
    todo.filter(p => line(left, right)(p) * line(left, right)(Point(0,0)) < 0)
  }
  def line(A:Point, B:Point)(C:Point):Double = {
    if(A.x == B.x) {C.x - A.x}
    else {
      val k = (A.y - B.y).toDouble / (A.x - B.x).toDouble // kx + b - y = 0
      val b = A.y - k * A.x
      k*C.x + b - C.y
    }
  }
  def nextLeft(h:Point, t:List[Point]):Point = {
    (t.filter(_.x < h.x).sortBy(x=> (- (x.y - h.y).toDouble/(x.x - h.x).toDouble, -x.x)) ++
      t.filter(_.x == h.x).sortBy(_.y) ++
      t.filter(_.x > h.x).sortBy(x=> (- (x.y - h.y).toDouble/(x.x - h.x).toDouble, x.x))).head
  }
  def nextRight(h:Point, t:List[Point]):Point = {
    (t.filter(_.x > h.x).sortBy(x=> ((x.y - h.y).toDouble/(x.x - h.x).toDouble, x.x))  ++
      t.filter(_.x == h.x).sortBy(_.y) ++
      t.filter(_.x < h.x).sortBy(x=> ((x.y - h.y).toDouble/(x.x - h.x).toDouble, -x.x))
      ).head
  }

  def distSq(p1:Point, p2:Point):Double = {
    Math.sqrt((List(p1.x, p1.y) zip List(p2.x, p2.y)).map(x => x._1 - x._2).map(x => x * x).sum)
  }
  def submit():Unit = {
    List(solver(List.fill(scala.io.StdIn.readInt)(arr2Point(scala.io.StdIn.readLine.trim.split(" ").map(_.toInt)))
    )).foreach(println)
  }
  def test():Unit = {
    val l = List(Point(1,1), Point(2,5), Point(3,3), Point(5,3), Point(3,2), Point(2,2))
    println(solver(l))
  }
  def test2():Unit = {
    val lines= scala.io.Source.fromFile("./src/test.txt").getLines()
    val n = lines.next().toInt
    println(solver(List.fill(n)(arr2Point(lines.next().split(" ").map(_.toInt)))))

  }
  def main(args: Array[String]): Unit = {
    submit()
   }
}
