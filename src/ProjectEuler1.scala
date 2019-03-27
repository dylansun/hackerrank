/**
  * Created by lilisun on 3/21/19.
  */
object ProjectEuler1 {
  def f(n:Int):BigInt = {
    def multiple(m:Int):Int = (n-1) / m
    val l = List[Int](3, 5, 15)
    val cl = l.map(multiple).map( x => BigInt(x)*BigInt(x+1)/2)
    println(cl)
    l(0)*cl(0) + l(1)*cl(1) - l(2)*cl(2)
  }
  def ans(n:Int):BigInt = {
    (1 until n).filter(x => x % 3 == 0 || x %5 == 0).sum
  }

  def test():Unit = {
    println(f(1000000000))
    //(1 to 100).foreach(x => println(f(x) == ans(x)))
  }
  def goodluck():Unit =  {
    val sc = new java.util.Scanner (System.in)
    val t = sc.nextInt()
    var a0 = 0
    while(a0 < t){
      val n = sc.nextInt()
      println(f(n))
      a0+=1
    }
  }
  def main(args: Array[String]): Unit = {
    test()
  }
}
