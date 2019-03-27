
import scala.collection.mutable
object ProjectEuler231 {
  def solver(N:Int, M:Int, k :Int):Unit = {
    //prime factorizastion
    val p = primes(N)
    val pfcnm = mutable.HashMap[Int, Int]()// prime, count
    // C(n,m) = n! / m! / (n - m)!

    val m = if(M > N/2) N-M else M

    (N-m+1 to N)
      .toList
      .map(x => primefactor(x, p, List[Int]()))
      .foreach(l => l.foreach(y => pfcnm.put(y, pfcnm.getOrElse(y, 0) +1)))
    (2 to m)
      .toList
      .map(x => primefactor(x, p, List[Int]()))
      .foreach(l => l.foreach(y => pfcnm.put(y, pfcnm.getOrElse(y, 0) -1)))
    for(x <- pfcnm.keySet if pfcnm(x) ==0) pfcnm -= x

    val candidate = pfcnm.keySet.toList
    var ans = List[List[BigInt]]()
    val n = candidate.length
    val l = Array.fill(k, n)(BigInt(0))
    val cuml = Array.fill(k, n)(BigInt(0))
    for(i <- 0 until k){
      i match {
        case 0 => for(j <- l(i).indices) l(i)(j) = candidate(j)
        case _ => {
          val q = i + 1
          for(j <- 0 until n-1){
            if(pfcnm(candidate(j)) >= q) l(i)(j) = bigIntPower(candidate(j), q)
            for(loop_k <- 1 until q if pfcnm(candidate(j)) >= loop_k ) {
        //      println(s"${BigInt(Math.pow(candidate(j), loop_k).toInt)} * ${cuml(i-loop_k)(j+1)}")
              l(i)(j) += bigIntPower(candidate(j), loop_k) * cuml(i-loop_k)(j+1)
            }
          }

          //the last prime
          if(pfcnm(candidate(n-1)) >= q) l(i)(n-1) = bigIntPower(candidate(n-1), q)
        }
      }
      cuml(i)(n-1) = l(i)(n-1)
      for(j <- (0 until n-1).reverse){
        cuml(i)(j) = cuml(i)(j+1) + l(i)(j)
      }
      //println(s"sum level $i: ${l(i).toList}")
      //println(s"cumlevel $i: ${cuml(i).toList}")
      println(cuml(i)(0))
    }

   // (1 to k)
   //   .foreach( x =>{
   //     val ans = dfs(pfcnm, candidate,x, List[List[BigInt]]())
   //     println(ans.map(_.product).sum)
   //   })
  }




  def bigIntPower(num:BigInt, k:Int ):BigInt = bigIntPower(num, k, 1)
  def bigIntPower(num:BigInt, k:Int, acc:BigInt ):BigInt = k match {
    case 0 => acc
    case _ => bigIntPower(num, k-1, acc * num)
  }
  def primefactor(x:Int, primes:List[Int], acc:List[Int]):List[Int] = x match {
    case 1 => acc
    case _ => x % primes.head match {
      case 0 => primefactor(x / primes.head, primes, primes.head::acc)
      case _ => primefactor(x, primes.tail, acc)
    }
  }

  def primes(N:Int):List[Int] ={
    (2 to N).toList.filter(isPrimes)
  }
  def isPrimes(n:Int):Boolean = {
    (2 to Math.sqrt(n).toInt).forall( x => n % x!=0)
  }

  // node, max visit time
  def dfs(table: mutable.HashMap[Int, Int], candidate:List[Int], k:Int,acc:List[List[BigInt]]):List[List[BigInt]] = k match {

    case 0 => acc
    case _ => acc match {
      case Nil => {
        val new_acc = candidate.map(x => List(BigInt(x)))
        dfs(table,candidate, k-1, new_acc)
      }
      case _ => {
        val new_acc = (for(path <- acc) yield{
          for(x <- candidate if x >= path.head && table(x) > path.count(_==x)) yield{ //
            BigInt(x)::path
          }
        }).flatten
        dfs(table,candidate, k-1, new_acc)
      }
    }
  }

  def test():Unit = {
    val line = scala.io.StdIn.readLine.trim.split(" ")
    solver(line(0).toInt, line(1).toInt, line(2).toInt)
  }
  def main(args: Array[String]): Unit = {
    val N = 9
    val M = 1
    val k = 4
    val ans = solver(N,M,k)
    //time0: 1043
    //1043,1126,1161, 1169,1200
    //68884
    //43497469555384
    // ans.foreach(x => println(x))
  }
}
