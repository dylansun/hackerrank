/**
  * Created by lilisun on 3/21/19.
  */
object ProjectEuler10 {
 /*
  def P10(n:Int):Int = {
    val r = Math.sqrt(n).toInt//int(n**0.5)
    //assert r*r <= n and (r+1)**2 > n
    var V = (for(i <- 1 to r) yield n / i).toList //V = [n//i for i in range(1,r+1)]
    V = V:::(1 until V.last).toList//V = list(range(V[-1]-1,0,-1))
    S = {i:i*(i+1)//2-1 for i in V}
      for p in range(2,r+1):
      if S[p] > S[p-1]:  # p is prime
      sp = S[p-1]  # sum of primes smaller than p
        p2 = p*p
      for v in V:
      if v < p2: break
      S[v] -= p*(S[v//p] - sp)
      return S[n]
  }*/


  def primes(N:Int):List[Int] ={
    (2 to N).toList.filter(isPrimes)
  }
  def isPrimes(n:Int):Boolean = {
    (2 to Math.sqrt(n).toInt).forall( x => n % x!=0)
  }

  def ans(n:Int):Int = primes(n).sum
  def goodluck():Unit = {
    val sc = new java.util.Scanner (System.in)
    val t = sc.nextInt()

    for(_  <- 1 to t){
      val n = sc.nextInt()
      println(ans(n))
    }
  }

}
