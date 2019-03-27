/**
  * Created by lilisun on 3/26/19.
  */
import scala.io.StdIn.{readInt, readLine}
object fpDicePath {

  case class Dice(top:Int, btm:Int, lft:Int, rit:Int, frt:Int, bak:Int)
  case class State(x:Int, dice:Dice)
  def rotR(dice: Dice):Dice =  Dice(dice.lft, dice.rit, dice.btm, dice.top, dice.frt, dice.bak)
  def rotD(dice: Dice):Dice =  Dice(dice.bak, dice.frt, dice.lft, dice.rit, dice.top, dice.btm)
  def movR(state: State) = State(state.x + rotR(state.dice).top, rotR(state.dice))
  def movD(state: State) = State(state.x + rotD(state.dice).top, rotD(state.dice))

  val memDice:Array[Array[Set[State]]] = Array.fill(61,61)(Set[State]()) // diceValue at position (i,j)
  def getState(m:Int,n:Int):Set[State] = {
    if(memDice(m)(n).nonEmpty) memDice(m)(n)
    else {
      val ans = (m, n) match {
        case (0,_) => Set[State]()
        case (_,0) => Set[State]()
        case (1,1) => Set(State(1,Dice(1,6, 3, 4, 2, 5)))
        case (_,1) => getState(m-1,1).map(movD)
        case (1,_) => getState(1, n-1).map(movR)
        case (_,_) => getState(m-1,n).map(movD) ++ getState(m, n-1).map(movR)
      }
      memDice(m)(n) = ans
      ans
    }
  }


  def main(args: Array[String]): Unit = {
    for(_ <- 1 to readInt)
      println(List(readLine.trim.split(" ").map(_.toInt)).map(arr=>getState(arr.array(0), arr.array(1))).map(l => l.map(state => state.x).max).mkString)
  }
}
