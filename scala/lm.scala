import Implicits._

object Util {
  val product = {(_:Double)*(_:Double)}.tupled

  import scala.util.Random.nextGaussian
  def rnorm(mean:Double=0, sigma:Double=1)(n:Int) = mean ∙+: (List.fill(n)(nextGaussian) ∙* sigma)
}

trait LM {

  def apply(x:List[Double], y:List[Double]):(Double, Double) = {
    val n = x.size
    val ẍ = x.sum.toDouble / n
    val ÿ = y.sum.toDouble / n
    val Sp = ((x ∙- ẍ) ∙* (y ∙- ÿ) sum) / (n-1)
    val Sx2 = ((x ∙- ẍ) ∙^ 2 sum) / (n-1)
    val ß1 = Sp / Sx2
    val ß0 = ÿ - ß1 * ẍ
    (ß0, ß1)
  }

}

object ExampleLM extends App {
  import Util._
  import Implicits._

  val lm = new LM(){}

  def test(ß0:Double = 18.1d, ß1:Double = 6d) = {
    val n = 10000
    val x:List[Double] = -n.toDouble to n by 1 toList
    val e = rnorm(0, 10)(2*n+1)
    val y:List[Double] = ß0 ∙+: (ß1 ∙*: x) ∙+: e
    lm(x, y)
  }
  println(test(103, 7))
}
