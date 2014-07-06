import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random.{nextInt, nextBoolean}
import Implicits._

trait Data {
  def dependent:List[Double]
  def observed:Matrix
  def sampling(proportion:Double):Future[Data]
}
case class SimpleData(dependent:List[Double], variable:List[Double]) extends Data {
  override val observed = Matrix(variable, 1)

  def sampling(proportion:Double) = future {
    val n = (dependent.size*proportion).toInt
    val indices = List.fill(n)(nextInt(dependent.size))
    val dep = indices map dependent
    val v = indices map variable
    SimpleData(dep, v)
  }
}
trait Model {
  type Coefs
  def apply(data:Data):Future[Coefs]
}
object ModelLM extends LM with Model {
  type Coefs = (Double, Double)
  def apply(data:Data) = future { apply(data.observed.t.rows.head, data.dependent) }
}

trait Aggregation[A] extends (List[A]=>A)

object Bagging {
  def apply(model:Model)(agg:Aggregation[model.Coefs], n:Int)(data:Data):Future[model.Coefs] = {
    def exec:Future[model.Coefs] =  for {
                                      sample <- data.sampling(0.6)
                                      coefs <- model(sample)
                                    } yield coefs
    val execs:List[Future[model.Coefs]] = List.fill(n)(exec)
    val coefsList:Future[List[model.Coefs]] = Future.sequence(execs)

    val result:Future[model.Coefs] = coefsList map agg
    result
  }
}

object ExampleBagging extends App {
  val mean = new Aggregation[(Double, Double)] {
    def apply(xs:List[(Double, Double)]):(Double, Double) = {
      val ß0s = xs map (_._1)
      val ß1s = xs map (_._2)
      (ß0s.sum / ß0s.size, ß1s.sum / ß1s.size )
    }
  }

  val ß0:Double = 18.1d
  val ß1:Double = 6d
  val n = 10000
  val x:List[Double] = -n.toDouble to n by 1 toList
  val e = Util.rnorm(0, 10)(2*n+1)
  val y:List[Double] = ß0 ∙+: (ß1 ∙*: x) ∙+: e

  val data = SimpleData(y, x)

  List(1, 10, 100) foreach { n =>
    val coefs1 = Bagging(ModelLM)(mean, n)(data)
    coefs1 onSuccess { case x => println(x) }
  }
}
