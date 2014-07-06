import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random.{nextInt, nextBoolean}

trait Lazy[+A] {
  val head:Option[A]
  def tail:Lazy[A]
  def ::[AA >: A](a:AA):Lazy[AA]
  def map[B](f:A=>B):Lazy[B]
  def each[B](f:A=>Unit):Unit
}
class LazyCons[+A](a:A, t: => Lazy[A]) extends Lazy[A] {
  val head = Some(a)
  lazy val tail = t
  def ::[AA >: A](a:AA) = new LazyCons[AA](a, this)

  def map[B](f:A=>B):Lazy[B] = new LazyCons(f(a), tail.map(f))

  def each[B](f:A=>Unit):Unit = {
    f(head.get)
    tail each f
  }
}
object EmptyLazy extends Lazy[Nothing] {
  val head = None
  def tail = throw new IllegalStateException("Cannot call tail on Empty")
  def ::[A](a:A) = new LazyCons[A](a, this)

  def map[B](f:Nothing=>B):Lazy[B] = this

  def each[B](f:Nothing=>Unit):Unit = ()
}
object Lazy {
  val empty = EmptyLazy
  def single[A](a:A) = new LazyCons(a, empty)
  def cons[A](a:A, t: =>Lazy[A]) = new LazyCons(a, t)
  def ints(start:Int):Lazy[Int] = cons(start, ints(start+1))
}

object ExampleLazy extends App {
  // simulate data collected with delays
  // like an iterator but without blocking on next, and without side-effect (.next())
  def fetch(file:String):Lazy[Future[String]] = {
    val con = io.Source.fromFile(new java.io.File(file))
    val texts = con.getLines
    def readLine(texts:Iterator[String]):Lazy[Future[String]] = {
        if (texts.isEmpty)
          Lazy.empty
        else
          Lazy.cons(
            future {
              if (nextBoolean) Thread.sleep(nextInt(5)) else ()
              texts.next
            },
            readLine(texts)
          )
      }
    readLine(texts)
  }

  val lz = fetch("./lazy.scala")
  lz each {f =>
    f onSuccess {
      case x => println(x)
    }
  }
}
