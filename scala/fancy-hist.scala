// some unfinish, unpolished tries...
// not really worth reading here

object HIST extends App {
  def noise(max:Double=1000d)(n:Int=100) = {
    import scala.util.Random.{nextDouble => G}
    List.fill(n)(G) map (_ * max)
  }

  def rnorm(n:Int=100) = {
    import scala.util.Random.{nextGaussian => G}
    List.fill(n)(G)
  }

  def hist(xs:List[Double], breaks:Int=10) = {
    val min = xs.min
    val max = xs.max

    val ð = 0.0000001d // to include the last bin! ↓
    val ticks = (min to max by ((max+ð-min)/breaks))

    val bins = ticks.sliding(2,1).map{ case Vector(x,y) => (x,y)}.toList

    // necessary to groupBy appartenance to a bin when computing inBin and the hist
    val indexedBins = bins.zipWithIndex.toList

    // find the bin in which a double resiseds in
    def inBin(v:Double) = indexedBins.find { case ((min, max),i) => min <= v && v <= max }.map(_._2)

    // compute the bin in which each value resides in
    val binned = xs map {x => (inBin(x), x)} collect {case (Some(i), v) => (i,v) }

    // create the hist => grouping by the bin then computing the size => shuffle + reduce
    val hist = binned groupBy (_._1) mapValues (_.size)

    // returns the bin info along with the computed bin
    (bins, hist)
  }

  def barPlot(points:List[(Double, Double)])= {
    val maxCount = points.maxBy(_._2)

    //create a loooon Char sequence for the whole hist
    val chars = for {
      l      <- maxCount._2 to 0 by -1
      (i, c) <- points
    } yield if (c < l) ' ' else 'X' // nothing to be printed when height is not enough

    // reshaping the chars and converting a row in a String
    val lines = chars.sliding(points.size, points.size) map (_.mkString) toList

    lines
  }

  def scatterPlot(points:List[(Double, Double)])= {
    val maxCount = points.maxBy(_._2)
    val minCount = points.minBy(_._2)

    //create a loooon Char sequence for the whole hist
    val chars = for {
      l      <- maxCount._2 to minCount._2 by -1
      (i, c) <- points
    } yield if (c.toInt - l.toInt != 0) ' ' else 'X'

    // reshaping the chars and converting a row in a String
    val lines = chars.sliding(points.size, points.size) map (_.mkString) toList

    lines
  }

  def plot(bins:List[(Double, Double)], hist:Map[Int, Int]) = {
    // compute a sorted version of the values => printed in the right order: by bin
    val sortedList = hist.toList.sortBy(_._1).map { case (x,y) => (x.toDouble,y.toDouble)}

    val lines = barPlot(sortedList)

    val binIndexes = (1 to bins.size).map(_.toString).toList

    val maxBinStringLength = binIndexes map (_.size) max
    val names = binIndexes map (s => s.padTo(maxBinStringLength, " ").mkString)

    val xlab = printLayer(names)
    val ylab = (lines.size to 1 by -1).toList map (_.toString)

    val plot = col2(ylab, lines) ::: xlab
    val legend = bins.zipWithIndex.map(_.swap) map (_.toString)
    (plot, legend)
  }

  def draw(plot:(List[String], List[String])) =  col2(plot._1, plot._2, " | ") foreach println

  def col2(col1:List[String], col2:List[String], sep:String=""):List[String] = {
    val col1Max = col1 map (_.size) max
    val padCol1 = col1 map {_.padTo(col1Max, " ").mkString}
    val col1Enxtended:List[String] =
      if (padCol1.size < col2.size) {
        val extension = List.fill(col2.size-padCol1.size)(" "*col1Max)
        padCol1 ::: extension
      } else
        padCol1
    val concatCols:List[String] = (col1Enxtended zip col2) map { case (x,y) => x+sep+y }
    concatCols ::: (col1Enxtended.drop(col2.size))
  }

  def printLayer(xs:List[String]):List[String] = {
    if (xs forall (_.isEmpty)) {
      Nil
    } else {
      val l =(xs map (_.headOption.getOrElse(" ")) mkString)
      val tails = printLayer(xs map (s => if (s.isEmpty) "" else s.tail.mkString))
      l :: tails
    }
  }

  def test = {
    val values = rnorm(1000)
    val (bins, h) = hist(values, 100)
    draw(plot(bins, h))
  }

  def testG(g:Int => List[Double] = rnorm) = {
    val values = g(1000)
    val (bins, h) = hist(values, 100)
    draw(plot(bins, h))
  }


  def lm(xs:List[Double], ys:List[Double]) = {
    val ẍ = xs.sum / xs.size
    val ÿ = ys.sum / ys.size
    val Sp = (xs map (_ - ẍ)) zip (ys map (_ - ÿ)) map { case (x,y) => x * y } sum
    val Sx2 = (xs map (x => math.pow(x - ẍ, 2))) sum
    val ß1 = Sp / Sx2
    val ß0 = ÿ - ß1 * ẍ
    (ß0, ß1)
  }

  object Implicits {
    implicit class Vectorized(xs:List[Double]) {
      private[this] def cb(other:List[Double])(at:(Double,Double)=>Double):List[Double] = (xs zip other) map at.tupled
      def /×(other:List[Double]) = cb(other) { case (x,y) => x*y }
      def \×(scalar:Double) = xs map { _* scalar }
      def /+(other:List[Double]) = cb(other) { case (x,y) => x+y }
    }
  }

  import Implicits._

  def test2 = {
    val beta1 = rnorm(1).head

    val example = (1d to 10 by 1d).toList  \× beta1 /+ ((noise(0.99d)(10)))
    println(example)

    val points = example.zipWithIndex.map(_.swap).map { case (i,c) => (i.toDouble, c)}
    println(points)

    val ylab = (points.head._2 to points.last._2 by -1).map(_.toString)
    println(ylab)

    col2(ylab.toList, scatterPlot(points)) foreach println
  }

  test
}
