object Implicits {
  implicit class Vectorized(xs:List[Double]) {
    private[this] def cb(other:List[Double])(at:(Double,Double)=>Double):List[Double] = (xs zip other) map at.tupled
    private[this] def cb(scalar:Double)(at:(Double,Double)=>Double):List[Double] = xs map { at(_, scalar) }
    def ∙*(other:List[Double]):List[Double] = cb(other){_*_}
    def ∙*(scalar:Double):List[Double] = cb(scalar){_*_}
    def ∙+(other:List[Double]):List[Double] = cb(other){_+_}
    def ∙+(scalar:Double):List[Double] = cb(scalar){_+_}
    def ∙-(other:List[Double]):List[Double] = cb(other){_-_}
    def ∙-(scalar:Double):List[Double] = cb(scalar){_-_}
    def ∙^(scalar:Double):List[Double] = cb(scalar){math.pow(_, _)}

    def ∙*:(other:List[Double]):List[Double] = cb(other){_*_}
    def ∙*:(scalar:Double):List[Double] = cb(scalar){_*_}
    def ∙+:(other:List[Double]):List[Double] = cb(other){_+_}
    def ∙+:(scalar:Double):List[Double] = cb(scalar){_+_}
    def ∙-:(other:List[Double]):List[Double] = cb(other){_-_}
    def ∙-:(scalar:Double):List[Double] = cb(scalar){_-_}
    def ∙^:(scalar:Double):List[Double] = cb(scalar){math.pow(_, _)}


    def |*(other:List[Double]):Double = (this ∙* other).sum
  }

  implicit class DoublePlus(d:Double) {
    def ^(e:Double) = math.pow(d, e)
  }

  class Matrix(val rows:List[List[Double]]) {
    val nrow = rows.size
    val ncol = rows.head.size
    lazy val t = Matrix(for {
      i   <- (0 until ncol).toList
      row <- rows
    } yield row(i), nrow)
    def *(other:Matrix) = {
      if (ncol != other.nrow) throw new IllegalArgumentException("Bad *")
      else {
        val ot = other.t
        for {
          r <- rows
          c <- ot.rows
        } yield r |* c
      }
    }
  }
  object Matrix {
    def apply(items:List[Double], ncol:Int):Matrix = new Matrix(items.sliding(ncol, ncol).toList)
  }
}
