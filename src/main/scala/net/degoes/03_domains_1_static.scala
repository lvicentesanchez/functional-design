package net.degoes

object spreadsheet_static {
  trait Spreadsheet {
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue[Any, Any]

    final def scan(range: Range): Stream[Cell] = {
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for {
        col <- (minCol to maxCol).toStream
        row <- (minRow to maxRow).toStream
      } yield Cell(col, row, valueAt(col, row)))
    }
  }

  final case class Range(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
  object Range {
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)
  }

  final case class Cell(col: Int, row: Int, contents: CalculatedValue[Any, Any])

  sealed trait CellType[+A]
  object CellType {
    implicit case object Int    extends CellType[scala.Int]
    implicit case object String extends CellType[java.lang.String]
    implicit case object Double extends CellType[scala.Double]
    implicit case object Float  extends CellType[scala.Float]
    implicit case object Long   extends CellType[scala.Long]
  }

  sealed trait Value[+E, +A]
  object Value {
    final case class Error[E](message: E)                        extends Value[E, Nothing]
    final case class Success[A](value: A, cellType: CellType[A]) extends Value[Nothing, A]
  }

  import CalculatedValue.const

  // const("foo") + const("bar")
  const(1) - const(2)
  const(1.2) - const(2.1)
  const(1L) - const(2L)

  /**
   * EXERCISE 1
   *
   * Design a data type called `CalculatedValue`, which represents a `Value` that is dynamically
   * computed from a `Spreadsheet`.
   */
  final case class CalculatedValue[+E, +A](evaluate: Spreadsheet => Value[E, A]) { self =>
    def asInt(implicit ev: E <:< String): CalculatedValue[String, Int] =
      CalculatedValue { s =>
        self.evaluate(s) match {
          case Value.Error(message) => Value.Error(ev(message))
          case Value.Success(value, cellType) =>
            if (cellType == CellType.Int) Value.Success(value.asInstanceOf[Int], CellType.Int)
            else Value.Error(s"Expected Int but found ${cellType}")
        }
      }

    /**
     * EXERCISE 2
     *
     * Add an operator that returns a new `CalculatedValue` that is the negated version of this one.
     */
    def unary_-[E1 >: E, A1 >: A](implicit numeric: Numeric[A1]): CalculatedValue[E1, A1] =
      CalculatedValue { s =>
        import Value._

        self.evaluate(s) match {
          case Success(value, cellType) => Success(numeric.negate(value), cellType)
          case Error(message)           => Error(message)
        }
      }

    /**
     * EXERCISE 3
     *
     * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
     * calculated values.
     */
    def +[A1 >: A](
      that: CalculatedValue[String, A1]
    )(implicit ev: E <:< String, numeric: Numeric[A1]): CalculatedValue[String, A1] =
      self.mapError(ev).binaryOp(that)("Can only add numeric values") {
        case (Value.Success(l, ct), Value.Success(r, _)) => Value.Success(numeric.plus(l, r), ct)
      }

    /**
     * EXERCISE 4
     *
     * Add a binary operator `-` that returns a new `CalculatedValue` that is the difere;nce of the
     * two calculated values.
     */
    def -[A1 >: A](
      that: CalculatedValue[String, A1]
    )(implicit ev: E <:< String, numeric: Numeric[A1]): CalculatedValue[String, A1] =
      self.mapError(ev) + (-that)

    protected def binaryOp[E1 >: E, A1 >: A](that: CalculatedValue[E1, A1])(error: E1)(
      f: PartialFunction[(Value[E1, A1], Value[E1, A1]), Value[E1, A1]]
    ): CalculatedValue[E1, A1] =
      CalculatedValue { s =>
        f.lift((self.evaluate(s), that.evaluate(s))).getOrElse(Value.Error(error))
      }

    protected def mapError[E2](f: E => E2): CalculatedValue[E2, A] =
      CalculatedValue(s =>
        self.evaluate(s) match {
          case Value.Error(e)      => Value.Error(f(e))
          case Value.Success(v, c) => Value.Success(v, c)
        }
      )
  }
  object CalculatedValue {

    /**
     * EXERCISE 5
     *
     * Add a constructor that makes an `CalculatedValue` from a `Value`.
     */
    def const[A](contents: A)(implicit cellType: CellType[A]): CalculatedValue[Nothing, A] =
      CalculatedValue(_ => Value.Success(contents, cellType))

    /**
     * EXERCISE 6
     *
     * Add a constructor that provides access to the value of the
     * specified cell, identified by col/row.
     */
    def at(col: Int, row: Int): CalculatedValue[Any, Any] =
      CalculatedValue(s => s.valueAt(col, row).evaluate(s))
  }

  /**
   * EXERCISE 7
   *
   * Describe a cell whose contents are the sum of the cells at (0, 0) and (1, 0).
   */
  lazy val cell1: Cell = ???
}
