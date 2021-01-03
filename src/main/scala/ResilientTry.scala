import scala.runtime.Statics
import scala.util.control.NonFatal

sealed abstract class ResilientTry[+T] extends Product with Serializable {

  def failed: ResilientTry.ResilientTryFailedProjection[T] = {
    ResilientTry.ResilientTryFailedProjection(this)
  }

  def success: ResilientTry.ResilientTrySuccessProjection[T] = {
    ResilientTry.ResilientTrySuccessProjection(this)
  }

  def isFailure: Boolean

  def isSuccess: Boolean

  def getOrElse[U >: T](default: => U): U

  def orElse[U >: T](default: => ResilientTry[U]): ResilientTry[U]

  def foreach[U](f: T => U): Unit

  def flatMap[U](f: T => ResilientTry[U]): ResilientTry[U]

  def map[U](f: T => U): ResilientTry[U]

  def collect[U](pf: PartialFunction[T, U]): ResilientTry[U]

  def filter(p: T => Boolean): ResilientTry[T]

  @inline final def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

  final class WithFilter(p: T => Boolean) {
    def map[U](f: T => U): ResilientTry[U] = ResilientTry.this filter p map f
    def flatMap[U](f: T => ResilientTry[U]): ResilientTry[U] =
      ResilientTry.this filter p flatMap f
    def foreach[U](f: T => U): Unit = ResilientTry.this filter p foreach f
    def withFilter(q: T => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }

  def recoverWith[U >: T](
      pf: PartialFunction[ResilientTryError[Throwable, DomainError], ResilientTry[U]]
  ): ResilientTry[U]

  def recover[U >: T](pf: PartialFunction[ResilientTryError[Throwable, DomainError], U]): ResilientTry[U]
  def recoverThrowable[U >: T](
      pf: PartialFunction[Throwable, U]
  ): ResilientTry[U]

  def toOption: Option[T]

  def flatten[U](implicit ev: T <:< ResilientTry[U]): ResilientTry[U]

  def transform[U](
      s: T => ResilientTry[U],
      f: ResilientTryError[Throwable, DomainError] => ResilientTry[U]
  ): ResilientTry[U]

  def fold[U](fa: ResilientTryError[Throwable, DomainError] => U, fb: T => U): U

}

object ResilientTry {

  def apply[T](r: => T): ResilientTry[T] =
    try ResilientTrySuccess(r)
    catch {
      case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
    }

  final case class ResilientTryFailedProjection[+T](e: ResilientTry[T]) {
    def get: ResilientTryError[Throwable, DomainError] = {
      e match {
        case ResilientTryFailed(b) => b
        case _                     => throw new NoSuchElementException("Either.right.get on Left")
      }
    }

    def getOrElse[U >: T](default: => U): U = default

    def orElse[U >: T](default: => ResilientTry[U]): ResilientTry[U] = {
      try default
      catch {
        case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
      }
    }

    def flatMap[U](f: T => ResilientTry[U]): ResilientTry[U] = {
      this.asInstanceOf[ResilientTry[U]]
    }

    def flatten[U](implicit ev: T <:< ResilientTry[U]): ResilientTry[U] = {
      this.asInstanceOf[ResilientTry[U]]
    }

    def foreach[U](f: T => U): Unit = {
      ()
    }

    def transform[U](
        s: T => ResilientTry[U],
        f: ResilientTryError[Throwable, DomainError] => ResilientTry[U]
    ): ResilientTry[U] = {
      e match {
        case ResilientTryFailed(error) =>
          try f(error)
          catch {
            case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
          }
        case _ => throw new NoSuchElementException("Either.right.get on Left")
      }
    }

    def map[U](f: T => U): ResilientTry[U] = {
      this.asInstanceOf[ResilientTry[U]]
    }

    def collect[U](pf: PartialFunction[T, U]): ResilientTry[U] = {
      this.asInstanceOf[ResilientTry[U]]
    }

    def filter(p: T => Boolean): ResilientTry[T] = {
      e match {
        case ResilientTryFailed(error) => ResilientTryFailed(error)
        case _                         => throw new NoSuchElementException("Either.right.get on Left")
      }
    }

    def recover[U >: T](pf: PartialFunction[ResilientTryError[Throwable, DomainError], U]): ResilientTry[U] = {
      e match {
        case ResilientTryFailed(error) => {
          val marker = Statics.pfMarker
          try {
            val v = pf.applyOrElse(
              error,
              (_: ResilientTryError[Throwable, DomainError]) => marker
            )
            if (marker ne v.asInstanceOf[AnyRef])
              ResilientTrySuccess(v.asInstanceOf[U])
            else ResilientTryFailed(error)
          } catch {
            case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
          }
        }
        case ResilientTrySuccess(value) => throw new NoSuchElementException("Either.right.get on Left")
      }
    }

    def recoverWith[U >: T](
        pf: PartialFunction[
          ResilientTryError[Throwable, DomainError],
          ResilientTry[U]
        ]
    ): ResilientTry[U] = {
      e match {
        case ResilientTryFailed(error) => {
          val marker = Statics.pfMarker
          try {
            val v = pf.applyOrElse(
              error,
              (_: ResilientTryError[Throwable, DomainError]) => marker
            )
            if (marker ne v.asInstanceOf[AnyRef]) v.asInstanceOf[ResilientTry[U]]
            else ResilientTryFailed(error)
          } catch {
            case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
          }

        }
        case ResilientTrySuccess(value) => throw new NoSuchElementException("Either.right.get on Left")
      }
    }

    def fold[U](fa: ResilientTryError[Throwable, DomainError] => U, fb: T => U): U = {
      e match {
        case ResilientTryFailed(error) => {
          fa(error)
        }
        case ResilientTrySuccess(value) => throw new NoSuchElementException("Either.right.get on Left")
      }
    }
  }

  final case class ResilientTrySuccessProjection[+T](e: ResilientTry[T]) {}
}

final case class ResilientTryFailed[+T](error: ResilientTryError[Throwable, DomainError]) extends ResilientTry[T] {

  override def isFailure: Boolean = true

  override def isSuccess: Boolean = false

  def get: ResilientTryError[Throwable, DomainError] = error

  override def getOrElse[U >: T](default: => U): U = default

  override def orElse[U >: T](default: => ResilientTry[U]): ResilientTry[U] = {
    try default
    catch {
      case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
    }
  }

  override def flatMap[U](f: T => ResilientTry[U]): ResilientTry[U] = {
    this.asInstanceOf[ResilientTry[U]]
  }

  override def flatten[U](implicit ev: T <:< ResilientTry[U]): ResilientTry[U] = {
    this.asInstanceOf[ResilientTry[U]]
  }

  override def foreach[U](f: T => U): Unit = {
    ()
  }

  override def transform[U](
      s: T => ResilientTry[U],
      f: ResilientTryError[Throwable, DomainError] => ResilientTry[U]
  ): ResilientTry[U] = {
    try f(error)
    catch {
      case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
    }
  }

  override def map[U](f: T => U): ResilientTry[U] = {
    this.asInstanceOf[ResilientTry[U]]
  }

  override def collect[U](pf: PartialFunction[T, U]): ResilientTry[U] = {
    this.asInstanceOf[ResilientTry[U]]
  }

  override def filter(p: T => Boolean): ResilientTry[T] = {
    this
  }

  override def recover[U >: T](pf: PartialFunction[ResilientTryError[Throwable, DomainError], U]): ResilientTry[U] = {
    val marker = Statics.pfMarker
    try {
      val v = pf.applyOrElse(
        error,
        (_: ResilientTryError[Throwable, DomainError]) => marker
      )
      if (marker ne v.asInstanceOf[AnyRef])
        ResilientTrySuccess(v.asInstanceOf[U])
      else this
    } catch {
      case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
    }
  }

  override def recoverWith[U >: T](
      pf: PartialFunction[
        ResilientTryError[Throwable, DomainError],
        ResilientTry[U]
      ]
  ): ResilientTry[U] = {
    val marker = Statics.pfMarker
    try {
      val v = pf.applyOrElse(
        error,
        (_: ResilientTryError[Throwable, DomainError]) => marker
      )
      if (marker ne v.asInstanceOf[AnyRef]) v.asInstanceOf[ResilientTry[U]]
      else this
    } catch {
      case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
    }
  }

  def recoverThrowable[U >: T](
      pf: PartialFunction[Throwable, U]
  ): ResilientTry[U] = {
    error match {
      case ResilientTryErrorThrowable(value) => {
        val marker = Statics.pfMarker
        try {
          val v = pf.applyOrElse(
            value,
            (_: Throwable) => marker
          )
          if (marker ne v.asInstanceOf[AnyRef]) v.asInstanceOf[ResilientTry[U]]
          else this
        } catch {
          case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
        }
      }
      case ResilientTryErrorDomainError(value) => this
    }
  }

  override def toOption: Option[T] = None

  override def fold[U](fa: ResilientTryError[Throwable, DomainError] => U, fb: T => U): U = {
    fa(error)
  }
}

final case class ResilientTrySuccess[+T](value: T) extends ResilientTry[T] {

  override def isFailure: Boolean = false

  override def isSuccess: Boolean = true

  def get: T = value

  override def getOrElse[U >: T](default: => U): U = {
    get
  }

  override def orElse[U >: T](default: => ResilientTry[U]): ResilientTry[U] = {
    this
  }

  override def flatMap[U](f: T => ResilientTry[U]): ResilientTry[U] = {
    try f(value)
    catch {
      case NonFatal(e) => ResilientTryFailed(ResilientTryErrorThrowable(e))
    }
  }

  override def flatten[U](implicit ev: T <:< ResilientTry[U]): ResilientTry[U] = {
    value
  }

  override def foreach[U](f: T => U): Unit = {
    f(value)
  }

  override def transform[U](
      s: T => ResilientTry[U],
      f: ResilientTryError[Throwable, DomainError] => ResilientTry[U]
  ): ResilientTry[U] = {
    this flatMap s
  }

  override def map[U](f: T => U): ResilientTry[U] = {
    ResilientTry[U](f(value))
  }

  override def collect[U](pf: PartialFunction[T, U]): ResilientTry[U] = {
    val marker = Statics.pfMarker
    try {
      val v =
        pf.applyOrElse(value, ((_: T) => marker).asInstanceOf[Function[T, U]])
      if (marker ne v.asInstanceOf[AnyRef]) ResilientTrySuccess(v)
      else
        ResilientTryFailed(
          ResilientTryErrorThrowable(
            new NoSuchElementException("Predicate does not hold for " + value)
          )
        )
    } catch {
      case NonFatal(e) =>
        ResilientTryFailed(
          ResilientTryErrorThrowable(
            e
          )
        )
    }
  }

  override def filter(p: T => Boolean): ResilientTry[T] = {
    try {
      if (p(value)) this
      else
        ResilientTryFailed[T](
          ResilientTryErrorThrowable(
            new NoSuchElementException("Predicate does not hold for " + value)
          )
        )
    } catch {
      case NonFatal(e) =>
        ResilientTryFailed[T](ResilientTryErrorThrowable(e))
    }
  }

  override def recover[U >: T](pf: PartialFunction[ResilientTryError[Throwable, DomainError], U]): ResilientTry[U] = {
    this
  }

  override def recoverWith[U >: T](
      pf: PartialFunction[ResilientTryError[Throwable, DomainError], ResilientTry[U]]
  ): ResilientTry[U] = {
    this
  }

  def recoverThrowable[U >: T](
      pf: PartialFunction[Throwable, U]
  ): ResilientTry[U] = {
    this
  }

  override def toOption: Option[T] = {
    Some(value)
  }

  override def fold[U](fa: ResilientTryError[Throwable, DomainError] => U, fb: T => U): U = {
    try { fb(value) }
    catch { case NonFatal(e) => fa(ResilientTryErrorThrowable(e)) }
  }
}
