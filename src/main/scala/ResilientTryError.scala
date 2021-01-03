sealed abstract class ResilientTryError[+Throwable, +DomainError] extends Product with Serializable {

  def throwable: ResilientTryError.ResilientTryErrorThrowableProjection[Throwable, DomainError] = {
    ResilientTryError.ResilientTryErrorThrowableProjection(this)
  }

  def domainError: ResilientTryError.ResilientTryErrorDomainErrorProjection[Throwable, DomainError] = {
    ResilientTryError.ResilientTryErrorDomainErrorProjection(this)
  }

  def fold[C](fa: Throwable => C, fb: DomainError => C): C =
    this match {
      case ResilientTryErrorDomainError(b) => fb(b)
      case ResilientTryErrorThrowable(a)   => fa(a)
    }

  def foreach[U](f: DomainError => U): Unit =
    this match {
      case ResilientTryErrorDomainError(b) => f(b)
      case _                               =>
    }

  final def containsDomainError[B1 >: DomainError](elem: B1): Boolean =
    this match {
      case ResilientTryErrorDomainError(b) => b == elem
      case _                               => false
    }

  final def containsThrowable[B1 >: Throwable](elem: B1): Boolean =
    this match {
      case ResilientTryErrorThrowable(b) => b == elem
      case _                             => false
    }

  def forallDomainError(f: DomainError => Boolean): Boolean =
    this match {
      case ResilientTryErrorDomainError(b) => f(b)
      case _                               => true
    }

  def forallThrowable(f: Throwable => Boolean): Boolean =
    this match {
      case ResilientTryErrorThrowable(b) => f(b)
      case _                             => true
    }

  def existsDomainError(p: DomainError => Boolean): Boolean =
    this match {
      case ResilientTryErrorDomainError(b) => p(b)
      case _                               => false
    }

  def existsThrowable(p: Throwable => Boolean): Boolean =
    this match {
      case ResilientTryErrorThrowable(b) => p(b)
      case _                             => false
    }

  def flatMap[A1 >: Throwable, B1](f: DomainError => ResilientTryError[A1, B1]): ResilientTryError[A1, B1] = {
    this match {
      case ResilientTryErrorDomainError(b) => f(b)
      case _                               => this.asInstanceOf[ResilientTryError[A1, B1]]
    }
  }

  def flatten[A1 >: Throwable, B1](implicit
      ev: DomainError <:< ResilientTryError[A1, B1]
  ): ResilientTryError[A1, B1] = {
    flatMap(ev)
  }

  def map[B1](f: DomainError => B1): ResilientTryError[Throwable, B1] = {
    this match {
      case ResilientTryErrorDomainError(b) =>
        ResilientTryErrorDomainError(f(b))
      case _ => this.asInstanceOf[ResilientTryError[Throwable, B1]]
    }
  }

  def isThrowable: Boolean

  def isDomainError: Boolean
}

final case class ResilientTryErrorThrowable[+Throwable, +DomainError](value: Throwable)
    extends ResilientTryError[Throwable, DomainError] {
  def isThrowable = true
  def isDomainError = false
}

final case class ResilientTryErrorDomainError[+A, +DomainError](value: DomainError)
    extends ResilientTryError[A, DomainError] {
  def isThrowable = false
  def isDomainError = true
}

object ResilientTryError {

  def cond[Throwable, DomainError](
      test: Boolean,
      right: => DomainError,
      left: => Throwable
  ): ResilientTryError[Throwable, DomainError] =
    if (test) ResilientTryErrorDomainError(right)
    else ResilientTryErrorThrowable(left)

  final case class ResilientTryErrorThrowableProjection[+Throwable, +DomainError](
      e: ResilientTryError[Throwable, DomainError]
  ) {

    @deprecated("use `Either.swap.getOrElse` instead", "2.13.0")
    def get: Throwable =
      e match {
        case ResilientTryErrorThrowable(a) => a
        case _                             => throw new NoSuchElementException("Either.left.get on Right")
      }

    def foreach[U](f: Throwable => U): Unit =
      e match {
        case ResilientTryErrorThrowable(a) => f(a)
        case _                             => ()
      }

    def getOrElse[A1 >: Throwable](or: => A1): A1 =
      e match {
        case ResilientTryErrorThrowable(a) => a
        case _                             => or
      }

    def forall(p: Throwable => Boolean): Boolean =
      e match {
        case ResilientTryErrorThrowable(a) => p(a)
        case _                             => true
      }

    def exists(p: Throwable => Boolean): Boolean =
      e match {
        case ResilientTryErrorThrowable(a) => p(a)
        case _                             => false
      }

    def flatMap[A1, B1 >: DomainError](
        f: Throwable => ResilientTryError[A1, B1]
    ): ResilientTryError[A1, B1] =
      e match {
        case ResilientTryErrorThrowable(a) => f(a)
        case _                             => e.asInstanceOf[ResilientTryError[A1, B1]]
      }

    def map[A1](f: Throwable => A1): ResilientTryError[A1, DomainError] =
      e match {
        case ResilientTryErrorThrowable(a) =>
          ResilientTryErrorThrowable(f(a))
        case _ => e.asInstanceOf[ResilientTryError[A1, DomainError]]
      }

    @deprecated("Use `filterToOption`, which more accurately reflects the return type", "2.13.0")
    def filter[B1](p: Throwable => Boolean): Option[ResilientTryError[Throwable, B1]] = {
      e match {
        case x @ ResilientTryErrorThrowable(a) if p(a) =>
          Some(x.asInstanceOf[ResilientTryError[Throwable, B1]])
        case _ => None
      }
    }

    def filterToOption[B1](
        p: Throwable => Boolean
    ): Option[ResilientTryError[Throwable, B1]] =
      e match {
        case x @ ResilientTryErrorThrowable(a) if p(a) =>
          Some(x.asInstanceOf[ResilientTryError[Throwable, B1]])
        case _ => None
      }
  }

  final case class ResilientTryErrorDomainErrorProjection[+Throwable, +DomainError](
      e: ResilientTryError[Throwable, DomainError]
  ) {

    @deprecated("Use `Either.getOrElse` instead", "2.13.0")
    def get: DomainError =
      e match {
        case ResilientTryErrorDomainError(b) => b
        case _                               => throw new NoSuchElementException("Either.right.get on Left")
      }

    def foreach[U](f: DomainError => U): Unit =
      e match {
        case ResilientTryErrorDomainError(b) => f(b)
        case _                               => ()
      }

    def getOrElse[B1 >: DomainError](or: => B1): B1 =
      e match {
        case ResilientTryErrorDomainError(b) => b
        case _                               => or
      }

    def forall(f: DomainError => Boolean): Boolean =
      e match {
        case ResilientTryErrorDomainError(b) => f(b)
        case _                               => true
      }

    def exists(p: DomainError => Boolean): Boolean =
      e match {
        case ResilientTryErrorDomainError(b) => p(b)
        case _                               => false
      }

    def flatMap[A1 >: Throwable, B1](f: DomainError => ResilientTryError[A1, B1]): ResilientTryError[A1, B1] =
      e match {
        case ResilientTryErrorDomainError(b) => f(b)
        case _                               => e.asInstanceOf[ResilientTryError[A1, B1]]
      }

    def map[B1](f: DomainError => B1): ResilientTryError[Throwable, B1] =
      e match {
        case ResilientTryErrorDomainError(b) =>
          ResilientTryErrorDomainError(f(b))
        case _ => e.asInstanceOf[ResilientTryError[Throwable, B1]]
      }

    @deprecated("Use `filterToOption`, which more accurately reflects the return type", "2.13.0")
    def filter[A1](p: DomainError => Boolean): Option[ResilientTryError[A1, DomainError]] = {
      e match {
        case ResilientTryErrorDomainError(b) if p(b) =>
          Some(ResilientTryErrorDomainError(b))
        case _ => None
      }
    }

    def filterToOption[A1](
        p: DomainError => Boolean
    ): Option[ResilientTryError[A1, DomainError]] =
      e match {
        case r @ ResilientTryErrorDomainError(b) if p(b) =>
          Some(r.asInstanceOf[ResilientTryError[A1, DomainError]])
        case _ => None
      }
  }
}
