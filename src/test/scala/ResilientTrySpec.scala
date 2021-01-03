import org.scalatest.flatspec.AnyFlatSpec

class ResilientTrySpec extends AnyFlatSpec {

  "A failed ResilientTry" should "return a ResilientTryFailed[Throwable, DomainError]" in {
    val resilientTry: ResilientTry[Int] = ResilientTry[Int]({
      throw new RuntimeException("RuntimeException")
    })

    resilientTry match {
      case ResilientTrySuccess(value)                                    =>
      case ResilientTryFailed(ResilientTryErrorThrowable(throwable))     =>
      case ResilientTryFailed(ResilientTryErrorDomainError(domainError)) =>
    }

    assert(resilientTry.isFailure)
    assert(resilientTry.failed.get.isThrowable)

    val recoveredThrowableResilientTry = resilientTry.recoverThrowable {
      case t: RuntimeException => ResilientTryFailed(ResilientTryErrorDomainError(SomeDatabaseError()))
    }
    assert(recoveredThrowableResilientTry.isFailure)
    assert(recoveredThrowableResilientTry.failed.get.isDomainError)
  }
}
