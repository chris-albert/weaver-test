package weaver
package internals

import cats.Show
import cats.data.{ NonEmptyList, Validated }
import cats.kernel.Eq

import com.eed3si9n.expecty._

private[weaver] trait ExpectSame {

  def eql[A](
      expected: A,
      found: A)(
      implicit eqA: Eq[A],
      showDiff: ShowDiff[A],
      loc: SourceLocation): Expectations = {

    if (eqA.eqv(expected, found))
      Expectations(Validated.validNel(()))
    else {

      val sourceLocs    = NonEmptyList.of(loc)
      val diff = showDiff.diff(expected, found)

      Expectations(
        Validated.invalidNel[AssertionException, Unit](
          new AssertionException(diff, sourceLocs)))
    }
  }

  /**
   * Same as eql but defaults to universal equality.
   */
  def same[A](
      expected: A,
      found: A)(
      implicit eqA: Eq[A] = Eq.fromUniversalEquals[A],
      showA: Show[A] = Show.fromToString[A],
      loc: SourceLocation): Expectations = eql(expected, found)
}
