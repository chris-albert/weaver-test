package weaver.internals

import cats.Show
import com.eed3si9n.expecty.DiffUtil

trait ShowDiff[A] {
  def diff(left: A, right: A): String
}

object ShowDiff {

  def diffUtil[A](implicit showA: Show[A]): ShowDiff[A] = {
    val header = "Values not equal:"
    (expected: A, found: A) => {
      val expectedLines = showA.show(expected).linesIterator.toSeq
      val foundLines    = showA.show(found).linesIterator.toSeq
      val diff = DiffUtil
        .mkColoredLineDiff(expectedLines, foundLines)
        .linesIterator
        .toSeq
        .map(str => Console.RESET.toString + str)
        .mkString("\n")

      header + "\n\n" + diff
    }
  }
}
