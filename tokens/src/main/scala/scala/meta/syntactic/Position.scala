package scala.meta
package syntactic

import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Position {
  def input: Input
  def start: Point
  def point: Point
  def end: Point
}

object Position {
  @leaf class Assorted(tokens: Tokens, point: Point) extends Position {
    def input = tokens
    def start = Point.Assorted(tokens)
    def end = Point.Assorted(tokens)
  }
  @leaf class Range(content: Content, start: Point, point: Point, end: Point) extends Position {
    def input = content
  }
}

@root trait Point {
  def input: Input
  def offset: Int
  def line: Int
  def column: Int
}

object Point {
  @leaf class Assorted(tokens: Tokens) extends Point {
    def input = tokens
    def offset = -1
    def line = -1
    def column = -1
  }
  @leaf class Offset(content: Content, offset: Int) extends Point {
    def input = content
    private lazy val (eolCount, eolPos) = {
      var i = 0
      var eolCount = 0
      var eolPos = -1
      while (i < Math.min(offset, content.chars.length)) {
        if (content.chars(i) == '\n') {
          eolCount += 1
          eolPos = i
        }
        i += 1
      }
      (eolCount, eolPos)
    }
    def line: Int = eolCount
    def column: Int = offset - eolPos + 1
  }
}
