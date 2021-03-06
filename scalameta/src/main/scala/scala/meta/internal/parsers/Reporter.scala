package scala.meta
package internal
package parsers

// TODO: when I grow up I want to become a monad, just like my daddy
private[meta] trait Reporter {
  def deprecationWarning(msg: String, at: Position): Unit = ()
  def deprecationWarning(msg: String, at: Token): Unit = deprecationWarning(msg, at.position)
  def deprecationWarning(msg: String, at: Tree): Unit = deprecationWarning(msg, at.position)
  def syntaxWarning(msg: String, at: Position): Unit = ()
  def syntaxWarning(msg: String, at: Token): Unit = syntaxWarning(msg, at.position)
  def syntaxWarning(msg: String, at: Tree): Unit = syntaxWarning(msg, at.position)
  def syntaxError(msg: String, at: Position): Nothing = throw new ParseException(at, msg)
  def syntaxError(msg: String, at: Token): Nothing = syntaxError(msg, at.position)
  def syntaxError(msg: String, at: Tree): Nothing = syntaxError(msg, at.position)
}

private[meta] object Reporter {
  def apply() = new Reporter {}
}
