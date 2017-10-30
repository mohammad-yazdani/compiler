package logic

trait DFA {
  /* Scala tip: single-argument member functions can be written in infix style without . or ()s
   * Don't use this if it hurts readability.
   */

  def states: Set[String]
  def alphabet: Set[Char]
  def transition: PartialFunction[(String, Char), String]
  def start: String
  def accepting: Set[String]

  /* This is unused elsewhere, but exists for completeness' sake.
   * It is extremely similar to scanOne in simplifiedMaximalMunch below, except it runs until
   * input is exhausted in all cases.
   */
  def recognize(input: Seq[Char], state: String = start): Boolean =
    if (input.isEmpty) accepting contains state
    else if (transition.isDefinedAt((state,input.head))) recognize(input.tail,transition((state,input.head)))
    else false

  /* recognize can also be defined in terms of abstract list functions, for example:
   * input.foldLeft(Some(start): Option[String])((st, ch) => st.flatMap(x => transition.lift((x, ch)))) match {
   *   case Some(state) => accepting contains state
   *   case None        => false
   * }
   */
}
