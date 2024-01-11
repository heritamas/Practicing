class BalancedParens(val text: String) {

  private val stack = scala.collection.mutable.Stack.empty[Char]

  private def checkBalanced(pairs: Map[Char, Char], lefts: Set[Char], rights: Set[Char]) : Boolean = {
    stack.clear()
    var result = true
    for
      ch <- text
      if result
    do
      ch match {
        case _ if lefts.contains(ch) => stack.push(ch)
        case _ if rights.contains(ch) => if ( stack.nonEmpty &&  pairs(stack.pop()) != ch ) result = false
      }

    result
  }

  def isBalanced(pairs: Seq[(Char, Char)]) : Boolean =  {
    val charPairs = pairs.toMap
    val lefts = charPairs.keySet
    val rights = charPairs.values.toSet

    checkBalanced(charPairs, lefts, rights)
  }
}


object BalancedParens extends App {

  val usual = Seq( ('(', ')'), ('{', '}'), ('[', ']') )
  val bal1 = new BalancedParens("{{)(}}")
  println(bal1.isBalanced(usual))

  val bal2 = new BalancedParens("[(])")
  println(bal2.isBalanced(usual))

  val bal3 = new BalancedParens("{[[()[]]]}")
  println(bal3.isBalanced(usual))
}
