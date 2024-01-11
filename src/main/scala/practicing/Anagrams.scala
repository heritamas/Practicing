package practicing

object Anagrams  extends App {

  def anagrams0(str: String) = {
    str.toSeq.permutations.distinct
  }

  def anagrams(str: String) = {
    val chars = str.toCharArray
    println(s"working on ${chars.mkString}")

    def loop(arr: Array[Char]) : List[Array[Char]] = {
      if arr.length == 1 then List(arr)
      else {
        //println(s"scrambling ${arr.head}")
        val perms = loop(arr.tail)
        //println(s"permutations so far ${perms.map(_.mkString).mkString(",")}")
        for {
          word <- perms
          i <- 0 to word.length
        } yield {
          //println(s"dissecting ${word.mkString} at ${i}")
          val (first, second) = word.splitAt(i)
          //println(s"constructing: ${first.mkString} .. ${arr.head} .. ${second.mkString}")
          first ++ Array(arr.head) ++ second
        }
      }
    }

    loop(chars)
  }

  val result = anagrams("biro")
  println(s"size: ${result.length}")
  result.map(_.mkString).foreach(println)

}
