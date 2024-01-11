package practicing

object NumberCombined extends App {

  val arrays = Seq(
    Array(50, 2, 1, 9),
    Array(5, 50, 56),
    Array(420, 42, 423)
  )

  def combine(nums: Array[Int]) : String = {
    val words = nums.map(_.toString)
    val sorted = words.sortWith { (s1, s2) =>  (s1+s2).compareTo(s2+s1) > 0 }
    sorted.mkString
  }

  arrays
    .map(combine)
    .foreach(println)

}
