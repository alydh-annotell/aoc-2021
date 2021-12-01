object SonarSweep {
  def slidingSweep(depths: Seq[Int], window: Int = 3): Int = sweep(
    depths
      .sliding(window)
      .map(_.sum)
      .toSeq
  )

  def sweep(depths: Seq[Int]): Int =
    depths
      .sliding(2)
      .map { case Seq(x, y) => x < y }
      .count(_ == true)
}
