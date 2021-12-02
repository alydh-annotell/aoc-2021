object SonarSweep {
  def slidingSweep(depths: Seq[Int], window: Int = 3): Int =
    sweep(
      depths
        .sliding(window)
        .map(_.sum)
        .toSeq
    )

  def sweep(depths: Seq[Int]): Int =
    depths
      .sliding(2)
      .map(_.reduce(_ - _))
      .count(0 < _)
}
