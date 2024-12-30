package eu.joaocosta.qrgen

object Penalty {
  // For use in getPenaltyScore(), when evaluating which mask is best.
  private val PENALTY_N1: Int = 3
  private val PENALTY_N2: Int = 3
  private val PENALTY_N3: Int = 40
  private val PENALTY_N4: Int = 10

  // Can only be called immediately after a light run is added, and
  // returns either 0, 1, or 2. A helper function for getPenaltyScore().
  private def finderPenaltyCountPatterns(size: Int, runHistory: Array[Int]): Int = {
    val n: Int = runHistory(1)
    assert(n <= size * 3)
    val core: Boolean =
      n > 0 && runHistory(2) == n && runHistory(3) == n * 3 && runHistory(4) == n && runHistory(5) == n

    (if (core && runHistory(0) >= n * 4 && runHistory(6) >= n) 1 else 0) +
      (if (core && runHistory(6) >= n * 4 && runHistory(0) >= n) 1 else 0)
  }

  // Must be called at the end of a line (row or column) of modules. A helper function for getPenaltyScore().
  private def finderPenaltyTerminateAndCount(
      size: Int,
      currentRunColor: Boolean,
      currentRunLength: Int,
      runHistory: Array[Int]
  ): Int = {
    if (currentRunColor) { // Terminate dark run
      finderPenaltyAddHistory(size, currentRunLength, runHistory)
      finderPenaltyAddHistory(size, size, runHistory) // Add light border to final run
    } else {
      finderPenaltyAddHistory(size, currentRunLength + size, runHistory) // Add light border to final run
    }
    finderPenaltyCountPatterns(size, runHistory)
  }

  // Pushes the given value to the front and drops the last value. A helper function for getPenaltyScore().
  private def finderPenaltyAddHistory(size: Int, currentRunLength: Int, runHistory: Array[Int]): Unit = {
    val addLightBorder = runHistory(0) == 0 // Add light border to initial run
    System.arraycopy(runHistory, 0, runHistory, 1, runHistory.length - 1)
    if (addLightBorder) runHistory(0) = currentRunLength + size
    else runHistory(0) = currentRunLength
  }

  // Calculates and returns the penalty score based on state of this QR Code's current modules.
  // This is used by the automatic mask choice algorithm to find the mask pattern that yields the lowest score.
  def getPenaltyScore(builder: QrCodeBuilder.View): Int = {
    var result: Int = 0

    def processRun(get: (Int, Int) => Boolean): Unit =
      (0 until builder.size).foreach { outer =>
        var runColor: Boolean      = false
        var run: Int               = 0
        val runHistory: Array[Int] = Array.ofDim[Int](7)
        (0 until builder.size).foreach { inner =>
          if (get(inner, outer) == runColor) {
            run = run + 1
            if (run == 5) result = result + PENALTY_N1
            else if (run > 5) result = result + 1
          } else {
            finderPenaltyAddHistory(builder.size, run, runHistory)
            if (!runColor)
              result = result + finderPenaltyCountPatterns(builder.size, runHistory) * PENALTY_N3
            runColor = get(inner, outer)
            run = 1
          }
        }
        result = result + finderPenaltyTerminateAndCount(builder.size, runColor, run, runHistory) * PENALTY_N3
      }

    // Adjacent modules in row having same color, and finder-like patterns
    processRun((inner, outer) => builder.getModule(inner, outer))

    // Adjacent modules in column having same color, and finder-like patterns
    processRun((inner, outer) => builder.getModule(outer, inner))

    // 2*2 blocks of modules having same color
    for {
      y <- (0 until builder.size - 1)
      x <- (0 until builder.size - 1)
      color = builder.getModule(x, y)
      if (color == builder.getModule(x + 1, y) &&
        color == builder.getModule(x, y + 1) &&
        color == builder.getModule(x + 1, y + 1))
    } result = result + PENALTY_N2

    // Compute the smallest integer k >= 0 such that (45-5k)% <= dark/total <= (55+5k)%
    val k =
      (Math.abs(builder.darkPoints * 20 - builder.totalPoints * 10) + builder.totalPoints - 1) / builder.totalPoints - 1
    assert(0 <= k && k <= 9)
    result = result + k * PENALTY_N4
    assert(0 <= result && result <= 2568888) // Non-tight upper bound based on default values of PENALTY_N1, ..., N4
    result
  }
}
