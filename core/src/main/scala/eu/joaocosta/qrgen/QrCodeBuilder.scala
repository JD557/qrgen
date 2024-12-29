package eu.joaocosta.qrgen;

import java.util.Arrays
import eu.joaocosta.qrgen.Helpers.*

/** Helper class to build QR Codes */
final class QrCodeBuilder(val size: Int) {
  // The modules of this QR Code (false = light, true = dark).
  private val modules: Array[Array[Boolean]] = Array.ofDim[Boolean](size, size)
  // Indicates function modules that are not subjected to masking
  private var isFunction: Array[Array[Boolean]] = Array.ofDim[Boolean](size, size)

  /** The total number of points in the QR code */
  val totalPoints: Int = size * size

  /** The number of dark points in the QR code */
  def darkPoints: Int = modules.view.map(_.count(identity)).sum

  /** Returns the value at a certain position */
  def getModule(x: Int, y: Int): Boolean =
    0 <= x && x < size && 0 <= y && y < size && modules(y)(x)

  /** Sets the value at a certain position */
  def setModule(x: Int, y: Int, isDark: Boolean): Unit =
    modules(y)(x) = isDark

  /** Updates the value at a certain position */
  def updateModule(x: Int, y: Int, f: Boolean => Boolean): Unit =
    setModule(x, y, f(getModule(x, y)))

  /** Checks if a certain position is a function module */
  def getFunctionModule(x: Int, y: Int): Boolean = {
    0 <= x && x < size && 0 <= y && y < size && isFunction(y)(x)
  }

  /** Sets the value at a certain position and marks it as a funciton module */
  def setFunctionModule(x: Int, y: Int, isDark: Boolean): Unit = {
    modules(y)(x) = isDark
    isFunction(y)(x) = true
  }

  /** Returns the final result */
  def result(): Array[Array[Boolean]] = modules.map(_.clone())

  // ---- Drawing helpers ----

  // XORs the codeword modules in this QR Code with the given mask pattern.
  // The function modules must be marked and the codeword bits must be drawn
  // before masking. Due to the arithmetic of XOR, calling applyMask with
  // the same mask value a second time will undo the mask. A final well-formed
  // QR Code needs exactly one (not zero, two, etc.) mask applied.
  def applyMask(mask: Int): Unit = {
    require(mask >= 0 && mask < 7, "Mask value out of range")
    for {
      y <- (0 until size)
      x <- (0 until size)
      isFunction = getFunctionModule(x, y)
    } {
      val invert = mask match {
        case 0 => (x + y)         % 2 == 0
        case 1 => y               % 2 == 0
        case 2 => x               % 3 == 0
        case 3 => (x + y)         % 3 == 0
        case 4 => (x / 3 + y / 2) % 2 == 0
        case 5 => x * y           % 2 + x * y % 3 == 0
        case 6 => (x * y          % 2 + x * y % 3) % 2 == 0
        case 7 => ((x + y)        % 2 + x * y % 3) % 2 == 0
        case _ => throw new IllegalArgumentException("Mask value out of range")
      }
      updateModule(x, y, _ ^ (invert & !isFunction))
    }
  }

  // Can only be called immediately after a light run is added, and
  // returns either 0, 1, or 2. A helper function for getPenaltyScore().
  private def finderPenaltyCountPatterns(runHistory: Array[Int]): Int = {
    val n: Int = runHistory(1)
    assert(n <= size * 3)
    val core: Boolean =
      n > 0 && runHistory(2) == n && runHistory(3) == n * 3 && runHistory(4) == n && runHistory(5) == n

    (if (core && runHistory(0) >= n * 4 && runHistory(6) >= n) 1 else 0) +
      (if (core && runHistory(6) >= n * 4 && runHistory(0) >= n) 1 else 0)
  }

  // Must be called at the end of a line (row or column) of modules. A helper function for getPenaltyScore().
  private def finderPenaltyTerminateAndCount(
      currentRunColor: Boolean,
      currentRunLength: Int,
      runHistory: Array[Int]
  ): Int = {
    if (currentRunColor) { // Terminate dark run
      finderPenaltyAddHistory(currentRunLength, runHistory)
      finderPenaltyAddHistory(size, runHistory) // Add light border to final run
    } else {
      finderPenaltyAddHistory(currentRunLength + size, runHistory) // Add light border to final run
    }
    finderPenaltyCountPatterns(runHistory)
  }

  // Pushes the given value to the front and drops the last value. A helper function for getPenaltyScore().
  private def finderPenaltyAddHistory(currentRunLength: Int, runHistory: Array[Int]): Unit = {
    val addLightBorder = runHistory(0) == 0 // Add light border to initial run
    System.arraycopy(runHistory, 0, runHistory, 1, runHistory.length - 1)
    if (addLightBorder) runHistory(0) = currentRunLength + size
    else runHistory(0) = currentRunLength
  }

  // Calculates and returns the penalty score based on state of this QR Code's current modules.
  // This is used by the automatic mask choice algorithm to find the mask pattern that yields the lowest score.
  def getPenaltyScore(): Int = {
    var result: Int = 0

    // Adjacent modules in row having same color, and finder-like patterns
    (0 until size).foreach { y =>
      var runColor: Boolean      = false;
      var runX: Int              = 0;
      val runHistory: Array[Int] = Array.ofDim[Int](7);
      (0 until size).foreach { x =>
        if (getModule(x, y) == runColor) {
          runX = runX + 1
          if (runX == 5) result = result + QrCodeBuilder.PENALTY_N1
          else if (runX > 5) result = result + 1
        } else {
          finderPenaltyAddHistory(runX, runHistory)
          if (!runColor)
            result = result + finderPenaltyCountPatterns(runHistory) * QrCodeBuilder.PENALTY_N3
          runColor = getModule(x, y)
          runX = 1
        }
      }
      result = result + finderPenaltyTerminateAndCount(runColor, runX, runHistory) * QrCodeBuilder.PENALTY_N3
    }
    // Adjacent modules in column having same color, and finder-like patterns
    (0 until size).foreach { x =>
      var runColor: Boolean      = false;
      var runY: Int              = 0;
      val runHistory: Array[Int] = Array.ofDim[Int](7);
      (0 until size).foreach { y =>
        if (getModule(x, y) == runColor) {
          runY = runY + 1
          if (runY == 5) result = result + QrCodeBuilder.PENALTY_N1
          else if (runY > 5) result = result + 1
        } else {
          finderPenaltyAddHistory(runY, runHistory)
          if (!runColor)
            result = result + finderPenaltyCountPatterns(runHistory) * QrCodeBuilder.PENALTY_N3
          runColor = getModule(x, y)
          runY = 1
        }
      }
      result = result + finderPenaltyTerminateAndCount(runColor, runY, runHistory) * QrCodeBuilder.PENALTY_N3
    }

    // 2*2 blocks of modules having same color
    for {
      y <- (0 until size - 1)
      x <- (0 until size - 1)
      color = getModule(x, y)
      if (color == getModule(x + 1, y) &&
        color == getModule(x, y + 1) &&
        color == getModule(x + 1, y + 1))
    } result = result + QrCodeBuilder.PENALTY_N2

    // Compute the smallest integer k >= 0 such that (45-5k)% <= dark/total <= (55+5k)%
    val k = (Math.abs(darkPoints * 20 - totalPoints * 10) + totalPoints - 1) / totalPoints - 1;
    assert(0 <= k && k <= 9)
    result = result + k * QrCodeBuilder.PENALTY_N4
    assert(0 <= result && result <= 2568888) // Non-tight upper bound based on default values of PENALTY_N1, ..., N4
    result
  }

  // Draws two copies of the format bits (with its own error correction code)
  // based on the given mask and this object's error correction level field.
  def drawFormatBits(errorCorrectionLevel: Ecc, mask: Int): Unit = {
    // Calculate error correction code and pack bits
    val data: Int = errorCorrectionLevel.formatBits << 3 | mask // errCorrLvl is uint2, mask is uint3
    var rem: Int  = data
    (0 until 10).foreach { i => rem = (rem << 1) ^ ((rem >>> 9) * 0x537) }
    val bits: Int = (data << 10 | rem) ^ 0x5412; // uint15
    assert(bits >>> 15 == 0)

    // Draw first copy
    (0 until 6).foreach { i => setFunctionModule(8, i, getBit(bits, i)) }
    setFunctionModule(8, 7, getBit(bits, 6))
    setFunctionModule(8, 8, getBit(bits, 7))
    setFunctionModule(7, 8, getBit(bits, 8))
    (9 until 15).foreach { i => setFunctionModule(14 - i, 8, getBit(bits, i)) }

    // Draw second copy
    (0 until 8).foreach { i => setFunctionModule(size - 1 - i, 8, getBit(bits, i)) }
    (8 until 15).foreach { i => setFunctionModule(8, size - 15 + i, getBit(bits, i)) }
    setFunctionModule(8, size - 8, true) // Always dark
  }

  // Draws two copies of the version bits (with its own error correction code),
  // based on this object's version field, iff 7 <= version <= 40.
  def drawVersion(version: Int): Unit = {
    if (version >= 7) {
      // Calculate error correction code and pack bits
      var rem: Int = version // version is uint6, in the range [7, 40]
      (0 until 12).foreach { i =>
        rem = (rem << 1) ^ ((rem >>> 11) * 0x1f25)
      }
      val bits: Int = version << 12 | rem; // uint18
      assert(bits >>> 18 == 0)

      // Draw two copies
      (0 until 18).foreach { i =>
        val bit: Boolean = getBit(bits, i)
        val a: Int       = size - 11 + i % 3
        val b: Int       = i / 3
        setFunctionModule(a, b, bit)
        setFunctionModule(b, a, bit)
      }
    }
  }

  // Draws a 9*9 finder pattern including the border separator,
  // with the center module at (x, y). Modules can be out of bounds.
  private def drawFinderPattern(x: Int, y: Int): Unit = {
    for {
      dy <- (-4 to 4)
      dx <- (-4 to 4)
      xx = x + dx
      yy = y + dy
      if (0 <= xx && xx < size && 0 <= yy && yy < size)
      dist = Math.max(Math.abs(dx), Math.abs(dy)) // Chebyshev/infinity norm
    } setFunctionModule(xx, yy, dist != 2 && dist != 4)
  }

  // Draws a 5*5 alignment pattern, with the center module
  // at (x, y). All modules must be in bounds.
  private def drawAlignmentPattern(x: Int, y: Int): Unit = {
    for {
      dy <- (-2 to 2)
      dx <- (-2 to 2)
    } setFunctionModule(x + dx, y + dy, Math.max(Math.abs(dx), Math.abs(dy)) != 1)
  }

  // Reads this object's version field, and draws and marks all function modules.
  def drawFunctionPatterns(version: Int, errorCorrectionLevel: Ecc): Unit = {
    // Draw horizontal and vertical timing patterns
    (0 until size).foreach { i =>
      setFunctionModule(6, i, i % 2 == 0)
      setFunctionModule(i, 6, i % 2 == 0)
    }

    // Draw 3 finder patterns (all corners except bottom right; overwrites some timing modules)
    drawFinderPattern(3, 3)
    drawFinderPattern(size - 4, 3)
    drawFinderPattern(3, size - 4)

    // Draw numerous alignment patterns
    val alignPatPos: Array[Int] = QrCode.getAlignmentPatternPositions(version, size)
    val numAlign: Int           = alignPatPos.length;
    for {
      i <- (0 until numAlign)
      j <- (0 until numAlign)
      // Don't draw on the three finder corners
      if (!(i == 0 && j == 0 || i == 0 && j == numAlign - 1 || i == numAlign - 1 && j == 0))
    } drawAlignmentPattern(alignPatPos(i), alignPatPos(j))

    // Draw configuration data
    drawFormatBits(errorCorrectionLevel, 0) // Dummy mask value; overwritten later in the constructor
    drawVersion(version)
  }

  // Draws the given sequence of 8-bit codewords (data and error correction) onto the entire
  // data area of this QR Code. Function modules need to be marked off before this is called.
  def drawCodewords(version: Int, data: Array[Byte]): Unit = {
    if (data.length != QrCode.getNumRawDataModules(version) / 8)
      throw new IllegalArgumentException()

    var i = 0 // Bit index into the data
    // Do the funny zigzag scan
    var right = size - 1 // Index of right column in each column pair
    while (right >= 1) {
      if (right == 6) right = 5
      for {
        vert <- (0 until size)
        j    <- (0 until 2)
      } {
        val x: Int          = right - j                               // Actual x coordinate
        val upward: Boolean = ((right + 1) & 2) == 0
        val y: Int          = if (upward) (size - 1 - vert) else vert // Actual y coordinate
        if (!getFunctionModule(x, y) && i < data.length * 8) {
          setModule(x, y, getBit(data(i >>> 3), 7 - (i & 7)))
          i = i + 1
        }
        // If this QR Code has any remainder bits (0 to 7), they were assigned as
        // 0/false/light by the constructor and are left unchanged by this method
      }
      right = right - 2
    }
    assert(i == data.length * 8)
  }
}

object QrCodeBuilder {
  // For use in getPenaltyScore(), when evaluating which mask is best.
  private val PENALTY_N1: Int = 3;
  private val PENALTY_N2: Int = 3;
  private val PENALTY_N3: Int = 40;
  private val PENALTY_N4: Int = 10;
}
