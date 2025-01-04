package eu.joaocosta.qrgen.internal

import eu.joaocosta.qrgen.Ecc
import eu.joaocosta.qrgen.QrCode
import java.util.Arrays

/** Helper class to build QR Codes */
final class QrCodeBuilder(val size: Int) extends QrCodeBuilder.View {
  // The modules of this QR Code (false = light, true = dark).
  private val modules: Array[Array[Boolean]] = Array.ofDim[Boolean](size, size)
  // Indicates function modules that are not subjected to masking
  private val isFunction: Array[Array[Boolean]] = Array.ofDim[Boolean](size, size)

  def darkPoints: Int = modules.view.map(_.count(identity)).sum

  def getModule(x: Int, y: Int): Boolean =
    0 <= x && x < size && 0 <= y && y < size && modules(y)(x)

  def getFunctionModule(x: Int, y: Int): Boolean = {
    0 <= x && x < size && 0 <= y && y < size && isFunction(y)(x)
  }

  /** Sets the value at a certain position */
  def setModule(x: Int, y: Int, isDark: Boolean): Unit =
    modules(y)(x) = isDark

  /** Updates the value at a certain position */
  def updateModule(x: Int, y: Int, f: Boolean => Boolean): Unit =
    setModule(x, y, f(getModule(x, y)))

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
    require(mask >= 0 && mask <= 7, "Mask value out of range")
    for {
      y <- (0 until size)
      x <- (0 until size)
      isFunction = getFunctionModule(x, y)
      if (!isFunction)
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
      updateModule(x, y, _ ^ invert)
    }
  }

  // Draws two copies of the format bits (with its own error correction code)
  // based on the given mask and this object's error correction level field.
  def drawFormatBits(errorCorrectionLevel: Ecc, mask: Int): Unit = {
    // Calculate error correction code and pack bits
    val data: Int = errorCorrectionLevel.formatBits << 3 | mask // errCorrLvl is uint2, mask is uint3
    var rem: Int  = data
    (0 until 10).foreach { i => rem = (rem << 1) ^ ((rem >>> 9) * 0x537) }
    val bits: Int = (data << 10 | rem) ^ 0x5412 // uint15
    assert(bits >>> 15 == 0)

    // Draw first copy
    (0 until 6).foreach { i => setFunctionModule(8, i, Helpers.getBit(bits, i)) }
    setFunctionModule(8, 7, Helpers.getBit(bits, 6))
    setFunctionModule(8, 8, Helpers.getBit(bits, 7))
    setFunctionModule(7, 8, Helpers.getBit(bits, 8))
    (9 until 15).foreach { i => setFunctionModule(14 - i, 8, Helpers.getBit(bits, i)) }

    // Draw second copy
    (0 until 8).foreach { i => setFunctionModule(size - 1 - i, 8, Helpers.getBit(bits, i)) }
    (8 until 15).foreach { i => setFunctionModule(8, size - 15 + i, Helpers.getBit(bits, i)) }
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
      val bits: Int = version << 12 | rem // uint18
      assert(bits >>> 18 == 0)

      // Draw two copies
      (0 until 18).foreach { i =>
        val bit: Boolean = Helpers.getBit(bits, i)
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
    val alignPatPos: Vector[Int] = QrCode.getAlignmentPatternPositions(version, size)
    val numAlign: Int            = alignPatPos.size
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
          setModule(x, y, Helpers.getBit(data(i >>> 3), 7 - (i & 7)))
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
  // Read-only view over a QR Code builder
  trait View {
    def size: Int

    /** The total number of points in the QR code */
    val totalPoints: Int = size * size

    /** The number of dark points in the QR code */
    def darkPoints: Int

    /** Returns the value at a certain position */
    def getModule(x: Int, y: Int): Boolean

    /** Checks if a certain position is a function module */
    def getFunctionModule(x: Int, y: Int): Boolean
  }
}
