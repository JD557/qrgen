package eu.joaocosta.qrgen;

import java.util.Arrays
import eu.joaocosta.qrgen.Helpers.*

/**
 * A QR Code symbol, which is a type of two-dimension barcode.
 * Invented by Denso Wave and described in the ISO/IEC 18004 standard.
 * <p>Instances of this class represent an immutable square grid of dark and light cells.
 * The class provides static factory functions to create a QR Code from text or binary data.
 * The class covers the QR Code Model 2 specification, supporting all versions (sizes)
 * from 1 to 40, all 4 error correction levels, and 4 character encoding modes.</p>
 * <p>Ways to create a QR Code object:</p>
 * <ul>
 *   <li><p>High level: Take the payload data and call {@link QrCode#encodeText(CharSequence,Ecc)}
 *     or {@link QrCode#encodeBinary(byte[],Ecc)}.</p></li>
 *   <li><p>Mid level: Custom-make the list of {@link QrSegment segments}
 *     and call {@link QrCode#encodeSegments(List,Ecc)} or
 *     {@link QrCode#encodeSegments(List,Ecc,int,int,int,boolean)}</p></li>
 *   <li><p>Low level: Custom-make the array of data codeword bytes (including segment headers and
 *     final padding, excluding error correction codewords), supply the appropriate version number,
 *     and call the {@link QrCode#QrCode(int,Ecc,byte[],int) constructor}.</p></li>
 * </ul>
 * <p>(Note that all ways require supplying the desired error correction level.)</p>
 * @param version the version number to use, which must be in the range 1 to 40 (inclusive)
 * @param errorCorrectionLevel the error correction level to use
 * @param dataCodewords the bytes representing segments to encode (without ECC)
 * @param mask the mask pattern to use, which is either -1 for automatic choice or from 0 to 7 for fixed choice
 * @see QrSegment
 */
final class QrCode(val version: Int, val errorCorrectionLevel: QrCode.Ecc, dataCodewords: Array[Byte], val mask: Option[Int]) {
  // Check arguments and initialize fields
  if (version < QrCode.MIN_VERSION || version > QrCode.MAX_VERSION)
    throw new IllegalArgumentException("Version value out of range")
  if (mask.exists(x => x < 0 || x > 7))
    throw new IllegalArgumentException("Mask value out of range")

  val size = version * 4 + 17

  val (modules, bestMask) = {
    // Private grids of modules/pixels, with dimensions of size*size
    val builder = new QrCodeBuilder(size)

    // Compute ECC, draw modules, do masking
    drawFunctionPatterns(builder)
    drawCodewords(builder, addEccAndInterleave(builder, dataCodewords))

    // Do masking
    val _bestMask = mask match {
      case Some(m) => m
      case None => // Automatically choose best mask
        (0 until 8).foldLeft((0, Integer.MAX_VALUE)) { case ((best, minPenalty), testMask) =>
          applyMask(builder, testMask)
          drawFormatBits(builder, testMask)
          val penalty: Int = getPenaltyScore(builder)
          applyMask(builder, testMask)  // Undoes the mask due to XOR
          if (penalty < minPenalty) (testMask, penalty)
          else (best, minPenalty)
        }._1
    }
    applyMask(builder, _bestMask)  // Apply the final choice of mask
    drawFormatBits(builder, _bestMask)  // Overwrite old format bits

    (builder.result(), _bestMask)
  }
  
  /**
   * Returns the color of the module (pixel) at the specified coordinates, which is {@code false}
   * for light or {@code true} for dark. The top left corner has the coordinates (x=0, y=0).
   * If the specified coordinates are out of bounds, then {@code false} (light) is returned.
   * @param x the x coordinate, where 0 is the left edge and size&#x2212;1 is the right edge
   * @param y the y coordinate, where 0 is the top edge and size&#x2212;1 is the bottom edge
   * @return {@code true} if the coordinates are in bounds and the module
   * at that location is dark, or {@code false} (light) otherwise
   */
  def getModule(x: Int, y: Int): Boolean = {
    0 <= x && x < size && 0 <= y && y < size && modules(y)(x);
  }

  // Reads this object's version field, and draws and marks all function modules.
  private def drawFunctionPatterns(builder: QrCodeBuilder): Unit = {
    // Draw horizontal and vertical timing patterns
    (0 until size).foreach { i =>
      builder.setFunctionModule(6, i, i % 2 == 0)
      builder.setFunctionModule(i, 6, i % 2 == 0)
    }
    
    // Draw 3 finder patterns (all corners except bottom right; overwrites some timing modules)
    drawFinderPattern(builder, 3, 3)
    drawFinderPattern(builder, size - 4, 3)
    drawFinderPattern(builder, 3, size - 4)
    
    // Draw numerous alignment patterns
    val alignPatPos: Array[Int] = getAlignmentPatternPositions()
    val numAlign: Int = alignPatPos.length;
    for {
      i <- (0 until numAlign)
      j <- (0 until numAlign)
      // Don't draw on the three finder corners
      if (!(i == 0 && j == 0 || i == 0 && j == numAlign - 1 || i == numAlign - 1 && j == 0))
    } drawAlignmentPattern(builder, alignPatPos(i), alignPatPos(j))
    
    // Draw configuration data
    drawFormatBits(builder, 0)  // Dummy mask value; overwritten later in the constructor
    drawVersion(builder)
  }
  
  
  // Draws two copies of the format bits (with its own error correction code)
  // based on the given mask and this object's error correction level field.
  private def drawFormatBits(builder: QrCodeBuilder, msk: Int): Unit = {
    // Calculate error correction code and pack bits
    val data: Int = errorCorrectionLevel.formatBits << 3 | msk  // errCorrLvl is uint2, mask is uint3
    var rem: Int = data
    (0 until 10).foreach { i => rem = (rem << 1) ^ ((rem >>> 9) * 0x537) }
    val bits: Int = (data << 10 | rem) ^ 0x5412;  // uint15
    assert(bits >>> 15 == 0)
    
    // Draw first copy
    (0 to 5).foreach { i => builder.setFunctionModule(8, i, getBit(bits, i)) }
    builder.setFunctionModule(8, 7, getBit(bits, 6))
    builder.setFunctionModule(8, 8, getBit(bits, 7))
    builder.setFunctionModule(7, 8, getBit(bits, 8))
    (9 until 15).foreach { i => builder.setFunctionModule(14 - i, 8, getBit(bits, i)) }
    
    // Draw second copy
    (0 until 8).foreach { i => builder.setFunctionModule(size - 1 - i, 8, getBit(bits, i))}
    (8 until 15).foreach { i => builder.setFunctionModule(8, size - 15 + i, getBit(bits, i)) }
    builder.setFunctionModule(8, size - 8, true)  // Always dark
  }
  
  // Draws two copies of the version bits (with its own error correction code),
  // based on this object's version field, iff 7 <= version <= 40.
  private def drawVersion(builder: QrCodeBuilder): Unit = {
    if (version >= 7) {
      // Calculate error correction code and pack bits
      var rem: Int = version  // version is uint6, in the range [7, 40]
      (0 until 12).foreach { i =>
        rem = (rem << 1) ^ ((rem >>> 11) * 0x1F25)
      }
      val bits: Int = version << 12 | rem;  // uint18
      assert(bits >>> 18 == 0)
      
      // Draw two copies
      (0 until 18).foreach { i =>
        val bit: Boolean = getBit(bits, i)
        val a: Int = size - 11 + i % 3
        val b: Int = i / 3
        builder.setFunctionModule(a, b, bit)
        builder.setFunctionModule(b, a, bit)
      }
    }
  }
  
  
  // Draws a 9*9 finder pattern including the border separator,
  // with the center module at (x, y). Modules can be out of bounds.
  private def drawFinderPattern(builder: QrCodeBuilder, x: Int, y: Int): Unit = {
    for {
      dy <- (-4 to 4)
      dx <- (-4 to 4)
      xx = x + dx
      yy = y + dy
      if (0 <= xx && xx < size && 0 <= yy && yy < size)
      dist = Math.max(Math.abs(dx), Math.abs(dy))  // Chebyshev/infinity norm
    } builder.setFunctionModule(xx, yy, dist != 2 && dist != 4)
  }
  
  
  // Draws a 5*5 alignment pattern, with the center module
  // at (x, y). All modules must be in bounds.
  private def drawAlignmentPattern(builder: QrCodeBuilder, x: Int, y: Int): Unit = {
    for {
      dy <- (-2 to 2)
      dx <- (-2 to 2)
    } builder.setFunctionModule(x + dx, y + dy, Math.max(Math.abs(dx), Math.abs(dy)) != 1)
  }
  
  
  // Returns a new byte string representing the given data with the appropriate error correction
  // codewords appended to it, based on this object's version and error correction level.
  private def addEccAndInterleave(builder: QrCodeBuilder, data: Array[Byte]): Array[Byte] = {
    if (data.length != QrCode.getNumDataCodewords(version, errorCorrectionLevel))
      throw new IllegalArgumentException()
    
    // Calculate parameter numbers
    val numBlocks: Int = QrCode.NUM_ERROR_CORRECTION_BLOCKS(errorCorrectionLevel.ordinal)(version)
    val blockEccLen: Int = QrCode.ECC_CODEWORDS_PER_BLOCK(errorCorrectionLevel.ordinal)(version)
    val rawCodewords: Int = QrCode.getNumRawDataModules(version) / 8
    val numShortBlocks: Int = numBlocks - rawCodewords % numBlocks
    val shortBlockLen: Int = rawCodewords / numBlocks
    
    // Split data into blocks and append ECC to each block
    val blocks: Array[Array[Byte]] = Array.ofDim[Array[Byte]](numBlocks)
    val rsDiv: Array[Byte] = ReedSolomon.reedSolomonComputeDivisor(blockEccLen)
    var k = 0
    (0 until numBlocks).foreach { i =>
      val dat: Array[Byte] = Arrays.copyOfRange(data, k, k + shortBlockLen - blockEccLen + (if (i < numShortBlocks) 0 else 1))
      k = k + dat.length
      val block: Array[Byte] = Arrays.copyOf(dat, shortBlockLen + 1)
      val ecc: Array[Byte] = ReedSolomon.reedSolomonComputeRemainder(dat, rsDiv)
      System.arraycopy(ecc, 0, block, block.length - blockEccLen, ecc.length)
      blocks(i) = block
    }
    
    // Interleave (not concatenate) the bytes from every block into a single sequence
    val result: Array[Byte] = Array.ofDim[Byte](rawCodewords)
    k = 0
    for {
      i <- (0 until blocks(0).length)
      j <- (0 until blocks.length)
      // Skip the padding byte in short blocks
      if (i != shortBlockLen - blockEccLen || j >= numShortBlocks)
    } {
      result(k) = blocks(j)(i)
      k = k + 1
    }
    result
  }
  
  
  // Draws the given sequence of 8-bit codewords (data and error correction) onto the entire
  // data area of this QR Code. Function modules need to be marked off before this is called.
  private def drawCodewords(builder: QrCodeBuilder, data: Array[Byte]): Unit = {
    if (data.length != QrCode.getNumRawDataModules(version) / 8)
      throw new IllegalArgumentException()
    
    var i = 0  // Bit index into the data
    // Do the funny zigzag scan
    var right = size - 1  // Index of right column in each column pair
    while (right >= 1) {
      if (right == 6) right = 5
      for {
        vert <- (0 until size)
        j <- (0 until 2)
      } {
        val x: Int = right - j  // Actual x coordinate
        val upward: Boolean = ((right + 1) & 2) == 0
        val y: Int = if (upward) (size - 1 - vert) else vert  // Actual y coordinate
        if (!builder.getFunctionModule(x, y) && i < data.length * 8) {
          builder.setModule(x, y, getBit(data(i >>> 3), 7 - (i & 7)))
          i = i + 1
        }
        // If this QR Code has any remainder bits (0 to 7), they were assigned as
        // 0/false/light by the constructor and are left unchanged by this method
      }
      right = right - 2
    }
    assert(i == data.length * 8)
  }
  
  
  // XORs the codeword modules in this QR Code with the given mask pattern.
  // The function modules must be marked and the codeword bits must be drawn
  // before masking. Due to the arithmetic of XOR, calling applyMask() with
  // the same mask value a second time will undo the mask. A final well-formed
  // QR Code needs exactly one (not zero, two, etc.) mask applied.
  private def applyMask(builder: QrCodeBuilder, msk: Int): Unit = {
    if (msk < 0 || msk > 7)
      throw new IllegalArgumentException("Mask value out of range")
    for {
      y <- (0 until size)
      x <- (0 until size)
      isFunction = builder.getFunctionModule(x, y)
    } {
        val invert = msk match {
          case 0 => (x + y) % 2 == 0
          case 1 => y % 2 == 0
          case 2 => x % 3 == 0
          case 3 => (x + y) % 3 == 0
          case 4 => (x / 3 + y / 2) % 2 == 0
          case 5 => x * y % 2 + x * y % 3 == 0
          case 6 => (x * y % 2 + x * y % 3) % 2 == 0
          case 7 => ((x + y) % 2 + x * y % 3) % 2 == 0
          case _ => throw new IllegalArgumentException("Mask value out of range")
        }
        builder.updateModule(x, y, _ ^ (invert & !isFunction))
      }
    }
  
  
  // Calculates and returns the penalty score based on state of this QR Code's current modules.
  // This is used by the automatic mask choice algorithm to find the mask pattern that yields the lowest score.
  private def getPenaltyScore(builder: QrCodeBuilder): Int = {
    var result: Int = 0
    
    // Adjacent modules in row having same color, and finder-like patterns
    (0 until size).foreach { y =>
      var runColor: Boolean = false;
      var runX: Int = 0;
      val runHistory: Array[Int] = Array.ofDim[Int](7);
      (0 until size).foreach { x =>
        if (builder.getModule(x, y) == runColor) {
          runX = runX + 1
          if (runX == 5) result = result + QrCode.PENALTY_N1
          else if (runX > 5) result = result + 1
        } else {
          finderPenaltyAddHistory(runX, runHistory)
          if (!runColor)
            result = result + finderPenaltyCountPatterns(runHistory) * QrCode.PENALTY_N3
          runColor = builder.getModule(x, y)
          runX = 1
        }
      }
      result = result + finderPenaltyTerminateAndCount(runColor, runX, runHistory) * QrCode.PENALTY_N3
    }
    // Adjacent modules in column having same color, and finder-like patterns
    (0 until size).foreach { x =>
      var runColor: Boolean = false;
      var runY: Int = 0;
      val runHistory: Array[Int] = Array.ofDim[Int](7);
      (0 until size).foreach { y =>
        if (builder.getModule(x, y) == runColor) {
          runY = runY + 1
          if (runY == 5) result = result + QrCode.PENALTY_N1
          else if (runY > 5) result = result + 1
        } else {
          finderPenaltyAddHistory(runY, runHistory)
          if (!runColor)
            result = result + finderPenaltyCountPatterns(runHistory) * QrCode.PENALTY_N3
          runColor = builder.getModule(x, y)
          runY = 1
        }
      }
      result = result + finderPenaltyTerminateAndCount(runColor, runY, runHistory) * QrCode.PENALTY_N3
    }
    
    // 2*2 blocks of modules having same color
    for {
      y <- (0 until size - 1)
      x <- (0 until size - 1)
      color = builder.getModule(x, y)
      if (color == builder.getModule(x + 1, y) &&
          color == builder.getModule(x, y + 1) &&
          color == builder.getModule(x + 1, y + 1))
    } result = result + QrCode.PENALTY_N2
    
    // Balance of dark and light modules
    val dark = builder.darkPoints
    val total = builder.totalPoints  // Note that size is odd, so dark/total != 1/2
    // Compute the smallest integer k >= 0 such that (45-5k)% <= dark/total <= (55+5k)%
    val k = (Math.abs(dark * 20 - total * 10) + total - 1) / total - 1;
    assert(0 <= k && k <= 9)
    result = result + k * QrCode.PENALTY_N4
    assert(0 <= result && result <= 2568888)  // Non-tight upper bound based on default values of PENALTY_N1, ..., N4
    result
  }
  
  // Returns an ascending list of positions of alignment patterns for this version number.
  // Each position is in the range [0,177), and are used on both the x and y axes.
  // This could be implemented as lookup table of 40 variable-length lists of unsigned bytes.
  private def getAlignmentPatternPositions(): Array[Int] = {
    if (version == 1) Array.empty[Int]
    else {
      val numAlign: Int = version / 7 + 2
      val step: Int = (version * 8 + numAlign * 3 + 5) / (numAlign * 4 - 4) * 2
      val result: Array[Int] = Array.ofDim[Int](numAlign)
      result(0) = 6
      var i = result.length - 1
      var pos = size - 7
      while (i >= 1) {
        result(i) = pos
        i = i - 1
        pos = pos - step
      }
      result
    }
  }
  
  
  
  // Can only be called immediately after a light run is added, and
  // returns either 0, 1, or 2. A helper function for getPenaltyScore().
  private def finderPenaltyCountPatterns(runHistory: Array[Int]): Int = {
    val n: Int = runHistory(1)
    assert(n <= size * 3)
    val core: Boolean = n > 0 && runHistory(2) == n && runHistory(3) == n * 3 && runHistory(4) == n && runHistory(5) == n

    (if (core && runHistory(0) >= n * 4 && runHistory(6) >= n) 1 else 0) +
    (if (core && runHistory(6) >= n * 4 && runHistory(0) >= n) 1 else 0)
  }
  
  
  // Must be called at the end of a line (row or column) of modules. A helper function for getPenaltyScore().
  private def finderPenaltyTerminateAndCount(currentRunColor: Boolean, currentRunLength: Int, runHistory: Array[Int]): Int = {
    if (currentRunColor) {  // Terminate dark run
      finderPenaltyAddHistory(currentRunLength, runHistory)
      finderPenaltyAddHistory(size, runHistory)   // Add light border to final run
    }
    else {
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
}

object QrCode {
  /** The minimum version number  (1) supported in the QR Code Model 2 standard. */
  val MIN_VERSION: Int =  1;
  
  /** The maximum version number (40) supported in the QR Code Model 2 standard. */
  val MAX_VERSION: Int = 40;
  
  // For use in getPenaltyScore(), when evaluating which mask is best.
  private val PENALTY_N1: Int =  3;
  private val PENALTY_N2: Int =  3;
  private val PENALTY_N3: Int = 40;
  private val PENALTY_N4: Int = 10;

  /**
   * The error correction level in a QR Code symbol.
   */
  enum Ecc(val formatBits: Int) {
    // Must be declared in ascending order of error protection
    // so that the implicit ordinal() and values() work properly
    /** The QR Code can tolerate about  7% erroneous codewords. */ case LOW extends Ecc(1)
    /** The QR Code can tolerate about 15% erroneous codewords. */ case MEDIUM extends Ecc(0)
    /** The QR Code can tolerate about 25% erroneous codewords. */ case QUARTILE extends Ecc(3)
    /** The QR Code can tolerate about 30% erroneous codewords. */ case HIGH extends Ecc(2)
  }  
  
  private val ECC_CODEWORDS_PER_BLOCK: Array[Array[Byte]] = Array(
    // Version: (note that index 0 is for padding, and is set to an illegal value)
    //0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
    Array[Byte](-1,  7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Low
    Array[Byte](-1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28),  // Medium
    Array[Byte](-1, 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Quartile
    Array[Byte](-1, 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // High
  )
  
  private val NUM_ERROR_CORRECTION_BLOCKS: Array[Array[Byte]] = Array(
    // Version: (note that index 0 is for padding, and is set to an illegal value)
    //0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
    Array[Byte](-1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25),  // Low
    Array[Byte](-1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49),  // Medium
    Array[Byte](-1, 1, 1, 2, 2, 4, 4, 6, 6, 8, 8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68),  // Quartile
    Array[Byte](-1, 1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81),  // High
  )

  // Returns the number of data bits that can be stored in a QR Code of the given version number, after
  // all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
  // The result is in the range [208, 29648]. This could be implemented as a 40-entry lookup table.
  private def getNumRawDataModules(ver: Int): Int = {
    if (ver < MIN_VERSION || ver > MAX_VERSION)
      throw new IllegalArgumentException("Version number out of range")
    
    val size: Int = ver * 4 + 17
    var result: Int = size * size   // Number of modules in the whole QR Code square
    result -= 8 * 8 * 3        // Subtract the three finders with separators
    result -= 15 * 2 + 1       // Subtract the format information and dark module
    result -= (size - 16) * 2  // Subtract the timing patterns (excluding finders)
    // The five lines above are equivalent to: int result = (16 * ver + 128) * ver + 64;
    if (ver >= 2) {
      val numAlign: Int = ver / 7 + 2
      result -= (numAlign - 1) * (numAlign - 1) * 25  // Subtract alignment patterns not overlapping with timing patterns
      result -= (numAlign - 2) * 2 * 20  // Subtract alignment patterns that overlap with timing patterns
      // The two lines above are equivalent to: result -= (25 * numAlign - 10) * numAlign - 55;
      if (ver >= 7)
        result -= 6 * 3 * 2  // Subtract version information
    }
    assert(208 <= result && result <= 29648)
    result
  }

  // Returns the number of 8-bit data (i.e. not error correction) codewords contained in any
  // QR Code of the given version number and error correction level, with remainder bits discarded.
  // This stateless pure function could be implemented as a (40*4)-cell lookup table.
  private def getNumDataCodewords(ver: Int, ecl: Ecc): Int = {
    getNumRawDataModules(ver) / 8
      - ECC_CODEWORDS_PER_BLOCK(ecl.ordinal)(ver)
      * NUM_ERROR_CORRECTION_BLOCKS(ecl.ordinal)(ver)
  }

  /**
   * Returns a QR Code representing the specified Unicode text string at the specified error correction level.
   * As a conservative upper bound, this function is guaranteed to succeed for strings that have 738 or fewer
   * Unicode code points (not UTF-16 code units) if the low error correction level is used. The smallest possible
   * QR Code version is automatically chosen for the output. The ECC level of the result may be higher than the
   * ecl argument if it can be done without increasing the version.
   * @param text the text to be encoded (not {@code null}), which can be any Unicode string
   * @param ecl the error correction level to use (not {@code null}) (boostable)
   * @return a QR Code (not {@code null}) representing the text
   * @throws DataTooLongException if the text fails to fit in the
   * largest version QR Code at the ECL, which means it is too long
   */
  def encodeText(text: CharSequence, ecl: Ecc): QrCode = {
    val segs: List[QrSegment] = QrSegment.makeSegments(text)
    encodeSegments(segs, ecl)
  }
  
  
  /**
   * Returns a QR Code representing the specified binary data at the specified error correction level.
   * This function always encodes using the binary segment mode, not any text mode. The maximum number of
   * bytes allowed is 2953. The smallest possible QR Code version is automatically chosen for the output.
   * The ECC level of the result may be higher than the ecl argument if it can be done without increasing the version.
   * @param data the binary data to encode (not {@code null})
   * @param ecl the error correction level to use (not {@code null}) (boostable)
   * @return a QR Code (not {@code null}) representing the data
   * @throws DataTooLongException if the data fails to fit in the
   * largest version QR Code at the ECL, which means it is too long
   */
  def encodeBinary(data: Seq[Byte], ecl: Ecc): QrCode = {
    val seg: QrSegment = QrSegment.makeBytes(data)
    encodeSegments(List(seg), ecl)
  }

  /**
   * Returns a QR Code representing the specified segments at the specified error correction
   * level. The smallest possible QR Code version is automatically chosen for the output. The ECC level
   * of the result may be higher than the ecl argument if it can be done without increasing the version.
   * <p>This function allows the user to create a custom sequence of segments that switches
   * between modes (such as alphanumeric and byte) to encode text in less space.
   * This is a mid-level API; the high-level API is {@link #encodeText(CharSequence,Ecc)}
   * and {@link #encodeBinary(byte[],Ecc)}.</p>
   * @param segs the segments to encode
   * @param ecl the error correction level to use (not {@code null}) (boostable)
   * @return a QR Code (not {@code null}) representing the segments
   * @throws DataTooLongException if the segments fail to fit in the
   * largest version QR Code at the ECL, which means they are too long
   */
  def encodeSegments(segs: Seq[QrSegment], ecl: Ecc): QrCode = {
    encodeSegments(segs, ecl, MIN_VERSION, MAX_VERSION, None, true)
  }

  /**
   * Returns a QR Code representing the specified segments with the specified encoding parameters.
   * The smallest possible QR Code version within the specified range is automatically
   * chosen for the output. Iff boostEcl is {@code true}, then the ECC level of the
   * result may be higher than the ecl argument if it can be done without increasing
   * the version. The mask number is either between 0 to 7 (inclusive) to force that
   * mask, or &#x2212;1 to automatically choose an appropriate mask (which may be slow).
   * <p>This function allows the user to create a custom sequence of segments that switches
   * between modes (such as alphanumeric and byte) to encode text in less space.
   * This is a mid-level API; the high-level API is {@link #encodeText(CharSequence,Ecc)}
   * and {@link #encodeBinary(byte[],Ecc)}.</p>
   * @param segs the segments to encode
   * @param ecl the error correction level to use (not {@code null}) (boostable)
   * @param minVersion the minimum allowed version of the QR Code (at least 1)
   * @param maxVersion the maximum allowed version of the QR Code (at most 40)
   * @param mask the mask number to use (between 0 and 7 (inclusive)), or None for automatic mask
   * @param boostEcl increases the ECC level as long as it doesn't increase the version number
   * @return a QR Code (not {@code null}) representing the segments
   * @throws DataTooLongException if the segments fail to fit in
   * the maxVersion QR Code at the ECL, which means they are too long
   */
  def encodeSegments(segs: Seq[QrSegment], ecl: Ecc, minVersion: Int, maxVersion: Int, mask: Option[Int], boostEcl: Boolean): QrCode = {
    require(MIN_VERSION <= minVersion, "Invalid minVersion")
    require(maxVersion <= MAX_VERSION, "Invalid maxVersion")
    require(mask.forall(x => x >= 0 && x <= 7), "Invalid mask")
    
    // Find the minimal version number to use
    var version = minVersion
    var dataUsedBits = 0
    var suitable = false
    while (!suitable) {
      val dataCapacityBits = getNumDataCodewords(version, ecl) * 8  // Number of data bits available
      dataUsedBits = QrSegment.getTotalBits(segs, version)
      if (dataUsedBits != -1 && dataUsedBits <= dataCapacityBits)
        suitable = true  // This version number is found to be suitable
      else if (version >= maxVersion) {  // All versions in the range could not fit the given data
        if (dataUsedBits != -1) throw new DataTooLongException(s"Data length = $dataUsedBits bits, Max capacity = $dataCapacityBits bits")
        else throw new DataTooLongException("Segment too long");
      }
      else version = version + 1
    }
    assert(dataUsedBits != -1)
    
    // Increase the error correction level while the data still fits in the current version number
    val boostedEcl = Ecc.values.foldLeft(ecl) { (oldEcl, newEcl) =>  // From low to high
      if (boostEcl && dataUsedBits <= getNumDataCodewords(version, newEcl) * 8) newEcl
      else oldEcl
    }
    
    // Concatenate all segments to create the data bit string
    val bb: BitBuffer = new BitBuffer();
    segs.foreach { seg =>
      bb.appendBits(seg.mode.modeBits, 4)
      bb.appendBits(seg.numChars, seg.mode.numCharCountBits(version))
      bb.appendData(seg.data)
    }
    assert(bb.getBitLength() == dataUsedBits)
    
    // Add terminator and pad up to a byte if applicable
    val dataCapacityBits = getNumDataCodewords(version, boostedEcl) * 8
    assert(bb.getBitLength() <= dataCapacityBits)
    bb.appendBits(0, Math.min(4, dataCapacityBits - bb.getBitLength()))
    bb.appendBits(0, (8 - bb.getBitLength() % 8) % 8)
    assert(bb.getBitLength() % 8 == 0)
    
    // Pad with alternating bytes until data capacity is reached
    var padByte = 0xEC
    while (bb.getBitLength() < dataCapacityBits) {
      bb.appendBits(padByte, 8)
      padByte = padByte ^ 0xEC ^ 0x11
    }
    
    // Pack bits into bytes in big endian
    val dataCodewords: Array[Byte] = Array.ofDim[Byte](bb.getBitLength() / 8)
    (0 until bb.getBitLength()).foreach { i =>
      dataCodewords(i >>> 3) = (dataCodewords(i >>> 3) | (bb.getBit(i) << (7 - (i & 7)))).toByte
    }
    
    // Create the QR Code object
    new QrCode(version, boostedEcl, dataCodewords, mask)
  }
}
