package eu.joaocosta.qrgen

import java.util.Arrays

/** The error correction level in a QR Code symbol.
  */
enum Ecc(val formatBits: Int) {
  // Must be declared in ascending order of error protection
  // so that the implicit ordinal() and values() work properly
  /** The QR Code can tolerate about  7% erroneous codewords. */
  case LOW extends Ecc(1)

  /** The QR Code can tolerate about 15% erroneous codewords. */
  case MEDIUM extends Ecc(0)

  /** The QR Code can tolerate about 25% erroneous codewords. */
  case QUARTILE extends Ecc(3)

  /** The QR Code can tolerate about 30% erroneous codewords. */
  case HIGH extends Ecc(2)
}

object Ecc {
  private val ECC_CODEWORDS_PER_BLOCK: Array[Array[Byte]] = Array(
    // Version: (note that index 0 is for padding, and is set to an illegal value)
    // 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
    Array[Byte](-1, 7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26,
      28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30), // Low
    Array[Byte](-1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28,
      28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28), // Medium
    Array[Byte](-1, 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30,
      28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30), // Quartile
    Array[Byte](-1, 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30,
      30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30) // High
  )

  private val NUM_ERROR_CORRECTION_BLOCKS: Array[Array[Byte]] = Array(
    // Version: (note that index 0 is for padding, and is set to an illegal value)
    // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
    Array[Byte](-1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8, 8, 9, 9, 10, 12, 12, 12, 13, 14, 15, 16,
      17, 18, 19, 19, 20, 21, 22, 24, 25), // Low
    Array[Byte](-1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26,
      28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49), // Medium
    Array[Byte](-1, 1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35,
      38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68), // Quartile
    Array[Byte](-1, 1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40,
      42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81) // High
  )

  // Returns the number of 8-bit data (i.e. not error correction) codewords contained in any
  // QR Code of the given version number and error correction level, with remainder bits discarded.
  // This stateless pure function could be implemented as a (40*4)-cell lookup table.
  def getNumDataCodewords(version: Int, errorCorrectionLevel: Ecc): Int = {
    QrCode.getNumRawDataModules(version) / 8
      - ECC_CODEWORDS_PER_BLOCK(errorCorrectionLevel.ordinal)(version)
      * NUM_ERROR_CORRECTION_BLOCKS(errorCorrectionLevel.ordinal)(version)
  }

  // Returns a new byte string representing the given data with the appropriate error correction
  // codewords appended to it, based on this object's version and error correction level.
  def addEccAndInterleave(version: Int, errorCorrectionLevel: Ecc, data: Array[Byte]): Array[Byte] = {
    require(data.length == getNumDataCodewords(version, errorCorrectionLevel), "Invalid data length")

    // Calculate parameter numbers
    val numBlocks: Int      = NUM_ERROR_CORRECTION_BLOCKS(errorCorrectionLevel.ordinal)(version)
    val blockEccLen: Int    = ECC_CODEWORDS_PER_BLOCK(errorCorrectionLevel.ordinal)(version)
    val rawCodewords: Int   = QrCode.getNumRawDataModules(version) / 8
    val numShortBlocks: Int = numBlocks - rawCodewords % numBlocks
    val shortBlockLen: Int  = rawCodewords / numBlocks

    // Split data into blocks and append ECC to each block
    val rsDiv: Array[Byte] = ReedSolomon.reedSolomonComputeDivisor(blockEccLen)
    val blocks = (0 until numBlocks).iterator.map { i =>
      val longBlocks = math.max(0, i - numShortBlocks)
      val k          = i * (shortBlockLen - blockEccLen) + longBlocks
      val dat: Array[Byte] =
        Arrays.copyOfRange(data, k, k + (shortBlockLen - blockEccLen) + (if (i < numShortBlocks) 0 else 1))
      val block: Array[Byte] = Arrays.copyOf(dat, shortBlockLen + 1)
      val ecc: Array[Byte]   = ReedSolomon.reedSolomonComputeRemainder(dat, rsDiv)
      System.arraycopy(ecc, 0, block, block.length - blockEccLen, ecc.length)
      block
    }.toArray

    // Interleave (not concatenate) the bytes from every block into a single sequence
    (for {
      i <- (0 until (shortBlockLen + 1)).iterator
      j <- (0 until blocks.length)
      // Skip the padding byte in short blocks
      if (i != shortBlockLen - blockEccLen || j >= numShortBlocks)
    } yield blocks(j)(i)).toArray
  }
}
