package eu.joaocosta.qrgen

import java.util.Arrays
import eu.joaocosta.qrgen.internal.*
import scala.collection.immutable.ArraySeq

/** A QR Code symbol, which is a type of two-dimension barcode.
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
final case class QrCode(version: Int, errorCorrectionLevel: Ecc, dataCodewords: ArraySeq[Byte], mask: Option[Int]) {
  // Check arguments and initialize fields
  require(version >= QrCode.MIN_VERSION && version <= QrCode.MAX_VERSION, "Version value out of range")
  require(mask.forall(x => x >= 0 && x <= 7), "Mask value out of range")

  val size = version * 4 + 17

  val (modules, bestMask) = {
    // Private grids of modules/pixels, with dimensions of size*size
    val builder = new QrCodeBuilder(size)

    // Compute ECC, draw modules, do masking
    builder.drawFunctionPatterns(version, errorCorrectionLevel)
    builder.drawCodewords(version, Ecc.addEccAndInterleave(version, errorCorrectionLevel, dataCodewords.toArray))

    // Do masking
    val _bestMask = mask match {
      case Some(m) => m
      case None => // Automatically choose best mask
        (0 until 8)
          .foldLeft((0, Integer.MAX_VALUE)) { case ((best, minPenalty), testMask) =>
            builder.applyMask(testMask)
            builder.drawFormatBits(errorCorrectionLevel, testMask)
            val penalty: Int = Penalty.getPenaltyScore(builder)
            builder.applyMask(testMask) // Undoes the mask due to XOR
            if (penalty < minPenalty) (testMask, penalty)
            else (best, minPenalty)
          }
          ._1
    }
    builder.applyMask(_bestMask)                            // Apply the final choice of mask
    builder.drawFormatBits(errorCorrectionLevel, _bestMask) // Overwrite old format bits

    (builder.result(), _bestMask)
  }

  /** Returns the color of the module (pixel) at the specified coordinates, which is {@code false}
    * for light or {@code true} for dark. The top left corner has the coordinates (x=0, y=0).
    * If the specified coordinates are out of bounds, then {@code false} (light) is returned.
    * @param x the x coordinate, where 0 is the left edge and size&#x2212;1 is the right edge
    * @param y the y coordinate, where 0 is the top edge and size&#x2212;1 is the bottom edge
    * @return {@code true} if the coordinates are in bounds and the module
    * at that location is dark, or {@code false} (light) otherwise
    */
  def getModule(x: Int, y: Int): Boolean = {
    0 <= x && x < size && 0 <= y && y < size && modules(y)(x)
  }
}

object QrCode {

  /** The minimum version number  (1) supported in the QR Code Model 2 standard. */
  val MIN_VERSION: Int = 1

  /** The maximum version number (40) supported in the QR Code Model 2 standard. */
  val MAX_VERSION: Int = 40

  // Returns the number of data bits that can be stored in a QR Code of the given version number, after
  // all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
  // The result is in the range [208, 29648]. This could be implemented as a 40-entry lookup table.
  def getNumRawDataModules(version: Int): Int = {
    require(version >= MIN_VERSION && version <= MAX_VERSION, "Version number out of range")
    val numAlign: Int = version / 7 + 2
    val size: Int     = version * 4 + 17
    val filledModules =
      (8 * 8 * 3) +         // Three finders with separators
        (15 * 2 + 1) +      // Format information and dark module
        ((size - 16) * 2) + // Timing patterns (excluding finders)
        (if (version >= 2) ((numAlign - 1) * (numAlign - 1) * 25)
         else 0) + // Alignment patterns not overlapping with timing patterns
        (if (version >= 2) ((numAlign - 2) * 2 * 20) else 0) + // Alignment patterns that overlap with timing patterns
        (if (version >= 7) (6 * 3 * 2) else 0)                 // Version information
    val result: Int = size * size - filledModules
    assert(208 <= result && result <= 29648)
    result
  }

  // Returns an ascending list of positions of alignment patterns for this version number.
  // Each position is in the range [0,177), and are used on both the x and y axes.
  // This could be implemented as lookup table of 40 variable-length lists of unsigned bytes.
  def getAlignmentPatternPositions(version: Int, size: Int): Vector[Int] = {
    if (version == 1) Vector.empty[Int]
    else {
      val numAlign: Int = version / 7 + 2
      val step: Int     = (version * 8 + numAlign * 3 + 5) / (numAlign * 4 - 4) * 2
      (6 +: Iterator.iterate(size - 7)(_ - step).take(numAlign - 1).toVector.reverse)
    }
  }

  /** Returns a QR Code representing the specified Unicode text string at the specified error correction level.
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

  /** Returns a QR Code representing the specified binary data at the specified error correction level.
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

  /** Returns a QR Code representing the specified segments at the specified error correction
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

  /** Returns a QR Code representing the specified segments with the specified encoding parameters.
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
  def encodeSegments(
      segs: Seq[QrSegment],
      ecl: Ecc,
      minVersion: Int,
      maxVersion: Int,
      mask: Option[Int],
      boostEcl: Boolean
  ): QrCode = {
    require(MIN_VERSION <= minVersion, "Invalid minVersion")
    require(maxVersion <= MAX_VERSION, "Invalid maxVersion")
    require(mask.forall(x => x >= 0 && x <= 7), "Invalid mask")

    // Find the minimal version number to use
    var version      = minVersion
    var dataUsedBits = 0
    var suitable     = false
    while (!suitable) {
      val dataCapacityBits = Ecc.getNumDataCodewords(version, ecl) * 8 // Number of data bits available
      dataUsedBits = QrSegment.getTotalBits(segs, version)
      if (dataUsedBits != -1 && dataUsedBits <= dataCapacityBits)
        suitable = true                 // This version number is found to be suitable
      else if (version >= maxVersion) { // All versions in the range could not fit the given data
        if (dataUsedBits != -1)
          throw new DataTooLongException(s"Data length = $dataUsedBits bits, Max capacity = $dataCapacityBits bits")
        else throw new DataTooLongException("Segment too long")
      } else version = version + 1
    }
    assert(dataUsedBits != -1)

    // Increase the error correction level while the data still fits in the current version number
    val boostedEcl = Ecc.values.foldLeft(ecl) { (oldEcl, newEcl) => // From low to high
      if (boostEcl && dataUsedBits <= Ecc.getNumDataCodewords(version, newEcl) * 8) newEcl
      else oldEcl
    }

    // Concatenate all segments to create the data bit string
    val bb = new BitBuffer.Mutable()
    segs.foreach { seg =>
      bb.appendBits(seg.mode.modeBits, 4)
      bb.appendBits(seg.numChars, seg.mode.numCharCountBits(version))
      bb.appendData(seg.data)
    }
    assert(bb.size == dataUsedBits)

    // Add terminator and pad up to a byte if applicable
    val dataCapacityBits = Ecc.getNumDataCodewords(version, boostedEcl) * 8
    assert(bb.size <= dataCapacityBits)
    bb.appendBits(0, Math.min(4, dataCapacityBits - bb.size))
    bb.appendBits(0, (8 - bb.size % 8) % 8)
    assert(bb.size                % 8 == 0)

    // Pad with alternating bytes until data capacity is reached
    var padByte = 0xec
    while (bb.size < dataCapacityBits) {
      bb.appendBits(padByte, 8)
      padByte = padByte ^ 0xec ^ 0x11
    }

    // Pack bits into bytes in big endian
    val dataCodewords: Array[Byte] = Array.ofDim[Byte](bb.size / 8)
    (0 until bb.size).foreach { i =>
      dataCodewords(i >>> 3) = (dataCodewords(i >>> 3) | (bb.getInt(i) << (7 - (i & 7)))).toByte
    }

    // Create the QR Code object
    new QrCode(version, boostedEcl, ArraySeq.unsafeWrapArray(dataCodewords), mask)
  }
}
