package eu.joaocosta.qrgen

import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

/** A segment of character/binary/control data in a QR Code symbol. */
final case class QrSegment(mode: QrSegment.Mode, numChars: Int, data: BitBuffer.Immutable)

/** The mid-level way to create a segment is to take the payload data and call a
  * static factory function such as [[QrSegment#makeNumeric(CharSequence)]].
  *
  * The low-level way to create a segment is to custom-make the bit buffer and call the
  * segment contructor  with appropriate values.
  *
  * This segment class imposes no length restrictions, but QR Codes have restrictions.
  * Even in the most favorable conditions, a QR Code can only hold 7089 characters of data.
  * Any segment longer than this is meaningless for the purpose of generating QR Codes.
  */
object QrSegment {

  // Describes precisely all strings that are encodable in numeric mode.
  private val NUMERIC_REGEX: Pattern = Pattern.compile("[0-9]*")

  // Describes precisely all strings that are encodable in alphanumeric mode.
  private val ALPHANUMERIC_REGEX: Pattern = Pattern.compile("[A-Z0-9 $%*+./:-]*")

  // The set of all legal characters in alphanumeric mode, where
  // each character value maps to the index in the string.
  private val ALPHANUMERIC_CHARSET: String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:"

  private def isNumeric(text: CharSequence): Boolean = {
    NUMERIC_REGEX.matcher(text).matches()
  }

  private def isAlphanumeric(text: CharSequence): Boolean = {
    ALPHANUMERIC_REGEX.matcher(text).matches()
  }

  /** Qr Segment Mode. */
  enum Mode(val modeBits: Int, val numBitsCharCount: Vector[Int]) {
    case NUMERIC      extends Mode(0x1, Vector(10, 12, 14))
    case ALPHANUMERIC extends Mode(0x2, Vector(9, 11, 13))
    case BYTE         extends Mode(0x4, Vector(8, 16, 16))
    case KANJI        extends Mode(0x8, Vector(8, 10, 12))
    case ECI          extends Mode(0x7, Vector(0, 0, 0))

    /** Returns the bit width of the character count field for a segment in this mode
      *     in a QR Code at the given version number. The result is in the range [0, 16].
      */
    def numCharCountBits(ver: Int): Int = {
      assert(QrCode.MIN_VERSION <= ver && ver <= QrCode.MAX_VERSION)
      numBitsCharCount((ver + 7) / 17)
    }
  }

  /** Returns a segment representing the specified binary data
    * encoded in byte mode. All input byte arrays are acceptable.
    * @param data the binary data
    * @return a segment containing the data
    */
  def makeBytes(data: Seq[Byte]): QrSegment = {
    val bb = new BitBuffer.Mutable()
    data.foreach(b => bb.appendBits(b & 0xff, 8))
    QrSegment(Mode.BYTE, data.length, bb.toImmutable)
  }

  /** Returns a segment representing the specified string of decimal digits encoded in numeric mode.
    * @param digits the text, with only digits from 0 to 9 allowed
    * @return a segment containing the text
    */
  def makeNumeric(digits: CharSequence): QrSegment = {
    require(isNumeric(digits), "String contains non-numeric characters")

    val bb = new BitBuffer.Mutable()
    digits.toString.sliding(3, 3).foreach { slice => // Process groups of 3
      bb.appendBits(slice.toInt, slice.size * 3 + 1)
    }
    QrSegment(Mode.NUMERIC, digits.length(), bb.toImmutable)
  }

  /** Returns a segment representing the specified text string encoded in alphanumeric mode.
    * The characters allowed are: 0 to 9, A to Z (uppercase only), space,
    * dollar, percent, asterisk, plus, hyphen, period, slash, colon.
    * @param text the text, with only certain characters allowed
    * @return a segment containing the text
    */
  def makeAlphanumeric(text: CharSequence): QrSegment = {
    require(isAlphanumeric(text), "String contains unencodable characters in alphanumeric mode")

    val bb = new BitBuffer.Mutable()
    var i  = 0
    while (i <= text.length() - 2) { // Process groups of 2
      var temp: Int = ALPHANUMERIC_CHARSET.indexOf(text.charAt(i)) * 45
      temp += ALPHANUMERIC_CHARSET.indexOf(text.charAt(i + 1))
      bb.appendBits(temp, 11)
      i += 2
    }
    if (i < text.length()) // 1 character remaining
      bb.appendBits(ALPHANUMERIC_CHARSET.indexOf(text.charAt(i)), 6)
    QrSegment(Mode.ALPHANUMERIC, text.length(), bb.toImmutable)
  }

  /** Returns a list of zero or more segments to represent the specified Unicode text string.
    * The result may use various segment modes and switch modes to optimize the length of the bit stream.
    * @param text the text to be encoded, which can be any Unicode string
    * @return a new list of segments containing the text
    */
  def makeSegments(text: CharSequence): List[QrSegment] = {
    if (text.equals("")) List.empty // Leave result empty
    else if (isNumeric(text))
      List(makeNumeric(text))
    else if (isAlphanumeric(text))
      List(makeAlphanumeric(text))
    else
      List(makeBytes(text.toString().getBytes(StandardCharsets.UTF_8).toIndexedSeq))
  }

  /** Returns a segment representing an Extended Channel Interpretation
    * (ECI) designator with the specified assignment value.
    * @param assignVal the ECI assignment number (see the AIM ECI specification)
    * @return a segment containing the data
    * @throws IllegalArgumentException if the value is outside the range [0, 10^6)
    */
  def makeEci(assignVal: Int): QrSegment = {
    val bb = new BitBuffer.Mutable()
    if (assignVal < 0)
      throw new IllegalArgumentException("ECI assignment value out of range")
    else if (assignVal < (1 << 7))
      bb.appendBits(assignVal, 8)
    else if (assignVal < (1 << 14)) {
      bb.appendBits(2, 2) // 0b10 = 2
      bb.appendBits(assignVal, 14)
    } else if (assignVal < 1_000_000) {
      bb.appendBits(6, 3) // 0b110 = 6
      bb.appendBits(assignVal, 21)
    } else
      throw new IllegalArgumentException("ECI assignment value out of range")
    QrSegment(Mode.ECI, 0, bb.toImmutable)
  }

  /** Calculates the number of bits needed to encode the given segments at the given version.
    * Returns a non-negative number if successful. Otherwise returns -1 if a segment has too
    * many characters to fit its length field, or the total bits exceeds Integer.MAX_VALUE.
    */
  def getTotalBits(segs: Seq[QrSegment], version: Int): Int = {
    segs
      .foldLeft(0L) { case (result, seg) =>
        if (result < 0) result
        else {
          val ccbits = seg.mode.numCharCountBits(version)
          if (seg.numChars >= (1 << ccbits)) -1 // The segment's length doesn't fit the field's bit width
          else {
            val newResult = result + 4 + ccbits + seg.data.size
            if (newResult > Integer.MAX_VALUE) -1 // The sum will overflow an int type
            else newResult
          }
        }
      }
      .toInt
  }
}
