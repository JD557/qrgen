package eu.joaocosta.qrgen

import scala.collection.mutable.BitSet
import eu.joaocosta.qrgen.Helpers

final class BitBuffer(val data: BitSet = new BitSet(), var bitLength: Int = 0) {

  /** Returns the length of this sequence, which is a non-negative value.
    * @return the length of this sequence
    */
  def getBitLength(): Int = bitLength

  /** Returns the bit at the specified index, yielding 0 or 1.
    * @param index the index to get the bit at
    * @return the bit at the specified index
    * @throws IndexOutOfBoundsException if index &lt; 0 or index &#x2265; bitLength
    */
  def getBit(index: Int): Int = {
    if (index < 0 || index >= bitLength)
      throw new IndexOutOfBoundsException()
    if (data(index)) 1 else 0
  }

  /** Appends the specified number of low-order bits of the specified value to this
    * buffer. Requires 0 &#x2264; len &#x2264; 31 and 0 &#x2264; val &lt; 2<sup>len</sup>.
    * @param value the value to append
    * @param len the number of low-order bits in the value to take
    * @throws IllegalArgumentException if the value or number of bits is out of range
    * @throws IllegalStateException if appending the data
    * would make bitLength exceed Integer.MAX_VALUE
    */
  def appendBits(value: Int, len: Int): Unit = {
    if (len < 0 || len > 31 || value >>> len != 0)
      throw new IllegalArgumentException("Value out of range")
    if (Integer.MAX_VALUE - bitLength < len)
      throw new IllegalStateException("Maximum length reached")
    var i: Int = len - 1
    while (i >= 0) {
      data.update(bitLength, Helpers.getBit(value, i))
      i = i - 1
      bitLength = bitLength + 1
    }
  }

  /** Appends the content of the specified bit buffer to this buffer.
    * @param bb the bit buffer whose data to append (not {@code null})
    * @throws IllegalStateException if appending the data
    * would make bitLength exceed Integer.MAX_VALUE
    */
  def appendData(bb: BitBuffer): Unit = {
    if (Integer.MAX_VALUE - bitLength < bb.bitLength)
      throw new IllegalStateException("Maximum length reached")
    var i: Int = 0
    while (i < bb.bitLength) {
      data.update(bitLength, bb.data(i));
      i = i + 1
      bitLength = bitLength + 1
    }
  }

  override def clone(): BitBuffer = {
    new BitBuffer(data.clone(), bitLength)
  }

}
