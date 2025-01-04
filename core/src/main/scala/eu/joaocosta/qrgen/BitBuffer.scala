package eu.joaocosta.qrgen

import scala.collection.*
import eu.joaocosta.qrgen.internal.*

/** Packed sequence of bits */
sealed trait BitBuffer {

  /** Size of this collection */
  def size: Int

  /** Returns the bit at the specified index, yielding false or true.
    * @param index the index to get the bit at
    * @return the bit at the specified index
    * @throws IndexOutOfBoundsException if index < 0 or index <= bitLength
    */
  def apply(index: Int): Boolean

  /** Returns the bit at the specified index, yielding 0 or 1.
    * @param index the index to get the bit at
    * @return the bit at the specified index
    * @throws IndexOutOfBoundsException if index < 0 or index <= bitLength
    */
  def getInt(index: Int): Int = if (apply(index)) 1 else 0

  /** Returns an immutable version of this bit buffer. */
  def toImmutable: BitBuffer.Immutable
}

object BitBuffer {
  final case class Immutable(data: immutable.BitSet = immutable.BitSet(), size: Int) extends BitBuffer {
    def apply(index: Int): Boolean = {
      if (index < 0 || index >= size)
        throw new IndexOutOfBoundsException()
      data(index)
    }

    def toImmutable: Immutable = this
  }
  final class Mutable(data: mutable.BitSet = new mutable.BitSet(), private var bitLength: Int = 0) extends BitBuffer {
    def size: Int = bitLength

    def apply(index: Int): Boolean = {
      if (index < 0 || index >= size)
        throw new IndexOutOfBoundsException()
      data(index)
    }

    def toImmutable: Immutable = Immutable(data.toImmutable, size)

    /** Appends the specified number of low-order bits of the specified value to this
      * buffer. Requires 0 < len < 31 and 0 <= val < 2^len.
      * @param value the value to append
      * @param len the number of low-order bits in the value to take
      * would make bitLength exceed Integer.MAX_VALUE
      */
    def appendBits(value: Int, len: Int): Unit = {
      require(len >= 0 && len <= 31 && value >>> len == 0, "Value out of range")
      require(Integer.MAX_VALUE - bitLength >= len, "Maximum length reached")
      var i: Int = len - 1
      while (i >= 0) {
        data.update(bitLength, Helpers.getBit(value, i))
        i = i - 1
        bitLength = bitLength + 1
      }
    }

    /** Appends the content of the specified bit buffer to this buffer.
      * @param bb the bit buffer whose data to append
      */
    def appendData(bb: BitBuffer): Unit = {
      require(Integer.MAX_VALUE - size >= bb.size, "Maximum length reached")
      var i: Int = 0
      while (i < bb.size) {
        data.update(bitLength, bb(i))
        i = i + 1
        bitLength = bitLength + 1
      }
    }

    override def clone(): BitBuffer.Mutable = {
      new BitBuffer.Mutable(data.clone(), size)
    }
  }
}
