package eu.joaocosta.qrgen.internal

/** Helper object containing Reed-Solomon operations.
  */
object ReedSolomon {

  /** Returns the product of the two given field elements modulo GF(2^8/0x11D).
    *  The arguments and result are unsigned 8-bit integers.
    */
  def reedSolomonMultiply(x: Int, y: Int): Int = {
    assert(x >> 8 == 0 && y >> 8 == 0)
    // Russian peasant multiplication
    var z = 0
    var i = 7
    while (i >= 0) {
      z = (z << 1) ^ ((z >>> 7) * 0x11d)
      z = z ^ ((y >>> i) & 1) * x
      i = i - 1
    }
    assert(z >>> 8 == 0)
    z
  }

  /** Returns the Reed-Solomon error correction codeword for the given data and divisor polynomials. */
  def reedSolomonComputeRemainder(data: Array[Byte], divisor: Array[Byte]): Array[Byte] = {
    val result: Array[Byte] = Array.ofDim[Byte](divisor.length)
    data.foreach { b => // Polynomial division
      val factor: Int = (b ^ result(0)) & 0xff
      System.arraycopy(result, 1, result, 0, result.length - 1)
      result(result.length - 1) = 0
      (0 until result.length).foreach { i =>
        result(i) = (result(i) ^ reedSolomonMultiply(divisor(i) & 0xff, factor)).toByte
      }
    }
    result
  }

  /** Returns a Reed-Solomon ECC generator polynomial for the given degree. */
  def reedSolomonComputeDivisor(degree: Int): Array[Byte] = {
    require(degree >= 1 && degree <= 255, "Degree out of range")
    // Polynomial coefficients are stored from highest to lowest power, excluding the leading term which is always 1.
    // For example the polynomial x^3 + 255x^2 + 8x + 93 is stored as the uint8 array {255, 8, 93}.
    val result = Array.ofDim[Byte](degree)
    result(degree - 1) = 1 // Start off with the monomial x^0

    // Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
    // and drop the highest monomial term which is always 1x^degree.
    // Note that r = 0x02, which is a generator element of this field GF(2^8/0x11D).
    var root: Int = 1
    (0 until degree).foreach { i =>
      // Multiply the current product by (x - r^i)
      (0 until result.length).foreach { j =>
        result(j) = reedSolomonMultiply(result(j) & 0xff, root).toByte
        if (j + 1 < result.length) result(j) = (result(j) ^ result(j + 1)).toByte
      }
      root = reedSolomonMultiply(root, 0x02)
    }
    result
  }
}
