package eu.joaocosta.qrgen

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class BaselineSpec extends ScalaCheckSuite {

  def compareQrCodes(a: QrCode, b: io.nayuki.qrcodegen.QrCode): Unit = {
    assert(a.size == b.size)
    assert(
      Vector.tabulate(a.size, a.size)((x, y) => a.getModule(x, y)) ==
        Vector.tabulate(b.size, b.size)((x, y) => b.getModule(x, y))
    )
  }

  property("QR Codes generated from text match the reference implementation") {
    forAllNoShrink { (string: String) =>
      compareQrCodes(
        QrCode.encodeText(string, Ecc.LOW),
        io.nayuki.qrcodegen.QrCode.encodeText(string, io.nayuki.qrcodegen.QrCode.Ecc.LOW)
      )
      compareQrCodes(
        QrCode.encodeText(string, Ecc.MEDIUM),
        io.nayuki.qrcodegen.QrCode.encodeText(string, io.nayuki.qrcodegen.QrCode.Ecc.MEDIUM)
      )
      compareQrCodes(
        QrCode.encodeText(string, Ecc.QUARTILE),
        io.nayuki.qrcodegen.QrCode.encodeText(string, io.nayuki.qrcodegen.QrCode.Ecc.QUARTILE)
      )
      compareQrCodes(
        QrCode.encodeText(string, Ecc.HIGH),
        io.nayuki.qrcodegen.QrCode.encodeText(string, io.nayuki.qrcodegen.QrCode.Ecc.HIGH)
      )
    }
  }

  property("QR Codes generated from binary match the reference implementation") {
    forAllNoShrink { (binary: Vector[Byte]) =>
      compareQrCodes(
        QrCode.encodeBinary(binary, Ecc.LOW),
        io.nayuki.qrcodegen.QrCode.encodeBinary(binary.toArray, io.nayuki.qrcodegen.QrCode.Ecc.LOW)
      )
      compareQrCodes(
        QrCode.encodeBinary(binary, Ecc.MEDIUM),
        io.nayuki.qrcodegen.QrCode.encodeBinary(binary.toArray, io.nayuki.qrcodegen.QrCode.Ecc.MEDIUM)
      )
      compareQrCodes(
        QrCode.encodeBinary(binary, Ecc.QUARTILE),
        io.nayuki.qrcodegen.QrCode.encodeBinary(binary.toArray, io.nayuki.qrcodegen.QrCode.Ecc.QUARTILE)
      )
      compareQrCodes(
        QrCode.encodeBinary(binary, Ecc.HIGH),
        io.nayuki.qrcodegen.QrCode.encodeBinary(binary.toArray, io.nayuki.qrcodegen.QrCode.Ecc.HIGH)
      )
    }
  }
}
