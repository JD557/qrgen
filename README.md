# QRgen

An opinionated port of https://github.com/nayuki/QR-Code-generator to Scala.

Targets the JVM, JS and Native.

## Usage

```scala
//> using scala 3.3.4
//> using dep eu.joaocosta::qrgen::0.1.0

import eu.joaocosta.qrgen.*

val qrCode = QrCode.encodeText("https://www.scala-lang.org/", Ecc.LOW)

qrCode.foreach { line =>
  println(line.map(x => if (x) "\u2588" else " ").mkString)
}
```

## Differences from the Java version

The code here was heavily based on [nayuki/QR-Code-generator/java](https://github.com/nayuki/QR-Code-generator/tree/master/java),
with a few differences, some of which include:

- Splitting the code in smaller, self contained classes
- Favor immutability (there's still some internal mutability, though)
- Remove `QrSegmentAdvanced`
- Use `String` instead of `CharSequence`
- Make QR Codes extend `IndexedSeq[IndexedSeq[Boolean]]`

This can have some performance impact, so the original version might be faster.
