package eu.joaocosta.qrgen

/** Thrown when the supplied data does not fit any QR Code version. */
final class DataTooLongException(msg: String) extends IllegalArgumentException(msg)
