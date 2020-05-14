package codec

import datatype.EncoderDecoder.Codec

object Codec {
  def encode[A](value: A)(implicit e: Codec[A]): String =
    e.encode(value)
  def decode[A](value: String)(implicit d: Codec[A]): A =
    d.decode(value)

}
