package printablelibrary



object Printer{

  //  Type class Printable
  trait Printable[A] {
    self =>
    def format(value: A): String

    def contramap[B](func: B=> A): Printable[B] =
      new Printable[B] {
        override def format(value: B): String =
          self.format(func(value))
      }
  }




}
