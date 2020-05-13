package printablelibrary



object Printer{

  //  Type class Printable
  trait Printable[A] {
    def format(value: A): String
  }



}
