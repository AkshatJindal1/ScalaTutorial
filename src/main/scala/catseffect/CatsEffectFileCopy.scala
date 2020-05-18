package catseffect

import cats.effect._
import cats.implicits._
import java.io._

class CatsEffectFileCopy {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f))
    }{ inStream =>
      IO(inStream.close()).handleErrorWith( _ => IO.unit)
    }

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(f))
    }{ outStream =>
      IO(outStream.close()).handleErrorWith( _ => IO.unit)
    }

  def inputOutputStreams(in: File, out: File)
  : Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield(inStream, outStream)

  def transmit(
                origin: InputStream,
                destination: OutputStream,
                buffer: Array[Byte],
                accumulator: Long): IO[Long] =
    for {
      amount <- IO(origin.read(buffer, 0, buffer.size))
      count <- if(amount > -1)
                  IO(destination.write(buffer, 0, amount)) >> //  >> operator is same is doing first.flatMap(_ => second)
                    transmit(origin, destination, buffer, accumulator + amount)
               else IO.pure(accumulator)
    } yield count

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10))
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use {
      case (in, out) => transfer(in, out)
    }

  /*
        Writing copy using bracket in cats effect :
          def copy2(origin: File, destination: File): IO[Long] = {
            val inIO: IO[InputStream] = IO(new FileInputStream(origin))
            val outIO: IO[OutputStream] = IO(new FileOutputStream(destination))

            (inIO, outIO).tupled                                  //  1. Getting resource
              .bracket {
                case (in, out) => transfer(in, out)               //  2. using resource to copy data
            } { case(in, out) =>
                  (IO(in.close), IO(out.close)).tupled.           //  3. freeing resources
                    handleErrorWith(_ => IO.unit).void
              }

          }

          This has issue because in case where input file open successfully but
          output file fails to open, then input file is not closed then and there
          thus leading to errors later. So our implementation solves that as we
          have separate instances and closing operation for both
   */

}

object Main extends IOApp {
  def copy(origin: File, destination: File): IO[Long] = ???

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- if(args.length < 2)
        IO.raiseError(new IllegalArgumentException("Need origin and destination file"))
      else IO.unit

      orig = new File(args(0))
      dest = new File(args(1))
      count <- copy(orig, dest)
      _ <- IO(println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}"))
    } yield ExitCode.Success
}
