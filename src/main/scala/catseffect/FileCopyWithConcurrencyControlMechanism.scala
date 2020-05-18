package catseffect

import java.io._

import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, IO, Resource}
import cats.implicits._

class FileCopyWithConcurrencyControlMechanism {

  def inputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f))
    }{ inStream =>
      guard.withPermit {
        IO(inStream.close()).handleErrorWith( _ => IO.unit)
      }
    }

  def outputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(f))
    }{ outStream =>
      guard.withPermit {
        IO(outStream.close()).handleErrorWith( _ => IO.unit)
      }
    }

  def inputOutputStreams(in: File, out: File, guard: Semaphore[IO])
  : Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in, guard)
      outStream <- outputStream(out, guard)
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

  def copy(origin: File, destination: File)
          (implicit concurrent: Concurrent[IO]): IO[Long] = {
    for {
      guard <- Semaphore[IO](1)
      count <- inputOutputStreams(origin, destination, guard).use {
        case (in, out) => transfer(in, out)
      }
    } yield count
  }


}
