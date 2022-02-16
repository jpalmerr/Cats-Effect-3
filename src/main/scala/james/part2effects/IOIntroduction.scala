package james.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // argument that should not have side effects: evaluated eagerly
  val aDelayedIO: IO[Int] = IO.delay({
    println("Im producing an integer")
    54
  })

  val shouldNotDoThis: IO[Int] = IO.pure({
    println("I am eagerly evaluated")
    54
  }) // notice evaluated eagerly => use delay

  val aDelayedIOv2: IO[Int] = IO {
    println("I am eagerly evaluated")
    54
  } // apply = delay

  // goal of cats effects: compose computations with IO data types

  // map, flatMap

  val improvedMeaningOfLife: IO[Int] = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife: IO[Unit] = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply.*
  val combinedMeaningOfLife = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)

  def smallProgramv2(): IO[Unit] = {
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)
  }

  /**
   * Exercises
   */

  // 1 - sequence two IOs and take the result of the LAST one
  def sequenceTakeLast[A, B](ioA: IO[A], ioB: IO[B]): IO[B] = {
    for {
      a <- ioA
      b <- ioB
    } yield b
  }

  def sequenceTakeLastv2[A, B](ioA: IO[A], ioB: IO[B]): IO[B] = {
    ioA *> ioB // andThen (productR)
  }

  def sequenceTakeLastv3[A, B](ioA: IO[A], ioB: IO[B]): IO[B] = {
    ioA >> ioB // andThen (passed by name, evaluation is lazy on 2nd argument)
  }

  // 2 - sequence two IOs and take the result of the FIRST one
  def sequenceTakeFirst[A, B](ioA: IO[A], ioB: IO[B]): IO[A] = {
    for {
      a <- ioA
      b <- ioB
    } yield a
  }

  // I use this pattern a lot when logging clients, i.e make call successfully, then log call, but then return call response
  def sequenceTakeFirstv2[A, B](ioA: IO[A], ioB: IO[B]): IO[A] = {
    ioA <* ioB
  }

  // 3 - repeat an IO effect forever
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io)

  def forever_v3INCORRECT[A](io: IO[A]): IO[A] =
    io *> forever_v3INCORRECT(io) // stack overflow, even without unsaferun, due to eager evaluation

  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion

  // 4 - convert IO to a different typ
  def convert[A, B](ioA: IO[A], value: B): IO[B] =
    ioA.map(_ => value)

  def convert_v2[A, B](ioA: IO[A], value: B): IO[B] =
    ioA.as(value)

  // 5 - discard value inside an IO and just return unit
  def asUnit[A](ioA: IO[A]): IO[Unit] =
    ioA.map(_ => ())

  def asUnit_v2[A](ioA: IO[A]): IO[Unit] =
    ioA.void

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      lastNumber    <- IO(n)
      previousSum   <- sumIO(n - 1)
    } yield previousSum + lastNumber

  // 7 - write a fibonnaci function that does not crash on recursion
  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO(fibonacci(n - 1)).flatten // flatMap(x => x)
      prev <- IO(fibonacci(n - 2)).flatten
    } yield last + prev

  // basically suspending an effect inside another effect and returning IO of same time
  def fibonacci_v2(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO.defer(fibonacci(n - 1)) // same as delay(..).flatten
      prev <- IO.defer(fibonacci(n - 2))
    } yield last + prev

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    println(aDelayedIO.unsafeRunSync())

//    smallProgram().unsafeRunSync()
//    smallProgramv2().unsafeRunSync()
    // exercises

//    forever_v2(IO{
//      println("forever!")
//      Thread.sleep(1000)
//    }).unsafeRunSync()

//    println(sumIO(20000).unsafeRunSync()) // won't stack overflow
    println(fibonacci_v2(10).unsafeRunSync())
    (1 to 10).foreach(i => println(fibonacci(i).unsafeRunSync())) // gets harder with each computation
  }
}
