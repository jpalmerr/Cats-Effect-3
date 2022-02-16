package james.part2effects

import cats.effect.{IO, IOApp}
import james.utils.*

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  // traverse concept
  import scala.concurrent.ExecutionContext.Implicits.global
  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I like cats effect", "Scala is great", "More stuff")

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  // traverse
  import cats.Traverse
  import cats.instances.list.*
  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    // ^^ this stores all the results in one future
    singleFuture.foreach(println)
  }

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel.*
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO) // === above

  /**
   * exercises
   */

  // 1 - sequence
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(identity)

  // define for any container where traverse is in scope
  def sequence_v2[F[_] : Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(listOfIOs)(x => x) // identity

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  def parSequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(identity)

  // existing sequence api (from traverse typeclass)

  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercises
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence // extension method


  override def run: IO[Unit] =
//    singleIO.void // // all run on same thread
//  singleIO.map(_.sum).debug.void
//  parallelSingleIO.map(_.sum).debug.void // notice on separate threads
  parallelSingleIO_v3.map(_.sum).debug.void
}
