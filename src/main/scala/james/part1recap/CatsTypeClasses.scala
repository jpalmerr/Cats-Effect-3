package james.part1recap

object CatsTypeClasses {

  /*
  - applicative
  - functor
  - flatMap
  - monad
  - apply
  - applicativeError/monadError
  - traverse
  */

  // functor - "mappable" data structures

  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*

  val listFunctor = Functor[List]

  // generalize "mapping" APIs
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.*
  def increment_v2[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ + 1)

  // applicatives - ability to "wrap" types
  trait MyApplicative[F[_]] extends MyFunctor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList: List[Int] = applicativeList.pure(43)

  import cats.syntax.applicative.* // import pure extension method

  val aSimpleList_v2: List[Int] = 43.pure[List]

  // flatMap - ability to chain multiple computations
  trait MyFlatMap[F[_]] extends MyFunctor[F] {
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }
  // extends Functor => we get map

  import cats.FlatMap // mainly used for its ext method
  val flatMapList = FlatMap[List]
  import cats.syntax.flatMap.* // flatMap extension method
  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  // monad - combination of applicative + flatMap
  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] =
      flatMap(initialValue)(a => pure(f(a)))
  }

  import cats.Monad
  val monadList = Monad[List]
  def crossProduct_v2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // applicativeError
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val appErrorForEither = ApplicativeError[ErrorOr, String]
  val desireableValue: ErrorOr[Int] = appErrorForEither.pure(42)
  val failedValue: ErrorOr[Int] = appErrorForEither.raiseError("Something failed")
  // since these are same types we can combine them

  import cats.syntax.applicativeError.* // raiseError method extension
  val failedValue_v2: ErrorOr[Int] = "Something failed".raiseError[ErrorOr, Int]

  // monadError

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]
  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]

  // traverse
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }

  // use case for traverse: turn nested wrappers inside out
  import cats.syntax.all.* // .some
  val listOfOptions: List[Option[Int]] = List(1.some, 2.some, 3.some)
  import cats.Traverse
  val listTraverse = Traverse[List]
  val optionList: Option[List[Int]] = listTraverse.traverse(List(1, 2, 3))(x => Option(x))

  import cats.syntax.traverse.*
  val optionList_v2: Option[List[Int]] = List(1,2,3).traverse(x => Option(x))

  def main(args: Array[String]): Unit = {

  }
}
