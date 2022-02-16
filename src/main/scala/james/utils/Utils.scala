package james.utils

import cats.effect.IO

// scala 3 we can just write top level. this is instead of a package object from Scala 2

extension [A](io: IO[A])
  def debug: IO[A] = for {
    a <- io
    t = Thread.currentThread().getName
    _ = println(s"[$t] $a")
  } yield a