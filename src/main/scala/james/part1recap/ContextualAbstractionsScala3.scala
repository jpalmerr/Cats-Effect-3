package james.part1recap

object ContextualAbstractionsScala3 {

  //given/using combo
  def increment(x: Int)(using amount: Int): Int = x + amount
  given defaultAmount: Int = 10
  val twelve = increment(2)

  def multiply(x: Int)(using factor: Int): Int = x * factor
  val aHundred = multiply(10)

  trait Combiner[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  def combineAll[A](values: List[A])(using combiner: Combiner[A]): A =
    values.foldLeft(combiner.empty)(combiner.combine)

  given intCombiner: Combiner[Int] with {
    override def combine(x: Int, y: Int): Int = x + y

    override def empty: Int = 0
  }

  val numbers = (1 to 10).toList
  val sum10 = combineAll(numbers)

  given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with {
    override def empty: Option[T] = Some(combiner.empty)
    override def combine(x: Option[T], y: Option[T]): Option[T] =
      for {
        vx <- x
        vy <- y
      } yield combiner.combine(vx, vy)
  }

  import cats.syntax.all.*
  val sumOptions: Option[Int] = combineAll(List(Some(1), None, Some(2)))
  val sumSomeOptions: Option[Int] = combineAll(List(1.some, 2.some, 3.some)) // note this fails when using Some(1) etc

  // extension methods

  case class Person(name: String) {
    def greet(): String = s"Hi my name is $name"
  }

  extension (name: String)
    def greet :String = Person(name).greet()

  val alicesGreeting = "Alice".greet

  extension [T](list: List[T])
    def reduceAll(using combiner: Combiner[T]): T =
      list.foldLeft(combiner.empty)(combiner.combine)

  val sum10_v2 = numbers.reduceAll

  def main(args: Array[String]): Unit =
    println(s"sumOptions: $sumOptions")
    println(s"sumSomeOptions: $sumSomeOptions")
    println(s"sum10_v2: $sum10_v2")

}

// type classes
object TypeClassesScala3 {
  case class Person(name: String, age: Int)

  // part 1 - Type class defn
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - define type class instances
  given stringSerializer: JSONSerializer[String] with {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  given intSerializer: JSONSerializer[Int] with {
    override def toJson(value: Int): String = value.toString
  }

  given personSerializer: JSONSerializer[Person] with {
    override def toJson(person: Person): String =
      s"""
         |{"name": ${person.name}", "age": ${person.name}}
         |""".stripMargin
  }

  // part 3 - user facing api
  def convertToJSon[T](value: T)(using serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def convertListToJSon[T](list: List[T])(using serializer: JSONSerializer[T]): String = {
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")
  }

  // part 4 - extension methods for the types you want to support
  extension [T](value: T)
    def toJson(using serializer: JSONSerializer[T]): String =
      serializer.toJson(value)

  def main(args: Array[String]): Unit = {
    println(convertListToJSon(List(Person("Alice", 23), Person("Bob", 46))))
    val bob = Person("Bob", 46)
    println(bob.toJson)
  }
}
