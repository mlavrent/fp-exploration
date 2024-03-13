import monads.*
import monads.given
import scala.compiletime.ops.string

@main def run(): Unit =
  val m = summon[Monad[[A] =>> Cont[Int, A]]]
  val cont = m.flatMap(m.pure(1))(a =>
    m.flatMap(m.pure(2))(b =>
      m.flatMap(m.pure(a + b))(c =>
        m.pure(c))))
  println(evalCont(cont))
