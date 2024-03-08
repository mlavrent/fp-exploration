package monads

trait Monad[M[_]]:
  def pure[A](a: A): M[A]

  def map[A, B](ma: M[A])(f: A => B): M[B]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def flatten[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

// ================ Identity monad ===================

type Identity[A] = A

given [A]: Monad[Identity] with
  override def pure[A](a: A): A = a

  override def map[A, B](ma: A)(f: A => B): B = f(ma)

  override def flatMap[A, B](ma: A)(f: A => B): B = f(ma)

// ================ Option monad ===================

given Monad[Option] with
  override def pure[A](a: A): Option[A] = Some(a)

  override def map[A, B](ma: Option[A])(f: A => B): Option[B] = ma match
    case Some(a) => Some(f(a))
    case None => None

  override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match
    case Some(a) => f(a)
    case None => None

// ================ List monad ===================

given Monad[List] with
  override def pure[A](a: A): List[A] = a :: Nil

  override def map[A, B](ma: List[A])(f: A => B): List[B] = ma match
    case h :: t => f(h) :: map(t)(f)
    case Nil => Nil

  override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma match
    case h :: t => f(h) ::: flatMap(t)(f)
    case Nil => Nil

// ================ Reader monad ===================

type Reader[E, A] = E => A

given [E]: Monad[[A] =>> Reader[E, A]] with
  override def pure[A](a: A): Reader[E, A] =
    _ => a

  override def map[A, B](ma: Reader[E, A])(f: A => B): Reader[E, B] =
    env => f(ma(env))

  override def flatMap[A, B](ma: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] =
    env => f(ma(env))(env)

// ================ Writer monad ===================

trait Monoid[A]:
  extension (a: A) def <>(b: A): A

  def empty: A

case class Writer[A, L: Monoid](value: A, log: L)

given [L](using monoid: Monoid[L]): Monad[[A] =>> Writer[A, L]] with
  override def pure[A](a: A): Writer[A, L] =
    Writer(a, monoid.empty)

  override def map[A, B](ma: Writer[A, L])(f: A => B): Writer[B, L] =
    Writer(f(ma.value), ma.log)

  override def flatMap[A, B](ma: Writer[A, L])(f: A => Writer[B, L]): Writer[B, L] =
    val mb = f(ma.value)
    Writer(mb.value, ma.log <> mb.log)


// ================ State monad ===================

case class StateAndValue[S, A](state: S, value: A)
type State[S, A] = S => StateAndValue[S, A]

given [S]: Monad[[A] =>> State[S, A]] with
  override def pure[A](a: A): State[S, A] = s => StateAndValue(s, a)

  override def map[A, B](ma: State[S, A])(f: A => B): State[S, B] = s =>
    val stateAndA = ma(s)
    StateAndValue(stateAndA.state, f(stateAndA.value))

  override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = s =>
    var stateAndA = ma(s)
    f(stateAndA.value)(stateAndA.state)

// ================ Continuation monad ===================

type Cont[R, A] = (A => R) => R

given [R]: Monad[[A] =>> Cont[R, A]] with
  override def pure[A](a: A): Cont[R, A] = k => k(a)

  override def map[A, B](ma: Cont[R, A])(f: A => B): Cont[R, B] =
    kb => ma(kb compose f)

  override def flatMap[A, B](ma: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] =
    kb => ma(a => f(a)(kb))

// ================ Selection monad ===================

type Sel[R, A] = (A => R) => A

given [R]: Monad[[A] =>> Sel[R, A]] with
  override def pure[A](a: A): Sel[R, A] = Function.const(a)

  override def map[A, B](ma: Sel[R, A])(f: A => B): Sel[R, B] =
   kb => f(ma(kb compose f))

  override def flatMap[A, B](ma: Sel[R, A])(f: A => Sel[R, B]): Sel[R, B] = ???
