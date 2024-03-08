package monads.transformers

import monads.*
import monads.given

trait MonadTrans[T[_[_], _]]:
  def lift[M[_], A](using Monad[M])(ma: M[A]): T[M, A]

// ================ IdentityT ===================

type IdentityT[M[_], A] = M[A]

given MonadTrans[IdentityT] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): IdentityT[M, A] = ma

given [M[_]](using Monad[M]): Monad[[A] =>> IdentityT[M, A]] with
  override def pure[A](a: A): IdentityT[M, A] = pure(a)

  override def map[A, B](ma: IdentityT[M, A])(f: A => B): IdentityT[M, B] = map(ma)(f)

  override def flatMap[A, B](ma: IdentityT[M, A])(f: A => IdentityT[M, B]): IdentityT[M, B] = flatMap(ma)(f)

type Identity[A] = A

// ================ OptionT ===================

type OptionT[M[_], A] = M[Option[A]]

given MonadTrans[OptionT] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): OptionT[M, A] = m.map(ma)(Some(_))

given [M[_]](using m: Monad[M]): Monad[[A] =>> OptionT[M, A]] with
  override def pure[A](a: A): OptionT[M, A] = m.pure(Some(a))

  override def map[A, B](ma: OptionT[M, A])(f: A => B): OptionT[M, B] =
    m.map(ma)(oa =>
      oa match
        case Some(a) => Some(f(a))
        case None => None)

  override def flatMap[A, B](ma: OptionT[M, A])(f: A => OptionT[M, B]): OptionT[M, B] =
    m.flatMap(ma)(oa =>
      oa match
        case Some(a) => f(a)
        case None => m.pure(None))

// ================ ListT ===================

type ListT[M[_], A] = M[List[A]]

given MonadTrans[ListT] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): ListT[M, A] = m.map(ma)(_ :: Nil)

given [M[_]](using m: Monad[M]): Monad[[A] =>> ListT[M, A]] with
  override def pure[A](a: A): ListT[M, A] = m.pure(a :: Nil)

  override def map[A, B](ma: ListT[M, A])(f: A => B): ListT[M, B] =
    m.map(ma)(_.map(f))

  override def flatMap[A, B](ma: ListT[M, A])(f: A => ListT[M, B]): ListT[M, B] =
    m.flatMap(ma)(la =>
      la match
        case h :: t => f(h) // TODO: this isn't right, fix this
        case Nil => m.pure(Nil))

// ================ ReaderT ===================

type ReaderT[M[_], E, A] = M[E => A]

given [E]: MonadTrans[[M[_], A] =>> ReaderT[M, E, A]] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): ReaderT[M, E, A] = m.map(ma)(Function.const)

given [M[_], E](using m: Monad[M]): Monad[[A] =>> ReaderT[M, E, A]] with
  override def pure[A](a: A): ReaderT[M, E, A] = m.pure(Function.const(a))

  override def map[A, B](ma: ReaderT[M, E, A])(f: A => B): ReaderT[M, E, B] =
    m.map(ma)(f.compose)

  override def flatMap[A, B](ma: ReaderT[M, E, A])(f: A => ReaderT[M, E, B]): ReaderT[M, E, B] =
    m.flatMap(ma)(ra => f(1)) // TODO: this is wrong, fix this

type Reader[E, A] = ReaderT[Identity, E, A]

// ================ WriterT ===================

type WriterT[M[_], L, A] = M[Writer[A, L]]

given [L](using monoid: Monoid[L]): MonadTrans[[M[_], A] =>> WriterT[M, L, A]] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): WriterT[M, L, A] = m.map(ma)(a => Writer(a, monoid.empty))

given [M[_], L](using m: Monad[M])(using Monoid[L])(using mt: MonadTrans[[M[_], A] =>> WriterT[M, L, A]]): Monad[[A] =>> WriterT[M, L, A]] with
  override def pure[A](a: A): WriterT[M, L, A] = mt.lift(m.pure(a))

  override def map[A, B](ma: WriterT[M, L, A])(f: A => B): WriterT[M, L, B] =
    m.map(ma)(wa => Writer(f(wa.value), wa.log))

  override def flatMap[A, B](ma: WriterT[M, L, A])(f: A => WriterT[M, L, B]): WriterT[M, L, B] = ???

// ================ StateT ===================

type StateT[M[_], S, A] = M[State[S, A]]

given [S]: MonadTrans[[M[_], A] =>> StateT[M, S, A]] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): StateT[M, S, A] = m.map(ma)(a => s => StateAndValue(s, a))

given [M[_], S](using m: Monad[M]): Monad[[A] =>> StateT[M, S, A]] with
  override def pure[A](a: A): StateT[M, S, A] = ???

  override def map[A, B](ma: StateT[M, S, A])(f: A => B): StateT[M, S, B] = ???

  override def flatMap[A, B](ma: StateT[M, S, A])(f: A => StateT[M, S, B]): StateT[M, S, B] = ???

// ================ ContinuationT ===================
