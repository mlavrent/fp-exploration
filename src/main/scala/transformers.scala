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

// ================ ReaderT ===================

type ReaderT[M[_], E, A] = E => M[A]

given [E]: MonadTrans[[M[_], A] =>> ReaderT[M, E, A]] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): ReaderT[M, E, A] = _ => m.map(ma)(identity)

given [M[_], E](using m: Monad[M]): Monad[[A] =>> ReaderT[M, E, A]] with
  override def pure[A](a: A): ReaderT[M, E, A] = _ => m.pure(a)

  override def map[A, B](ma: ReaderT[M, E, A])(f: A => B): ReaderT[M, E, B] = e => m.map(ma(e))(f)

  override def flatMap[A, B](ma: ReaderT[M, E, A])(f: A => ReaderT[M, E, B]): ReaderT[M, E, B] = e => m.flatMap(ma(e))(a => f(a)(e))

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

type StateT[M[_], S, A] = S => M[StateAndValue[S, A]]

given [S]: MonadTrans[[M[_], A] =>> StateT[M, S, A]] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): StateT[M, S, A] = s => m.map(ma)(a => StateAndValue(s, a))

given [M[_], S](using m: Monad[M]): Monad[[A] =>> StateT[M, S, A]] with
  override def pure[A](a: A): StateT[M, S, A] = s => m.pure(StateAndValue(s, a))

  override def map[A, B](ma: StateT[M, S, A])(f: A => B): StateT[M, S, B] = ???

  override def flatMap[A, B](ma: StateT[M, S, A])(f: A => StateT[M, S, B]): StateT[M, S, B] = ???

// ================ ContinuationT ===================

type ContT[M[_], R, A] = (A => M[R]) => M[R]

given [R]: MonadTrans[[M[_], A] =>> ContT[M, R, A]] with
  override def lift[M[_], A](using m: Monad[M])(ma: M[A]): ContT[M, R, A] = ka => m.flatMap(ma)(ka)

given [M[_], R]: Monad[[A] =>> ContT[M, R, A]] with
  override def pure[A](a: A): ContT[M, R, A] = ka => ka(a)

  override def map[A, B](ma: ContT[M, R, A])(f: A => B): ContT[M, R, B] =
    kb => ma(kb compose f)

  override def flatMap[A, B](ma: ContT[M, R, A])(f: A => ContT[M, R, B]): ContT[M, R, B] = ???

type Cont[R, A] = ContT[Identity, R, A]

// ================ SelectT ===================

type SelectT[M[_], R, A] = (A => M[R]) => M[A]

given [R]: MonadTrans[[M[_], A] =>> SelectT[M, R, A]] with
  override def lift[M[_], A](using Monad[M])(ma: M[A]): SelectT[M, R, A] = Function.const(ma)

given [M[_], R]: Monad[[A] =>> SelectT[M, R, A]] with
  override def pure[A](a: A): SelectT[M, R, A] = ???

  override def map[A, B](ma: SelectT[M, R, A])(f: A => B): SelectT[M, R, B] = ???

  override def flatMap[A, B](ma: SelectT[M, R, A])(f: A => SelectT[M, R, B]): SelectT[M, R, B] = ???
