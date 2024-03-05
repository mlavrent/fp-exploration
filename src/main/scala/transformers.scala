package monads.transformers

import monads.*
import monads.given

trait MonadTrans[T[_[_], _]]:
  def lift[M[_], A](ma: M[A]): T[M, A]

// ================ IdentityT ===================

type IdentityT[M[_], A] = M[A]

given MonadTrans[IdentityT] with
  override def lift[M[_], A](ma: M[A]): IdentityT[M, A] = ma

given [M[_]](using Monad[M]): Monad[[A] =>> IdentityT[M, A]] with
  override def pure[A](a: A): IdentityT[M, A] = pure(a)

  override def map[A, B](ma: IdentityT[M, A])(f: A => B): IdentityT[M, B] = map(ma)(f)

  override def flatMap[A, B](ma: IdentityT[M, A])(f: A => IdentityT[M, B]): IdentityT[M, B] = flatMap(ma)(f)

// ================ OptionT ===================

type OptionT[M[_], A] = M[Option[A]]

given MonadTrans[OptionT] with
  override def lift[M[_], A](ma: M[A]): OptionT[M, A] = ???

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
  override def lift[M[_], A](ma: M[A]): ListT[M, A] = ???

given [M[_]](using m: Monad[M]): Monad[[A] =>> ListT[M, A]] with
  override def pure[A](a: A): ListT[M, A] = m.pure(a :: Nil)

  override def map[A, B](ma: ListT[M, A])(f: A => B): ListT[M, B] =
    m.map(ma)(_.map(f))

  override def flatMap[A, B](ma: ListT[M, A])(f: A => ListT[M, B]): ListT[M, B] =
    m.flatMap(ma)(???)

// ================ ReaderT ===================

// ================ WriterT ===================
// ================ StateT ===================
// ================ ContinuationT ===================
