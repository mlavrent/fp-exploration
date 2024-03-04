package monads.transformers

import monads.*
import monads.given

// ================ IdentityT ===================

type IdentityT[M[_], A] = M[Identity[A]]

given [M[_]](using Monad[M]): Monad[[A] =>> IdentityT[M, A]] with
  override def pure[A](a: A): IdentityT[M, A] = pure(a)

  override def map[A, B](ma: IdentityT[M, A])(f: A => B): IdentityT[M, B] = map(ma)(f)

  override def flatMap[A, B](ma: IdentityT[M, A])(f: A => IdentityT[M, B]): IdentityT[M, B] =
    flatMap(ma)(f)

// ================ OptionT ===================
// ================ ListT ===================
// ================ ReaderT ===================

// ================ WriterT ===================
// ================ StateT ===================
// ================ ContinuationT ===================
