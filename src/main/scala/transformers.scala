package monads.transformers

import monads.*
import monads.given

trait MonadTrans[T[_[_], _]]:
  def lift[M[_], A](ma: M[A]): T[M, A]

// ================ IdentityT ===================

type IdentityT[M[_], A] = M[Identity[A]]

given MonadTrans[IdentityT] with
  override def lift[M[_], A](ma: M[A]): IdentityT[M, A] = ma

// ================ OptionT ===================


// ================ ListT ===================
// ================ ReaderT ===================

// ================ WriterT ===================
// ================ StateT ===================
// ================ ContinuationT ===================
