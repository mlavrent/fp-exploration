package monads.indexed

import monads.*
import monads.given

trait IMonad[M[_, _, _]]:
  def ipure[I, A](a : A): M[I, I, A]

  def imap[I, J, A, B](ma: M[I, J, A])(f: A => B): M[I, J, B]

  def iflatMap[I, J, K, A, B](ma: M[I, J, A])(f: A => M[J, K, B]): M[I, K, B]

given [M[_, _, _], I](using im: IMonad[M]): Monad[[A] =>> M[I, I, A]] with
  override def pure[A](a: A): M[I, I, A] = im.ipure(a)

  override def map[A, B](ma: M[I, I, A])(f: A => B): M[I, I, B] = im.imap(ma)(f)

  override def flatMap[A, B](ma: M[I, I, A])(f: A => M[I, I, B]): M[I, I, B] = im.iflatMap(ma)(f)

// ================ Indexed state monad ===================

type IState[SI, SJ, A] = SI => StateAndValue[SJ, A]

given IMonad[IState] with
  override def ipure[SI, A](a: A): IState[SI, SI, A] = s => StateAndValue(s, a)

  override def imap[SI, SJ, A, B](ma: IState[SI, SJ, A])(f: A => B): IState[SI, SJ, B] = s =>
    val stateAndA = ma(s)
    StateAndValue(stateAndA.state, f(stateAndA.value))

  override def iflatMap[SI, SJ, SK, A, B](ma: IState[SI, SJ, A])(f: A => IState[SJ, SK, B]): IState[SI, SK, B] = s =>
    val stateAndA = ma(s)
    f(stateAndA.value)(stateAndA.state)

type State[S, A] = IState[S, S, A]

// ================ Indexed continuation monad ===================

type ICont[RI, RJ, A] = (A => RJ) => RI

given IMonad[ICont] with
  override def ipure[RI, A](a: A): ICont[RI, RI, A] = ka => ka(a)

  override def imap[RI, RJ, A, B](ma: ICont[RI, RJ, A])(f: A => B): ICont[RI, RJ, B] =
    kb => ma(kb compose f)

  override def iflatMap[RI, RJ, RK, A, B](ma: ICont[RI, RJ, A])(f: A => ICont[RJ, RK, B]): ICont[RI, RK, B] =
    kb => ma(a => f(a)(kb))

type Cont[R, A] = ICont[R, R, A]
