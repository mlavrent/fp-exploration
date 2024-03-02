sealed trait Functor[F[_]]:
  extension[A] (fa : F[A]) def map[B](f : A => B) : F[B]

// ================ Catamorphisms ===================

type Algebra[F[_], A] = F[A] => A

final case class Fix[F[_]](unfix : F[Fix[F]]):
  override def toString(): String = this.unfix.toString()

def cata[F[_], A](using Functor[F])(alg : Algebra[F, A]) : Fix[F] => A =
  f => alg(f.unfix.map(cata(alg)))

// ================ Anamorphisms ===================

type Coalgebra[F[_], A] = A => F[A]

def ana[F[_], A](using Functor[F])(coalg : Coalgebra[F, A]) : A => Fix[F] =
  a => Fix(coalg(a).map(ana(coalg)))

// ================ Hylomorphisms ===================

def hylo[F[_], A](using Functor[F])(coalg : Coalgebra[F, A], alg : Algebra[F, A]) : A => A =
  a => alg(coalg(a).map(hylo(coalg, alg)))

// ================ Expr example (cata) ===================

enum ExprF[A]:
  case Add(l : A, r : A)
  case Multiply(l : A, r : A)
  case Const(v : Int)

given Functor[ExprF] with
  extension[A] (fa : ExprF[A]) def map[B](f : A => B) : ExprF[B] =
    fa match
      case ExprF.Add(l : A, r : A) => ExprF.Add(f(l), f(r))
      case ExprF.Multiply(l : A, r : A) => ExprF.Multiply(f(l), f(r))
      case ExprF.Const(v) => ExprF.Const(v)

def intExprAlgebra : Algebra[ExprF, Int] = e => e match
  case ExprF.Add(l, r) => l + r
  case ExprF.Multiply(l, r) => l * r
  case ExprF.Const(v) => v

def evalIntExpr : Fix[ExprF] => Int =
  cata[ExprF, Int](intExprAlgebra)

val myExpr = Fix(ExprF.Multiply(
  Fix(ExprF.Add(Fix(ExprF.Const(1)), Fix(ExprF.Const(2)))),
  Fix(ExprF.Const(3))))

// ================ List example (ana) ===================

enum ChainF[+E, A]:
  case Link(h : E, t : A)
  case Empty()

  override def toString(): String = this match
    case Link(h, t) => s"$h :: $t"
    case Empty() => "()"


given chainFAsFunctor[E]: Functor[[A] =>> ChainF[E, A]] with
  extension[A] (fa : ChainF[E, A]) def map[B](f : A => B) : ChainF[E, B] =
    fa match
      case ChainF.Link(h : E, t : A) => ChainF.Link(h, f(t))
      case ChainF.Empty() => ChainF.Empty()

def sumChainAlgebra : Algebra[[A] =>> ChainF[Int, A], Int] = c => c match
  case ChainF.Link(h, t) => h + t
  case ChainF.Empty() => 0

def sumChain : Fix[[A] =>> ChainF[Int, A]] => Int =
  cata[[A] =>> ChainF[Int, A], Int](sumChainAlgebra)

val myChain = Fix(ChainF.Link(5, Fix(ChainF.Link(4, Fix(ChainF.Link(3, Fix(ChainF.Empty())))))))

def halveCoalgebra : Coalgebra[[A] =>> ChainF[Int, A], Int] = i =>
  if i < 1 then
    ChainF.Empty()
  else
    ChainF.Link(i, i / 2)

def div2Chain : Int => Fix[[A] =>> ChainF[Int, A]] =
  ana[[A] =>> ChainF[Int, A], Int](halveCoalgebra)

// ================ Factorial example (hylo) ===================

def decrementCoalgebra : Coalgebra[[A] =>> ChainF[Int, A], Int] = i =>
  if i < 1 then
    ChainF.Empty()
  else
    ChainF.Link(i, i - 1)

def prodAlgebra : Algebra[[A] =>> ChainF[Int, A], Int] = l => l match
  case ChainF.Link(h, t) => h * t
  case ChainF.Empty() => 1

def factorial(n : Int) : Int = hylo[[A] =>> ChainF[Int, A], Int](decrementCoalgebra, prodAlgebra)(n)
