enum List[+A]:
  case Cons(head : A, tail : List[A])
  case Nil
