package week4

import week3._;

object List {
  def apply[T](x1: T, x2: T): week3.List[T] = new Cons(x1, new Cons(x2, new Nil));

  def apply[T](x: T): week3.List[T] = new Cons(x, new Nil);

  def apply[T](): week3.List[T] = new Nil;
}
