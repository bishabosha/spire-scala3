package spire.algebra.strawman.std

import spire.algebra.strawman.CRig

given as CRig[Int] {
  def zero  = 0
  def one   = 1
  def (x: Int) plus  (y: Int) = x + y
  def (x: Int) times (y: Int) = x * y
}

given as CRig[Long] {
  def zero = 0L
  def one  = 1L
  def (x: Long) plus  (y: Long) = x + y
  def (x: Long) times (y: Long) = x * y
}

given as CRig[Unit] {
  def zero = ()
  def one  = ()
  def (x: Unit) plus  (y: Unit) = ()
  def (x: Unit) times (y: Unit) = ()
}