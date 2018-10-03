package pl.class05

import scala.annotation.tailrec

object Fac {
  
  def fac(n: Int): Int = 
    if (n == 0) 1 else n * fac(n - 1)

  def fac_loop(n0: Int): Int = {
    var acc = 1
    var n = n0
    
    while (n != 0) {
      acc = n * acc
      n = n - 1
    }
    
    acc
  }
  
  def fac_tr(n: Int, acc: Int): Int =
    if (n == 0) acc else fac_tr(n - 1, n * acc)
  
  def fac_tr(n: Int): Int = fac_tr(n, 1)

}
