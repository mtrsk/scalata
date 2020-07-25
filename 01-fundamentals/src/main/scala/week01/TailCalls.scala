package Week01

object Factorial extends App {
 def factorial(n: Int): Int = {
   @annotation.tailrec
   def loop(acc: Int, n: Int): Int = {
     if (n == 0) acc
     else loop(acc*n, n-1)
   }

   loop(1, n)
 }

 println( factorial(0) );
 println( factorial(1) );
 println( factorial(2) );
 println( factorial(3) );
 println( factorial(4) );
 println( factorial(5) );
 println( factorial(12) );
}
