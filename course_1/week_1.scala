object session{

	def abs(x: Double ) = if (x >= 0) x else -x	

	def and(x: Boolean, y: => Boolean) = if(x) true else y	
	/* tips	

	 true && obj -> obj
	 false && obj -> false
	 true || obj -> true
	 false || obj -> obj

	 если val - то сначала выполняется потом присваивается

	 => - call by name parameter - не считается если не вызывается	

	 @tailrec - указывается перед функцией с хвостовой рекурсией для оптимизации
	*/	

	//val x = 2
	//val y = square(x) 	
	

	def sqrt(x: Double): Double = {	

		def is_good_enought(guess: Double): Boolean = 
			abs(guess*guess - x) /x < 0.001		

		def improve(guess: Double): Double = 
			(guess + x /guess)/2	
			

		def sqrt_iter(guess: Double): Double =
			if ( is_good_enought(guess) ) guess
			else sqrt_iter(improve(guess))		

		sqrt_iter(1)
	}

	val o_x = 6
	val result = {
		val i_x = 34
		i_x
	} + o_x

	def pascal(c: Int, r: Int): Int = {
		if (c == 0 || r < 2 || c == r) 1
		else pascal(c - 1, r - 1) + pascal(c, r - 1)
	}

	def balance(chars: List[Char]): Boolean = {
		
		def cut_parentheses(chars: List[Char],count: Int): Boolean ={
			(chars, count) match {
				case(head :: tail, count) if (count < 0) => false
				case(head :: tail, count) if (head == '(') => cut_parentheses(tail, count+1)
				case(head :: tail, count) if (head == ')') => cut_parentheses(tail, count-1)
				case(head :: tail, count) if (head != ')' && head != '(' ) => cut_parentheses(tail, count)
				case(Nil, count) if (count == 0) => true
				case(Nil, count) if (count < 0) => false
			}
		}

		cut_parentheses(chars,0)
	}	

  def main(args: Array[String]): Unit = {
    //println(pascal(1,3))
    println(balance(":0))".toList))
  }

}

