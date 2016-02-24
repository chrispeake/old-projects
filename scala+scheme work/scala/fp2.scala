// GENERATED

/* INSTRUCTIONS
 *
 * Complete the exercises below.  For each "EXERCISE" comment, add
 * code immediately below the comment.
 *
 * Please see README.md for instructions, including compilation and testing.
 * 
 * GRADING
 * 
 * 1. Submissions MUST compile using SBT with UNCHANGED configuration and tests with no
 *    compilation errors.  Submissions with compilation errors will receive 0 points.
 *    Note that refactoring the code will cause the tests to fail.
 *
 * 2. You MUST NOT edit the SBT configuration and tests.  Altering it in your submission will
 *    result in 0 points for this assignment.
 *
 * 3. You MUST NOT use while loops or (re)assignment to variables (you can use "val" declarations,
 *    but not "var" declarations).  You must use recursion instead.
 *
 * 4. You may declare auxiliary functions if you like.
 *
 * SUBMISSION
 *
 * 1. Push your local repository to the repository created for you on Bitbucket before the deadline.
 *
 * 2. Late submissions will not be permitted because solutions will be discussed in class.
 * 
 */

object fp2 {

  // EXERCISE 1: complete the following recursive definition of a "map" function
  // for Scala's builtin List type.  You must not use the builtin "map" method.
  // Your implementation of "map" MUST be recursive.
  def map [A,B] (xs:List[A], f:A=>B) : List[B] = {
    // TODO: Provide definition here.
    xs match {
		case Nil	=> Nil
		case y::ys	=> f(y) :: map(ys, f)
	}
  }

  // EXERCISE 2: complete the following recursive definition of a "filter" function
  // for Scala's builtin List type.  You must not use the builtin "filter" method.
  // Your implementation of "filter" MUST be recursive.
  def filter [A] (xs:List[A], f:A=>Boolean) : List[A] = {
    // TODO: Provide definition here.
    xs match {
		case Nil				=> Nil
		case y::ys	if ( f(y) )	=> y :: filter(ys, f)
		case y::ys				=> filter(ys, f)
	}
  }

  // EXERCISE 3: complete the following recursive definition of an "append" function
  // for Scala's builtin List type.  You must not use the builtin ":::" method.
  // Your implementation of "append" MUST be recursive.
  // HINT: use "::" in the body of the cons-cell case.
  def append [A] (xs:List[A], ys:List[A]) : List[A] = {
    // TODO: Provide definition here.
    xs match {
		case Nil	=> ys
		case z::zs	=> z :: append(zs, ys)
	}
  }

  // EXERCISE 4: complete the following recursive definition of a "flatten" function
  // for Scala's builtin List type.  You must not use the builtin "flatten" method.
  // Your implementation of "flatten" MUST be recursive.
  // HINT: use either ":::" or your definition of "append" in the body of the cons-cell case.
  // EXAMPLE:
  // - flatten (List ((1 to 5).toList, (6 to 10).toList, (11 to 15).toList)) == (1 to 15).toList
  def flatten [A] (xss:List[List[A]]) : List[A] = {
    // TODO: Provide definition here.
    xss match {
		case Nil	=> Nil
		case y::ys	=> append (y, flatten(ys))
		//case y::ys	=> y ::: flatten(ys)
	}
  }

  // Exercise 5: complete the following recursive definition of a "foldLeft" function
  // for Scala's builtin list type.  You must not use the builtin "foldLeft" method.
  // Your implementation of "foldLeft" MUST be recursive.
  // HINT:   foldLeft (  Nil, e, f) == e
  //         foldLeft (y::ys, e, f) == foldLeft (ys, f (e, y), f)
  def foldLeft [A,B] (xs:List[A], e:B, f:(B,A)=>B) : B = {
    // TODO: Provide definition here.
    xs match {
		case Nil	=> e
		case y::ys	=> foldLeft(ys, f(e, y), f)
	}
  }

  // Exercise 6: complete the following recursive definition of a "foldRight" function
  // for Scala's builtin list type.  You must not use the builtin "foldRight" method.
  // Your implementation of "foldRight" MUST be recursive.
  // HINT:   foldRight (  Nil, e, f) == e
  //         foldRight (y::ys, e, f) == f (y, foldRight (ys, e, f))
  def foldRight [A,B] (xs:List[A], e:B, f:(A,B)=>B) : B = {
    // TODO: Provide definition here.
    xs match {
		case Nil	=> e
		case y::ys	=> f(y, foldRight(ys, e, f))
	}
  }

  // Exercise 7: complete the following definition of a "joinTerminateLeft" function
  // to take a list of strings "xs" and concatenate all strings using a string "term"
  // as a terminator (not delimiter) between strings.  You MUST use your foldLeft defined above.
  // EXAMPLES: 
  // - joinTerminateLeft (Nil, ";") == ""
  // - joinTerminateLeft (List ("a","b","c","d"), ";") == "a;b;c;d;"
  def joinTerminateLeft (xs : List[String], term : String) : String = {
    // TODO: Provide definition here.
	def f (s:String, t:String) : String = s + t + term
    xs match {
		case Nil	=> ""
		case y::ys	=> foldLeft(ys, f(y, ""), f)
	}
  }

  // Exercise 8: complete the following definition of a "joinTerminateRight" function
  // to take a list of strings "xs" and concatenate all strings using a string "term"
  // as a terminator (not delimiter) between strings.  You MUST use your foldRight defined above.
  // EXAMPLES: 
  // - joinTerminateRight (Nil, ";") == ""
  // - joinTerminateRight (List ("a","b","c","d"), ";") == "a;b;c;d;"
  def joinTerminateRight (xs : List[String], delimiter : String) : String = {
    // TODO: Provide definition here.
	def f (s:String, t:String) : String = delimiter + s + t
    xs match {
		case Nil	=> ""
		case y::ys	=> y + foldRight(ys, delimiter, f)
	}
  }

  // Exercise 9: complete the following recursive definition of a "firstNumGreaterThan" function
  // to find the first number greater than or equal to "a" in a list of integers "xs".
  // If the list is empty or there is no number greater than or equal to "a", throw a RuntimeException (with no argument).
  // Your implementation of "firstNumGreaterThan" MUST be recursive.
  // EXAMPLES:
  // - firstNumGreaterThan (5, List (4, 6, 8, 5)) == 6
  def firstNumGreaterThan (a : Int, xs : List[Int]) : Int = {
    // TODO: Provide definition here.
    xs match {
		case Nil				=> throw new RuntimeException
		case y::ys	if (y >= a)	=> y
		case y::ys				=> firstNumGreaterThan(a, ys)
	}
  }
  
  // Exercise 10: complete the following recursive definition of a "firstIndexNumGreaterThan" function
  // to find the index (position) of the first number greater than or equal to "a" in a list of integers "xs".
  // If the list is empty or there is no number greater than or equal to "a", throw a RuntimeException (with no argument).
  // The first index should be zero (not one).
  // Your implementation of "member" MUST be recursive.
  // EXAMPLES:
  // - firstIndexNumGreaterThan (5, List (4, 6, 8, 5)) == 1
  // HINT: this is a bit easier to write if you use an auxiliary function.
  def firstIndexNumGreaterThan (a : Int, xs : List[Int]) : Int = {
    // TODO: Provide definition here.
	def aux (a : Int, xs : List[Int], i : Int) : Int = {
		xs match {
			case Nil				=> throw new RuntimeException
			case y::ys	if (y >= a)	=> i
			case y::ys				=> aux(a, ys, i+1)
		}
	}
    aux( a, xs, 0)
  }

  // Exercise 11: complete the following recursive definition of a "member" function 
  // to check whether an element "a" is a member of a list of integers "xs".
  // Your implementation of "member" MUST be recursive and not use the builtin "contains" method from the List class.
  // EXAMPLES: 
  // - member (5, List (4, 6, 8, 5)) == true
  // - member (3, List (4, 6, 8, 5)) == false
  def member (a : Int, xs : List[Int]) : Boolean = {
    // TODO: Provide definition here.
    xs match {
		case Nil				=> false
		case y::ys	if (y == a)	=> true
		case y::ys				=> member(a, ys)
	}
  }

  // Exercise 12: complete the following recursive definition of an "allEqual" function
  // to check whether all elements in a list of integers are equal.
  // EXAMPLES:
  // - allEqual (Nil) == true
  // - allEqual (List (5)) == true
  // - allEqual (List (5, 5, 5)) == true
  // - allEqual (List (6, 5, 5, 5)) == false
  // - allEqual (List (5, 5, 6, 5)) == false
  // - allEqual (List (5, 5, 5, 6)) == false
  def allEqual (xs : List[Int]) : Boolean = {
    // TODO: Provide definition here.
    xs match {
		case Nil	=> true
		case y::Nil	=> true
		case y::ys	=> 
			if (y != ys.head)
				false
			else
				allEqual(ys)
	}
  }

  // EXERCISE 13: complete the definition of the following function that computes the length of
  // each String in a list, and returns the original Strings paired with their length.
  // For example:
  // 
  //   stringLengths (List ("the", "rain")) == List (("the", 3), ("rain", 4))
  //
  // You must not use recursion directly in the function.  You can use the "map" method 
  // of the List class.
  def stringLengths (xs:List[String]) : List[(String, Int)] = {
    // TODO: Provide definition here.
    xs match {
		case Nil	=> Nil
		case y::ys	=> (y, y.length) :: ys.map( z => (z, z.length) )
	}
  }

  // EXERCISE 14: complete the function definition for "delete1" that takes
  // an element "x" and a list "ys", then returns the list where any
  // occurrences of "x" in "ys" have been removed.  Your definition of
  // "delete1" MUST be recursive.
  // EXAMPLE:
  // - delete1 ("the", List ("the","the","was","a","product","of","the","1980s")) == List ("was","a","product","of","1980s")
  def delete1 [X] (x:X, ys:List[X]) : List[X] = {
    // TODO: Provide definition here.
    ys match {
		case Nil				=> Nil
		case z::zs	if (z == x)	=> delete1(x, zs)
		case z::zs				=> z :: delete1(x, zs)
	}
  }

  // EXERCISE 15: complete the function definition for "delete2" below.  It must 
  // have the same behavior as "delete1".  It must be written using "for comprehensions" 
  // and not use recursion explicitly.
  def delete2 [X] (x:X, ys:List[X]) : List[X] = {
    // TODO: Provide definition here.
    for (y <- ys; if y != x) yield y
  }

  // EXERCISE 16: complete the function definition for "delete3" below.  It must 
  // have the same behavior as "delete1".  It must be written using the
  // builtin "filter" method for Lists and not use recursion explicitly.
  def delete3 [X] (x:X, ys:List[X]) : List[X] = {
    // TODO: Provide definition here.
	def f (y:X) : Boolean = y != x
    filter(ys, f)
  }

  // EXERCISE 17: complete the function definition for "removeDupes1" below.
  // It takes a list as argument, then returns the same list with
  // consecutive duplicate elements compacted to a single element.  
  // Duplicate elements that are separated by at least one distinct
  // element should be left alone.
  // EXAMPLE:
  // - removeDupes1 (List (1,1,2,3,3,3,4,4,5,6,7,7,8,9,2,2,2,9)) == List (1,2,3,4,5,6,7,8,9,2,9)
  def removeDupes1 [X] (xs:List[X]) : List[X] = {
    // TODO: Provide definition here.
	def aux (prev : X, ys : List[X]) : List[X] = {
		ys match {
			case Nil					=> Nil
			case z::zs	if (z == prev)	=> aux (prev, zs)
			case z::zs					=> z :: aux (z, zs)
		}
	}
    xs match {
		case Nil	=> Nil
		case y::ys	=> y :: aux(y, ys) 
	}
  }


  // EXERCISE 18: write a function "removeDupes2" that behaves like "removeDupes1",
  // but also includes a count of the number of consecutive duplicate
  // elements in the original list (thus producing a simple run-length
  // encoding).  The counts are paired with each element in the output
  // list.
  // EXAMPLE:
  // - removeDupes2 (List (1,1,2,3,3,3,4,4,5,6,7,7,8,9,2,2,2,9)) == List ((2,1),(1,2),(3,3),(2,4),(1,5),(1,6),(2,7),(1,8),(1,9),(3,2),(1,9))
  def removeDupes2 [X] (xs:List[X]) : List[(Int, X)] = {
    // TODO: Provide definition here.
	def aux (prev : X, ys : List[X], cnt : Int) : List[(Int, X)] = {
		ys match {
			case Nil					=> List((cnt, prev))
			case z::zs	if (z == prev)	=> aux (prev, zs, cnt + 1)
			case z::zs					=> (cnt, prev) :: aux (z, zs, 1)
		}
	}
    xs match {
		case Nil	=> Nil
		case y::ys	=> aux(y, ys, 1) 
	}
  }


  // EXERCISE 19: complete the following definition of a function that splits a list
  // into a pair of two lists.  The offset for the the split position is given
  // by the Int argument.
  // The behavior is determined by:
  //
  //   for all n, xs:
  //     splitAt (n, xs) == (take (n, xs), drop (n, xs))   
  //
  // Your definition of "splitAt" must be recursive and must not use "take" or "drop".
  //
  // Your definition of "splitAt" must only travere the list once.  So
  // you cannot define your own versions of "take"/"drop" and use them
  // (because that would entail one traversal of the list with "take"
  // and then a second traversal with "drop").
  def splitAt [X] (n:Int, xs:List[X]) : (List[X], List[X]) = {
    // TODO: Provide definition here.
	def aux (left:List[X], xs:List[X], n:Int) : (List[X], List[X]) = {
	  xs match {
		case Nil				=>     (left, Nil)
		case y::ys if (n <= 0)	=> 	   (left, y::ys) 
		case y::ys				=> aux (left ::: List(y), ys, n - 1) 
	  }
	}
	aux(Nil, xs, n)
  }

  // EXERCISE 20: complete the following definition of an "allDistinct" function that checks
  // whether all values in list are distinct.  You should use your "member" function defined earlier.
  // Your implementation must be recursive.
  // EXAMPLE:
  // - allDistinct (Nil) == true
  // - allDistinct (List (1,2,3,4,5)) == true
  // - allDistinct (List (1,2,3,4,5,1)) == false
  // - allDistinct (List (1,2,3,2,4,5)) == false
  def allDistinct (xs : List[Int]) : Boolean = {
    // TODO: Provide definition here.
    xs match {
		case Nil					=> true
		case y::ys if member(y, ys)	=> false
		case y::ys					=> allDistinct(ys)
	}
  }
}

