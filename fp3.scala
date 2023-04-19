  // GENERATED
/* INSTRUCTIONS
 *
 * Complete the exercises below.  For each "EXERCISE" comment, add code
 * immediately below the comment.
 *
 * Please see README.md for instructions, including compilation and testing.
 *
 * GRADING
 *
 * 1. Submissions MUST compile using SBT with UNCHANGED configuration and
 *    tests with no compilation errors.  Submissions with compilation errors
 *    will receive 0 points.  Note that refactoring the code will cause the
 *    tests to fail.
 *
 * 2. You MUST NOT edit the SBT configuration and tests.  Altering it in your
 *    submission will result in 0 points for this assignment.
 *
 * 3. You MUST NOT use while loops or (re)assignment to variables (you can
 *    use "val" declarations, but not "var" declarations).  You must use
 *    recursion instead.
 *
 * 4. You may declare auxiliary functions if you like.
 *
 * SUBMISSION
 *
 * 1. Submit this file on D2L before the deadline.
 *
 * 2. Late submissions will not be permitted because solutions will be
 *    discussed in class.
 *
 */

object fp3:

  // EXERCISE 1: complete the following recursive definition of a "member"
  // function to check whether an element "a" is a member of a list of
  // integers "xs".
  //
  // Your implementation of "member" MUST be recursive and not use the
  // builtin "contains" method from the List class.
  //
  // EXAMPLES:
  // - member (5, List (4, 6, 8, 5)) == true
  // - member (3, List (4, 6, 8, 5)) == false
  def member(a: Int, xs: List[Int]): Boolean ={
    // TODO: Provide definition here. return type is boolean so after => must be boolean
    //if a==x return true else false 
	xs match{
		case Nil => false
		case x::xs =>{
			if a==x then true
			else member(a,xs)}
}
}

  // EXERCISE 2: complete the following recursive definition of an "allEqual"
  // function to check whether all elements in a list of integers are equal.
  //
  // EXAMPLES:
  // - allEqual (Nil) == true
  // - allEqual (List (5)) == true
  // - allEqual (List (5, 5, 5)) == true
  // - allEqual (List (6, 5, 5, 5)) == false
  // - allEqual (List (5, 5, 6, 5)) == false
  // - allEqual (List (5, 5, 5, 6)) == false
  def allEqual(xs: List[Int]): Boolean ={
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
	xs match{
	case Nil => true //allEqual (Nil) == true
	case x::Nil => true//- allEqual (List (5)) == true
	case x::xs =>{
		if x==xs.head then allEqual(xs)
		else false
}
}
}

  // EXERCISE 3: complete the definition of the following function that
  // computes the length of each String in a list, and returns the original
  // Strings paired with their length.  For example:
  //
  //   stringLengths (List ("the", "rain")) == List (("the", 3), ("rain", 4))
  //
  // You must not use recursion directly in the function.
  //
  // You can use the "map" method of the List class.
  def stringLengths(xs: List[String]): List[(String, Int)] =xs.map(x=>(x,x.length))
    // TODO: Provide definition here.
 // throw UnsupportedOperationException()

  // EXERCISE 4: complete the function definition for "delete1" that takes
  // an element "x" and a list "ys", then returns the list where any
  // occurrences of "x" in "ys" have been removed.
  //
  // Your definition of "delete1" MUST be recursive.
  //
  // EXAMPLE:
  // - delete1 ("the", List ("the","the","was","a","product","of","the","1980s"))
  //                == List ("was","a","product","of","1980s")
  def delete1[X](x: X, ys: List[X]): List[X] ={
    // TODO: Provide definition here.
//throw UnsupportedOperationException()
	ys match{
	case Nil=>Nil
	case y::ys=>{
	if (x==y) then delete1(y,ys)
	else y::delete1(x,ys)}
}
}
	


  // EXERCISE 5: complete the function definition for "delete2" below.  It
  // must have the same behavior as "delete1".
  //
  // It must be written using "for comprehensions" and not use recursion
  // explicitly.
  def delete2[X](x: X, ys: List[X]): List[X] = for y<-ys if y!= x
	yield y
    // TODO: Provide definition here.
//  throw UnsupportedOperationException()

  // EXERCISE 6: complete the function definition for "delete3" below.  It
  // must have the same behavior as "delete1".
  //
  // It must be written using the builtin "filter" method for Lists and not
  // use recursion explicitly.
  def delete3[X](x: X, ys: List[X]): List[X] =ys.filter(y=>(y!=x))
    // TODO: Provide definition here.
  //  throw UnsupportedOperationException()

  // EXERCISE 7: complete the function definition for "removeDupes1" below.
  // It takes a list as argument, then returns the same list with consecutive
  // duplicate elements compacted to a single element.
  //
  // Duplicate elements that are separated by at least one distinct element
  // should be left alone.
  //
  // EXAMPLE:
  // - removeDupes1 (List (1,1,2,3,3,3,4,4,5,6,7,7,8,9,2,2,2,9))
  //              == List (1,2,3,4,5,6,7,8,9,2,9)
  def removeDupes1[X](xs: List[X]): List[X] ={
    // TODO: Provide definition here.
 //   throw UnsupportedOperationException()
	xs match{
	case Nil =>Nil
	case x :: Nil =>List(x)
	case x :: xs=>{
		if (x ==xs.head) then removeDupes1(xs)
		else x::removeDupes1(xs)
}
}
}




  // EXERCISE 8: write a function "removeDupes2" that behaves like
  // "removeDupes1", but also includes a count of the number of consecutive
  // duplicate elements in the original list (thus producing a simple
  // run-length encoding).  The counts are paired with each element in the
  // output list.
  //
  // EXAMPLE:
  // - removeDupes2 (List (1,1,2,3,3,3,4,4,5,6,7,7,8,9,2,2,2,9))
  //              == List ((2,1),(1,2),(3,3),(2,4),(1,5),(1,6),(2,7),(1,8),(1,9),(3,2),(1,9))
  
  // TODO: Provide definition here.
    
  def countHelper[X](elements:X, xs:List[X], count:Int):List[(Int,X)]={
	xs match{
	case Nil => {//empty list then nil or they don't have duplicate (1, element)
		if (elements!=Nil) then (count, elements)::Nil
		else Nil
}
	case x :: xs => { 
		if (x != elements) then
			(count, elements)::countHelper(x,xs,1)
		else countHelper(x,xs,count+1)
		
}
}
}
	

  def removeDupes2[X](xs: List[X]): List[(Int, X)] ={//throw UnsupportedOperationException()	
	xs match{
	case Nil => Nil
	case x::Nil=>List((1,x))
	//case x::xs => xs.map(x=>(xs.count(_==x),x))--understand wrong way :( 
	case x::xs => countHelper(x,xs,1)
}
}
    



  // EXERCISE 9: complete the following definition of a function that splits
  // a list into a pair of two lists.  The offset for the the split position
  // is given by the Int argument.
  //
  // The behavior is determined by:
  //
  //   for all n, xs:
  //     splitAt (n, xs) == (xs.take(n), xs.drop(n))
  //
  // (Treat negative n like 0)
  //
  // Some examples:
  // splitAt(0,List(        31,41,51)) --> (List(       ), List(31,41,51))
  // splitAt(1,List(     21,31,41,51)) --> (List(     21), List(31,41,51))
  // splitAt(2,List(  11,21,31,41,51)) --> (List(  11,21), List(31,41,51))
  // splitAt(3,List(1,11,21,31,41,51)) --> (List(1,11,21), List(31,41,51))
  //
  // splitAt(3,     List())  --> (List(     ), List())
  // splitAt(4,   List(51))  --> (List(   51), List())
  // splitAt(5,List(41,51))  --> (List(41,51), List())
  //
  // Your definition of "splitAt" must be recursive and must not use "take"
  // or "drop".
  //
  // Your definition of "splitAt" must only travere the list once.  So you
  // cannot define your own versions of "take"/"drop" and use them (because
  // that would entail one traversal of the list with "take" and then a
  // second traversal with "drop").
  //

  def splitAt[X](n: Int, xs: List[X]): (List[X], List[X]) ={
    // TODO: Provide definition here.
    throw UnsupportedOperationException()

}


  // EXERCISE 10: complete the following definition of an "allDistinct"
  // function that checks whether all values in list are distinct.  You
  // should use your "member" function defined earlier.
  //
  // Your implementation must be recursive.
  //
  // EXAMPLE:
  // - allDistinct (Nil) == true
  // - allDistinct (List (1,2,3,4,5)) == true
  // - allDistinct (List (1,2,3,4,5,1)) == false
  // - allDistinct (List (1,2,3,2,4,5)) == false
  def allDistinct(xs: List[Int]): Boolean ={
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
	xs match{
		case Nil => true
		case x::xs =>{
			if (member(x,xs)) then false
			else allDistinct(xs)
}
}
}