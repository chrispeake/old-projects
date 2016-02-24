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
 * 3. You MAY use (re)assignment to variables and "val" and "var" declarations.
 *
 *
 * SUBMISSION
 *
 * 1. Push your local repository to the repository created for you on Bitbucket before the deadline.
 *
 * 2. Late submissions will not be permitted because solutions will be discussed in class.
 * 
 * BACKGROUND INFORMATION
 * 
 * Review the Hindley-Milner algorithm in the Mitchell textbook (p135, section 6.3) and the implementation at:
 * 
 *   http://fpl.cs.depaul.edu/cpitcher/courses/csc347/examples/hindley-milner/
 * 
 * Remember that you can download a ZIP archive of all examples including the one above from:
 *
 *   http://fpl.cs.depaul.edu/cpitcher/courses/csc347/examples.zip
 *
 * TASK
 *
 * For this assignment, you must convert the entire Hindley-Milner implementation (seen above) from Java to Scala.
 * A skeleton implementation is provided below.  When you have completed the exercises below and passed the unit
 * tests, your implementation is complete.
 * 
 * The Scala code you write MUST be idiomatic (the skeleton implementation already is):
 *
 * a. You MUST use the builtin Scala immutable lists and maps rather than the ones in the "util" subdirectory.
 *
 * b. You MUST use pattern matching in inferType instead of instanceof and downcasting.
 *
 * c. You MUST use recursion instead of while loops.
 */

object hm {
  import scala.collection.Map

  class Fresh (prefix:String) {
    var counter = -1
    // EXERCISE 1: the getNext method should return a new (fresh) name of the form 'prefix + counter' each time it is called.
    // The counter should be incremented after each call to getNext.
    // For example, if prefix is "A", calling getNext three times should return "A0", "A1", "A2" respectively.
    def getNext () : String = {
      // TODO: Complete the definition.
	  counter += 1
	  prefix + counter
    }
  }

  trait Exp
  case class  ExpApply      (operator:Exp, operand:Exp)                           extends Exp
  case class  ExpCons       (head:Exp, tail:Exp)                                  extends Exp
  case class  ExpFun        (functionName:String, parameterName:String, body:Exp) extends Exp
  case class  ExpIfThenElse (condition:Exp, trueBranch:Exp, falseBranch:Exp)      extends Exp
  case class  ExpInt        (value:Int)                                           extends Exp
  case class  ExpLet        (varName:String, exp1:Exp, exp2:Exp)                  extends Exp
  case object ExpNil                                                              extends Exp
  case class  ExpString     (value:String)                                        extends Exp
  case class  ExpVar        (name:String)                                         extends Exp

  trait Type
  case class TypeBase       (name:String)                                         extends Type
  case class TypeFun        (ty1:Type, ty2:Type)                                  extends Type
  case class TypeList       (ty:Type)                                             extends Type
  case class TypeVar        (name:String)                                         extends Type

  case class TypeScheme     (boundVars:List[String], ty:Type)

  // EXERCISE 2: the substitute function replaces occurrences of TypeVar inside the argument 'ty'.  
  // If 'ty' contains an occurrence of a TypeVar, and the TypeVar appears as a key in the Map 'subst', 
  // then the TypeVar occurrence should be replaced with the value for that key.
  def substitute (ty:Type, subst:Map[String,Type]) : Type = {
    // TODO: Complete the definition.
	ty match {
		case TypeBase (s) 		 => subst.getOrElse(s,ty)
		case TypeFun  (ty1, ty2) => TypeFun (substitute (ty1, subst), substitute (ty2, subst))
		case TypeList (ty1)		 => TypeList(substitute (ty1, subst)) 
		case TypeVar  (s) 		 => subst.getOrElse(s,ty)
	}
  }

  // EXERCISE 3: the instantiateWithFreshTypeVariables function replaces TypeVar occurrences
  // in tysch.ty with fresh type variables (but only if those type variables occur in tysch.boundVars).
  // You should create a new substitution Map that replaces every name from tysch.boundVars with a fresh type variable,
  // then use the 'substitute' function from above with the map.
  // For example, if
  //
  //   val fresh : Fresh = new Fresh ("A")
  //   val bvs : List[String] = List ("X", "Y")
  //   val t1 : Type = TypeFun (TypeFun (TypeVar ("X"), TypeVar ("Y")), TypeFun (TypeVar ("Z"), TypeVar ("X")))
  //   val t2 : Type = TypeFun (TypeFun (TypeVar ("A0"), TypeVar ("A1")), TypeFun (TypeVar ("Z"), TypeVar ("A0")))
  // 
  // then
  //
  //   instantiateWithFreshTypeVariables (fresh, TypeScheme (bvs, t1)) == t2
  //
  def instantiateWithFreshTypeVariables (fresh:Fresh, tysch:TypeScheme) : Type = { 
    // TODO: Complete the definition.
    var subst = Map[String, TypeVar]()
	subst = aux(fresh, tysch)
	
	def aux (fr:Fresh, TySch:TypeScheme) : Map[String, TypeVar] = {
		TySch.boundVars match {
			case Nil => subst
			case y::ys	=> subst + (y -> TypeVar (fr.getNext)) ++ aux (fr, TypeScheme(ys, tysch.ty))
		}
	}
	substitute(tysch.ty, subst)
	
  }

  // A Constraint represents an equality between two types, 
  // e.g., (TypeVar ("A0"), TypeVar ("A1")) is a Constraint used to record that A0 is equal to A1.
  type Constraint = (Type, Type)

  // Constraints stores a collection of Constraint instances.  This is a mutable object that is updated during
  // the type inference implementation.
  class Constraints (var contents:List[Constraint]) {
    private def extend (t1:Type, t2:Type) : Unit = contents = (t1, t2) :: contents

    def add (t1:Type, t2:Type) : Unit = {
      (t1, t2) match {
        case (TypeFun (t11, t12), TypeFun (t21, t22)) => 
          add (t11, t21)
          add (t12, t22)
        case (TypeList (t11), TypeList (t21)) => 
          add (t11, t21)
        case (TypeBase (_), TypeBase (_)) => 
          extend (t1, t2)
        case (TypeBase (_), _) => 
          add (t2, t1)
        case (_, _) => 
          extend (t1, t2)
      }
    }

    override def toString () : String = {
      contents.foldLeft ("") { case (result:String, next:Constraint) => result + next + "\n" }
    }
  }

  class TypeCheckerException (message:String) extends Exception (message)

  // Infer generates constraints for type inference (but does not solve the constraints).
  class Infer (typeSchemes:Map[String, TypeScheme])
  {
    val fresh = new Fresh ("A")
    val constraints = new Constraints (Nil)

    // EXERCISE 4: inferType is called with
    // - a 'context' map from variable names to their types
    // - 'e' an expression to have its type inferred
    // It is expected to return a new type and update 'constraints'.
    // As it runs, it makes uses of 'fresh' to get fresh type variables.
    def inferType (context:Map[String,Type], e:Exp) : Type = {
      val result : Type = e match {
        case ExpApply (operator, operand) =>
          // TODO: Complete the definition.
          null
        case ExpCons (head, tail) =>
          // TODO: Complete the definition.
		  val h = inferType (context, head)
		  val t = inferType (context, tail)
		  constraints.add(t,h)
		  t
		  
        case ExpFun (functionName, parameterName, body) =>
          // TODO: Complete the definition.
          null
        case ExpIfThenElse (condition, trueBranch, falseBranch) =>
          // TODO: Complete the definition.
          null
        case ExpInt (_) => 
          // TODO: Complete the definition.
          TypeBase("Int")
        case ExpLet (_, _, _) =>
          throw new RuntimeException ("Not handling ExpLet")
        case ExpNil =>
          // TODO: Complete the definition.
          TypeVar(fresh.getNext())
		  //TypeList(TypeVar(fresh.getNext()))
        case ExpString (_) =>
          // TODO: Complete the definition.
          TypeBase("String")
        case  ExpVar (name) =>
          // TODO: Complete the definition.
          null
      }
      result
    }
/*     def inferType (context:Map[String,Type], e:Exp) : Type = {
      val result : Type = e match {
        case ExpApply (operator, operand) =>
          // TODO: Complete the definition.
          null
        case ExpCons (head, tail) =>
          // TODO: Complete the definition.
		  val h = inferType(context, head)
          val t = inferType(context, tail)
		  //constraints.add(t,h)
          //TypeList(inferType(context, tail))
		  t
        case ExpFun (functionName, parameterName, body) =>
          // TODO: Complete the definition.
          null
        case ExpIfThenElse (condition, trueBranch, falseBranch) =>
          // TODO: Complete the definition.
          null
        case ExpInt (_) => 
          // TODO: Complete the definition.
          TypeBase("Int")
        case ExpLet (_, _, _) =>
          throw new RuntimeException ("Not handling ExpLet")
        case ExpNil =>
          // TODO: Complete the definition.
          TypeList(TypeVar(fresh.getNext()))
        case ExpString (_) =>
          // TODO: Complete the definition.
          TypeBase("String")
        case  ExpVar (name) =>
          // TODO: Complete the definition.
          null
      }
      result
    } */
  }
}

