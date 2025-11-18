// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {


// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// ADD YOUR CODE BELOW
//======================


// (1) 
def is_op(op: String) : Boolean =	{
	ops.contains(op)
}
def prec(op1: String, op2: String) : Boolean =	{
	(precs.get(op1), precs.get(op2)) match	{
		case (Some(p1), Some(p2)) => p1 >= p2
		case _ => false
	}
}

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks =	toks match {

	case Nil => out ++ st
    
	case token :: rest if token.forall(_.isDigit) => syard(rest, st, out :+ token)

	case token::rest if is_op(token) =>
		st match {
			case operator :: _ if is_op(operator) && prec(operator, token) =>
				syard(toks, st.tail, out:+ operator)
			case _ =>
				syard(rest, token::st, out)
		}

	case "(" :: rest => 
		syard(rest, "("::st, out)
	
	case ")" :: rest =>
		val (first, second) = st.span(_ != "(")
		syard(rest, second.tail, out ++ first)
	
}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) 
def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match	{
	case token :: rest if token.forall(_.isDigit) => compute(rest, token.toInt :: st)
	
	case "+" :: rest =>
		val num1 = st.head
		val num2 = st(1)
		compute(rest, (num1+num2) :: st.drop(2))
	case "-" :: rest =>
		val num1 = st.head
		val num2 = st(1)
		compute(rest, (num2-num1) :: st.drop(2))

	case "*" :: rest =>
		val num1 = st.head
		val num2 = st(1)
		compute(rest, (num1*num2) :: st.drop(2))
	case "/" :: rest =>
		val num1 = st.head
		val num2 = st(1)
		compute(rest, (num2/num1) :: st.drop(2))

	case Nil => st.head
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


