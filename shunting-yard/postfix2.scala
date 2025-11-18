// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object C3b {

// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// ADD YOUR CODE BELOW
//======================



// (3) 
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match{
    case Nil => out ++ st
      
    case token :: rest if token.forall(_.isDigit) => syard(rest, st, out :+ token)

    case op :: rest if ops.contains(op) =>
      st.span(s => ops.contains(s) && (
        assoc(op) match {
          case LA => precs(op) <= precs(s)
          case RA => precs(op) < precs(s)
        }
      )) match{
        case(firstHalf, secHalf) =>
          syard(rest, op:: secHalf, out ++ firstHalf)
      }

    case "(" :: rest => 
      syard(rest, "("::st, out)

    case ")" :: rest =>
      val (first, second) = st.span(_ != "(")
      syard(rest, second.tail, out ++ first)
	
}


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4)
def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match {
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

  case "^" :: rest =>
    val num1 = st.head
    val num2 = st(1)
    compute(rest, ((BigInt(num2).pow(num1)).toInt) :: st.drop(2))

  case Nil => st.head
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
