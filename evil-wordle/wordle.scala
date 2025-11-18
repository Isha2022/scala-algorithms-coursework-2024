// Main Part 2 about Evil Wordle
//===============================


object M2 { 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

//(1)
def get_wordle_list(url: String) : List[String] =   {

    Try(Source.fromURL(url).getLines().toList).getOrElse(List())

}

// val secrets = get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
// secrets.length // => 12972
// secrets.filter(_.length != 5) // => Nil

//(2)
def removeN[A](xs: List[A], elem: A, n: Int) : List[A] =    {
    if (n<=0) xs

    val noOfElem = xs.count(_ == elem)

    if (noOfElem <= n)  {
        xs.filter(_!=elem)
    }
    else    {
        def removeFromList(list2: List[A], removed: Int, newList: List[A]): List[A] = list2 match{
            case Nil => newList
            
            case x :: y =>
                if (x == elem && removed<n) {
                    removeFromList(y, removed+1, newList)
                }
                else    {
                    removeFromList(y, removed, newList :+ x)
                }
        }
        removeFromList(xs, 0, List())
    }
    
}


// removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
// removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
// removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
// removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

// (3)
abstract class Tip
case object Absent extends Tip
case object Present extends Tip
case object Correct extends Tip


def pool(secret: String, word: String) : List[Char] =   {
    (0 to 4).toList.collect {
        case n if secret(n) != word(n) => secret(n)
    }
} 

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] =   {
    (secret, word) match    {
        case(Nil, Nil) => Nil
        case (s :: ss, w :: ws) =>
            if (s==w) Correct :: aux(ss,ws,pool)
            else if (pool.contains(w)) Present :: aux(ss, ws, pool.filter(_ != w))
            else Absent :: aux(ss,ws,pool)
    }
}

def score(secret: String, word: String) : List[Tip] =   {
    aux(secret.toList, word.toList, pool(secret, word))
}


// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int =  t match {
    case Correct => 10
    case Present => 1
    case Absent => 0
}

def iscore(secret: String, word: String) : Int =    {
    score(secret, word).map(eval).sum
}

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] =  secrets match {
    case Nil => acc
    case x :: rest =>
        val score = iscore(x, word)
        if (score < current) lowest(rest, word, score, List(x))
        else if (score == current) lowest(rest, word, current, x :: acc)
        else lowest(rest, word, current, acc)
}

def evil(secrets: List[String], word: String) : List[String] =  {
    lowest(secrets, word, Int.MaxValue, List())
}


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] =    {
    val chars = secrets.flatten
    val charsMap = chars.groupBy(identity)
    val charsMapCount = charsMap.view.mapValues(_.size).toMap

    charsMapCount.map   { case(letter, count) =>
        letter -> (1.0- (count / chars.size.toDouble))
    }
}
// (7)
def rank(frqs: Map[Char, Double], s: String) : Double = {
    s.toList.map(c => frqs.getOrElse(c, 0.0)).sum
}

def ranked_evil(secrets: List[String], word: String) : List[String] =   {
    // val evilwords = evil(secrets, word)
    // val frqs = frequencies(secrets)
    // List(evilwords.map(word => (word, rank(frqs, word))).sortBy(-_._2).map(_._1).head)
    val evilWords = evil(secrets, word)
    val frqs = frequencies(secrets)
    val rankedWords = evilWords.map(word => (word, rank(frqs, word)))
    val max = rankedWords.map(_._2).max
    rankedWords.filter(_._2 == max).map(_._1)
}
}
