// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


////////// OLD FUNCTIONS ////////////
def load_bff(name: String) : String =   {
    Try(Source.fromFile(name).getLines.mkString("\n")).getOrElse("")
}

def sread(mem: Mem, mp: Int) : Int =    {
  mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem =    {
  mem.updated(mp,v)
}
////////// OLD FUNCTIONS ////////////



// (6) 
def jtable(pg: String) : Map[Int, Int] =  {
  jtable_helper(pg, 0, List(), Map())
}

def jtable_helper(pg: String, pc: Int, stack: List[Int], table: Map[Int, Int]): Map[Int, Int] = {
  if (pc >= pg.length) table
  else pg(pc) match {
    case '[' => jtable_helper(pg, pc + 1, pc :: stack, table)
    case ']' =>
      val openPos = stack.head
      val newTable = table + (openPos -> (pc + 1)) + (pc -> (openPos + 1))
      jtable_helper(pg, pc + 1, stack.tail, newTable)
    case _ => jtable_helper(pg, pc + 1, stack, table)
  }
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)



def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc<0 || pc >= pg.length) mem
  else pg(pc) match {
    case '>' => compute2(pg, tb, pc+1, mp+1, mem)
    case '<' => compute2(pg, tb, pc+1, mp-1, mem)

    case '+' =>
      val incValue = sread(mem, mp) + 1
      compute2(pg, tb, pc+1, mp, write(mem, mp, incValue))
    case '-' =>
      val decValue = sread(mem, mp) - 1
      compute2(pg, tb, pc+1, mp, write(mem, mp, decValue))

    case '.' => 
      print(sread(mem, mp).toChar)
      compute2(pg, tb, pc+1, mp, mem)
    case ',' =>
      val input = io.StdIn.readInt()
      compute2(pg, tb, pc+1, mp, write(mem, mp, input))

    case '[' =>
      if (sread(mem, mp) ==0) compute2(pg, tb, tb(pc), mp, mem)
      else compute2(pg, tb, pc + 1, mp, mem)
    case ']' =>
      if (sread(mem, mp) !=0) compute2(pg, tb, tb(pc), mp, mem)
      else compute2(pg, tb, pc + 1, mp, mem)

    case _ => compute2(pg, tb, pc + 1, mp, mem)

  }
}

def run2(pg: String, m: Mem = Map()) : Mem =  {
  compute2(pg, jtable(pg), 0, 0, m)
}

// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (7) 

def optimise(s: String) : String =  {
  val noDeadCode = s.replaceAll("""[^<>+\-.\[\]]""", "")
  noDeadCode.replaceAll("""\[-\]""", "0")
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc<0 || pc >= pg.length) mem
  else pg(pc) match {
    case '>' => compute3(pg, tb, pc+1, mp+1, mem)
    case '<' => compute3(pg, tb, pc+1, mp-1, mem)

    case '+' =>
      val incValue = sread(mem, mp) + 1
      compute3(pg, tb, pc+1, mp, write(mem, mp, incValue))
    case '-' =>
      val decValue = sread(mem, mp) - 1
      compute3(pg, tb, pc+1, mp, write(mem, mp, decValue))

    case '.' => 
      print(sread(mem, mp).toChar)
      compute3(pg, tb, pc+1, mp, mem)
    case ',' =>
      val input = io.StdIn.readInt()
      compute3(pg, tb, pc+1, mp, write(mem, mp, input))

    case '[' =>
      if (sread(mem, mp) ==0) compute3(pg, tb, tb(pc), mp, mem)
      else compute3(pg, tb, pc + 1, mp, mem)
    case ']' =>
      if (sread(mem, mp) !=0) compute3(pg, tb, tb(pc), mp, mem)
      else compute3(pg, tb, pc + 1, mp, mem)

    case '0' =>
      compute3(pg, tb, pc+1, mp, write(mem, mp, 0))

    case _ => compute3(pg, tb, pc + 1, mp, mem)

  }
}

def run3(pg: String, m: Mem = Map()) : Mem =  {
  val optimisedPg = optimise(pg)
  compute3(optimisedPg, jtable(optimisedPg), 0, 0, m)
}


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (8)  
def combine(s: String) : String = {
  def combine_helper(char: Char, count: Int): String =  {
    if (count<=26) s"$char${('A'+count-1).toChar}"
    else s"$char" + "Z" + combine_helper(char, count - 26)
  }



  val pattern = """([+\-<>])\1*""".r
  pattern.replaceAllIn(s, x => {
    val char = x.toString.head
    val length = x.toString.length
    combine_helper(char, length)
})
}

// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc<0 || pc >= pg.length) mem
  else pg(pc) match {
    case '>' => 
      val change = pg(pc+1) - 'A' +1
      compute4(pg, tb, pc+2, mp+change, mem)
    case '<' => 
      val change = pg(pc+1) - 'A' +1
      compute4(pg, tb, pc+2, mp-change, mem)

    case '+' =>
      val incValue = sread(mem, mp) + (pg(pc + 1) - 'A' + 1)
      compute4(pg, tb, pc+2, mp, write(mem, mp, incValue))
    case '-' =>
      val decValue = sread(mem, mp) - (pg(pc + 1) - 'A' + 1)
      compute4(pg, tb, pc+2, mp, write(mem, mp, decValue))

    case '.' => 
      print(sread(mem, mp).toChar)
      compute4(pg, tb, pc+1, mp, mem)
    case ',' =>
      val input = io.StdIn.readInt()
      compute4(pg, tb, pc+1, mp, write(mem, mp, input))

    case '[' =>
      if (sread(mem, mp) ==0) compute4(pg, tb, tb(pc), mp, mem)
      else compute4(pg, tb, pc + 1, mp, mem)
    case ']' =>
      if (sread(mem, mp) !=0) compute4(pg, tb, tb(pc), mp, mem)
      else compute4(pg, tb, pc + 1, mp, mem)

    case '0' =>
      compute4(pg, tb, pc+1, mp, write(mem, mp, 0))

    case _ => compute4(pg, tb, pc + 1, mp, mem)

  }
}

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) : Mem =  {
  val combinedOptimisedPg = combine(optimise(pg))
  compute4(combinedOptimisedPg, jtable(combinedOptimisedPg), 0, 0, m)
}


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}
