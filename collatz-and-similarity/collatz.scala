// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {


// ADD YOUR CODE BELOW
//======================

//(1) 
def collatz(n: Long) : Long =   {
    if (n == 1)
        0
    //it's even
    else if (n%2 == 0) 
        collatz(n/2) + 1
    //it's odd
    else
        collatz(3*n +1) + 1
}

//(2) 
def collatz_max(bnd: Long) : (Long, Long) = {
    val lst = (1.toLong to bnd)
    val finalList = for (n <- lst) yield    {
        (collatz(n),n)
    }
    finalList.maxBy(_._1)
}

//(3)
def is_pow_of_two(n: Long) : Boolean =  {
    val check = n & (n-1)
    check == 0
}

def is_hard(n: Long) : Boolean = {
    is_pow_of_two(3*n +1)
}

def last_odd(n: Long) : Long =  {
    def generateCollatz(n2: Long, lastOdd: Long): Long = {
        if (n2 == 1) lastOdd
        else if (is_pow_of_two(n2)) lastOdd
        else 
            if (n2%2 == 0)
                generateCollatz(n2/2, lastOdd)
            else    {
                val newLastOdd = n2
                generateCollatz(3*n2 +1, newLastOdd)
            }
    }

    generateCollatz(n, 1L)

}

}