package intro

/**
  * This part has some exercises for you to practice with the recursive lists and functions.
  * For the exercises in this part you are _not_ allowed to use library functions,
  * you should implement everything yourself.
  * Use recursion to process lists, iteration is not allowed.
  *
  * This part is worth 5 points.
  */
object Practice {

    /** Q5 (2p)
      * Implement the function that returns the first n elements from the list.
      * Note that `n` is an upper bound, the list might not have `n` elements.
      *
      * @param xs list to take items from.
      * @param n amount of items to take.
      * @return the first n items of xs.
      */
    def firstN(xs: List[Int], n: Int): List[Int] = xs match {
        case value :: tail => {
            if (n > 0) {
                return value +: firstN(tail, n - 1)
            } else {
                return List()
            }
        }
        case Nil => List()
    }



    /** Q6 (3p)
      * Implement the function that returns the maximum value in the list.
      *
      * @param xs list to process.
      * @return the maximum value in the list.
      */
    def maxValue(xs: List[Int]): Int =  {
            def maxRecursive(list: List[Int], maxNumber: Int = Integer.MIN_VALUE): Int = list match {
                case (value: Int) :: tail => {
                    if (value > maxNumber) {
                        maxRecursive(tail, value)
                    } else {
                        maxRecursive(tail, maxNumber)
                    }
                }
                case Nil => maxNumber
            }
        return maxRecursive(xs)
    }






}
