package fp_practice

/**
  * In this part you can practice your FP skills with some small exercises.
  * Hint: you can find many useful functions in the documentation of the List class.
  *
  * This part is worth 15 points.
  */
object FPPractice {

    /** Q13 (4p)
      * Returns the sum of the first 10 numbers larger than 25 in the given list.
      * @param xs the list to process.
      * @return the sum of the first 10 numbers larger than 25.
      */
    def first10Above25(xs: List[Int]): Int = {
        def filter[A](xs: List[A], f: A => Boolean): List[A] = xs match {
            case value :: tail => {
                if (f(value)) {
                    value +: filter(tail, f)
                } else {
                    filter(tail, f)
                }
            }
            case Nil => List()
        }

        def over25(xs: List[Int]): List[Int] = {
            return filter(xs, (value: Int) => value > 25)
        }
        val newList = over25(xs)

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
        val evenNewerList = firstN(newList, 10)

        def sum(xs: List[Int]): Int = {

            // Using `xs match` we "match" the value of the list.
            // This is called pattern matching.
            xs match {

                // The base case of the recursive definition:
                // the sum of an empty list is 0.
                case Nil => 0

                // If the list has a number,
                // add the value of the head to the sum of the tail
                case i :: tail => i + sum(tail)
            }
        }
        return sum(evenNewerList)
    }


    /** Q14 (5p)
      * Provided with a list of all grades for each student of a course,
      * count the amount of passing students.
      * A student passes the course when the average of the grades is at least 5.75 and no grade is lower than 4.
      *
      * @param grades a list containing a list of grades for each student.
      * @return the amount of students with passing grades.
      */
    def passingStudents(grades: List[List[Int]]): Int = {
        def sum(xs: List[Int]): Int = {

            // Using `xs match` we "match" the value of the list.
            // This is called pattern matching.
            xs match {

                // The base case of the recursive definition:
                // the sum of an empty list is 0.
                case Nil => 0

                // If the list has a number,
                // add the value of the head to the sum of the tail
                case i :: tail => i + sum(tail)
            }
        }

        def listSize (inputList: List[Int]): Int = inputList match {
            case _ :: tail => 1 + listSize(tail)
            case Nil => 0
        }

        def filter[A](xs: List[A], f: A => Boolean): List[A] = xs match {
            case value :: tail => {
                if (f(value)) {
                    value +: filter(tail, f)
                } else {
                    filter(tail, f)
                }
            }
            case Nil => List()
        }

        grades match {
            case value :: tail => {
                if ((sum(value) / listSize(value) >= 5.75) && listSize(filter(value, (grade:Int) => grade < 4)) == 0)  {
                    1 + passingStudents(tail)
                } else {
                    0 + passingStudents(tail)
                }
            }
            case Nil => 0
        }
    }


    /** Q15 (6p)
      * Return the length of the first list of which the first item's value is equal to the sum of all other items.
      * @param xs the list to process
      * @return the length of the first list of which the first item's value is equal to the sum of all other items,
      *         or None if no such list exists.
      *
      * Read the documentation on the `Option` class to find out what you should return.
      * Hint: it is very similar to the `OptionalInt` you saw earlier.
      */
    def headSumsTail(xs: List[List[Int]]): Option[Int] = {
        def sum(xs: List[Int]): Int = {

            // Using `xs match` we "match" the value of the list.
            // This is called pattern matching.
            xs match {

                // The base case of the recursive definition:
                // the sum of an empty list is 0.
                case Nil => 0

                // If the list has a number,
                // add the value of the head to the sum of the tail
                case i :: tail => i + sum(tail)
            }
        }

        def listSize (inputList: List[Int]): Int = inputList match {
            case _ :: tail => 1 + listSize(tail)
            case Nil => 0
        }

        xs match {
            case listOfInts :: tail => {
                if (sum(listOfInts) - listOfInts(0) == listOfInts(0)) {
                    Some(listSize(listOfInts))
                } else {
                    headSumsTail(tail)
                }
            }
            case Nil => None
        }
    }

}
