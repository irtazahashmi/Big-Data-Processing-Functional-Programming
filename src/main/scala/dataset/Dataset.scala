package dataset

import java.util.{Calendar, TimeZone}

import dataset.util.Commit.{Commit, File, Stats}

/**
  * Use your knowledge of functional programming to complete the following functions.
  * You are recommended to use library functions when possible.
  *
  * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
  * When asked for dates, use the `commit.commit.committer.date` field.
  *
  * This part is worth 40 points.
  */
object Dataset {

    /** Q16 (5p)
      * For the commits that are accompanied with stats data, compute the average of their additions.
      * You can assume a positive amount of usable commits is present in the data.
      * @param input the list of commits to process.
      * @return the average amount of additions in the commits that have stats data.
      */
    def avgAdditions(input: List[Commit]): Int = {
        val valuesofAdditions = input.filter(commit => commit.stats.isInstanceOf[Some[Stats]])
          .map(commit => commit.stats.asInstanceOf[Some[Stats]].value.additions)
        val result = valuesofAdditions.sum/valuesofAdditions.length
        return result
    }

    /** Q17 (8p)
      * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
      * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
      * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
      * @param input list of commits to process.
      * @return the hour and the amount of files changed during this hour.
      */
    def jsTime(input: List[Commit]): (Int, Int) = {

        val jsCommits = input.filter(commit => commit.files.exists(
            file => file.filename.asInstanceOf[Some[String]].value.endsWith(".js")))

        val datesOfJS = jsCommits.map(commit => {
            val calender: Calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
            calender.setTime(commit.commit.committer.date)
            (calender.get(Calendar.HOUR_OF_DAY), commit.files.count(
                file => file.filename.asInstanceOf[Some[String]].value.endsWith(".js")
            ))
        })

        val results = datesOfJS.groupBy(_._1).map{
            case (key, value) => key -> value.map(_._2).sum
        }
        val fR = results.toList.sortBy(_._2)
        val fRR = fR(fR.length - 1)
        return fRR
    }

    /** Q18 (9p)
      * For a given repository, output the name and amount of commits for the person
      * with the most commits to this repository.
      * For the name, use `commit.commit.author.name`.
      * @param input the list of commits to process.
      * @param repo the repository name to consider.
      * @return the name and amount of commits for the top committer.
      */
    def topCommitter(input: List[Commit], repo: String): (String, Int) = {
        val commits = input.filter(commit => commit.url.contains(repo))
        val authors = commits.map(commit => {
            (commit.commit.author.name, commit)
        })

        val results = authors.groupBy(_._1).map{
            case (key, value) => key -> value.map(_._2).size
        }

        val fR = results.toList.sortBy(_._2)
        val fRR = fR(fR.length - 1)
        return (fRR)
    }


    /** Q19 (9p)
      * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
      * Leave out all repositories that had no activity this year.
      * @param input the list of commits to process.
      * @return a map that maps the repo name to the amount of commits.
      *
      * Example output:
      * Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
      */
    def commitsPerRepo(input: List[Commit]): Map[String, Int] = {

        val rightYear = input.filter(commit => {
            val calender: Calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
            calender.setTime(commit.commit.committer.date)
            calender.get(Calendar.YEAR).equals(2019)
        })

        val commits = rightYear.map(commit => {

            (commit.url.substring(commit.url.indexOf("/repos") + 7, commit.url.indexOf("/commits")), commit)
        })

        val results = commits.groupBy(_._1).map{
            case (key, value) => key -> value.map(_._2).size
        }

        return results
    }

    /** Q20 (9p)
      * Derive the 5 file types that appear most frequent in the commit logs.
      * @param input the list of commits to process.
      * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
      */
    def topFileFormats(input: List[Commit]): List[(String, Int)] = {
        val result = input.map(commit => {
            val regEx = """(.*)[.]([^.]*)""".r
            commit.files.filter(file => file.filename.isInstanceOf[Some[String]])
              .map(file => file.filename.asInstanceOf[Some[String]].value)
              .filter(fileName => fileName.matches(regEx.toString()))
              .map {
                  case regEx(_, ext) => (ext, 1)
              }
              .groupBy(_._1)
              .map {
                  case (key, value) => key -> value.map(_._2).sum
              }
        })

        val fResult = result.flatMap(_.toList).groupBy(_._1).map {
              case (key, value) => key -> value.map(_._2).sum
        }

        val fRR = fResult.toList.sortBy(_._2).takeRight(5).reverse
        return fRR
    }
}