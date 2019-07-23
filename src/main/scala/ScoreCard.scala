import scala.collection.mutable

case class Marks(studentId: Long, marks: Map[Long, Float])

case class Student(studentName: String, studentId: Long, marks: Marks)

case class ScoreCard(studentId: Long, marks: Map[Long, Float], percentage: Float)

object StudentMarks extends App {

  val studentMarks = new Array[Marks](10)
  for (i <- 0 to 8)
    studentMarks(i) = Marks(1300l + i, Map(101l -> (77 + i), 102l -> (44 + i), 103l -> (67 + i)))
  studentMarks(9) = Marks(1300l + 9, Map(101l -> (77 + 9), 102l -> (44 + 9), 103l -> (67 + 9)))

  val students = new Array[Student](10)
  for (i <- 0 to 8)
    students(i) = Student(s"Student-$i", 1300l + i, studentMarks(i))
  students(9) = Student(s"Student-0", 1300l + 9, studentMarks(9))


  def printScoreCard(name: String) = {
    val studentToScoreCardMap = nameScoreCard()
    studentToScoreCardMap.get(name) match {
      case Some(scoreBoard) => scoreBoard
      case None => "No Data Found"
    }
  }

  def nameScoreCard(): mutable.Map[String, AnyRef] = {
    val mapScoreCard = mutable.Map.empty[String, AnyRef]
    for {
      student <- students
      marks <- studentMarks
      if (student.studentId == marks.studentId)
    } yield {
      val name = student.studentName
      val percentage: Float = ((((for ((key, value) <- marks.marks) yield (value)).sum) * 100).toFloat) / marks.marks.size.toFloat
      if (!mapScoreCard.contains(name)) {
        mapScoreCard += (name -> ScoreCard(marks.studentId, marks.marks, percentage))
      } else {
        val value = mapScoreCard(name)
        mapScoreCard += (name -> (value :: ScoreCard(marks.studentId, marks.marks, percentage) :: Nil))
      }
      mapScoreCard
    }
    mapScoreCard
  }

  println(printScoreCard("Student-4546"))
}

