package com.knoldus.kip.models

case class Student(id: Int, firstName: String, middleName: Option[String],
                   lastName: String, rollNo: Int, age: Option[Int], gender: Char,
                   enrollmentNo: Long, address: Option[String]) extends ModelIdentifier{

  def getAddress: String = {
    address.fold("N/A"){ele => ele
    //Or we can write _.toString
    }
  }

  def getMiddleName: String = {
    middleName.map(_.split(" ")(0)).getOrElse("Got no value")
  }

}

case class Subject(id: Int, name: String, maxMarks: Int, obtainedMarks: Int) extends ModelIdentifier
case class Course(id: Int, name: String, category: String, subjects: List[Subject]) extends ModelIdentifier


case class Scorecard(id: Int, student: Student, subjects: List[Subject],
                     total: Int, percentage: Double, grade: String) extends ModelIdentifier{

  def getSubjectsWithHighestScore: List[Subject] = {
    val highestScore = subjects.map(_.obtainedMarks).max

    subjects.filter(_.obtainedMarks == highestScore)
  }

  def getSubjectsWithLowestScore: List[Subject] = {
    val lowestScore = subjects.map(_.obtainedMarks).min

    subjects.filter(_.obtainedMarks == lowestScore)
  }
}

object Scorecard {

  def apply(student: Student, subjects: List[Subject]): Scorecard = {

    val id = student.id

    val total = subjects.map(_.obtainedMarks).sum
    val max = subjects.map(_.maxMarks).sum
    val percentage = total / max * 100

    val grade = if(percentage >= 95){
      "A+"
    }
    else if(percentage >= 90){
      "A"
    }
    else if(percentage >= 85){
      "B+"
    }
    else if(percentage >= 80){
      "B"
    }
    else if(percentage >= 70){
      "C+"
    }
    else if(percentage >= 60){
      "C"
    }
    else if(percentage >= 50){
      "D+"
    }
    else if(percentage >= 40){
      "D"
    }
    else{
      "E"
    }

    new Scorecard(id, student, subjects, total, percentage, grade)

  }

}

case class CoursePerformance(id: Int, year: Int, course: Course, scorecards: List[Scorecard]) extends ModelIdentifier
