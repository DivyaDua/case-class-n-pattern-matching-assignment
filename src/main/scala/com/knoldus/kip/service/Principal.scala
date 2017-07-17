package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models._

trait Principal {

  def findOutIfCSE(id: Int): CoursePerformance = {
    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)

    val course: String = coursePerformance.map(_.course.name).getOrElse("Does not exist")

    course match {
      case "CSE" => coursePerformance.get
      case "Does not exist" => throw new Exception(s"Not CSE")
    }

  }

  def findOutIfAnyCourse(id: Int, courseName: String): CoursePerformance = {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)

    val course: String = coursePerformance.map(_.course.name).getOrElse("")

    course match {
      case `courseName` => coursePerformance.get
      case "" => throw new Exception(s"Not matched")
    }
  }

  def expression(mod: Any): String = {
    mod match {
      case Student(id,fName,mName,lName,roll,age,gender,enrollNo,address) => s"Shut up"
      case Subject(id,name,maxMarks,obtainedMarks) => s"aha"
      case Scorecard(id,student,subjects,total,percentage,grade) => s"Hmmm..."
      case _ => s"!!!!????"
    }
  }

  def checkScoreboard(scorecards: List[Scorecard]): List[String] = {

    for{sc <- scorecards
        sub <- sc.subjects}
      yield {sc.student.firstName + " " + sub.name + " " + sub.obtainedMarks}

  }

  def expressionRevisited: PartialFunction[ModelIdentifier, String] = {
    case Student(id,fName,mName,lName,roll,age,gender,enrollNo,address) => s"Shut up"
    case Subject(id,name,maxMarks,obtainedMarks) => s"aha"
    case Scorecard(id,student,subjects,total,percentage,grade) => s"Hmmm..."
    case _ => s"!!!!????"
  }

}
