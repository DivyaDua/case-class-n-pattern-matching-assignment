package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{CoursePerformance, Scorecard, Student}

trait Postman {


  def getTheFirstAddressOfFirstYearPerformance(id: Int) : String = {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)

    val scorecard: Option[Scorecard] = coursePerformance.map(_.scorecards.headOption).getOrElse(None)

    val student: Option[Student] = scorecard.map(_.student)

    val address = student.map(_.getAddress).getOrElse("N/A")

    address

    }


}
