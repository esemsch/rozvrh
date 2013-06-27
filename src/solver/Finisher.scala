package solver

object Finisher extends App {

  val ss = Conversions.remapGrades(Input.readScheduleFromFile("schedule_bkp.txt"))

  Output.printSchedule(ss)
//  Output.printSchedule(ss,false)

  Checker.check(ss,Data.data3._2.flatMap(x => x._2).toList)

}
