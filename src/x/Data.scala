package x

object Data {

  def data3 = {
    val jobs = data2
    jobs.foldLeft(Map[Teacher,Map[Set[Int],(Int,String)]]())((map,job) => {
      val newRecord: (Teacher, Map[Set[Int], (Int, String)]) = map.get(job.teacher) match {
        case None => {
          job.teacher -> Map(job.classHour.classes ->(job.count, job.classHour.subjects.mkString(",")))
        }
        case Some(clsToReduced) => {
          clsToReduced.get(job.classHour.classes) match {
            case None => {
              job.teacher -> (clsToReduced + (job.classHour.classes ->(job.count, job.classHour.subjects.mkString(","))))
            }
            case Some(reduced) => {
              job.teacher -> (clsToReduced + (job.classHour.classes ->(reduced._1 + job.count, reduced._2 + {
                val newSubjs = job.classHour.subjects.filter(sbj => !reduced._2.endsWith(sbj) && !reduced._2.contains(sbj+",")).mkString(",")
                if (!newSubjs.isEmpty) "," + newSubjs else ""
              })))
            }
          }
        }
      }
      map + newRecord
    }).flatMap(tm => tm._2.map(cls => Job(tm._1,ClassHour(cls._2._2+" "+cls._1.map(c=>c+1).mkString("/"),cls._1),cls._2._1))).toList
  }

  def data2 = {
    def createJob(s:String, i:Int, n:String) = {
      val clss = s.filter(_.isDigit).map(_.toString.toInt-1).toSet
      val ch = new ClassHour(s,clss)
      Job(Teacher(n),ch,i)
    }

    val TerezaJobs = Map(
      "Aj 8/9" -> 1,
      "Aj 8" -> 2,
      "Aj 9" -> 2,
      "Aj 6/7" -> 1,
      "Aj 6" -> 2,
      "Aj 7" -> 2,
      "Vl 4/5" -> 1,
      "Vl 4" -> 1,
      "Vl 5" -> 1,
      "Prv 3/Př 5" -> 2,
      "Prv 3" -> 1,
      "Vv 2/4" -> 1,
      "Vv 4" -> 1,
      "Hv 3/5" -> 1,
      "Vv 6/7" -> 2,
      "Vv 8/9" -> 1
    ).map(e => createJob(e._1,e._2,"Tereza")).toList

    val AlenaJobs = Map(
      "F/M 6/7" -> 1,
      "M/F 6/7" -> 1,
      "F 6" -> 1,
      "F 7" -> 1,
      "M 6" -> 3,
      "M 7" -> 3,
      "M 6/7" -> 1,
      "F/M 8/9" -> 1,
      "M/F 8/9" -> 1,
      "M 8" -> 3,
      "M 9" -> 3,
      "M 8/9" -> 1,
      "F 8" -> 1,
      "F 9" -> 1
    ).map(e => createJob(e._1,e._2,"Alena")).toList

    val HanaJobs = Map(
      "Čj 6/7" -> 1,
      "Čj 6" -> 3,
      "Čj 7" -> 3,
      "D 6/7" -> 1,
      "D 6" -> 1,
      "D 7" -> 1,
      "D 8" -> 2,
      "D 9" -> 2,
      "Z 6/7" -> 1,
      "Z 6" -> 1,
      "Z 7" -> 1,
      "Z 8/9" -> 1,
      "Z 8" -> 1,
      "Z 9" -> 1,
      "Hv 6/7" -> 1,
      "Hv 8/9" -> 1
    ).map(e => createJob(e._1,e._2,"Hana")).toList

    val GitaJobs = Map(
      "Čj 2/4" -> 8,
      "Čj 2" -> 2,
      "M 2/4" -> 5,
      "Prv/Př  2/4" -> 1,
      "Prv 2" -> 1,
      "Hv 2/4" -> 1,
      "Tv 2/4" -> 2,
      "Pč 2/4" -> 1
    ).map(e => createJob(e._1,e._2,"Gita")).toList

    val MartinaJobs = Map(
      "Čj 3/5" -> 7,
      "Čj 3" -> 2,
      "M 3/5" -> 5,
      "Aj 4/5" -> 3,
      "Aj 3" -> 3,
      "Vv 3/5" -> 1,
      "Vv 5" -> 1
    ).map(e => createJob(e._1,e._2,"Martina")).toList

    val BohunkaJobs = Map(
      "Čj 8/9" -> 1,
      "Čj 8" -> 3,
      "Čj 9" -> 3,
      "SPV 9" -> 1
    ).map(e => createJob(e._1,e._2,"Bohunka")).toList

    val LuckaJobs = Map(
      "Př 8/9" -> 1,
      "Př 8" -> 1,
      "Př 9" -> 1,
      "Ch 8/9" -> 1,
      "Ch 8" -> 1,
      "Ch 9" -> 1,
      "Př 6/7" -> 1,
      "Př 6" -> 1,
      "Př 7" -> 1,
      "Vo 6/7" -> 1,
      "Rv 7" -> 1,
      "Tv 3/5" -> 2,
      "Tv_Dív 6/7/8/9" -> 2,
      "Tv_Chl 6/7/8/9" -> 2
    ).map(e => createJob(e._1,e._2,"Lucka")).toList

    val EvaJobs = Map(
      "Inf 5/6" -> 1,
      "Inf 8" -> 2,
      "Inf 9" -> 2,
      "Spv 8" -> 1,
      "Vo 8/9" -> 1,
      "Rv 8/9" -> 1,
      "Pč 3/5" -> 1,
      "Pč 6/7" -> 1,
      "Pč 8/9" -> 1
    ).map(e => createJob(e._1,e._2,"Eva")).toList

    val IvaJobs = Map(
      "Rj 7" ->	2
    ).map(e => createJob(e._1,e._2,"Iva")).toList

//    val freeHours = (FIRST_GRADE+5 to LAST_GRADE).map(gr => Job(Teacher("Volna"+(gr+1)),ClassHour("Volna "+(gr+1),Set(gr)),if(gr>4) 3 else 5))

    TerezaJobs ++ AlenaJobs ++ HanaJobs ++ GitaJobs ++ MartinaJobs ++ BohunkaJobs ++ LuckaJobs ++ EvaJobs ++ IvaJobs
//    TerezaJobs ++ AlenaJobs ++ HanaJobs ++ GitaJobs ++ MartinaJobs ++ BohunkaJobs ++ LuckaJobs ++ EvaJobs ++ IvaJobs ++ freeHours
  }

  def data = {
    val teachers = Set("Tereza","Alena","Hana","Gita","Martina","Bohunka","Lucka","Eva","Iva").map(n => Teacher(n))

    var teachersJobs = data2.flatMap(_.toTeachersJobs)

    (teachersJobs,teachers)
  }

}
