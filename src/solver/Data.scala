package solver

import scala.collection.mutable

object Data {

  def data3 = {
    val jobs = data2
    val jobsMapping = new mutable.HashMap[Job,List[TeachersJob]]()
    val mergedJobs = jobs.foldLeft(Map[Teacher,Map[Set[Int],(Int, String, List[TeachersJob])]]())((map,job) => {
      val newRecord: (Teacher, Map[Set[Int], (Int, String, List[TeachersJob])]) = map.get(job.teacher) match {
        case None => {
          job.teacher -> Map(job.classHour.classes ->(job.count, job.classHour.subjects.mkString(","), job.toTeachersJobs))
        }
        case Some(clsToReduced) => {
          clsToReduced.get(job.classHour.classes) match {
            case None => {
              job.teacher -> (clsToReduced + (job.classHour.classes ->(job.count, job.classHour.subjects.mkString(","), job.toTeachersJobs)))
            }
            case Some(reduced) => {
              job.teacher -> (clsToReduced + (job.classHour.classes ->(reduced._1 + job.count, reduced._2 + {
                val newSubjs = job.classHour.subjects.filter(sbj => !reduced._2.endsWith(sbj) && !reduced._2.contains(sbj+",")).mkString(",")
                if (!newSubjs.isEmpty) "," + newSubjs else ""
              },job.toTeachersJobs ::: reduced._3)))
            }
          }
        }
      }
      map + newRecord
    }).flatMap(tm => tm._2.map(cls => {
      val job = Job(tm._1,ClassHour(cls._2._2+" "+cls._1.map(c=>c+1).mkString("/"),cls._1),cls._2._1)
      jobsMapping += (job -> cls._2._3)
      job
    })).toList
    (mergedJobs,jobsMapping)
  }

  def createClassHour(s:String) = {
    val clss = s.filter(_.isDigit).map(_.toString.toInt-1).toSet
    new ClassHour(s,clss)
  }

  def createJob(s:String, cnt:Int, teacher:String) = {
    Job(Teacher(teacher),createClassHour(s),cnt)
  }

  def data2 = {

    val TerezaJobs = Map(
      "Aj 8/9" -> 1,
      "Aj 8" -> 2,
      "Aj 9" -> 2,
      "Aj 6/7" -> 2,
      "Aj 6" -> 1,
      "Aj 7" -> 1,
      "Hv 2/3" -> 1,
      "Aj 4/5" -> 1,
      "Aj 4" -> 2,
      "Aj 5" -> 2,
      "Vv 6/7" -> 2,
      "Vv 8/9" -> 1,
      "Vv 2/3" -> 1,
      "Vv 4/5" -> 2,
      "Pč 2/3" -> 1
    ).map(e => createJob(e._1,e._2,"Tereza")).toList

    val AlenaJobs = Map(
      "M 6/7" -> 3,
      "M 6" -> 2,
      "M 7" -> 2,
      "F 6/7" -> 1,
      "F 6" -> 1,
      "F 7" -> 1,
      "M 8/9" -> 1,
      "M 8" -> 4,
      "M 9" -> 4,
      "F 8/9" -> 1,
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
      "D 8/9" -> 1,
      "D 8" -> 1,
      "D 9" -> 1,
      "Z 6/7" -> 1,
      "Z 6" -> 1,
      "Z 7" -> 1,
      "Z 8/9" -> 1,
      "Z 8" -> 1,
      "Z 9" -> 1,
      "Tv_Chl 6/7/8/9" -> 2,
      "Hv 6/7" -> 1
    ).map(e => createJob(e._1,e._2,"Hana")).toList

    val GitaJobs = Map(
      "Čj 4/5" -> 7,
      "Čj 4" -> 1,
      "M 4/5" -> 5,
      "Př 4/5" -> 1,
      "Př 5" -> 1,
      "Vl 4/5" -> 1,
      "Vl 4" -> 1,
      "Vl 5" -> 1,
      "Tv 4/5" -> 2,
      "Hv 4/5" -> 1
    ).map(e => createJob(e._1,e._2,"Gita")).toList

    val MartinaJobs = Map(
      "Prv 2/3" -> 2,
      "Prv 3" -> 1,
      "Aj 3" -> 3,
      "Čj 2/3" -> 9,
      "M 2/3" -> 4,
      "M 3" -> 1,
      "Tv 2/3" -> 2
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
      "Př 6/7" -> 1,
      "Př 6" -> 1,
      "Př 7" -> 1,
      "Ch 8/9" -> 1,
      "Ch 8" -> 1,
      "Ch 9" -> 1,
      "Hv 8/9" -> 1,
      "Tv_Dív 6/7/8/9" -> 2,
      "Vo 6/7" -> 1,
      "Rv 7" -> 1
    ).map(e => createJob(e._1,e._2,"Lucka")).toList

    val EvaJobs = Map(
      "Inf 5/6" -> 1,
      "Inf 9" -> 2,
      "Inf 8" -> 1,
      "Vo 8/9" -> 1,
      "Rv 8/9" -> 1,
      "Pč 4/5" -> 1,
      "Pč 6/7" -> 1,
      "Pč 8/9" -> 1
    ).map(e => createJob(e._1,e._2,"Eva")).toList

    val IvaJobs = Map(
      "Rj 7" -> 2,
      "Rj 8" -> 2
    ).map(e => createJob(e._1,e._2,"Iva")).toList

    TerezaJobs ++ AlenaJobs ++ HanaJobs ++ GitaJobs ++ MartinaJobs ++ BohunkaJobs ++ LuckaJobs ++ EvaJobs ++ IvaJobs
  }

  def data = {
    val teachers = Set("Tereza","Alena","Hana","Gita","Martina","Bohunka","Lucka","Eva","Iva").map(n => Teacher(n))

    var teachersJobs = data2.flatMap(_.toTeachersJobs)

    (teachersJobs,teachers)
  }

}
