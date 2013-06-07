package x

object Y extends App {

  var jobs = Data.data2

  val schoolSchedule = new SchoolSchedule

  // <preassignment>
  def preassign(subject:String,teacher:String,day:Int,hour:Int) {
    jobs.find(job => job.classHour.subject==subject && job.teacher == Teacher(teacher)) match {
      case None => throw new RuntimeException("Job: Subject = "+subject+" Teacher = "+teacher+" does not exist")
      case Some(job) => {
        preassign(job,day,hour)
        if(job.count>1) {
          jobs = jobs ++ List(Job(job.teacher,job.classHour,job.count-1))
        }
      }
    }
  }
  def preassign(job:Job,day:Int,hour:Int):Unit = {
    jobs = jobs diff List(job)
    job.classHour.classes.foreach(c => schoolSchedule.schoolSchedule(c).classSchedule(day)(hour) = job.toTeachersJob)
  }

  preassign("Rj 7","Iva",FRIDAY,2)
  preassign("Rj 7","Iva",FRIDAY,3)

  preassign("Tv_Dív 6/7/8/9","Lucka",MONDAY,5)
  preassign("Tv_Dív 6/7/8/9","Lucka",THURSDAY,6)
  preassign("Tv_Chl 6/7/8/9","Lucka",MONDAY,6)
  preassign("Tv_Chl 6/7/8/9","Lucka",THURSDAY,5)

  preassign("Vv 6/7","Tereza",MONDAY,4)
  preassign("Vv 6/7","Tereza",MONDAY,7)
//  preassign("Vv 8/9","Tereza",THURSDAY,7)
  // </preassignment>

  case class Possibility(day:Int,hour:Int)

  trait Group {
    def getN:Int
    def getP:Int
    def update(j:Job,p:Possibility,noRevert:Boolean)
    def unUpdate
    def unUpdate(j:Job,p:Possibility)
    def isScheduled:Boolean
    def isImpossible:Boolean
  }

  trait Availability {
    def getPossibilities:Set[Possibility]

    var plainPossibilities:Set[Possibility] = null

    val hoursByGrades = Map(1 -> (1 to 5), 2 -> (1 to 5), 3 -> (0 to 5), 4 -> (0 to 7),
      5 -> (0 to 7), 6 -> (0 to 7), 7 -> (0 to 7), 8 -> (0 to 7), 9 -> (0 to 7))

    val mainHoursByGrades = Map(1 -> (1 to 5), 2 -> (1 to 5), 3 -> (0 to 5), 4 -> (0 to 5),
      5 -> (0 to 7), 6 -> (0 to 7), 7 -> (0 to 7), 8 -> (0 to 7), 9 -> (0 to 7))

    val excludedDaysByTeacher = Map(
      Teacher("Bohunka") -> Set(TUESDAY,WEDNESDAY,THURSDAY),
      Teacher("Eva") -> Set(TUESDAY,FRIDAY),
      Teacher("Lucka") -> Set(FRIDAY),
      Teacher("Tereza") -> Set[Int](),
      Teacher("Hana") -> Set[Int](),
      Teacher("Gita") -> Set[Int](),
      Teacher("Martina") -> Set[Int](),
      Teacher("Iva") -> Set[Int](),
      Teacher("Alena") -> Set[Int]()
    )

    var updated:Possibility = null

    def update(thisJob:Job,scheduledJob:Job,p:Possibility,noRevert:Boolean) {
      val exclusive = thisJob.classHour.isMutuallyExclusive(scheduledJob.classHour) || thisJob.teacher==scheduledJob.teacher
      if (exclusive) {
        val aux = plainPossibilities - p
        if (aux.size < plainPossibilities.size) {
          plainPossibilities = aux
          if(!noRevert) updated = p
        }
      }
      if (noRevert) {
        updated = null
      }
    }

    def unUpdate {
      if (updated!=null) {
        plainPossibilities = plainPossibilities + updated
        updated = null
      }
    }

    def unUpdate(j:Job,p:Possibility) {
      plainPossibilities = null
    }

    protected def checkSchedulable(job:Job,p:Possibility) = {
      val hourSch = schoolSchedule.schoolSchedule.map(cs => cs.classSchedule(p.day)(p.hour))
      val free = job.classHour.classes.forall(cls => hourSch(cls)==null)
      val teacherOk = hourSch.forall(tj => tj==null || tj.teacher!=job.teacher)
      free && teacherOk
    }

    protected def getPlainPossibilities(job:Job,prefHours:Seq[Int]) = {
      cnt = cnt + 1
      if(cnt%100000==0)println(cnt)
      if (plainPossibilities==null) {
        plainPossibilities = (MONDAY to FRIDAY).toSet.diff(excludedDaysByTeacher(job.teacher)).flatMap(d => {
          val redPrefHours = if(d!=MONDAY && d!=THURSDAY) prefHours.filter(h => h<=5) else prefHours
//          val redPrefHours = prefHours
          redPrefHours.map(h => Possibility(d,h)).filter(p => checkSchedulable(job,p))
        }).toSet
      }
      plainPossibilities
    }
  }

  var cnt = 0

  class MainSubjectAvailability(job:Job,schoolSchedule:SchoolSchedule) extends Availability {
    override def getPossibilities = getPlainPossibilities(job,1 to 5)
  }

  class RestOfSubjectsAvailability(job:Job,schoolSchedule:SchoolSchedule) extends Availability {
    override def getPossibilities = getPlainPossibilities(job,hoursByGrades(job.classHour.lowestClass))
  }

  class TwoHourSubjectAvailability(job:Job,schoolSchedule:SchoolSchedule) extends Availability {
    private var consequents = Set[Possibility]()
    private var lastConsequent:Possibility = null
    val tj = job.toTeachersJob

    private def isConsequent(tj1:TeachersJob,tj2:TeachersJob) = {
      ((tj1.classHour.classes diff tj2.classHour.classes).size == 0 || (tj2.classHour.classes diff tj1.classHour.classes).size == 0) &&
        ((tj1.classHour.subjects diff tj2.classHour.subjects).size < tj1.classHour.subjects.size)
    }
    override def update(thisJob:Job,scheduledJob:Job,p:Possibility,noRevert:Boolean) {
      val otherTj = scheduledJob.toTeachersJob
      if(isConsequent(tj,otherTj) || isConsequent(otherTj,tj)) {
        lastConsequent = p
        consequents = consequents ++ Set(lastConsequent)
      }
      super.update(thisJob,scheduledJob,p,noRevert)
    }
    override def unUpdate {
      super.unUpdate
      if(lastConsequent!=null) {
        consequents = consequents -- Set(lastConsequent)
      }
      lastConsequent = null
    }
    override def getPossibilities = {
      if(consequents.isEmpty) {
        getPlainPossibilities(job,hoursByGrades(tj.classHour.lowestClass))
      } else {
        getPlainPossibilities(job,hoursByGrades(tj.classHour.lowestClass)).filter(p => consequents.forall(consqP => math.abs(consqP.day-p.day)>1))
      }
    }
  }

  def getAvailability(job:Job,schoolSchedule:SchoolSchedule):Availability = {
    if(job==null || schoolSchedule==null) null
    else if(job.classHour.mainSubject) new MainSubjectAvailability(job,schoolSchedule)
    else if(job.classHour.twoHour) new TwoHourSubjectAvailability(job,schoolSchedule)
    else new RestOfSubjectsAvailability(job,schoolSchedule)
  }

  class SimpleGroup(val job:Job, val schoolSchedule:SchoolSchedule) extends Group {
    private val availability = getAvailability(job,schoolSchedule)
    private var left = if(job!=null) job.count else 0
    private var posCache:Set[Possibility] = if(job!=null) availability.getPossibilities else null
    private var scheduled:Boolean = false

    private def ensureCache {
      isScheduled
      if(posCache==null) {
        if(!scheduled) posCache = availability.getPossibilities
        else posCache = Set[Possibility]()
      }
    }

    override def getN = left
    override def getP = {
      ensureCache
      posCache.size
    }
    override def update(j:Job,p:Possibility,noRevert:Boolean) {
      if(!scheduled) {
        ensureCache
        availability.update(job,j,p,noRevert)
        posCache = null
      }
    }
    override def unUpdate {
      if(!scheduled) {
        availability.unUpdate
        posCache = null
      }
    }
    override def unUpdate(j:Job,p:Possibility) {
      availability.unUpdate(j,p)
      posCache = null
    }
    override def isScheduled = {
      scheduled = left == 0
      scheduled
    }
    override def isImpossible = {
      ensureCache
      left>0 && posCache.size==0
    }
    def getPossibilities:Set[Possibility] = {
      ensureCache
      posCache
    }

    def schedule(p:Possibility) {
      left = left-1
      job.classHour.classes.foreach(cls => {
        schoolSchedule.schoolSchedule(cls).classSchedule(p.day)(p.hour) = job.toTeachersJob
      })
    }

    def unschedule(p:Possibility) {
      left = left+1
      job.classHour.classes.foreach(cls => {
        schoolSchedule.schoolSchedule(cls).classSchedule(p.day)(p.hour) = null
      })
    }

    override def toString = "SimpleGroup: "+job.toTeachersJob.toString+" P = "+getP+" ["+getN+"/"+job.count+"]"

    override def hashCode(): Int = job.hashCode()

    override def equals(obj: Any): Boolean = {
      if(obj != null && obj.isInstanceOf[SimpleGroup]) {
        val sg = obj.asInstanceOf[SimpleGroup]
        sg.job == job
      } else false
    }
  }

  class CombinedGroup(val simpleGroups:Seq[SimpleGroup]) extends Group {

    val NOT_CACHED = Integer.MIN_VALUE
    var pCache:Int = NOT_CACHED
    var nCache:Int = NOT_CACHED

    override def getN = {
      if(nCache==NOT_CACHED) {
        nCache = simpleGroups.foldLeft(0)((totalLeft,sg) => totalLeft + sg.getN)
      }
      nCache
    }
    override def getP = {
      if(pCache==NOT_CACHED) {
        pCache = simpleGroups.foldLeft(Set[Possibility]())((totalSet,sg) => totalSet ++ sg.getPossibilities).size
      }
      pCache
    }

    override def update(j: Job, p: Possibility, noRevert: Boolean) {
      nCache = NOT_CACHED
      pCache = NOT_CACHED
      simpleGroups.foreach(_.update(j,p,noRevert))
    }

    override def unUpdate {
      nCache = NOT_CACHED
      pCache = NOT_CACHED
      simpleGroups.foreach(_.unUpdate)
    }

    override def unUpdate(j:Job,p:Possibility) {
      simpleGroups.foreach(_.unUpdate(j,p))
    }

    override def isScheduled = simpleGroups.forall(_.isScheduled)

    override def isImpossible = simpleGroups.exists(_.isImpossible)

    override def toString = "CombinedGroup: <"+simpleGroups.mkString(",")+"> P = "+getP+" [[["+getN+"/"+(simpleGroups.foldLeft(0)((total,sg)=>total+sg.job.count))+"]]]"

    override def hashCode(): Int = simpleGroups.foldLeft(0)((hash,sg) => hash + sg.hashCode())

    override def equals(obj: Any): Boolean = {
      if(obj!=null && obj.isInstanceOf[CombinedGroup]) {
        val cg = obj.asInstanceOf[CombinedGroup]
        cg.simpleGroups.size == simpleGroups.size && (cg.simpleGroups.toSet diff simpleGroups.toSet).isEmpty
      } else false
    }
  }

  val simpleGroups = jobs.map(j => new SimpleGroup(j,schoolSchedule))
  val groups = {
    val teachersGroups = jobs.foldLeft(Set[Teacher]())((allTeachers, job) => allTeachers ++ Set(job.teacher)).map(t => {
      new CombinedGroup(simpleGroups.filter(sg => sg.job.teacher == t))
    })
    val classGroupsCombined = (FIRST_GRADE to LAST_GRADE).map(cls => {
      new CombinedGroup(simpleGroups.filter(sg => sg.job.classHour.classes.contains(cls) && sg.job.classHour.combinedClasses))
    })
    val classGroupsNotCombined = (FIRST_GRADE to LAST_GRADE).map(cls => {
      new CombinedGroup(simpleGroups.filter(sg => sg.job.classHour.classes.contains(cls) && !sg.job.classHour.combinedClasses))
    })
    simpleGroups ++ teachersGroups ++ classGroupsCombined ++ classGroupsNotCombined
  }

  def getM(j:Job,p:Possibility,changingGroup:SimpleGroup) = {
    groups.filter(!_.isScheduled).foldLeft(0.0)((total,group)=>{
      val pBefore = group.getP.toDouble
      group.update(j,p,false)
      val nAfter = group.getN.toDouble
      val pAfter = group.getP.toDouble
      group.unUpdate
      val possDiff: Double = (pAfter - nAfter)
      val m: Double = if(possDiff>=0) ((pBefore - pAfter) * nAfter) / (possDiff+1) else Double.MaxValue
      (total + m)
    })
  }

  def getBestPossibility(sg:SimpleGroup) = {
    val pos = sg.getPossibilities
    pos.foldLeft((Possibility(1000,1000),Double.MaxValue))((best,pos) => {
      val m = getM(sg.job,pos,sg)
      if(m<best._2 && (pos.hour<best._1.hour || best._1.hour==0)) (pos,m)
      else best
    })
  }

  def getBestGroupAndPossibility = {
    simpleGroups.filter(!_.isScheduled).foldLeft((new SimpleGroup(null,null),Possibility(1000,1000),Double.MaxValue))((best,sg) => {
      val curr = getBestPossibility(sg)
//      println(curr+" ---> "+sg)
      if(curr._2<best._3 && (curr._1.hour<best._2.hour || best._2.hour==0)) (sg,curr._1,curr._2)
      else best
    })
  }

  def getBestGroupsAndPossibilities = {
    simpleGroups.filter(!_.isScheduled).flatMap(sg => {
      val curr = getBestPossibility(sg)
      if(curr._2<Double.MaxValue) List((sg,curr))
      else List[(SimpleGroup,(Possibility,Double))]()
    })
  }

  def findBreakingGroups = {
    val allPoss = simpleGroups.filter(!_.isScheduled).map(sg => (sg.job,sg.getPossibilities)).flatMap(jobPoss => jobPoss._2.map(pos => (jobPoss._1,pos)))
    val fg = groups.filter(!_.isScheduled)
    val failedGroups = allPoss.map(jobPos => fg.filter(group => {
      group.update(jobPos._1,jobPos._2,false)
      val fail = (group.getP-group.getN)<0
//      if(fail) println("Failing: "+List(jobPos._1,jobPos._1,group).mkString(" "))
      group.unUpdate
      fail
    }))
    failedGroups.map(l => l.toSet).foldLeft(Set[Group]())((allFailedGroups,failedForAJob) => allFailedGroups ++ failedForAJob).mkString("\n")
  }

  var total = jobs.foldLeft(0)((total,j) => total + j.count)
  var continue = true

  def search:Boolean = {
    if(simpleGroups.forall(_.isScheduled)) true
    else if(simpleGroups.exists(_.isImpossible)) false
    else {
      val best = getBestGroupsAndPossibilities.sortWith((b1,b2) => b1._2._2<=b2._2._2).take(2)
      if(best.isEmpty) {
        println("!!!SCHEDULING FAILED!!!")
        println("Breaking groups = "+findBreakingGroups)
        println("Backtracking")
        false
      } else {
        best.exists(b => {
          total = total - 1
          println(total+": "+b)
          val pos = b._2._1
          b._1.schedule(pos)
          groups.foreach(_.update(b._1.job,pos,true))
          val ok = search
          if(!ok) {
            b._1.unschedule(pos)
            groups.foreach(_.unUpdate(b._1.job,pos))
            total = total + 1
          }
          ok
        })
      }
    }
  }

//  while(continue && simpleGroups.exists(!_.isScheduled) && simpleGroups.forall(!_.isImpossible)) {
//    val best = getBestGroupAndPossibility
//    continue = if(best._3==Double.MaxValue) {
//      println("!!!SCHEDULING FAILED!!!")
//      println("Breaking groups = "+findBreakingGroups)
//      false
//    } else {
//      total = total - 1
//      println(total+": "+best)
//      best._1.schedule(best._2)
//      groups.foreach(_.update(best._1.job,best._2,true))
//      true
//    }
//  }

  search

  Output.printSchedule(schoolSchedule)

//  Checker.check(schoolSchedule,jobs.flatMap(_.toTeachersJobs))
}
