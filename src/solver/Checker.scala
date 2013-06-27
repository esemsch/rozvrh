package solver

object Checker {

  def check(schoolSchedule:SchoolSchedule,teachersJobs:Seq[TeachersJob]) {
    val odpol = new OdpoledniVUrciteDny(schoolSchedule,Set(MONDAY,THURSDAY))
    val volna = new VolnaHodina(schoolSchedule)
    val prvniDruha = new PrvniDruha(schoolSchedule)
    val neprerusene = new NepreruseneVyucovaniDopoledne(schoolSchedule)
    val exklusivitaUcitele = new JedenUcitelJednaHodina(schoolSchedule)
    val vvVzdyPoSobe = new VvVzdyPoSobe(schoolSchedule)
    val dvojHodinoveNePoSobe = new DvojhodinnovePredmetyNeVeDnechPoSobe(schoolSchedule)
    val stejnyNeVeStejnyDen = new StejnyPredmetNeVTenSamyDen(schoolSchedule)
    val hlavniPredmety = new HlavniPredmetyRano(schoolSchedule)
    val spojenePredmety = new SpojenePredmetyRano(schoolSchedule)
    val spravnePrirazene = new PredmetySpravnePrirazeneTridam(schoolSchedule)

    val reditelUciJenVPoAPa = new UcitelUciVUrciteDny(schoolSchedule,Teacher("Bohunka"),Set(MONDAY,FRIDAY))
    val evaVolnoVUtAPa = new UcitelUciVUrciteDny(schoolSchedule,Teacher("Eva"),Set(MONDAY,WEDNESDAY,THURSDAY))
    val luckaVolnoVPa = new UcitelUciVUrciteDny(schoolSchedule,Teacher("Lucka"),Set(MONDAY,TUESDAY,WEDNESDAY,THURSDAY))
    val druzinarkaHana = new Druzinar(schoolSchedule,Teacher("Hana"),4)
    val ucitelNeuciVicNez5 = new UcitelNeuciVicNez5(schoolSchedule)

    val scheduledJobs: List[TeachersJob] = schoolSchedule.schoolSchedule.foldLeft(List[TeachersJob]())((coll, cs) => coll ++ cs.classSchedule.foldLeft(List[TeachersJob]())((ccoll, ds) => {
      ccoll ++ ds.filter(_ != null)
    }))

    println("Total jobs = "+(teachersJobs.foldLeft(0)((total,tj) => total + tj.classHour.classes.size)))
    println("Scheduled jobs = "+scheduledJobs.size)
    println(teachersJobs diff scheduledJobs)

    println("odpol = "+(odpol.valid))
    println("volna = "+(volna.valid))
    println("prvniDruha = "+(prvniDruha.valid))
//    println("neprerusene = "+(neprerusene.valid))
    println("exklusivitaUcitele = "+(exklusivitaUcitele.valid))
    println("vvVzdyPoSobe = "+(vvVzdyPoSobe.valid))
    println("dvojHodinoveNePoSobe = "+(dvojHodinoveNePoSobe.valid))
    println("stejnyNeVeStejnyDen = "+(stejnyNeVeStejnyDen.h))
    println("hlavniPredmety = "+(hlavniPredmety.h))
    println("spojenePredmety = "+(spojenePredmety.h))
    println("spravnePrirazene = "+(spravnePrirazene.valid))

    println("reditelUciJenVPoAPa = "+(reditelUciJenVPoAPa.valid))
    println("evaVolnoVUtAPa = "+(evaVolnoVUtAPa.valid))
    println("luckaVolnoVPa = "+(luckaVolnoVPa.valid))
    println("ucitelNeuciVicNez5 = "+(ucitelNeuciVicNez5.valid))
  }

}
