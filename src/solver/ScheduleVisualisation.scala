package solver

import scala.swing.ScrollPane.BarPolicy
import scala.swing._
import scala.swing.event.ButtonClicked

object ScheduleVisualisation extends App {
  val schedule = Input.readScheduleFromFile("schedule.txt")

  val v = new ScheduleVisualisation
  v.refresh(schedule)
}

object RowDialog {
  var loc:Option[Point] = None
}

class RowDialog(pTitle: String, options:List[Array[String]]) extends Dialog {

  modal = true

  title = pTitle

  var selected:Option[Int] = None

  contents = new ScrollPane() {
    verticalScrollBarPolicy = BarPolicy.Always
    private val length: Int = options.maxBy(_.length).length
    contents = new GridPanel(options.size+1, length) {
      contents+=new Label("")
      (FIRST_GRADE to LAST_GRADE).foreach(g => contents+=new Label(""+(g+1)))
      options.zipWithIndex.foreach(o => {
        val button = new Button("" + (o._2+1))
        button.reactions += {
          case e: ButtonClicked =>
            selected = Some(o._2)
            RowDialog.loc = Some(new Point(
              RowDialog.this.location.x,
              RowDialog.this.location.y
            ))
            close()
        }
        contents += button
        o._1.padTo(length,"").foreach(contents += new Label(_))
      })
    }
  }
  if (!RowDialog.loc.isDefined) centerOnScreen()
  else location = new Point(RowDialog.loc.get.x,RowDialog.loc.get.y)

  preferredSize = new Dimension(800,800)
  open()

}

class ScheduleVisualisation {

  var labels:Array[Array[Array[Label]]] = (MONDAY to FRIDAY).map(d =>
    (FIRST_GRADE to LAST_GRADE).map(g =>
      (FIRST_HOUR to LAST_HOUR).map(h =>
        new Label()
      ).toArray
    ).toArray
  ).toArray;


  val mf = new MainFrame {
    title = "Rozvrh"

    override def closeOperation() {}

  }

  var scroll: ScrollPane = null

  mf.contents = new FlowPanel() {
    scroll = new ScrollPane() {

      verticalScrollBarPolicy = BarPolicy.Always

      preferredSize = new Dimension(1300, 750)

      contents = new GridPanel(5, 1) {
        (MONDAY to FRIDAY).foreach(d =>
          contents += new GridPanel(10, 9) {
            (FIRST_GRADE to LAST_GRADE).foreach(g =>
              (FIRST_HOUR to LAST_HOUR).foreach(h => {
                labels(d)(g)(h).text = "-"
                labels(d)(g)(h).opaque = true
                contents += labels(d)(g)(h)
              })
            )
            border = Swing.EmptyBorder(15, 10, 10, 10)
          }
        )
      }
    }
    contents+=scroll
    border = Swing.EmptyBorder(15, 10, 10, 10)

  }

  mf.pack();

  mf.visible = true;

  def refresh(schedule:SchoolSchedule) {
    (MONDAY to FRIDAY).foreach(d =>
        (FIRST_GRADE to LAST_GRADE).foreach(g =>
          (FIRST_HOUR to LAST_HOUR).foreach(h => {
            val x = Option(schedule.schoolSchedule(g).classSchedule(d)(h))
            labels(d)(g)(h).text = x.map(o => o.toString).getOrElse("-")
          })
        )
    )
  }

  def highlight(d:Int, h:Int, color:java.awt.Color): Unit = {
    (FIRST_GRADE to LAST_GRADE).foreach(g => {
      labels(d)(g)(h).background = color
      val bounds: Rectangle = labels(d)(g)(h).peer.getParent.getBounds
      scroll.peer.getViewport.scrollRectToVisible(bounds)
    })
  }

}
