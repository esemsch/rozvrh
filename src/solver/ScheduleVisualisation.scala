package solver

import java.awt.event.{MouseEvent, MouseAdapter}

import scala.swing.ScrollPane.BarPolicy
import scala.swing._
import scala.swing.event.{MousePressed, MouseClicked, ButtonClicked}

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
      (FIRST_GRADE to LAST_GRADE).foreach(g => contents+={
        val l = new Label(""+(g+1))
        l.horizontalAlignment = Alignment.Left
        l
      })
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
        o._1.padTo(length,"").foreach(s => contents += {
          val l = new Label(s)
          l.horizontalAlignment = Alignment.Left
          l
        })
      })
    }
  }
  if (!RowDialog.loc.isDefined) centerOnScreen()
  else location = new Point(RowDialog.loc.get.x,RowDialog.loc.get.y)

  preferredSize = new Dimension(800,400)
  maximumSize = new Dimension(1200,450)
  open()

}

object AuxObj extends App {
  val ss = Input.readScheduleFromFile("schedule.txt")
  ScheduleVisualisation.vis.refresh(ss)
}

object ScheduleVisualisation {

  val vis = new ScheduleVisualisation
}

class ScheduleVisualisation {

  var labels:Array[Array[Array[Label]]] = (MONDAY to FRIDAY).map(d =>
    (FIRST_GRADE to LAST_GRADE).map(g =>
      (FIRST_HOUR to LAST_HOUR).map(h => {
        val l = new Label()
        l.horizontalAlignment = Alignment.Left
        l
      }).toArray
    ).toArray
  ).toArray;


  val mf = new MainFrame {
    title = "Rozvrh"

  }

  val scroll: ScrollPane = new ScrollPane() {

    verticalScrollBarPolicy = BarPolicy.Always
    horizontalScrollBarPolicy = BarPolicy.Always

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
          border = Swing.EmptyBorder(3, 2, 2, 2)
        }
      )
    }
  }

  mf.contents = scroll

  mf.pack();

  mf.visible = true;

  def refresh(schedule:SchoolSchedule) {
    (MONDAY to FRIDAY).foreach(d =>
        (FIRST_GRADE to LAST_GRADE).foreach(g =>
          (FIRST_HOUR to LAST_HOUR).foreach(h => {
            val x = Option(schedule.schoolSchedule(g).classSchedule(d)(h))
            labels(d)(g)(h).text = x.map(o => o.toString).getOrElse("-")
            labels(d)(g)(h).peer.addMouseListener(new MouseAdapter {

              override def mouseClicked(e: MouseEvent): Unit = {
                val dialog = new Dialog()
                dialog.modal = true
                dialog.contents = new Label(Output.printDayAndHour(d, h))
                dialog.pack()
                dialog.centerOnScreen();
                dialog.open()
              }

            })
          }
        )
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