/**
  * Created by anton on 12.07.17.
  */
import java.awt.image.BufferedImage
import java.io._
import java.io.File
import javax.imageio.ImageIO

import scalafx.scene.image.{ImageView, WritableImage}
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.embed.swing.SwingFXUtils
import scalafx.event.{ActionEvent, EventDispatcher}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane, FlowPane}
import scalafx.scene.paint.Stop.sfxStop2jfx
import scalafx.scene.paint.{Color, CycleMethod, LinearGradient, Stop}
import scalafx.scene.shape.{LineTo, MoveTo, Path, Rectangle}
import scalafx.scene.{Group, Scene}
import scalafx.scene.paint.Color._
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import org.scilab.forge.jlatexmath.Box
import org.scilab.forge.jlatexmath.TeXConstants
import org.scilab.forge.jlatexmath.TeXFormula
import org.scilab.forge.jlatexmath.TeXIcon

import scala.collection.mutable.ListBuffer
import scalafx.stage.WindowEvent


/**
  * Example adapted from code showed in [[http://docs.oracle.com/javafx/2/canvas/jfxpub-canvas.htm]].
  *
  */

case class Point(x: Double, y: Double) {

  def distanceTo(p: Point) = {
    math.hypot(x-p.x, y-p.y)
  }

  override def toString() = {
    x.toString + ";" + y.toString
  }

  def getX() = x

  def getY() = y
}

object Main extends JFXApp {

  val canv_width = 100
  val canv_height = 100

  val approxPoints = 32

  val canvas = new Canvas(canv_width, canv_height)

  val gridPane = new GridPane {
    padding = Insets(2)
    hgap = 5
    vgap = 5
  }

  // Draw background with gradient
  val rect = new Rectangle {
    height = canv_height
    width = canv_width
    fill = LightGreen
  }

  val currCharPath = new ListBuffer[Point]
  val charPaths = new ListBuffer[ListBuffer[Point]]()

  val btnSave = new Button {
    text = "save"
  }

  var alphabet = List("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  var counter = 0
  val latexDescriptor = "a_to_z"

  val imageView = new ImageView()
  imageView.image = updateTex(alphabet(counter).toString)
  gridPane.add(imageView, 0, 0)

  val pw = new PrintWriter(
    new File(s"data/paths/paths_${latexDescriptor}.csv" )
  )

  val header = (0 until approxPoints*2 toList)
  pw.println(header.mkString(";") + ";label")

  btnSave.onAction = (event: ActionEvent) => {
    var postFix = 0
    var file = new File(s"data/pics/${latexDescriptor}_${alphabet(counter)}_$postFix.png")
    while (file.exists) {
      postFix += 1
      file = new File(s"data/pics/${latexDescriptor}_${alphabet(counter)}_$postFix.png")
    }
    val writableImage = new WritableImage(canv_width, canv_height)
    canvas.snapshot(null, writableImage)
    val renderedImage = SwingFXUtils.fromFXImage(writableImage, null)

    ImageIO.write(renderedImage, "png", file)

    charPaths += currCharPath
    val path = charPaths(0)

    val approxPath = PathOptimizer.smartSubsetPoints(path, 32)

    //val doublePath = coordToDoubleArr(approxPath)
    //val scaledArr = scalePath(doublePath)

    pw.print(approxPath.mkString(";"))
    pw.println(";" + alphabet(counter).toString)


    counter += 1
    if (counter == alphabet.length){
      counter = 0
    }
    imageView.image = updateTex(alphabet(counter).toString)

    currCharPath.clear()
    reset(Color.White, charPaths)
  }

  val rootPane = new Group
  rootPane.children = List(rect, canvas)


  gridPane.add(rootPane, 0, 1)
  gridPane.add(btnSave, 0, 2)

  stage = new PrimaryStage {
    title = "wacom2latex"
    scene = new Scene(canv_width, canv_height + 80) {
      content = gridPane
    }
  }

  //canvas.translateX = 100
  //canvas.translateY = 100

  val gc = canvas.graphicsContext2D

  reset(White, charPaths)

  stage.onCloseRequest = (e: WindowEvent) => {
    pw.close
  }


  canvas.onMousePressed = (e: MouseEvent) => {
    gc.beginPath()
    gc.rect(e.x, e.y, 1, 1)
    gc.moveTo(e.x, e.y)
    gc.stroke()

    currCharPath += new Point(e.x, e.y)
  }

  canvas.onMouseDragged = (e: MouseEvent) => {
    if (e.x < canv_width && e.y < canv_height && e.x > 0 && e.y > 0) {
      gc.lineTo(e.x, e.y)
      gc.stroke()
      currCharPath += new Point(e.x, e.y)
    }
  }

  canvas.onMouseReleased = (e: MouseEvent) => {
    //charPaths += currCharPath.toList
    //currCharPath.clear()
  }

  canvas.onMouseClicked = (e: MouseEvent) => {
    if (e.clickCount > 1) {
      reset(Color.White, charPaths)
    }
  }

  private def reset(color: Color, charPaths: ListBuffer[ListBuffer[Point]]) = {
    gc.fill = color
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get)
    charPaths.clear()
  }

  private def updateTex(texText: String): WritableImage = {
    val tex = new TeXFormula(texText)
    val awtImage = tex.createBufferedImage(TeXConstants.STYLE_TEXT, 26, java.awt.Color.BLACK, null)
    val fxImage = SwingFXUtils.toFXImage(awtImage.asInstanceOf[BufferedImage], null)
    fxImage
  }

  def scalePath(approxPath: Array[Double]) : Array[Double] = {
    val scaledPath = Array.ofDim[Double](approxPath.length)

    val max = approxPath.max + 1.0
    for (i <- approxPath.indices){
      scaledPath(i) = (approxPath(i) + 1.0) / max
    }

    scaledPath
  }

  def coordToDoubleArr(coordArr: Array[Point]) : Array[Double] = {
    val doubleArr = Array.ofDim[Double](coordArr.length * 2)

    for(i <- coordArr.indices){
      doubleArr(i*2) = coordArr(i).getX()
      doubleArr((i*2)+1) = coordArr(i).getY()
    }

    doubleArr
  }

}