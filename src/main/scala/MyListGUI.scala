import scalafx.Includes.{jfxDialogPane2sfx, jfxNode2sfx}
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.paint.Color.White
import scalafx.scene.text.Text

import scala.util.Random

//noinspection DuplicatedCode
object MyListGUI extends JFXApp3 {
  override def start(): Unit = {
    stage = new PrimaryStage {
      var N = 5
      var integerList = new MyList[Integer](N)
      var point2DList = new MyList[Point2D](N)

      title.value = "Lab2"
      width = 830
      height = 420

      val toggleGroup = new ToggleGroup()

      val integerRadioButton: RadioButton = new RadioButton("Integer")
      integerRadioButton.setSelected(true)
      integerRadioButton.setToggleGroup(toggleGroup)
      integerRadioButton.setLayoutX(20)
      integerRadioButton.setLayoutY(20)

      val point2DRadioButton: RadioButton = new RadioButton("Point2D")
      point2DRadioButton.setSelected(false)
      point2DRadioButton.setToggleGroup(toggleGroup)
      point2DRadioButton.setLayoutX(100)
      point2DRadioButton.setLayoutY(20)

      val nText: Text = new Text("N = ")
      nText.setLayoutX(180)
      nText.setLayoutY(33)

      val nTextField = new TextField()
      nTextField.setLayoutX(205)
      nTextField.setLayoutY(18)
      nTextField.setPrefSize(40, 20)
      nTextField.setText("5")

      val randomButton = new Button("Добавить 10 случайных элементов")
      randomButton.setLayoutX(20)
      randomButton.setLayoutY(60)

      val addButton = new Button("Добавить в конец")
      addButton.setLayoutX(20)
      addButton.setLayoutY(100)

      val addAtIndexButton = new Button("Добавить по индексу")
      addAtIndexButton.setLayoutX(20)
      addAtIndexButton.setLayoutY(140)

      val multiplyButton = new Button("Умножить все на число")
      multiplyButton.setLayoutX(20)
      multiplyButton.setLayoutY(180)

      val deleteAtIndexButton = new Button("Удалить по индексу")
      deleteAtIndexButton.setLayoutX(20)
      deleteAtIndexButton.setLayoutY(220)

      val sortButton = new Button("Сортировать")
      sortButton.setLayoutX(20)
      sortButton.setLayoutY(260)

      val saveToFileButton = new Button("Сохранить в файл")
      saveToFileButton.setLayoutX(20)
      saveToFileButton.setLayoutY(300)

      val loadFromFileButton = new Button("Загрузить из файла")
      loadFromFileButton.setLayoutX(20)
      loadFromFileButton.setLayoutY(340)

      val listTextArea = new TextArea
      listTextArea.setLayoutX(300)
      listTextArea.setLayoutY(20)
      listTextArea.setEditable(false)
      listTextArea.setWrapText(true)
      listTextArea.setPrefSize(500, 270)

      val vectorTextArea = new TextArea
      vectorTextArea.setLayoutX(300)
      vectorTextArea.setLayoutY(300)
      vectorTextArea.setEditable(false)
      vectorTextArea.setWrapText(true)
      vectorTextArea.setPrefSize(500, 70)

      scene = new Scene {
        fill = White
        content = List(integerRadioButton, point2DRadioButton, nText, nTextField, randomButton, addButton, addAtIndexButton, multiplyButton, deleteAtIndexButton, sortButton, saveToFileButton, loadFromFileButton, listTextArea, vectorTextArea)
      }

      integerRadioButton.onAction = _ => {
        if (integerRadioButton.selected.value) {
          integerList = new MyList[Integer](N)
          listTextArea.clear()
          vectorTextArea.clear()
        }
      }

      point2DRadioButton.onAction = _ => {
        if (point2DRadioButton.selected.value) {
          point2DList = new MyList[Point2D](N)
          listTextArea.clear()
          vectorTextArea.clear()
        }
      }

      nTextField.text.onChange { (_, _, newValue) =>
        if (newValue.trim.isEmpty) {
        } else {
          try {
            val newN: Int = newValue.toInt
            if (newN > 0) {
              N = newN
              integerList = new MyList[Integer](N)
              point2DList = new MyList[Point2D](N)
            } else {
              //showErrorAlert("Ошибка ввода")
              nTextField.setText(Integer.toString(N))
            }
          } catch {
            case _: NumberFormatException =>
              //showErrorAlert("Ошибка ввода")
              nTextField.setText(Integer.toString(N))
          }
        }
      }

      private def updateListTextArea[T](list: MyList[T]): Unit = {
        listTextArea.setText(list.toString)
        vectorTextArea.setText(list.vectorToString())
      }

      randomButton.onAction = _ => {
        if (integerRadioButton.selected.value) {
          val random = new Random
          for (_ <- 0 until 10) {
            val randomValue = random.nextInt(100)
            integerList.add(randomValue)
          }
          updateListTextArea(integerList)
        } else if (point2DRadioButton.selected.value) {
          val random = new Random
          for (_ <- 0 until 10) {
            val x = random.nextInt(100)
            val y = random.nextInt(100)
            point2DList.add(new Point2D(x, y))
          }
          updateListTextArea(point2DList)
        }
      }

      addButton.onAction = _ => {
        case class Result(value1: String, value2: String)
        val dialog = new Dialog[Result]() {
          initOwner(stage)
          title = "Добавить в конец"
        }
        val okButtonType = new ButtonType("OK", ButtonData.OKDone)
        dialog.dialogPane().buttonTypes = Seq(okButtonType, ButtonType.Cancel)
        val value1TextField = new TextField()
        value1TextField.setMaxSize(50, 20)
        val value2TextField = new TextField()
        value2TextField.setMaxSize(50, 20)
        val grid = new GridPane() {
          alignment = Pos.Center
          if (integerRadioButton.selected.value) {
            add(new Label("Значение: "), 0, 0)
            add(value1TextField, 1, 0)
          } else if (point2DRadioButton.selected.value) {
            add(new Label("X = "), 0, 0)
            add(value1TextField, 1, 0)
            add(new Label("Y = "), 0, 1)
            add(value2TextField, 1, 1)
          }
        }
        val okButton = dialog.dialogPane().lookupButton(okButtonType)
        okButton.disable = true
        value1TextField.text.onChange { (_, _, newValue) =>
          okButton.disable = newValue.trim().isEmpty
        }
        dialog.dialogPane().content = grid
        Platform.runLater(value1TextField.requestFocus())
        dialog.resultConverter = dialogButton =>
          if (dialogButton == okButtonType)
            Result(value1TextField.text(), value2TextField.text())
          else
            null
        val result = dialog.showAndWait()
        result match {
          case Some(Result(value1Res, value2Res)) =>
            if (integerRadioButton.selected.value) {
              integerList.add(value1Res.toInt)
              updateListTextArea(integerList)
            } else if (point2DRadioButton.selected.value) {
              val x = value1Res.toDouble
              val y = value2Res.toDouble
              point2DList.add(new Point2D(x, y))
              updateListTextArea(point2DList)
            }
        }
      }

      addAtIndexButton.onAction = _ => {
        case class Result(value1: String, value2: String, index: String)
        val dialog = new Dialog[Result]() {
          initOwner(stage)
          title = "Добавить в конец"
        }
        val okButtonType = new ButtonType("OK", ButtonData.OKDone)
        dialog.dialogPane().buttonTypes = Seq(okButtonType, ButtonType.Cancel)
        val indexTextField = new TextField()
        indexTextField.setMaxSize(50, 20)
        val value1TextField = new TextField()
        value1TextField.setMaxSize(50, 20)
        val value2TextField = new TextField()
        value2TextField.setMaxSize(50, 20)
        val grid = new GridPane() {
          alignment = Pos.Center
          add(new Label("Индекс: "), 0, 0)
          add(indexTextField, 1, 0)
          if (integerRadioButton.selected.value) {
            add(new Label("Значение: "), 0, 1)
            add(value1TextField, 1, 1)
          } else if (point2DRadioButton.selected.value) {
            add(new Label("X = "), 0, 1)
            add(value1TextField, 1, 1)
            add(new Label("Y = "), 0, 2)
            add(value2TextField, 1, 2)
          }
        }
        val okButton = dialog.dialogPane().lookupButton(okButtonType)
        okButton.disable = true
        value1TextField.text.onChange { (_, _, newValue) =>
          okButton.disable = newValue.trim().isEmpty
        }
        dialog.dialogPane().content = grid
        Platform.runLater(value1TextField.requestFocus())
        dialog.resultConverter = dialogButton =>
          if (dialogButton == okButtonType)
            Result(value1TextField.text(), value2TextField.text(), indexTextField.text())
          else
            null
        val result = dialog.showAndWait()
        result match {
          case Some(Result(value1Res, value2Res, indexRes)) =>
            if (integerRadioButton.selected.value) {
              integerList.insert(indexRes.toInt, value1Res.toInt)
              updateListTextArea(integerList)
            } else if (point2DRadioButton.selected.value) {
              val x = value1Res.toDouble
              val y = value2Res.toDouble
              point2DList.insert(indexRes.toInt, new Point2D(x, y))
              updateListTextArea(point2DList)
            }
        }
      }

      multiplyButton.onAction = _ => {
        case class Result(value: String)
        val dialog = new Dialog[Result]() {
          initOwner(stage)
          title = "Умножить все на число"
        }
        val okButtonType = new ButtonType("OK", ButtonData.OKDone)
        dialog.dialogPane().buttonTypes = Seq(okButtonType, ButtonType.Cancel)
        val value1TextField = new TextField()
        value1TextField.setMaxSize(50, 20)
        val grid = new GridPane() {
          alignment = Pos.Center
          add(new Label("Значение: "), 0, 0)
          add(value1TextField, 1, 0)
        }
        val okButton = dialog.dialogPane().lookupButton(okButtonType)
        okButton.disable = true
        value1TextField.text.onChange { (_, _, newValue) =>
          okButton.disable = newValue.trim().isEmpty
        }
        dialog.dialogPane().content = grid
        Platform.runLater(value1TextField.requestFocus())
        dialog.resultConverter = dialogButton =>
          if (dialogButton == okButtonType)
            Result(value1TextField.text())
          else
            null
        val result = dialog.showAndWait()
        result match {
          case Some(Result(value1Res)) =>
            if (integerRadioButton.selected.value) {
              val updatedIntegerList = new MyList[Integer](N)
              integerList.forEach((v: Integer) => {
                updatedIntegerList.add(v * value1Res.toInt)
                v
              })
              integerList = updatedIntegerList
              updateListTextArea(integerList)
            } else if (point2DRadioButton.selected.value) {
              val updatedPoint2DList = new MyList[Point2D](N)
              point2DList.forEach((v: Point2D) => {
                v.setX(v.getX * value1Res.toDouble)
                v.setY(v.getY * value1Res.toDouble)
                updatedPoint2DList.add(v)
                v
              })
              point2DList = updatedPoint2DList
              updateListTextArea(point2DList)
            }
        }
      }

      deleteAtIndexButton.onAction = _ => {
        case class Result(value: String)
        val dialog = new Dialog[Result]() {
          initOwner(stage)
          title = "Удалить по индексу"
        }
        val okButtonType = new ButtonType("OK", ButtonData.OKDone)
        dialog.dialogPane().buttonTypes = Seq(okButtonType, ButtonType.Cancel)
        val value1TextField = new TextField()
        value1TextField.setMaxSize(50, 20)
        val grid = new GridPane() {
          alignment = Pos.Center
          add(new Label("Индекс: "), 0, 0)
          add(value1TextField, 1, 0)
        }
        val okButton = dialog.dialogPane().lookupButton(okButtonType)
        okButton.disable = true
        value1TextField.text.onChange { (_, _, newValue) =>
          okButton.disable = newValue.trim().isEmpty
        }
        dialog.dialogPane().content = grid
        Platform.runLater(value1TextField.requestFocus())
        dialog.resultConverter = dialogButton =>
          if (dialogButton == okButtonType)
            Result(value1TextField.text())
          else
            null
        val result = dialog.showAndWait()
        result match {
          case Some(Result(value1Res)) =>
            if (integerRadioButton.selected.value) {
              integerList.remove(value1Res.toInt)
              updateListTextArea(integerList)
            } else if (point2DRadioButton.selected.value) {
              point2DList.remove(value1Res.toInt)
              updateListTextArea(point2DList)
            }
        }
      }

      sortButton.onAction = _ => {
        if (integerRadioButton.selected.value) {
          integerList.mergeSort()
          updateListTextArea(integerList)
        } else if (point2DRadioButton.selected.value) {
          point2DList.mergeSort()
          updateListTextArea(point2DList)
        }
      }

      saveToFileButton.onAction = _ => {
        if (integerRadioButton.selected.value) {
          integerList.serializeToFile("IntegerList.json")
          showAlertDialog("Данные успешно сохранены в файл IntegerList.json")
        } else if (point2DRadioButton.selected.value) {
          point2DList.serializeToFile("Point2DList.json")
          showAlertDialog("Данные успешно сохранены в файл Point2DList.json")
        }
      }

      loadFromFileButton.onAction = _ => {
        if (integerRadioButton.selected.value) {
          val tmpIntList = FileReader.deserializeFromFile("IntegerList.json", classOf[Integer])
          N = tmpIntList.getN
          nTextField.setText(String.valueOf(N))
          integerList = tmpIntList
          updateListTextArea(integerList)
        } else if (point2DRadioButton.selected.value) {
          val tmpPoint2DList = FileReader.deserializeFromFile("Point2DList.json", classOf[Point2D])
          N = tmpPoint2DList.getN
          nTextField.setText(String.valueOf(N))
          point2DList = tmpPoint2DList
          updateListTextArea(point2DList)
        }
      }

      def showAlertDialog(text: String): Unit = {
        val alert = new Alert(AlertType.Information)
        alert.setTitle("Сохранение в файл")
        alert.setHeaderText(null)
        alert.setContentText(text)
        alert.showAndWait
      }
    }
  }
}