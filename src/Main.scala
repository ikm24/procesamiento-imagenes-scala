import Util._

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Main {
  def pedir_ruta(): String = {
    println(s"El directorio actual es: $IMAGEN_DEFECTO")
    println(s"Introduzca la ruta de la imagen: \n(Pulse Enter para ruta por defecto): $IMAGEN_DEFECTO")

    val ruta = StdIn.readLine()

    if (ruta == "") {
       IMAGEN_DEFECTO
    } else {
      if (!esBMPValido(ruta)) {
        println("La ruta introducida no es válida")
        println(s"Cargando ruta predeterminada: $IMAGEN_DEFECTO")
        IMAGEN_DEFECTO
      } else {
        println("La ruta introducida es válida")
        ruta
      }
    }

  }
  def menu(): Unit = {
    println("Opciones:")
    println("\t\t(1). Convertir a Blanco y Negro")
    println("\t\t(2). Pixelar")
    println("\t\t(3). Filtrar Colores")
    println("\t\t(4). Salir")
  }
  def main(args: Array[String]): Unit = {
    val ruta = pedir_ruta()
    val tryImagen: Try[ImageData] = leerImagenBMP(ruta)

    val tryImagenBN: Try[ImageData] = tryImagen match {
      case Success(imagen) => Success(convertirABlancoYNegro(imagen))
      case failure@Failure(_) => failure
    }

    tryImagenBN match {
      case Success(imagenBN) =>
        val rutaSalida = "bn_" + new java.io.File(ruta).getName
        if (escribirImagenBMP(imagenBN, rutaSalida)) {
          println(s"Imagen en blanco y negro guardada como: $rutaSalida")
        } else {
          println("Error al guardar la imagen")
        }

      case Failure(e) =>
        println(s"Error al procesar la imagen: ${e.getMessage}")
    }
  }
}

