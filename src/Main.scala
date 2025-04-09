import Util._

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Main {
  def pedir_ruta(): String = {
    println(s"El directorio actual es: ${System.getProperty("user.dir")}")
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

    menu()
    val opcion = StdIn.readInt()
    opcion match {
      case 1 => { //Caso Blanco y Negro
        val tryImagenBN: Try[ImageData] = tryImagen match {
          case Success(imagen) => Success(convertirABlancoYNegro(imagen))
          case failure@Failure(_) => failure
        }

        tryImagenBN match {
          case Success(imagenBN) =>
            val rutaSalida = "./img/bn_" + new java.io.File(ruta).getName
            if (escribirImagenBMP(imagenBN, rutaSalida)) {
              println(s"Imagen en blanco y negro guardada como: $rutaSalida")
            } else {
              println("Error al guardar la imagen")
            }

          case Failure(e) =>
            println(s"Error al procesar la imagen: ${e.getMessage}")
        }
      }
      case 2 => { // Caso Pixelar
        println("Opción 2 elegida")
        println("Elija el tamaño de pixelado: ")
        val tamPixelado = StdIn.readInt()
        val tryImagenPixelada: Try[ImageData] = tryImagen match {
          case Success(imagen) => Success(pixelar(imagen, tamPixelado))
          case failure@Failure(_) => failure
        }

        tryImagenPixelada match {
          case Success(imagenPixelada) =>
//            if (imagenPixelada.pixeles.exists(_.isEmpty)) {
//              println("Error: Hay filas vacías en la imagen pixelada.")
//            }
//            println("No tiene filas vacías ")
//          println("Dimensiones imagen pixelada: " + imagenPixelada.ancho + "x" + imagenPixelada.alto)
            val rutaSalida = "./img/pixelada_" + new java.io.File(ruta).getName
            if (escribirImagenBMP(imagenPixelada, rutaSalida)) {
              println(s"Imagen pixelada guardada como: $rutaSalida")
            } else {
              println("Error al guardar la imagen")
            }

          case Failure(e) =>
            println(s"Error al procesar la imagen: ${e.getMessage}")
        }

      }
      case 3 => { // Caso Filtrar Colores
        println("Opción 3 elegida")
        print("Elija el factor de magnitud: ")
        val factorMagnitud = StdIn.readDouble()
        print("Elija el umbral: ")
        val umbral = StdIn.readInt()

        val tryImagenFiltrada: Try[(ImageData, ImageData, ImageData, contadorColores)] = tryImagen match {
          case Success(imagen) => Success(identificarColores(imagen, factorMagnitud, umbral))
          case Failure(e) => Failure(new Exception("Error al procesar la imagen", e))
        }

        tryImagenFiltrada match {
          case Success((imagenRoja, imagenVerde, imagenAzul, contador)) =>
            val rutaBase = "./img/"
            val nombreArchivo = new java.io.File(ruta).getName

            val rutaRoja = rutaBase + "roja_" + nombreArchivo
            val rutaVerde = rutaBase + "verde_" + nombreArchivo
            val rutaAzul = rutaBase + "azul_" + nombreArchivo

            if (escribirImagenBMP(imagenRoja, rutaRoja)) {
              println(s"Imagen filtrada roja guardada como: $rutaRoja")
            } else {
              println("Error al guardar la imagen roja")
            }

            if (escribirImagenBMP(imagenVerde, rutaVerde)) {
              println(s"Imagen filtrada verde guardada como: $rutaVerde")
            } else {
              println("Error al guardar la imagen verde")
            }

            if (escribirImagenBMP(imagenAzul, rutaAzul)) {
              println(s"Imagen filtrada azul guardada como: $rutaAzul")
            } else {
              println("Error al guardar la imagen azul")
            }

          case Failure(e) =>
            println(s"Error al procesar la imagen: ${e.getMessage}")
        }

      }
      case 4 => { // Salir
        println("Saliendo...")
        System.exit(0)
      }
      case _ => {
        println("No se reconoce el caracter introducido")
        System.exit(0) // Por ahora, hay que introducir el bucle
      }
    }
  }
}

