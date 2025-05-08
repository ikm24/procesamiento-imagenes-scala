import Util_Cloud._
import Util._

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Main {

  /**
   * Función que pide al usuario la ruta de la imagen a procesar.
   * @return Ruta de la imagen a procesar
   */
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

  /**
   * Función que pregunta al usuario si quiere subir la imagen a la nube.
   * @return true si el usuario quiere subir la imagen, false en caso contrario
   */
  def preguntar_subir_cloud(): Boolean = {
    val resp = StdIn.readLine("¿Quieres subir la imagen a la nube? (s/n): ")
    return resp == "s" || resp == "S"
  }

  /**
   * Función que pregunta el nombre del usuario.
   * @return Nombre introducido por el usuario
   */
  def preguntar_nombre_usuario(): String = {
    StdIn.readLine("¿Cuál es tu nombre de usuario? ")
  }

  /**
   * Función para manejar la subida de imágenes al cloud.
   * @param imagen Imagen procesada en formato ImageData.
   * @param rutaSalida Ruta donde se guardó la imagen procesada.
   * @param tipo_operacion Tipo de operación realizada (Blanco y Negro, Pixelado, etc.).
   */
  def manejarSubidaCloud(imagen: ImageData, rutaSalida: String, tipoOperacion: String,
                         nombreUsuario: String, factorMagnitud : Option[Double], umbral: Option[Int]): Unit = {


    // Contar los píxeles de cada color
    val contador = contarColoresRGB(imagen,factorMagnitud, umbral)
    val nombreImagen = extraer_nombre_imagen(rutaSalida)
    println(s"Colores contados en la imagen:")
    println(s"Rojo: ${contador.rojo}")
    println(s"Verde: ${contador.verde}")
    println(s"Azul: ${contador.azul}")
    println(s"Total de píxeles: ${contador.contador}")
    println(s"Subiendo la imagen al cloud para el usuario: $nombreUsuario...")
    publicarImagen(nombreUsuario, nombreImagen, tipoOperacion,
      contador.rojo, contador.verde, contador.azul)

  }


  /**
   * Función que muestra el menú de opciones al usuario.
   */
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
    var tipo_operacion = ""
    val opcion = StdIn.readInt()
    opcion match {
      case 1 => { //Caso Blanco y Negro
        tipo_operacion = "Blanco y Negro"
        val tryImagenBN: Try[ImageData] = tryImagen match {
          case Success(imagen) => Success(convertirABlancoYNegro(imagen))
          case failure@Failure(_) => failure
        }

        tryImagenBN match {
          case Success(imagenBN) =>
            {
              val rutaSalida = "./img/bn_" + new java.io.File(ruta).getName
              if (escribirImagenBMP(imagenBN, rutaSalida)) {
                println(s"Imagen en blanco y negro guardada como: $rutaSalida")
              } else {
                println("Error al guardar la imagen")
              }

              if(preguntar_subir_cloud()){
                val nombreUsuario = preguntar_nombre_usuario()
                manejarSubidaCloud(imagenBN, rutaSalida, tipo_operacion, nombreUsuario, None, None)
              }
            }

          case Failure(e) =>
            println(s"Error al procesar la imagen: ${e.getMessage}")
        }
      }
      case 2 => { // Caso Pixelar
        tipo_operacion = "Pixelado"
        println("Opción 2 elegida")
        println("Elija el tamaño de pixelado: ")
        val tamPixelado = StdIn.readInt()
        val tryImagenPixelada: Try[ImageData] = tryImagen match {
          case Success(imagen) => Success(pixelar(imagen, tamPixelado))
          case failure@Failure(_) => failure
        }

        tryImagenPixelada match {
          case Success(imagenPixelada) => {
            val rutaSalida = "./img/pixelada_" + new java.io.File(ruta).getName
            if (escribirImagenBMP(imagenPixelada, rutaSalida)) {
              println(s"Imagen pixelada guardada como: $rutaSalida")
            } else {
              println("Error al guardar la imagen")
            }
            if(preguntar_subir_cloud()){
              // Preguntar el nombre de usuario
              val nombreUsuario = preguntar_nombre_usuario()
              manejarSubidaCloud(imagenPixelada, rutaSalida, tipo_operacion, nombreUsuario, None, None)
            }
          }

          case Failure(e) =>
            println(s"Error al procesar la imagen: ${e.getMessage}")
        }

      }
      case 3 => { // Caso Filtrar Colores
        tipo_operacion = "Filtrar Colores"
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
          case Success((imagenRoja, imagenVerde, imagenAzul, contador)) => {
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

            if(preguntar_subir_cloud()){
              // Preguntar el nombre de usuario
              val nombreUsuario = preguntar_nombre_usuario()
              // Para la imagen roja
              manejarSubidaCloud(imagenRoja, rutaRoja, tipo_operacion, nombreUsuario, Some(factorMagnitud), Some(umbral))
              // Para la imagen verde
              manejarSubidaCloud(imagenVerde, rutaVerde, tipo_operacion, nombreUsuario, Some(factorMagnitud), Some(umbral))
              // Para la imagen azul
              manejarSubidaCloud(imagenAzul, rutaAzul, tipo_operacion, nombreUsuario, Some(factorMagnitud), Some(umbral))
            }
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
        println("No se reconoce el carácter introducido")
        System.exit(0)
      }
    }
  }
}

