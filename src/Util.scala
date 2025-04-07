import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.util.Try
object Util {

  val IMAGEN_DEFECTO = "../img/squirtle_squad_BMP_00.bmp"

  /**
   * Clase para representar la estructura de una cabecera de un
   * archivo BMP.
   * @param tamano Tamaño de la imagen
   * @param dataOffset Offset de los datos
   * @param ancho ancho de la imagen
   * @param alto alto de la imagen
   */
  case class BMPHeader(
                        tamano:Int,
                        dataOffset: Int,
                        ancho: Int,
                        alto: Int
                      )
  /**
   Clase para representar la estructura de un pixel
   en formato R, G, B
   */
  case class Pixel(
                    r: Int, g: Int, b: Int
                  )

  /**
   * Clase para representar los datos de una imagen BMP
   * @param ancho ancho de la imagen
   * @param alto alto de la imagen
   * @param pixeles los píxeles de la imagen
   */
  case class ImageData(
                        ancho: Int,
                        alto: Int,
                        pixeles: List[List[Pixel]]
                      )

  /**
   * Clase para llevar la cuenta de los píxeles de cada color
   * en la fase de la identificación de colores
   * @param rojo Contador de píxeles de color rojo
   * @param verde Contador de píxeles de color verde
   * @param azul Contador de píxeles de color azul
   * @param contador Contador general
   */
  case class contadorColores(
                              rojo: Int,
                              verde: Int,
                              azul: Int,
                              contador: Int
                            )

// Funciones propias
  /**
   * Función para invertir la lista de píxeles
   * @param list(List[Pixel]) lista de píxeles a invertir
   * @param acc(List[Pixel]) acumulador que lleva la lista invertir hasta el momento
   * @return (List[Pixel]) lista resultado
   */
  @tailrec
  def reverseLista(list: List[Pixel], acc: List[Pixel]): List[Pixel] = {
    list match {
      case Nil => acc
      case head::tail => reverseLista(tail, head :: acc)
    }
  }

  @tailrec
  def reverseImagen(list: List[List[Pixel]], acc: List[List[Pixel]] = Nil): List[List[Pixel]] = {
    list match {
      case Nil => acc
      case head :: tail => reverseImagen(tail, reverseLista(head, Nil) :: acc)
    }
  }
  def esBMPValido(ruta: String): Boolean = {
    val archivo = new File(ruta)

    // Verifica existencia, tipo de archivo y extensión
    val condicionesCumplidas = archivo.exists() &&
      archivo.isFile &&
      ruta.toLowerCase.endsWith(".bmp") &&
      Files.isReadable(Paths.get(ruta))

    condicionesCumplidas
  }

  /**
   * Función para leer una imagen BMP desde una ruta
   * @param ruta (String) ruta de la imagen BMP
   * @return ImageData la imagen ancho: Int, alto: Int, pixeles: List[List[Pixel]]
   */
  def leerImagenBMP(ruta: String):  Try[ImageData] = {
    Try {
      val imagen = ImageIO.read(new File(ruta))
      val ancho = imagen.getWidth
      val alto = imagen.getHeight

      @tailrec
      def leerFilas(y: Int, acc: List[List[Pixel]]): List[List[Pixel]] = {
        y match {
          case _ if y < 0 => acc
          case _ => {
            val fila = leerFila(imagen, ancho, y)
            leerFilas(y - 1, fila :: acc)
          }
        }
      }

      val pixeles = leerFilas(alto - 1, Nil)
      ImageData(ancho, alto, pixeles)
    }
  }

  /**
   * Función que procesa una fila en concreto
   * @param imagen(BufferedImage) La imagen BMP de entrada
   * @param ancho(Int) El ancho de dicha imagen
   * @param y(Int) El índice de fila que estamos considerando
   * @return (List[Pixel]) - Lista de píxeles procesados de dicha imagen
   */
  def leerFila(imagen: BufferedImage, ancho: Int, y: Int) : List[Pixel] = {

    /**
     * Función que procesa los píxeles de una fila
     * @param x(Int) La columna actual que estamos considerando
     * @param acc(List[Pixel]) El acumulador que lleva la lista de píxeles procesados hasta el momento
     * @return (List[Pixel]) Lista de píxeles
     */
    @tailrec
    def bucle(x: Int, acc: List[Pixel]): List[Pixel] = {
      x match {
        case _ if x >= ancho => acc
        case _ => {
          val rgb = imagen.getRGB(x, y)
          val r = (rgb >> 16) & 0xFF
          val g = (rgb >> 8) & 0xFF
          val b = rgb  & 0xFF
          bucle(x + 1, Pixel(r, g, b) :: acc)
        }
      }
    }

    reverseLista(bucle(0, Nil), Nil)  //se invierte ya que las imágenes BMP se leen al revés
  }

  def escribirImagenBMP(imagenData: ImageData, rutaDestino: String): Boolean = {
    Try {
      val bufferedImage = new BufferedImage(
        imagenData.ancho,
        imagenData.alto,
        BufferedImage.TYPE_INT_RGB
      )

      // Función tail-rec para procesar filas
      @tailrec
      def procesarFilas(y: Int): Unit = {
        if (y < imagenData.alto) {
          procesarPixelesEnFila(y, 0)
          procesarFilas(y + 1)
        }
      }

      // Función tail-rec para procesar píxeles en una fila
      @tailrec
      def procesarPixelesEnFila(y: Int, x: Int): Unit = {
        if (x < imagenData.ancho) {
          val pixel = imagenData.pixeles(y)(x)
          val rgb = (pixel.r << 16) | (pixel.g << 8) | pixel.b
          bufferedImage.setRGB(x, y, rgb)
          procesarPixelesEnFila(y, x + 1)
        }
      }

      // Inicia el procesamiento desde la fila 0
      procesarFilas(0)

      // Guarda la imagen
      ImageIO.write(bufferedImage, "bmp", new File(rutaDestino))
    }.isSuccess
  }

  /**
   * Convierte una imagen a blanco y negro usando recursión por la cola.
   * @param imagenData Imagen original en formato ImageData.
   * @return Nueva ImageData en escala de grises.
   */
  def convertirABlancoYNegro(imagenData: ImageData): ImageData = {
    /**
     * Aplica la fórmula de luminancia a un píxel.
     */
    def calcularGris(pixel: Pixel): Pixel = {
      val gris = (0.299f * pixel.r + 0.587f * pixel.g + 0.114f * pixel.b).toInt
      Pixel(gris, gris, gris)
    }

    /**
     * Procesa una fila de píxeles (tail-rec).
     */
    @tailrec
    def procesarFila(pixeles: List[Pixel], acc: List[Pixel]): List[Pixel] = {
      pixeles match {
        case Nil => reverseLista(acc,Nil)  // Invertir para mantener el orden original
        case head :: tail =>
          procesarFila(tail, calcularGris(head) :: acc)
      }
    }

    /**
     * Procesa todas las filas (tail-rec).
     */
    @tailrec
    def procesarFilas(filas: List[List[Pixel]], acc: List[List[Pixel]]): List[List[Pixel]] = {
      filas match {
        case Nil => reverseImagen(acc,Nil)
        case head :: tail =>
          procesarFilas(tail, procesarFila(head, Nil) :: acc)
      }
    }

    ImageData(
      imagenData.ancho,
      imagenData.alto,
      procesarFilas(imagenData.pixeles, Nil)
    )
  }
}
