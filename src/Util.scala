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

  /**
   * Método para invertir una imagen formato List[List[Pixel]].
   * @param list Lista de listas de píxeles a invertir
   * @param acc Acumulador invertido hasta el momento
   * @return List[List[Pixel]] Lista de listas de píxeles invertida.
   */
  @tailrec
  def reverseImagen(list: List[List[Pixel]], acc: List[List[Pixel]] = Nil): List[List[Pixel]] = {
    list match {
      case Nil => acc
      case head :: tail => reverseImagen(tail, reverseLista(head, Nil) :: acc)
    }
  }


  /**
   * Concatena dos listas en una sola.
   * @param lista1 Primera lista.
   * @param lista2 Segunda lista.
   * @param acc Acumulador de elementos concatenados.
   * @Tparam A Tipo de elemento.
   * @return Lista resultante de la concatenación.
   */
  @tailrec
  def concat[A](lista1: List[A], lista2: List[A], acc: List[A] = Nil): List[A] = {
    lista1 match {
      case Nil => appendReverse(acc, lista2)
      case head :: tail => concat(tail, lista2, head :: acc)
    }
  }


  /**
   * Agrega una lista invertida al final de otra lista.
   * @param listaInvertida Lista invertida.
   * @param listaDestino Lista destino.
   * @return Lista resultante de la combinación.
   */
  @tailrec
  def appendReverse[A](listaInvertida: List[A], listaDestino: List[A]): List[A] = {
    listaInvertida match {
      case Nil => listaDestino
      case head :: tail => appendReverse(tail, head :: listaDestino)
    }
  }

  /**
   * Invierte una lista de elementos.
   * @param lista Lista a invertir.
   * @param acc Acumulador de elementos invertidos.
   * @return Lista invertida.
   */
  @tailrec
  def reverse[A](lista: List[A], acc: List[A] = Nil): List[A] = {
    lista match {
      case Nil => acc
      case head :: tail => reverse(tail, head :: acc)
    }
  }

  /**
   * Método para saber si un archivo es de tipo BMP
   * @param ruta (String) la ruta del archivo
   * @return Boolean: True si el archivo es de tipo BMP, False en caso contrario
   */
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
   * Función para calcular la longitud de una lista l
   * @param l Lista de elementos
   * @tparam A Tipo de elemento
   * @return (Int) Longitud de l
   */
  def longitud_lista[A](l:List[A]): Int = {
    l match {
      case Nil => 0
      case head::tail => 1 + longitud_lista(tail)
    }
  }

  /**
   * Función para tomar los primeros n elementos de l
   * @param n (Int) Número de elementos a tomar
   * @param l List: Lista de elementos
   * @tparam A Tipo de elemento
   * @return List[A] Lista con los primeros n elementos de l
   */
  def toma[A](n: Int, l: List[A]): List[A] = {
    l match{
      case Nil => Nil
      case _ if n == 0 => Nil
      case head::tail => head::toma(n-1, tail)
    }
  }

  /**
   * Función para eliminar los primeros n elementos de una lista
   * @param n Número de elementos a eliminar, empezando desde el principio
   * @param l Lista de los elementos
   * @tparam A Tipo de elemento
   * @return (List[A]) Lista sin los primeros n elementos de l
   */
  def deja[A](n:Int, l:List[A]): List[A] = {
    l match {
      case Nil => Nil
      case _ if n == 0 => l
      case head::tail => deja(n-1, tail)
    }
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

    bucle(0, Nil)  //se invierte ya que las imágenes BMP se leen al revés
  }

  /**
   * Método para guardar una imagen tipo BMP
   * @param imagenData Los datos de la imagen a guardar
   * @param rutaDestino La ruta donde guardar la imagen
   * @return (Boolean) False en caso de error
   */
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
   * Fase 01: Convierte una imagen a blanco y negro usando recursión.
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


  /**
   * Fase 02: Pixelar una imagen dada usando recursión.
   * @param imagenData Imagen original en formato ImageData
   * @param tamanoPixel Tamaño de pixelado
   * @return Nueva Imagen pixelada.
   */

  def pixelar(imagenData: ImageData, tamanoPixel: Int): ImageData = {
    val ancho = imagenData.ancho
    val alto = imagenData.alto

    /**
     * Calcula el color promedio de un bloque de píxeles en la imagen.
     * @param startX Coordenada X inicial del bloque.
     * @param startY Coordenada Y inicial del bloque.
     * @param tamano Tamaño del bloque (en píxeles).
     * @param imagenData Datos de la imagen original.
     * @return Pixel con el color promedio del bloque.
     */
    @tailrec
    def calcularColoresPromedio(y: Int, x: Int, acc: List[List[Pixel]]): List[List[Pixel]] = {
      if (y >= alto) acc                                                      //si nos pasamos de los límites devolver el acumulador
      // si nos pasamos de los límites en la fila, ir a la siguiente fila
      else if (x >= ancho) calcularColoresPromedio(y + tamanoPixel, 0, acc)
      // si nos pasamos de los límites en la columna, ir a la siguiente columna
      else if (x + tamanoPixel > ancho) calcularColoresPromedio(y, x + tamanoPixel, acc)
      else {
        // Calcular el color promedio del bloque
        val colorPromedio = calcularColorMedioBloque(x, y, tamanoPixel, imagenData)
        // Si es el primer bloque, inicializar la fila
        // Si no es el primer bloque, agregar el color promedio al inicio de la fila
        val nuevaFila = if (x == 0) List(colorPromedio) else acc.head match {
          case existente :: resto => colorPromedio :: existente :: resto
          case Nil => List(colorPromedio)
        }

        // Actualizar el acumulador con la nueva fila
        val nuevoAcc = if (x == 0) nuevaFila :: acc else nuevaFila :: acc.tail

        calcularColoresPromedio(y, x + tamanoPixel, nuevoAcc)
      }
    }

    /**
     * Expande los colores promedio a bloques completos en la imagen.
     * @param coloresPromedio Lista de colores promedio por bloque.
     * @param accFilas Acumulador de filas expandidas.
     * @return Lista de listas de píxeles con los bloques expandidos.
     */
    @tailrec
    def expandirImagenPixelada(coloresPromedio: List[List[Pixel]], accFilas: List[List[Pixel]] = Nil): List[List[Pixel]] = {
      coloresPromedio match {
        case Nil => accFilas
        case filaColores :: restoFilas =>
          // Expandir esta fila de colores a varias filas de píxeles
          val filasExpandidas = expandirFilaVertical(filaColores, tamanoPixel, Nil)
          expandirImagenPixelada(restoFilas, concat(accFilas, filasExpandidas))
      }
    }

    /**
     * Expande una fila de colores promedio verticalmente.
     * @param filaColores Fila de colores promedio.
     * @param alturaRestante Altura restante para expandir.
     * @param accFilas Acumulador de filas expandidas.
     * @return Lista de filas expandidas verticalmente.
     */
    @tailrec
    def expandirFilaVertical(filaColores: List[Pixel], alturaRestante: Int, accFilas: List[List[Pixel]]): List[List[Pixel]] = {
      if (alturaRestante <= 0) accFilas
      else {
        val filaExpandida = expandirFilaHorizontal(filaColores, Nil)
        expandirFilaVertical(filaColores, alturaRestante - 1, filaExpandida :: accFilas)
      }
    }

    /**
     * Expande una fila de colores promedio horizontalmente.
     * @param filaColores Fila de colores promedio.
     * @param accFila Acumulador de píxeles expandidos horizontalmente.
     * @return Fila expandida horizontalmente.
     */
    @tailrec
    def expandirFilaHorizontal(filaColores: List[Pixel], accFila: List[Pixel]): List[Pixel] = {
      filaColores match {
        case Nil => accFila
        case color :: restoColores =>
          val pixelesExpandidos = repetirPixel(color, tamanoPixel, Nil)
          expandirFilaHorizontal(restoColores, concat(accFila, pixelesExpandidos))
      }
    }

    /**
     * Repite un píxel un número específico de veces.
     * @param pixel Píxel a repetir.
     * @param cantidad Número de repeticiones.
     * @param acc Acumulador de píxeles repetidos.
     * @return Lista de píxeles repetidos.
     */
    @tailrec
    def repetirPixel(pixel: Pixel, cantidad: Int, acc: List[Pixel]): List[Pixel] = {
      if (cantidad <= 0) acc
      else repetirPixel(pixel, cantidad - 1, pixel :: acc)
    }

    /**
     * Recorta la imagen expandida a las dimensiones originales.
     * @param imagenExpandida Imagen expandida con bloques.
     * @param accFilas Acumulador de filas recortadas.
     * @return Lista de listas de píxeles recortada a las dimensiones originales.
     */
    @tailrec
    def recortarImagen(imagenExpandida: List[List[Pixel]], accFilas: List[List[Pixel]] = Nil): List[List[Pixel]] = {
      if (longitud_lista(accFilas) >= alto) {
        // Ya tenemos suficientes filas, recortamos y terminamos
        val filasRecortadas = toma(alto, accFilas)
        filasRecortadas
      } else imagenExpandida match {
        case Nil => accFilas
        case fila :: restoFilas =>
          // Recortar esta fila al ancho correcto
          val filaRecortada = if (longitud_lista(fila) > ancho) toma(ancho, fila) else fila
          recortarImagen(restoFilas, filaRecortada :: accFilas)
      }
    }

    // Calcular colores promedio para cada bloque
    val coloresPromedio = calcularColoresPromedio(0, 0, Nil)

    // Expandir colores promedio a bloques completos
    val imagenExpandida = expandirImagenPixelada(coloresPromedio)

    // Recortar a dimensiones originales
    val imagenFinal = recortarImagen(imagenExpandida)

    ImageData(ancho, alto, imagenFinal)
  }

  /**
   * Calcula el color promedio de un bloque de píxeles en la imagen.
   * @param startX Coordenada X inicial del bloque.
   * @param startY Coordenada Y inicial del bloque.
   * @param tamano Tamaño del bloque (en píxeles).
   * @param imagenData Datos de la imagen original.
   * @return Pixel con el color promedio del bloque.
   */
  def calcularColorMedioBloque(startX: Int, startY: Int, tamano: Int, imagenData: ImageData): Pixel = {
    val ancho = imagenData.ancho
    val alto = imagenData.alto

    @tailrec
    def sumaBloque(y: Int, x: Int, rSum: Int, vSum: Int, aSum: Int, contador: Int): (Int, Int, Int, Int) = {
      // Si nos pasamos de los límites de la imagen en el alto, devolver los contadoresa acumulados
      if(y >= startY + tamano || y >= alto) (rSum, vSum, aSum, contador)
      // Si nos pasamos de los límites de la imagen en el ancho, pasar a la siguiente fila
      else if(x >= startX + tamano || x >= ancho)  sumaBloque(y + 1, startX, rSum, vSum, aSum, contador)
      // Sino, procesar el píxel actual
      else {
        val pixel = imagenData.pixeles(y)(x)
        sumaBloque(y, x + 1, rSum + pixel.r, vSum + pixel.g, aSum + pixel.b, contador + 1)
      }
    }

    val (rSum, vSum, aSum, contador) = sumaBloque(startY, startX, 0, 0, 0, 0)

    if (contador > 0) Pixel(rSum / contador, vSum / contador, aSum / contador)
    else Pixel(0, 0, 0)

  }

}
