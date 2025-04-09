import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.util.Try
object Util {

  /**
   * Ruta de imagen por defecto
   */
  val IMAGEN_DEFECTO = "./img/squirtle_squad_BMP_00.bmp"
  /**
   * Pixel gris constante para la fase 3
   * específicamente para los píxeles que no cumplen
   * la condición
   */
  val PIXEL_GRIS = Pixel(128, 128, 128)

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
  @tailrec
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
      if (y >= alto) acc
      else if (x >= ancho) calcularColoresPromedio(y + tamanoPixel, 0, acc)
      else {
        // Ajustar el tamaño del bloque si excede los límites
        val bloqueAncho = Math.min(tamanoPixel, ancho - x)
        val bloqueAlto = Math.min(tamanoPixel, alto - y)

        val colorPromedio = calcularColorMedioBloque(x, y, bloqueAncho, bloqueAlto, imagenData)

        val nuevaFila = if (x == 0) List(colorPromedio) else acc.head :+ colorPromedio
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
          val filasExpandidas = expandirFilaVertical(filaColores, tamanoPixel, Nil)
          expandirImagenPixelada(restoFilas, concat(accFilas,filasExpandidas))
      }
    }

    /**
     * Expande los colores promedio a bloques completos en la imagen.
     * @param coloresPromedio Lista de colores promedio por bloque.
     * @param accFilas Acumulador de filas expandidas.
     * @return Lista de listas de píxeles con los bloques expandidos.
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
     * @param alturaRestante Altura restante para expandir.
     * @param accFilas Acumulador de filas expandidas.
     * @return Lista de filas expandidas horizontalmente.
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

    val coloresPromedio = calcularColoresPromedio(0, 0, Nil)
    val imagenExpandida = expandirImagenPixelada(coloresPromedio)

    ImageData(ancho, alto, reverseImagen(imagenExpandida))
  }

  /**
   * Calcula el color promedio de un bloque de píxeles en la imagen.
   * @param startX Coordenada X inicial del bloque.
   * @param startY Coordenada Y inicial del bloque.
   * @param tamano Tamaño del bloque (en píxeles).
   * @param imagenData Datos de la imagen original.
   * @return Pixel con el color promedio del bloque.
   */
  def calcularColorMedioBloque(startX: Int, startY: Int, bloqueAncho: Int, bloqueAlto: Int, imagenData: ImageData): Pixel = {
    @tailrec
    def sumaBloque(y: Int, x: Int, rSum: Int, gSum: Int, bSum: Int, contador: Int): (Int, Int, Int, Int) = {
      if (y >= startY + bloqueAlto) (rSum, gSum, bSum, contador)
      else if (x >= startX + bloqueAncho) sumaBloque(y + 1, startX, rSum, gSum, bSum, contador)
      else {
        val pixel = imagenData.pixeles(y)(x)
        sumaBloque(y, x + 1, rSum + pixel.r, gSum + pixel.g, bSum + pixel.b, contador + 1)
      }
    }

    val (rSum, gSum, bSum, contador) = sumaBloque(startY, startX, 0, 0, 0, 0)
    if (contador > 0) Pixel(rSum / contador, gSum / contador, bSum / contador)
    else Pixel(0, 0, 0)
  }


  /**
   * Evaluar si un píxel es rojo.
   * @param pixel Píxel a evaluar
   * @param factorMagnitud Factor de magnitud para la comparación
   * @param umbral Umbral míino para considerar el color
   * @return Boolean indica si el píxel es rojo
   */
  def esRojo(pixel: Pixel, factorMagnitud: Double, umbral: Int) : Boolean = {
    pixel.r > (pixel.g * factorMagnitud) && pixel.r > (pixel.b * factorMagnitud) && pixel.r > umbral
  }

  /**
   * Evaluar si un píxel es verde.
   * @param pixel Píxel a evaluar
   * @param factorMagnitud Factor de magnitud para la comparación
   * @param umbral Umbral mínimo para considerar el color
   * @return Boolean indica si el píxel es verde
   */
  def esVerde(pixel: Pixel, factorMagnitud: Double, umbral: Int) : Boolean = {
    pixel.g > (pixel.r * factorMagnitud) && pixel.g > (pixel.b * factorMagnitud) && pixel.g > umbral
  }

  /**
   * Evaluar si un píxel es azul.
   * @param pixel Píxel a evaluar
   * @param factorMagnitud Factor de magnitud para la comparación
   * @param umbral Umbral mínimo para considerar el color
   * @return Boolean indica si el píxel es azul
   */
  def esAzul(pixel: Pixel, factorMagnitud: Double, umbral: Int) : Boolean = {
    pixel.b > (pixel.r * factorMagnitud) && pixel.b > (pixel.g * factorMagnitud) && pixel.b > umbral
  }

  /**
   * Función para procesar una fila de píxeles y cuenta los colores
   * @param fila fila de píxeles originales
   * @param filaRoja Acumulador para los píxeles rojos procesados
   * @param filaVerde Acumulador para los píxeles verdes procesados
   * @param filaAzul Acumulador para los píxeles azules procesados
   * @param contador Contador de colores actual
   * @param factoMagnitud Factor de magnitud para la comparación
   * @param umbral Umbral mímino
   * @return Tupla con las filas procesadas y el contador actualizado
   */
  @tailrec
  def procesarFilaColores(fila: List[Pixel],
                          filaRoja: List[Pixel],
                          filaVerde: List[Pixel],
                         filaAzul: List[Pixel],
                          contador: contadorColores,
                          factoMagnitud: Double,
                          umbral: Int): (List[Pixel], List[Pixel], List[Pixel], contadorColores) = {

    fila match {
      case Nil =>
        (reverse(filaRoja), reverse(filaVerde), reverse(filaAzul), contador)
      case pixel :: resto => {
        // Evaluar las condiciones para cada color
        val esPixelRojo = esRojo(pixel, factoMagnitud, umbral)
        val esPixelVerde = esVerde(pixel, factoMagnitud, umbral)
        val esPixelAzul = esAzul(pixel, factoMagnitud, umbral)

        // sActualizar el contador
        val nuevoRojo = if (esPixelRojo) contador.rojo + 1 else contador.rojo
        val nuevoVerde = if (esPixelVerde) contador.verde + 1 else contador.verde
        val nuevoAzul = if (esPixelAzul) contador.azul + 1 else contador.azul
        val nuevoContador = contador.contador + 1

        // Procesar el pixel para cada imagen de salida
        val pixelSalidaRojo = if (esPixelRojo) pixel else PIXEL_GRIS
        val pixelSalidaVerde = if (esPixelVerde) pixel else PIXEL_GRIS
        val pixelSalidaAzul = if (esPixelAzul) pixel else PIXEL_GRIS

        // Llamar a la función recursiva
        procesarFilaColores(
          resto,
          pixelSalidaRojo :: filaRoja,
          pixelSalidaVerde :: filaVerde,
          pixelSalidaAzul :: filaAzul,
          contadorColores(nuevoRojo, nuevoVerde, nuevoAzul, nuevoContador),
          factoMagnitud,
          umbral
        )
      }
    }
  }

  /**
   * Función para procesar una imagen completa y contar los colores
 *
   * @param fila Lista de filas de píxeles originales
   * @param acumRojo Acumulador para filas de salida rojas
   * @param acumVerde Acumulador para filas de salida verdes
   * @param acumAzul Acumulador para filas de salida azules
   * @param contador Contador de colores actual
   * @param factarMagnitud Factor de magnitud para la comparación
   * @param umbral Umbral mínimo
   * @return Tupla con las tres imágenes de salida y el contador actualizado
   */
  @tailrec
  def procesarImagenColores(fila: List[List[Pixel]],
                            acumRojo: List[List[Pixel]],
                            acumVerde: List[List[Pixel]],
                            acumAzul: List[List[Pixel]],
                            contador: contadorColores,
                            factarMagnitud: Double,
                            umbral: Int): (List[List[Pixel]], List[List[Pixel]], List[List[Pixel]], contadorColores) = {
    fila match {
      case Nil =>
        (reverseImagen(acumRojo), reverseImagen(acumVerde), reverseImagen(acumAzul), contador)
      case fila :: resto => {
        val (filaRoja, filaVerde, filaAzul, contadorActualizado) =
          procesarFilaColores(fila, Nil, Nil, Nil, contador, factarMagnitud, umbral)

        procesarImagenColores(
          resto,
          filaRoja :: acumRojo,
          filaVerde :: acumVerde,
          filaAzul :: acumAzul,
          contadorActualizado,
          factarMagnitud,
          umbral
        )
      }
    }
  }


  /**
   * Función que identifica los colores de una imagen dada
   * @param imagenData Imagen original en formato ImageData
   * @param factorMagnitud Factor de magnitud para la comparación
   * @param umbral Umbral mínimo para considerar el color
   * @return Tupla con las imágenes filtradas y el contador de colores
   */
  def identificarColores(imagenData: ImageData,
                         factorMagnitud: Double,
                         umbral: Int) : (ImageData, ImageData, ImageData, contadorColores) = {
    val contadorInicial = contadorColores(0, 0, 0, 0)

    val (pixelesRojos, pixelesVerdes, pixelesAzules, resultados) =
      procesarImagenColores(
        imagenData.pixeles,
        Nil,
        Nil,
        Nil,
        contadorInicial,
        factorMagnitud,
        umbral
      )

    // Crear una imagen para cada color
    val imagenRoja = ImageData(imagenData.ancho, imagenData.alto, pixelesRojos)
    val imagenVerde = ImageData(imagenData.ancho, imagenData.alto, pixelesVerdes)
    val imagenAzul = ImageData(imagenData.ancho, imagenData.alto, pixelesAzules)

    // Calcular y mostrar estadísticas
    val total = resultados.contador.toDouble
    val porcentajeRojo = if (total > 0) (resultados.rojo.toDouble / total) * 100 else 0.0
    val porcentajeVerde = if (total > 0) (resultados.verde.toDouble / total) * 100 else 0.0
    val porcentajeAzul = if (total > 0) (resultados.azul.toDouble / total) * 100 else 0.0

    // Imprimir las estadísticas
    println(s"Análisis de colores completado:")
    println(s"Factores utilizados: magnitud=${factorMagnitud}, umbral=${umbral}")
    println(s"Píxeles totales: ${resultados.contador}")
    println(s"Píxeles rojos: ${resultados.rojo} (${porcentajeRojo.formatted("%.2f")}%)")
    println(s"Píxeles verdes: ${resultados.verde} (${porcentajeVerde.formatted("%.2f")}%)")
    println(s"Píxeles azules: ${resultados.azul} (${porcentajeAzul.formatted("%.2f")}%)")

    (imagenRoja, imagenVerde, imagenAzul, resultados)
  }




}
