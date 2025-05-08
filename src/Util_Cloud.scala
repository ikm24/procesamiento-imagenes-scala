import java.io.OutputStream
import java.net.{HttpURLConnection, URI, URL, URLEncoder}
import java.nio.charset.StandardCharsets

object Util_Cloud {

  /**
   * Función para publicar datos en el cloud
   * @param usuario Nombre de usuario
   * @param imagen Nombre de la imagen
   * @param tipo Tipo de operación que se ha realizado
   * @param pixelesR Contador de píxeles rojos
   * @param pixelesG Contador de píxeles verdes
   * @param pixelesB Contador de píxeles azules
   */
  def publicarImagen(usuario: String, imagen: String, tipo: String,
                     pixelesR: Int, pixelesG: Int, pixelesB: Int): Unit = {

    // URL donde se va a subir la imagen
    val url = new URL("http://msdocs-python-postgres-001-hrenasdzhmhgagh6.canadacentral-01.azurewebsites.net/add")
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]

    // Configuración de la conexión
    connection.setRequestMethod("POST")
    connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")
    connection.setDoOutput(true)

    // Generar los parámetros de forma dinámica
    val params = Map(
      "usuario" -> usuario.toString,
      "imagen" -> imagen.toString,
      "tipo" -> tipo.toString,
      "pixelesR" -> pixelesR.toString,
      "pixelesG" -> pixelesG.toString,
      "pixelesB" -> pixelesB.toString
    )

    // Codificar los parámetros para la URL
    val encodedParams = params.map { case (key, value) =>
      s"${URLEncoder.encode(key, StandardCharsets.UTF_8)}=${URLEncoder.encode(value, StandardCharsets.UTF_8)}"
    }.mkString("&")

    // Enviar los parámetros al servidor
    val outputStream: OutputStream = connection.getOutputStream
    outputStream.write(encodedParams.getBytes(StandardCharsets.UTF_8))
    outputStream.close()

    // Leer la respuesta del
    val mensajeRespuesta = connection.getResponseMessage
    val responseCode = connection.getResponseCode
    if (responseCode == HttpURLConnection.HTTP_OK || responseCode == HttpURLConnection.HTTP_CREATED) {
      println(s"Código recibido: $responseCode $mensajeRespuesta")
      println("¡Imagen registrada correctamente!")
    } else {
      println(s"Error al registrar imagen. Código de respuesta: $responseCode")
    }
  }

}
