import Util._
object Main {
  def pedir_ruta(): Unit = {
    println("El directorio actual es: ", IMAGEN_DEFECTO)
  }
  def menu(): Unit = {
    println("Opciones:")
    println("\t\t(1). Convertir a Blanco y Negro")
    println("\t\t(2). Pixelar")
    println("\t\t(3). Filtrar Colores")
    println("\t\t(4). Salir")
  }
  def main(args: Array[String]): Unit = {
    menu()
  }
}

