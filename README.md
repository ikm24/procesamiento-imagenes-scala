# Procesamiento de Imágenes con Scala e Integración Cloud

![Scala](https://img.shields.io/badge/Scala-2.13%2B-DC322F?logo=scala)
![Azure](https://img.shields.io/badge/Cloud-Microsoft_Azure-0089D6?logo=microsoft-azure)
## Universidad de Alcalá de Henares - Paradigmas Avanzados de Programación

🎯 El objetivo es desarrollar una aplicación en **Scala** para la manipulación de imágenes BMP utilizando técnicas de programación funcional y, posteriormente, integrar la solución con un servicio Cloud para almacenar y visualizar los resultados obtenidos.

## Tabla de Contenidos
1. [Funcionalidades](#funcionalidades)
   - [Procesamiento de Imágenes](#1-procesamiento-de-imágenes-en-scala)
   - [Integración Cloud](#2-integración-cloud)
2. [Requisitos](#requisitos)
3. [Instalación](#instalación)
4. [Uso](#uso)
5. [Autores](#autores)     

## Funcionalidades ##
El proyecto está compuesto de dos partes iniciales:   

---

### 1. Procesamiento de Imágenes en Scala  
  
- **Operaciones implementadas**:
  | Fase | Descripción |
  |------|-------------|
  | 01   | Conversión a blanco y negro (luminosidad: `0.3*R + 0.59*G + 0.11*B`) |
  | 02   | Pixelado de imagen (agrupación de píxeles en bloques) |
  | 03   | Identificación de colores únicos (recursividad de cola) |
  
- **Restricciones técnicas**:
  - ✔️ Programación funcional pura: inmutabilidad, `val`, recursividad de cola.
  - ❌ Prohibido: bucles (`for`, `while`), concatenación de listas (`++`, `:::`), librerías externas de imágenes.
  - ❌ Estructuras mutables (`var`, `ArrayBuffer`).

---

### 2. Integración Cloud  
  

* Opción para subir datos al Cloud como el nombre del archivo y conteo de colores por píxeles.
* Almacenamiento en una **Base de Datos en Microsoft Azure.**
* Desarrollo de una visor para permitir la visualización de los resultados almacenados, con opciones de filtrado y ordenación.

--- 

## Requisitos ##
| Componente               | Versión/Detalles                          |
|--------------------------|-------------------------------------------|
| **Entorno de Desarrollo**| IntelliJ IDEA 2023.3+ con plugin Scala    |
| **JDK**                 | OpenJDK 11+                              |
| **SBT**                 | 1.8.2+                                   |
| **Azure**               | Cuenta gratuita (necesaria para despliegue) |
| **Formato de Imágenes** | BMP sin compresión (24 bits)             |

--- 

## Instalación
1. **Clonar el repositorio**:
   ```bash
   git clone https://github.com/ikm24/procesamiento-imagenes-scala.git
   cd procesamiento-imagenes-scala-cloud
2. **Compilar con SBT**:
   ```bash
   sbt clean compile

---

## Uso
1. Ejecutar la aplicación
   ```bash
   sbt run
  - Menú Interactivo
    ```bash
    1. Cargar imagen BMP
    2. Convertir a blanco y negro
    3. Pixelar imagen
    4. Identificar colores
    5. Subir datos al Cloud
    6. Salir
    

--- 
## Autores ##
- **Iker Manzano Martínez** - Universidad de Alcalá
- **Zakaria Abdessamad Lassakeur** - Universidad de Alcalá

---

📌 Universidad de Alcalá - Paradigmas Avanzados de Programación, 2025

---
