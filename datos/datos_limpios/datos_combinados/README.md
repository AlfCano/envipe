# Repositorio para el Procesamiento y Análisis de la Encuesta ENVIPE

[![R Linter](https://github.com/AlfCano/envipe/actions/workflows/lintr.yml/badge.svg)](https://github.com/AlfCano/envipe/actions/workflows/lintr.yml)

## 1. Resumen

Este repositorio contiene un conjunto de scripts en R diseñados para **automatizar la descarga, limpieza, estandarización y combinación de las bases de datos de la Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública (ENVIPE)** de múltiples años.

El objetivo principal es proporcionar un flujo de trabajo reproducible que transforme los microdatos anuales en un único `data.frame` limpio, consistente y listo para el análisis estadístico, especialmente utilizando el paquete `survey` para diseños de encuestas complejas.

## 2. Objetivos del Repositorio

*   **Facilitar el acceso:** Automatizar la descarga de los archivos de datos de la ENVIPE desde un repositorio centralizado.
*   **Estandarizar:** Resolver inconsistencias estructurales entre las bases de datos de diferentes años, homologando los tipos de datos de las columnas.
*   **Limpiar:** Corregir sistemáticamente errores comunes en variables categóricas clave, como nombres de municipios (`NOM_MUN`) y entidades (`NOM_ENT`), resolviendo problemas de acentos, capitalización, espacios en blanco y sinónimos.
*   **Combinar:** Unir múltiples encuestas anuales en una sola base de datos, añadiendo una variable `year` para facilitar el análisis longitudinal.
*   **Preparar para el análisis:** Generar un objeto `survey.design` que incorpore los elementos del diseño muestral de la encuesta (estratos, UPMs y ponderadores), permitiendo realizar inferencias estadísticas válidas.

## 3. Contenido del Repositorio

*   **/datos/datos_limpios/**: Contiene las bases de datos anuales de la ENVIPE en formato `.RData`. Estos archivos han sido pre-procesados para facilitar su importación.
*   **/R/**: Contiene los scripts de R que ejecutan el flujo de trabajo completo. El script principal se encarga de orquestar todo el proceso, desde la importación hasta la creación del objeto de encuesta.
*   **README.md**: Este archivo, que proporciona una guía completa del proyecto.

## 4. Requisitos

Para ejecutar los scripts de este repositorio, necesitarás:
*   Una versión reciente de **R** (v4.0.0 o superior recomendada).
*   **RKWard** (opcional, pero las funciones de etiquetado `rk.*` están presentes en el script).
*   Los siguientes **paquetes de R**:
    - `librarian`: Para una gestión sencilla de los paquetes.
    - `rio`: Para la importación robusta de datos.
    - `dplyr`: Para la manipulación de datos.
    - `stringr`: Para operaciones con cadenas de texto.
    - `forcats`: Para el manejo avanzado de factores.
    - `survey`: Para el análisis de datos de encuestas complejas.

El script principal utiliza `librarian::shelf()` para instalar y cargar automáticamente estos paquetes.

## 5. Instrucciones de Uso

1.  **Clonar el Repositorio:**
    ```bash
    git clone https://github.com/AlfCano/envipe.git
    ```

2.  **Establecer el Directorio de Trabajo:**
    Abre R o RKWard y establece el directorio de trabajo en la carpeta raíz del repositorio clonado.
    ```R
    setwd("/ruta/a/la/carpeta/envipe")
    ```

3.  **Ejecutar el Script Principal:**
    Ejecuta el script de procesamiento principal (por ejemplo, `procesamiento_completo.R`). Este script se encargará de todo el flujo de trabajo de forma automática.

    ```R
    source("R/procesamiento_completo.R")
    ```

Al finalizar, tendrás dos objetos clave en tu entorno de trabajo global:
*   `combined_df`: Un `data.frame` que contiene los datos de todos los años, limpios y combinados.
*   `survey.design`: Un objeto de tipo `svydesign` listo para ser utilizado con las funciones del paquete `survey`.

## 6. Flujo de Trabajo Detallado

El script principal sigue un proceso robusto dividido en varias fases:

1.  **Importación Automática:** Descarga los archivos `.RData` de cada año y los carga en una lista de R, donde cada elemento es un `data.frame` anual.

2.  **Conformidad de Estructura:** Antes de combinar, el script utiliza la encuesta más reciente como "plantilla". Itera sobre las encuestas más antiguas y:
    *   Respalda las etiquetas de metadatos (RKWard).
    *   Fuerza la conversión de tipos de datos de cada columna para que coincidan con la plantilla, evitando errores al combinar.
    *   Añade una columna `year` para identificar el año de cada registro.
    *   Restaura las etiquetas.

3.  **Combinación y Etiquetado:**
    *   Utiliza `dplyr::bind_rows()` para unir eficientemente todos los data frames de la lista.
    *   Aplica las etiquetas de la plantilla al nuevo data frame combinado.

4.  **Limpieza Detallada de Variables Geográficas:**
    *   **Municipios (`NOM_MUN`):** Aplica un algoritmo de limpieza que:
        1.  Elimina espacios en blanco.
        2.  Crea un mapa de corrección para unificar nombres con y sin acentos, conservando la versión acentuada (ej. `ACAMBARO` -> `ACÁMBARO`).
        3.  Formatea el resultado final a "Tipo Título" (ej. `Acapulco de Juárez`).
    *   **Entidades (`NOM_ENT`):** Realiza un proceso similar, con un paso adicional para estandarizar sinónimos (ej. `MEXICO` se convierte en `ESTADO DE MEXICO`).

5.  **Estandarización de Variables de Respuesta:**
    *   Unifica los niveles en variables de respuesta múltiple (ej. en las columnas `AP4_2_*`, los niveles `"Sí"` y `"SÍ"` se fusionan en `"Sí"`).

6.  **Creación del Objeto de Encuesta:**
    *   Finalmente, utiliza el `data.frame` limpio para crear el objeto `survey.design`, especificando los estratos (`EST_DIS`), conglomerados (`UPM`) y ponderadores (`FAC_ELE`).

## 7. Ejemplo de Análisis Básico

Una vez que el objeto `survey.design` está creado, puedes realizar análisis estadísticamente válidos. Por ejemplo, para calcular la tasa de prevalencia delictiva a nivel nacional (suponiendo que la variable se llama `PREVALENCIA` y es 1 para víctima, 0 para no víctima):

```R
library(survey)

# Calcular la tasa de prevalencia nacional (promedio ponderado)
tasa_nacional <- svymean(~PREVALENCIA, design = survey.design, na.rm = TRUE)

# Imprimir el resultado
print(tasa_nacional)

# Extraer el coeficiente y el intervalo de confianza
prevalencia <- coef(tasa_nacional)
ci <- confint(tasa_nacional)

cat(paste0("Tasa de Prevalencia Delictiva: ", round(prevalencia * 100, 2), "%\n"))
```

## 8. Contribuciones
Las contribuciones son bienvenidas. Si encuentras un error, tienes una sugerencia para mejorar el proceso de limpieza o quieres añadir nuevas funcionalidades, por favor abre un "Issue" o envía un "Pull Request".

## 9. Fuente de Datos y Agradecimientos
Los datos originales son producidos y distribuidos por el **Instituto Nacional de Estadística y Geografía (INEGI)** de México. Este repositorio solo facilita su acceso y procesamiento.

Se recomienda encarecidamente visitar la [página oficial de la ENVIPE](https://www.inegi.org.mx/programas/envipe/2023/) para consultar la documentación completa, los cuestionarios y los metadatos de cada año.
