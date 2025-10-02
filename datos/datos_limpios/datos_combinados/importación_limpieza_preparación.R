# -----------------------------------------------------------------------------
# SCRIPT DE IMPORTACIÓN, LIMPIEZA Y PREPARACIÓN DE DATOS DE LA ENCUESTA ENVIPE
# -----------------------------------------------------------------------------
# Autor: [Tu Nombre]
# Fecha: [Fecha Actual]
#
# Descripción:
# Este script realiza un proceso completo para trabajar con múltiples años de
# la encuesta ENVIPE. Las fases incluyen:
#   1. Configuración del entorno e instalación de paquetes.
#   2. Descarga e importación de los archivos de datos desde un repositorio.
#   3. Estandarización de la estructura y tipos de datos de cada encuesta anual.
#   4. Combinación de todos los años en un único data frame.
#   5. Limpieza y estandarización detallada de variables clave (municipio y entidad).
#   6. Unificación de niveles de respuesta en variables de selección múltiple.
#   7. Creación de un objeto de diseño de encuesta compleja para el análisis.
#
# El script está diseñado para ser reproducible y no contaminar el entorno de
# trabajo global, utilizando bloques 'local({})' para encapsular operaciones.
# -----------------------------------------------------------------------------


# =============================================================================
# FASE 1: CONFIGURACIÓN Y GESTIÓN DE PAQUETES
# =============================================================================

# Se establece el directorio de trabajo. Es una buena práctica, aunque en este
# script los datos se cargan desde URLs.
local({
  setwd("/home/cano/Escritorio/R/ENVIPE")
})

# Se utiliza el paquete 'librarian' para gestionar las dependencias.
# 'shelf()' se asegura de que los paquetes necesarios estén instalados y
# los carga en la sesión actual.
require("librarian")
shelf("rio", "dplyr", "stringr", "survey", "forcats", "rkward")


# =============================================================================
# FASE 2: DESCARGA E IMPORTACIÓN DE DATOS ANUALES
# =============================================================================

# Se importan los datos en una lista, donde cada elemento será un data frame
# correspondiente a un año de la encuesta.
envipe_list <- local({

  # 2a. Definir las URLs de los archivos de datos
  # Se construye dinámicamente una lista de URLs para los archivos RData
  # de la ENVIPE para los años 2021 a 2025.
  base_url <- "https://github.com/AlfCano/envipe/raw/main/datos/datos_limpios/"
  file_names <- paste0("sdem_vics_14_", 2021:2025, ".RData")
  full_urls <- paste0(base_url, file_names)

  # 2b. Descargar y cargar los datos en una lista
  # Se itera sobre cada URL. Los datos se descargan y se cargan usando rio::import().
  # El nombre del archivo (sin extensión) se usa como el nombre del elemento en la lista.
  envipe_data_list <- list()
  for(url in full_urls) {
    name <- sub(".*/(.*)\\.RData$", "\\1", url) # Extrae el nombre del archivo
    data <- rio::import(url, trust = TRUE)
    envipe_data_list[[name]] <- data
    cat("Importado:", url, "\n")
  }

  # 2c. Retornar la lista completa
  # Al final del bloque local, se retorna la lista para asignarla a 'envipe_list'.
  return(envipe_data_list)
})


# =============================================================================
# FASE 3: ESTANDARIZACIÓN DE ESTRUCTURA Y TIPOS DE DATOS (CONFORMIDAD)
# =============================================================================
# Objetivo: Garantizar que todos los data frames en la lista tengan la misma
#           estructura (columnas y tipos de datos) antes de combinarlos.
#           Esto previene errores comunes al usar dplyr::bind_rows().

envipe_list <- local({
  cat("--- Ejecutando Fase 3: Forzando conformidad con plantilla ---\n")

  # 3a. Definir plantilla y lista de trabajo
  # El data frame más reciente (el último en la lista) se usa como "plantilla"
  # o estándar de oro para los tipos de datos.
  template_df <- envipe_list[[length(envipe_list)]]
  current_list <- envipe_list # Trabajar con una copia local

  # 3b. Iterar y conformar cada data frame
  for (name in names(current_list)) {
    cat("  Conformando:", name, "\n")
    df_original <- current_list[[name]]

    # 3c. Respaldar etiquetas de RKWard
    # Antes de modificar los tipos de datos (lo que puede eliminar atributos),
    # se guardan todas las etiquetas existentes en un diccionario temporal.
    label_dictionary <- sapply(df_original, rk.get.label)

    # 3d. Forzar tipos de datos según la plantilla
    # Se recorren las columnas de la plantilla. Si una columna existe en el
    # data frame actual pero tiene un tipo diferente, se convierte forzosamente.
    for (col in names(template_df)) {
      if (col %in% names(df_original)) {
        target_class <- class(template_df[[col]])[1]
        if (class(df_original[[col]])[1] != target_class) {
          cat("    Cambiando tipo de '", col, "' a '", target_class, "'\n")
          df_original[[col]] <- switch(target_class,
                                       "factor"    = as.factor(df_original[[col]]),
                                       "character" = as.character(df_original[[col]]),
                                       "numeric"   = as.numeric(df_original[[col]]),
                                       "integer"   = as.integer(df_original[[col]]),
                                       df_original[[col]])
        }
      }
    }

    # 3e. Añadir la variable 'year'
    # Se extrae el año del nombre del data frame y se añade como una nueva columna.
    df_original$year <- as.numeric(stringr::str_extract(name, "[0-9]{4}"))

    # 3f. Restaurar las etiquetas
    # Se vuelven a aplicar las etiquetas guardadas al data frame ya conformado.
    for (col_name in names(df_original)) {
      if (!is.null(label_dictionary[[col_name]])) {
        rk.set.label(df_original[[col_name]], label_dictionary[[col_name]])
      }
    }

    current_list[[name]] <- df_original
  }

  cat("--- Fase 3 completada. Todos los data frames son ahora consistentes. ---\n\n")
  return(current_list)
})


# =============================================================================
# FASE 4: COMBINACIÓN DE DATOS Y ETIQUETADO FINAL
# =============================================================================
# Objetivo: Unir los data frames (ahora consistentes) en una sola tabla y
#           aplicar las etiquetas finales.

combined_df <- local({
  cat("--- Ejecutando Fase 4: Combinando y etiquetando ---\n")

  # 4a. Respaldar etiquetas desde la plantilla
  # Se extraen las etiquetas del data frame más reciente, que servirá como
  # fuente de verdad para el data frame combinado.
  label_source_df <- envipe_list[[length(envipe_list)]]
  label_dictionary <- sapply(label_source_df, rk.get.label)

  # 4b. Combinar los data frames
  # Con la estructura ya estandarizada, bind_rows() funciona de forma segura.
  cat("  Combinando los datos...\n")
  final_df <- dplyr::bind_rows(envipe_list)

  # 4c. Restaurar todas las etiquetas en la tabla combinada
  cat("  Restaurando etiquetas...\n")
  for (col_name in names(final_df)) {
    if (!is.null(label_dictionary[[col_name]])) {
      rk.set.label(final_df[[col_name]], label_dictionary[[col_name]])
    }
  }

  # 4d. Crear y etiquetar nuevas variables
  # Se añade una columna 'n' para conteos y se etiquetan 'n' y 'year'.
  cat("  Etiquetando variables nuevas...\n")
  final_df$n <- 1
  rk.set.label(final_df$n, "Conteo de casos (n=1)")
  rk.set.label(final_df$year, "Año de la encuesta")

  cat("--- Fase 4 completada. Objeto 'combined_df' creado. ---\n\n")
  return(final_df)
})


# =============================================================================
# FASE 5: LIMPIEZA DETALLADA DE VARIABLES GEOGRÁFICAS (NOM_MUN Y NOM_ENT)
# =============================================================================
# Objetivo: Estandarizar los nombres de municipios y entidades federativas,
#           resolviendo inconsistencias de espacios, acentos, mayúsculas y
#           sinónimos. El resultado final son nombres en formato "Tipo Título".

combined_df <- local({

  # Se trabajará sobre una copia local del data frame.
  df_to_clean <- combined_df

  # --- 5a. Definir función de formateo (reutilizable) ---
  formatear_nombre_propio <- function(texto) {
    sapply(texto, function(item) {
      item_lower <- tolower(item)
      palabras <- strsplit(item_lower, " ")[[1]]
      excepciones <- c("de", "del", "el", "la", "los", "las", "y", "a", "en", "con")
      palabras_formateadas <- sapply(seq_along(palabras), function(i) {
        palabra <- palabras[i]
        if (i > 1 && palabra %in% excepciones) return(palabra)
        else paste0(toupper(substr(palabra, 1, 1)), substr(palabra, 2, nchar(palabra)))
      })
      return(paste(palabras_formateadas, collapse = " "))
    }, USE.NAMES = FALSE)
  }

  # --- 5b. Limpieza de NOM_MUN (Nombre del Municipio) ---
  cat("  Limpiando y estandarizando NOM_MUN...\n")

  # i. Limpieza base y creación de mapa de corrección para acentos
  columna_base_mun <- trimws(as.character(df_to_clean$NOM_MUN))
  original_levels_mun <- unique(toupper(columna_base_mun))
  deaccented_levels_mun <- iconv(original_levels_mun, from = "UTF-8", to = "ASCII//TRANSLIT")
  level_groups_mun <- split(original_levels_mun, deaccented_levels_mun)

  correction_map_mun <- sapply(level_groups_mun, function(group) {
    accented_version <- group[group != iconv(group, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "byte")]
    if (length(accented_version) > 0) return(accented_version[1]) else return(group[1])
  })

  # ii. Aplicar mapa y formateo
  lookup_keys_mun <- iconv(toupper(columna_base_mun), from = "UTF-8", to = "ASCII//TRANSLIT")
  columna_unificada_mun <- unname(correction_map_mun[lookup_keys_mun])
  factor_unificado_mun <- as.factor(columna_unificada_mun)
  levels(factor_unificado_mun) <- formatear_nombre_propio(levels(factor_unificado_mun))
  df_to_clean$NOM_MUN <- factor_unificado_mun

  # --- 5c. Limpieza de NOM_ENT (Nombre de la Entidad) ---
  cat("  Limpiando y estandarizando NOM_ENT...\n")

  # i. Limpieza base y manejo de sinónimos ("MÉXICO" -> "ESTADO DE MÉXICO")
  entidad_col <- toupper(trimws(as.character(df_to_clean$NOM_ENT)))
  entidad_col[entidad_col %in% c("MEXICO", "MÉXICO")] <- "ESTADO DE MEXICO"

  # ii. Crear mapa de corrección para acentos
  original_levels_ent <- unique(entidad_col)
  deaccented_levels_ent <- iconv(original_levels_ent, from = "UTF-8", to = "ASCII//TRANSLIT")
  level_groups_ent <- split(original_levels_ent, deaccented_levels_ent)

  correction_map_ent <- sapply(level_groups_ent, function(group) {
    accented_version <- group[group != iconv(group, from = "UTF-8", to = "ASCII//TRANSLIT")]
    if (length(accented_version) > 0) return(accented_version[1]) else return(group[1])
  })

  # iii. Aplicar mapa y formateo
  lookup_keys_ent <- iconv(entidad_col, from = "UTF-8", to = "ASCII//TRANSLIT")
  entidad_col_unificada <- unname(correction_map_ent[lookup_keys_ent])
  factor_unificado_ent <- as.factor(entidad_col_unificada)
  levels(factor_unificado_ent) <- formatear_nombre_propio(levels(factor_unificado_ent))
  df_to_clean$NOM_ENT <- factor_unificado_ent

  cat("--- Fase 5 completada. Variables geográficas estandarizadas. ---\n\n")
  return(df_to_clean)
})


# =============================================================================
# FASE 6: ETIQUETADO FINAL Y LIMPIEZA DE VARIABLES DE RESPUESTA
# =============================================================================
cat("--- Ejecutando Fase 6: Etiquetado final y limpieza de respuestas ---\n")

# --- 6a. Asignar etiquetas de variable a NOM_ENT y NOM_MUN ---
rk.set.label(combined_df$NOM_ENT, label = "Nombre de la Entidad")
rk.set.label(combined_df$NOM_MUN, label = "Nombre del municipio")

# --- 6b. Unificar niveles de respuesta en variables 'AP4_2_*' ---
# Se unifican los niveles "Sí" y "SÍ" en un solo nivel "Sí" para todas
# las columnas que comienzan con "AP4_2_", usando dplyr y forcats.
combined_df <- combined_df %>%
  mutate(across(starts_with("AP4_2_"),
                ~ fct_recode(., "Sí" = "SÍ")))

cat("--- Fase 6 completada. ---\n\n")


# =============================================================================
# FASE 7: CREACIÓN DEL OBJETO DE DISEÑO DE ENCUESTA COMPLEJA
# =============================================================================
# Objetivo: Preparar el data frame para análisis estadístico que tome en cuenta
#           el diseño muestral de la ENVIPE (estratificación, conglomerados, etc.).

survey.design <- local({
  cat("--- Ejecutando Fase 7: Creando objeto de diseño de encuesta ---\n")

  # Se utiliza la función svydesign del paquete 'survey'.
  # ids = ~UPM: Define las Unidades Primarias de Muestreo.
  # strata = ~EST_DIS: Define los estratos.
  # weights = ~FAC_ELE: Define el factor de ponderación.
  # nest = TRUE: Indica que los conglomerados están anidados dentro de los estratos.
  design_object <- svydesign(ids = ~UPM, strata = ~EST_DIS, weights = ~FAC_ELE, data = combined_df, nest = TRUE)

  cat("--- Fase 7 completada. Objeto 'survey.design' creado. ---\n")
  rk.header("Create Survey Design results")
  rk.header("Survey design object saved as: survey.design")

  return(design_object)
})
