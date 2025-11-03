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
# FASE 3: ARMONIZACIÓN INTEGRAL CON PRESERVACIÓN DE ETIQUETAS (VERSIÓN FINAL)
# =============================================================================
envipe_list <- local({
  cat("--- Ejecutando Fase 3: Armonización Integral con Preservación de Etiquetas ---\n")

  # --- PASO 1: DEFINIR LA "RECETA MAESTRA" DE ARMONIZACIÓN ---
  cat("  Paso 3a: Definiendo la receta maestra de renombrado y etiquetado...\n")

  master_recipe <- data.frame(
    year_range = I(list(2023:2024, 2023:2024, 2023:2024, 2023:2024, 2025, 2025, 2025, 2025, 2021:2024, 2021:2024, 2025, 2025)),
    original_name = c("AP5_4_8", "AP5_5_8", "AP5_4_9", "AP5_5_9", "AP5_4_11", "AP5_5_11", "AP5_4_12", "AP5_5_12", "AP5_4_10", "AP5_5_10", "AP5_4_9", "AP5_5_9"),
    final_name = c("PERCEP_DESEMP_MP_ESTATAL", "PERCEP_CORRUP_MP_ESTATAL", "PERCEP_DESEMP_FGR", "PERCEP_CORRUP_FGR", "PERCEP_DESEMP_MP_ESTATAL", "PERCEP_CORRUP_MP_ESTATAL", "PERCEP_DESEMP_FGR", "PERCEP_CORRUP_FGR", "PERCEP_DESEMP_JUECES", "PERCEP_CORRUP_JUECES", "PERCEP_DESEMP_JUECES", "PERCEP_CORRUP_JUECES"),
    final_label = c("Confianza en Ministerio Público (MP) y Fiscalías Estatales", "Percepción sobre corrupción de Ministerio Público (MP) y Fiscalías Estatales", "Confianza en Fiscalía General de la República (FGR)", "Percepción sobre corrupción de Fiscalía General de la República (FGR)", "Confianza en Ministerio Público (MP) y Fiscalías Estatales", "Percepción sobre corrupción de Ministerio Público (MP) y Fiscalías Estatales", "Confianza en Fiscalía General de la República (FGR)", "Percepción sobre corrupción de Fiscalía General de la República (FGR)", "Confianza en jueces", "Percepción sobre corrupción de jueces", "Confianza en jueces", "Percepción sobre corrupción de jueces")
  )
  # Añadir aquí el resto de las reglas para las demás variables que cambian...

  # --- PASO 2: ARMONIZACIÓN SEMÁNTICA Y CAPTURA DE TODAS LAS ETIQUETAS ---
  cat("  Paso 3b: Armonizando nombres y capturando todas las etiquetas...\n")

  master_label_dictionary <- list()
  harmonized_list <- list()

  for (name in names(envipe_list)) {
    df <- envipe_list[[name]]
    year <- as.numeric(stringr::str_extract(name, "[0-9]{4}"))

    # 1. Limpiar nombres y capturar etiquetas originales
    names(df) <- gsub("_0([1-9])", "_\\1", toupper(trimws(names(df))))
    original_labels <- sapply(df, rk.get.label)

    # 2. Poblar el diccionario maestro con TODAS las etiquetas disponibles.
    for(col_name in names(original_labels)){
        label <- original_labels[[col_name]]
        if(!is.null(label) && !(col_name %in% names(master_label_dictionary))){
            master_label_dictionary[[col_name]] <- label
        }
    }

    # 3. Renombrar las columnas del data.frame actual usando la receta
    recipe_for_year <- master_recipe[sapply(master_recipe$year_range, function(yr) year %in% yr), ]
    map_for_year <- setNames(recipe_for_year$final_name, recipe_for_year$original_name)

    # --- INICIO DE LA CORRECCIÓN DEFINITIVA ---
    # Reemplazar dplyr::rename con un bucle de R base que es robusto y funciona
    for (old_name in names(map_for_year)) {
      if (old_name %in% names(df)) {
        names(df)[names(df) == old_name] <- map_for_year[[old_name]]
      }
    }
    # --- FIN DE LA CORRECCIÓN DEFINITIVA ---

    harmonized_list[[name]] <- df
  }

  # 4. SOBRESCRIBIR el diccionario maestro con las etiquetas correctas de la receta.
  recipe_labels <- setNames(as.character(master_recipe$final_label), master_recipe$final_name)
  for(final_name in names(recipe_labels)){
    master_label_dictionary[[final_name]] <- recipe_labels[[final_name]]
  }

  # --- PASO 3: CONFORMIDAD ESTRUCTURAL POR RECONSTRUCCIÓN ---
  cat("  Paso 3c: Reconstruyendo cada año para conformidad estructural...\n")
  all_column_names <- unique(unlist(lapply(harmonized_list, names)))
  type_map <- sapply(all_column_names, function(col) {
    all_types <- sapply(harmonized_list, function(df) if (col %in% names(df)) class(df[[col]])[1] else NA)
    all_types <- na.omit(all_types)
    if ("numeric" %in% all_types) return("numeric")
    if ("integer" %in% all_types) return("integer")
    if ("factor" %in% all_types) return("factor")
    return("character")
  }, USE.NAMES = TRUE, simplify = FALSE)

  final_list <- list()
  for (name in names(harmonized_list)) {
    source_df <- harmonized_list[[name]]
    rebuilt_list <- list()
    for (col_name in all_column_names) {
      target_class <- type_map[[col_name]]
      data_vector <- if (col_name %in% names(source_df)) source_df[[col_name]] else NA
      if (target_class %in% c("numeric", "integer")) {
        converted_vector <- suppressWarnings(as.numeric(as.character(data_vector)))
        if (target_class == "integer") converted_vector <- as.integer(converted_vector)
      } else if (target_class == "factor") {
        converted_vector <- as.factor(data_vector)
      } else {
        converted_vector <- as.character(data_vector)
      }
      rebuilt_list[[col_name]] <- converted_vector
    }
    final_df <- as.data.frame(rebuilt_list, stringsAsFactors = FALSE)
    final_df$year <- as.numeric(stringr::str_extract(name, "[0-9]{4}"))
    final_list[[name]] <- final_df
  }

  attr(final_list, "master_labels") <- master_label_dictionary
  return(final_list)
})

# =============================================================================
# FASE 4: COMBINACIÓN Y ETIQUETADO FINAL (CON TODAS LAS ETIQUETAS)
# =============================================================================
combined_df <- local({
  cat("--- Ejecutando Fase 4: Combinando y Etiquetando Datos Finales ---\n")

  master_labels <- attr(envipe_list, "master_labels")

  cat("  Combinando los datos...\n")
  final_df <- dplyr::bind_rows(envipe_list)

  cat("  Aplicando todas las etiquetas de variables desde el diccionario maestro...\n")
  for (col_name in names(final_df)) {
    if (col_name %in% names(master_labels)) {
      rk.set.label(final_df[[col_name]], master_labels[[col_name]])
    }
  }

  rk.set.label(final_df$year, "Año de la encuesta")

  cat("--- Fase 4 completada. Objeto 'combined_df' creado con datos y todas las etiquetas correctas. ---\n\n")
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
require("dplyr")
require("forcats")

combined_df <- combined_df %>%
  mutate(across(starts_with("AP4_2_"),
                ~ fct_recode(., "Sí" = "SÍ")))

combined_df <- combined_df %>%
  mutate(across(where(is.factor), ~ fct_relabel(., ~ trimws(.))))

cat("--- Fase 6 completada. ---\n\n")
