# -----------------------------------------------------------------------------
# SCRIPT DE IMPORTACIÓN, LIMPIEZA Y PREPARACIÓN DE DATOS DE LA ENCUESTA ENVIPE
# -----------------------------------------------------------------------------
# Autor: Alfonso Cano Robles
# Asistente: Gemini
# Fecha: 01-dic-2025
# Version: 2.0
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
#
#
# El script está diseñado para ser reproducible y no contaminar el entorno de
# trabajo global, utilizando bloques 'local({})' para encapsular operaciones.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT MAESTRO ENVIPE - CORRECCIÓN TOTAL (MP, FGR, JUECES)
# -----------------------------------------------------------------------------

# FASE 1: PAQUETES
require("librarian")
shelf("rio", "dplyr", "stringr", "survey", "forcats", "rkward")

# FASE 2: DESCARGA LIMPIA (Crucial para eliminar renombres previos)
envipe_list <- local({
  cat("--- Descargando datos crudos ---\n")
  base_url <- "https://github.com/AlfCano/envipe/raw/main/datos/datos_limpios/"
  # Descargamos 2021 a 2025
  file_names <- paste0("sdem_vics_14_", 2021:2025, ".RData")
  full_urls <- paste0(base_url, file_names)

  envipe_data_list <- list()
  for(url in full_urls) {
    name <- sub(".*/(.*)\\.RData$", "\\1", url)
    # trust=TRUE mantiene las etiquetas originales y niveles
    data <- rio::import(url, trust = TRUE)
    envipe_data_list[[name]] <- data
    cat("Importado:", name, "\n")
  }
  return(envipe_data_list)
})

# FASE 3: ARMONIZACIÓN BLINDADA (RECETA BASADA EN RADAR)
envipe_list <- local({
  cat("--- Ejecutando Fase 3: Armonización corregida por Radar ---\n")

  master_recipe <- dplyr::bind_rows(

    # =========================================================================
    # 1. MINISTERIO PÚBLICO (MP)
    # Radar confirmó: Es AP5_4_6 en 2021, 2023 y 2025. Asumimos consistencia total.
    # =========================================================================
    data.frame(
      years = I(list(2021:2025)),
      orig  = "AP5_4_6",
      final = "CONFIANZA_MP_ESTATAL",
      label = "Confianza en MP y Fiscalías Estatales"
    ),

    # =========================================================================
    # 2. FISCALÍA GENERAL DE LA REPÚBLICA (FGR / PGR)
    # Radar confirmó: Es AP5_4_7 en 2021 ("PGR"), 2023 y 2025. Consistencia total.
    # =========================================================================
    data.frame(
      years = I(list(2021:2025)),
      orig  = "AP5_4_7",
      final = "CONFIANZA_FGR",
      label = "Confianza en Fiscalía General de la República (FGR)"
    ),

    # =========================================================================
    # 3. JUECES
    # Historia distinta: Se movieron en 2025.
    # =========================================================================
    # 2025: Radar confirmó posición 11
    data.frame(
      years = I(list(2025)),
      orig  = "AP5_4_11",
      final = "CONFIANZA_JUECES",
      label = "Confianza en Jueces"
    ),
    # 2021-2024: Radar previo confirmó posición 10
    data.frame(
      years = I(list(2021:2024)),
      orig  = "AP5_4_10",
      final = "CONFIANZA_JUECES",
      label = "Confianza en Jueces"
    )
  )

  # --- GENERAR RECETA DE CORRUPCIÓN (AUTOMÁTICA) ---
  # Asumimos que la sección de corrupción (AP5_5) sigue los mismos índices que confianza (AP5_4)
  corrup_recipe <- master_recipe
  corrup_recipe$orig <- gsub("AP5_4", "AP5_5", corrup_recipe$orig)
  corrup_recipe$final <- gsub("CONFIANZA", "PERCEP_CORRUP", corrup_recipe$final)
  corrup_recipe$label <- gsub("Confianza en", "Corrupción en", corrup_recipe$label)

  full_recipe <- rbind(master_recipe, corrup_recipe)

  # --- PROCESAMIENTO ---
  master_label_dictionary <- list()
  harmonized_list <- list()

  for (name in names(envipe_list)) {
    df <- envipe_list[[name]]
    year <- as.numeric(stringr::str_extract(name, "[0-9]{4}"))

    # 1. Limpieza de nombres (AP5_4_06 -> AP5_4_6)
    names(df) <- gsub("_0([1-9])", "_\\1", toupper(trimws(names(df))))

    # 2. Capturar etiquetas originales
    original_labels <- sapply(df, rk.get.label)
    for(col in names(original_labels)) {
       if(!is.null(original_labels[[col]]) && !(col %in% names(master_label_dictionary))) {
         master_label_dictionary[[col]] <- original_labels[[col]]
       }
    }

    # 3. Aplicar receta
    recipe_subset <- full_recipe[sapply(full_recipe$years, function(y_range) year %in% y_range), ]
    for(i in seq_len(nrow(recipe_subset))) {
      old_name <- recipe_subset$orig[i]
      final_name <- recipe_subset$final[i]
      if(old_name %in% names(df)) {
        names(df)[names(df) == old_name] <- final_name
      }
    }

    harmonized_list[[name]] <- df
  }

  # 4. Actualizar diccionario maestro con etiquetas finales
  unique_finals <- unique(full_recipe[, c("final", "label")])
  for(i in seq_len(nrow(unique_finals))) {
    master_label_dictionary[[unique_finals$final[i]]] <- unique_finals$label[i]
  }

  # 5. Reconstrucción de tipos (Crucial para bind_rows)
  cat("  Reconstruyendo tipos de datos para consistencia...\n")
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
        vec <- suppressWarnings(as.numeric(as.character(data_vector)))
        if (target_class == "integer") vec <- as.integer(vec)
        rebuilt_list[[col_name]] <- vec
      } else if (target_class == "factor") {
        rebuilt_list[[col_name]] <- as.factor(data_vector)
      } else {
        rebuilt_list[[col_name]] <- as.character(data_vector)
      }
    }
    final_df <- as.data.frame(rebuilt_list, stringsAsFactors = FALSE)
    final_df$year <- as.numeric(stringr::str_extract(name, "[0-9]{4}"))
    final_list[[name]] <- final_df
  }
  attr(final_list, "master_labels") <- master_label_dictionary
  return(final_list)
})
# =============================================================================
# FASE 4: COMBINACIÓN
# =============================================================================
combined_df <- local({
  cat("--- Combinando datos finales ---\n")
  final_df <- dplyr::bind_rows(envipe_list)
  master_labels <- attr(envipe_list, "master_labels")

  for (col_name in names(final_df)) {
    if (col_name %in% names(master_labels)) {
      rk.set.label(final_df[[col_name]], master_labels[[col_name]])
    }
  }
  rk.set.label(final_df$year, "Año de la encuesta")
  return(final_df)
})

# =============================================================================
# FASE 5: LIMPIEZA ROBUSTA DE VARIABLES GEOGRÁFICAS (CORREGIDA)
# =============================================================================
combined_df <- local({
  cat("--- Ejecutando Fase 5: Estandarización Geográfica Robusta ---\n")

  df <- combined_df
  require("dplyr")
  require("stringr")

  # --- 1. FUNCIÓN DE FORMATO TIPO TÍTULO (Mejorada) ---
  # Convierte "CIUDAD DE MEXICO" a "Ciudad de México" respetando conectores.
  to_title_mx <- function(x) {
    # Convertimos todo a minúsculas primero
    s <- str_to_lower(x)
    # Convertimos a Title Case general
    s <- str_to_title(s)
    # Corregimos las preposiciones que deben ir en minúsculas
    # Se usa regex con límites de palabra (\\b) para no romper palabras internas
    s <- gsub("\\bDe\\b", "de", s)
    s <- gsub("\\bDel\\b", "del", s)
    s <- gsub("\\bLa\\b", "la", s)
    s <- gsub("\\bLas\\b", "las", s)
    s <- gsub("\\bLos\\b", "los", s)
    s <- gsub("\\bCon\\b", "con", s)
    s <- gsub("\\bY\\b", "y", s)
    s <- gsub("\\bEn\\b", "en", s)
    return(s)
  }

  # --- 2. ESTANDARIZACIÓN DE ENTIDADES (NOM_ENT) ---
  # Usamos un diccionario manual para evitar errores de acentos o variantes
  cat("  Estandarizando Entidades Federativas...\n")

  # Limpieza previa: Mayúsculas y sin espacios extra
  df$NOM_ENT <- toupper(trimws(as.character(df$NOM_ENT)))

  # Diccionario de correcciones comunes (Prioriza el nombre corto y común)
  df <- df %>%
    mutate(NOM_ENT = case_when(
      grepl("MEXICO|MÉXICO", NOM_ENT) & !grepl("CIUDAD", NOM_ENT) ~ "Estado de México",
      grepl("DISTRITO|CDMX|CIUDAD DE M", NOM_ENT) ~ "Ciudad de México",
      grepl("COAHUILA", NOM_ENT) ~ "Coahuila",
      grepl("MICHOAC", NOM_ENT) ~ "Michoacán",
      grepl("VERACRUZ", NOM_ENT) ~ "Veracruz",
      grepl("QUER", NOM_ENT) ~ "Querétaro",
      grepl("YUCAT", NOM_ENT) ~ "Yucatán",
      grepl("SAN LUIS", NOM_ENT) ~ "San Luis Potosí",
      grepl("NUEVO LE", NOM_ENT) ~ "Nuevo León",
      TRUE ~ to_title_mx(NOM_ENT) # Para el resto, aplica formato estándar
    )) %>%
    mutate(NOM_ENT = as.factor(NOM_ENT))

  # --- 3. ESTANDARIZACIÓN DE MUNICIPIOS (NOM_MUN) ---
  cat("  Estandarizando Municipios...\n")

  # Convertimos a caracter, limpiamos espacios y aplicamos formato Título
  # Nota: No intentamos "inventar" acentos si no vienen en la base original,
  # pero estandarizamos la capitalización para agruparlos.
  df$NOM_MUN <- trimws(as.character(df$NOM_MUN))
  df$NOM_MUN <- to_title_mx(df$NOM_MUN)
  df$NOM_MUN <- as.factor(df$NOM_MUN)

  return(df)
})

# =============================================================================
# FASE 6: ETIQUETADO FINAL Y LIMPIEZA DE RESPUESTAS
# =============================================================================
cat("--- Ejecutando Fase 6: Etiquetado final y limpieza de respuestas ---\n")

combined_df <- local({
  df <- combined_df
  require("dplyr")
  require("forcats")
  require("rkward")

  # --- 6a. Etiquetas de variables ---
  # Verificamos si las columnas existen antes de etiquetar para no generar error
  if("NOM_ENT" %in% names(df)) rk.set.label(df$NOM_ENT, "Entidad Federativa")
  if("NOM_MUN" %in% names(df)) rk.set.label(df$NOM_MUN, "Municipio")

  # --- 6b. Unificar niveles de respuesta (Sí/No) ---
  # Unificamos "SÍ" (con acento y mayúscula) a "Sí" (Tipo título)
  df <- df %>%
    mutate(across(starts_with("AP4_2_"),
                  ~ fct_recode(., "Sí" = "SÍ", "Sí" = "SI", "Sí" = "Si")))

  # --- 6c. Limpieza general de factores ---
  # Elimina niveles vacíos y espacios en blanco en las etiquetas de todos los factores
  df <- df %>%
    mutate(across(where(is.factor), ~ fct_drop(.))) %>%
    mutate(across(where(is.factor), ~ fct_relabel(., ~ trimws(.))))

  cat("--- Fase 6 completada. Base de datos lista. ---\n\n")
  return(df)
})

