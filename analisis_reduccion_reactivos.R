# =============================================================================
# ANÁLISIS PSICOMÉTRICO PARA REDUCCIÓN DE REACTIVOS
# Figuras Educativas: DOC, EST, DIR, PMF
# Cuestionarios: HD, HSXXI, HI, CTXT
# Ciclos: 2024-2025 (2425) y 2025-2026 (2526)
#
# ── PIPELINE A: INSTRUMENTOS DE PERCEPCIÓN (HD, HSXXI, HI) ──────────────────
#   Supuesto reflectivo: ítems son manifestaciones de una variable latente.
#   1. TCT — Alpha, ritc, dificultad        → VOTA en decisión
#   2. GRM (Samejima)                       → VOTA en decisión
#   3. EFA unifactorial (comunalidades)     → VOTA en decisión
#   4. AFC unifactorial (lavaan, WLSMV)     → VOTA en decisión
#   5. Rasch/PCM — b, infit, outfit         → solo descriptivo
#   6. EGA (bootEGA + itemStability)        → solo descriptivo
#   7. Omega (psych) — omega_total/reducida → solo descriptivo
#
#   Regla de decisión (máx 4 votos):
#     ELIMINAR  ≥ 4 votos  |  REVISAR = 3  |  Conservar ≤ 2
#
# ── PIPELINE B: CUESTIONARIO DE CONTEXTO (CTXT) ─────────────────────────────
#   CTXT no es un instrumento reflectivo: los reactivos describen
#   características del hogar, acceso, recursos, participación, etc.
#   No se aplican Alpha, Omega, Rasch, GRM, EFA, EGA ni AFC.
#   Solo análisis descriptivo por ítem:
#     pct_NA, media, sd, varianza, frecuencias, prop_modal,
#     n_categorias_usadas, baja_variabilidad, efecto_techo/piso
#   Clasificación (no eliminación automática):
#     CONSERVAR          0 criterios problemáticos
#     REVISAR            1 criterio problemático
#     INFORMACIÓN LIMITADA ≥ 2 criterios problemáticos
#   Criterios: pct_NA>20% | prop_modal>80% | n_cat<2 | sd<0.50
#
# ── NOTA SOBRE AFC MULTIDIMENSIONAL ─────────────────────────────────────────
#   El crosswalk (CROSSWALK_COMPLETO) contiene la columna 'dimension' que
#   mapea cada ítem a su dimensión teórica. cargar_crosswalk_completo()
#   actualmente descarta esa columna. Cuando se quiera implementar AFC por
#   dimensión, basta con:
#     (a) preservar sub_cw$dimension en cargar_crosswalk_completo()
#     (b) construir un modelo CFA multi-grupo en analizar_items_psicometrico()
#         usando esa estructura como especificación.
#   VER REPORTE al final del script (función reportar_dimensiones_cw).
# =============================================================================

# =============================================================================
# 0. PAQUETES
# =============================================================================

paquetes <- c(
  "psych", "mirt", "EGAnet",
  "lavaan", "GPArotation",
  "ggplot2", "dplyr", "tidyr",
  "openxlsx", "readxl", "stringr", "purrr"
)

paquetes_faltantes <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes, dependencies = TRUE)
}
invisible(lapply(paquetes, library, character.only = TRUE))

# =============================================================================
# 1. CONFIGURACIÓN GLOBAL
# =============================================================================

CONFIG <- list(
  # --- Rutas ---
  dir_datos   = "C:/Users/almejia/Desktop/ANALISIS REACTIVOS/datos",
  dir_2425    = "C:/Users/almejia/Desktop/ANALISIS REACTIVOS/datos/2425",
  dir_2526    = "C:/Users/almejia/Desktop/ANALISIS REACTIVOS/datos/2526",
  dir_salida  = "C:/Users/almejia/Desktop/ANALISIS REACTIVOS/resultados",

  # --- Figuras y cuestionarios válidos ---
  figuras      = c("DIR", "DOC", "EST", "PMF"),
  cuestionarios = c("HD", "HSXXI", "HI", "CTXT"),

  # Combinaciones cuestionario × figura que realmente existen
  # CTXT: todas las figuras | HD y HSXXI: DOC y EST | HI: solo DOC
  figuras_por_cuestion = list(
    CTXT  = c("DIR", "DOC", "EST", "PMF"),
    HD    = c("DOC", "EST"),
    HSXXI = c("DOC", "EST"),
    HI    = c("DOC")
  ),

  # --- Máximo teórico por cuestionario (escala inicia en 0) ---
  # Se usa cuando TODOS los ítems del cuestionario comparten el mismo máximo.
  # Si el cuestionario mezcla escalas (como HD con 0-3 y 0-4), usar NULL
  # para que el pipeline detecte el máximo de cada ítem automáticamente.
  #
  # HD:    mezcla hd_conocimiento/habilidad (0-3) y hd_frecuencia (0-4) → NULL
  # HSXXI: hsxxi_acuerdo / hsxxi_frecuencia → 0-4 (max = 4)
  # HI:    hi_frecuencia / hi_acuerdo       → 0-4 (max = 4)
  # CTXT:  mezcla ctx_frecuencia (0-3) y ctx_acuerdo (0-4) → NULL
  #        ctx_binaria (0-1) se trata aparte en pipeline_ctxt rama "dicotomico"
  max_escala = list(
    HD    = NULL,  # mezcla 0-3 y 0-4 → detectar por ítem automáticamente
    HSXXI = 4,     # homogéneo 0-4
    HI    = 4,     # homogéneo 0-4
    CTXT  = NULL   # mezcla → detectar por ítem automáticamente
  ),

  # --- Umbrales psicométricos ---
  umbral_NA_pct      = 20,    # % de NAs por ítem para marcar como crítico
  umbral_alpha       = 0.70,
  umbral_ritc        = 0.30,
  umbral_dif_min     = 0.20,  # índice de dificultad normalizado mínimo
  umbral_dif_max     = 0.80,  # índice de dificultad normalizado máximo
  umbral_infit       = 1.30,
  umbral_outfit      = 1.30,
  umbral_a_grm       = 0.70,  # discriminación GRM mínima (↑ de 0.50)
  umbral_comunalidad = 0.30,  # comunalidad EFA mínima   (↑ de 0.20)
  umbral_estabilidad = 0.50,  # EGA item stability
  umbral_carga_AFC   = 0.40,  # carga estandarizada mínima en AFC unifactorial
  umbral_R2_AFC      = 0.20,  # R² mínimo en AFC (= carga² mínimo)
  umbral_votos       = 3      # votos para REVISAR; ≥4 → ELIMINAR (Rasch/EGA excluidos)
)

# Tratamiento de "No lo sé / Prefiero no contestar" → NA
# JUSTIFICACIÓN: estas respuestas indican ausencia de información sobre el
# constructo, no el nivel más bajo de la escala. Imputarlas como 0 sesgaría
# la media a la baja y distorsionaría ritc, b_Rasch y parámetro a del GRM.
# Se mantienen como NA y se reporta su proporción por ítem (flag_NA).
VALORES_NA <- c(
  "No lo sé/Prefiero no contestar",
  "No lo sé/ Prefiero no contestar",
  "No lo sé / Prefiero no contestar",
  "No lo sé", "Prefiero no contestar", "No aplica", "NA"
)

dir.create(CONFIG$dir_salida, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 2. CROSSWALK DE VARIABLES ENTRE CICLOS
# =============================================================================
# Los Q-números NO son estables entre ciclos. El crosswalk (validado por
# experto) define qué ítems incluir y cómo alinearlos.
#
# Lógica de la columna 'validado':
#   ELIMINAR  → excluir del análisis (variable admin, informativa, o sin interés)
#   OK        → incluir; el comportamiento depende de 'tipo_match':
#     EXACTO    → mismo constructo en ambos ciclos; comparar entre ciclos
#     NUEVO     → existe solo en 2526; analizar solo ese ciclo
#     SIN_MATCH → existe solo en 2425; analizar solo ese ciclo
#
# Nombre canónico: para ítems EXACTO se usa el code_2526 como referencia.

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Ruta al crosswalk — EDITA ESTA LÍNEA con la ruta exacta en tu equipo
CROSSWALK_PATH <- "C:/Users/almejia/Desktop/ANALISIS REACTIVOS/crosswalk_variables_2425_2526.xlsx"

cat(sprintf("[CONFIG] Crosswalk: %s — %s\n",
            CROSSWALK_PATH,
            if (file.exists(CROSSWALK_PATH)) "ENCONTRADO" else "NO ENCONTRADO"))

#' Carga y estructura el crosswalk para una hoja dada.
#'
#' @param hoja  Ej. "CTXT_DIR", "HD_DOC", "HSXXI_EST"
#' @return list con tres data.frames: $exacto, $nuevo, $sin_match
#'   $exacto   : code_2425, canon (= code_2526)  — presentes en ambos ciclos
#'   $nuevo    : canon (= code_2526)              — solo en 2526
#'   $sin_match: canon (= code_2425)              — solo en 2425
#'   Devuelve NULL si el crosswalk no existe o la hoja no se encuentra.

#' Carga el crosswalk completo para una hoja (con texto y metadatos).
#' @return data.frame: code_2425, code_2526, texto_2425, texto_2526, tipo_match
#'   Incluye todas las filas validado=="OK". NULL si no hay datos.
cargar_crosswalk_completo <- function(hoja) {
  if (!file.exists(CROSSWALK_PATH)) return(NULL)
  cw <- tryCatch(
    openxlsx::read.xlsx(CROSSWALK_PATH, sheet = "CROSSWALK_COMPLETO",
                        na.strings = c("", "NA")),
    error = function(e) NULL
  )
  if (is.null(cw)) return(NULL)

  cw$hoja <- stringr::str_replace_all(trimws(as.character(cw$hoja)), " ", "_")
  hoja    <- stringr::str_replace_all(trimws(hoja), " ", "_")

  sub_cw <- cw[!is.na(cw$hoja) & cw$hoja == hoja, ]
  ok     <- !is.na(sub_cw$validado) & trimws(sub_cw$validado) == "OK"
  sub_cw <- sub_cw[ok, ]
  if (nrow(sub_cw) == 0) return(NULL)

  sub_cw$tipo_match <- trimws(toupper(as.character(sub_cw$tipo_match)))
  # Normalizar NUEVO_2526 → NUEVO
  sub_cw$tipo_match[sub_cw$tipo_match == "NUEVO_2526"] <- "NUEVO"

  data.frame(
    code_2425  = trimws(as.character(sub_cw$code_2425)),
    code_2526  = trimws(as.character(sub_cw$code_2526)),
    texto_2425 = trimws(as.character(
      if ("texto_2425" %in% names(sub_cw)) sub_cw$texto_2425 else sub_cw$code_2425)),
    texto_2526 = trimws(as.character(
      if ("texto_2526" %in% names(sub_cw)) sub_cw$texto_2526 else sub_cw$code_2526)),
    tipo_match = sub_cw$tipo_match,
    stringsAsFactors = FALSE
  )
}

cargar_crosswalk <- function(hoja) {
  cw_df <- cargar_crosswalk_completo(hoja)
  if (is.null(cw_df)) {
    cat(sprintf("  [CROSSWALK] Sin datos para hoja '%s'\n", hoja))
    return(NULL)
  }

  df_exacto <- cw_df[cw_df$tipo_match == "EXACTO" &
                       !is.na(cw_df$code_2425) & cw_df$code_2425 != "NA" &
                       !is.na(cw_df$code_2526) & cw_df$code_2526 != "NA", ]
  df_nuevo  <- cw_df[cw_df$tipo_match == "NUEVO" &
                       !is.na(cw_df$code_2526) & cw_df$code_2526 != "NA", ]
  df_sinm   <- cw_df[cw_df$tipo_match == "SIN_MATCH" &
                       !is.na(cw_df$code_2425) & cw_df$code_2425 != "NA", ]

  cat(sprintf("  [CROSSWALK] '%s': %d EXACTO | %d NUEVO | %d SIN_MATCH\n",
              hoja, nrow(df_exacto), nrow(df_nuevo), nrow(df_sinm)))

  list(
    exacto    = data.frame(code_2425 = df_exacto$code_2425,
                           canon     = df_exacto$code_2526,
                           stringsAsFactors = FALSE),
    nuevo     = data.frame(canon = df_nuevo$code_2526, stringsAsFactors = FALSE),
    sin_match = data.frame(canon = df_sinm$code_2425,  stringsAsFactors = FALSE)
  )
}

# =============================================================================
# 3. CARGA Y ESTANDARIZACIÓN DE DATOS
# =============================================================================

#' Carga un archivo Excel, estandariza nombres de columna y aplica el crosswalk.
#'
#' Según el ciclo y el crosswalk, selecciona solo las columnas que corresponden:
#'   Ciclo 2425 → columnas en $exacto$code_2425 (renombradas a canon) + $sin_match$canon
#'   Ciclo 2526 → columnas en $exacto$canon + $nuevo$canon
#'
#' @param ruta    Ruta completa al archivo .xlsx
#' @param ciclo   "2425" o "2526"
#' @param mapa_cw Lista devuelta por cargar_crosswalk(), o NULL
#' @return data.frame con columnas en nombres canónicos y atributo 'tipo_item'

#' Carga datos brutos: extrae Q-columnas con sus nombres naturales (sin renombrar).
#' Para 2425: extrae código Q del nombre compuesto (PRE_CTXT_DIR_Q10_LAPTOP → Q10_LAPTOP).
#' Para 2526: columnas ya tienen nombres Q directos.
#' @return data.frame con columnas nombradas por su código Q (code_2425 ó code_2526)
cargar_base <- function(ruta, ciclo, mapa_cw = NULL) {
  dat <- tryCatch(
    readxl::read_excel(ruta, na = c("", "NA", "N/A")),
    error = function(e) { warning("No se pudo leer: ", ruta, "\n  ", e$message); NULL }
  )
  if (is.null(dat)) return(NULL)

  # Extraer código Q de nombres compuestos en 2425
  # PRE_CTXT_DIR_Q10_LAPTOP  →  Q10_LAPTOP
  if (ciclo == "2425") {
    extraido   <- stringr::str_extract(names(dat), "Q\\d+.*$")
    extraido   <- stringr::str_replace_all(extraido, "[^A-Za-z0-9_]", "_")
    extraido   <- stringr::str_remove(extraido, "_+$")   # quitar guiones finales
    names(dat) <- ifelse(is.na(extraido), paste0("VAR_", seq_along(extraido)), extraido)
  }

  # Seleccionar columnas Q y recodificar etiquetas de texto a numérico
  cols_q <- stringr::str_detect(names(dat), "^Q\\d+")
  dat_q  <- dat[, cols_q, drop = FALSE]

  # Respuestas que deben tratarse como dato perdido (psicométricamente justificado)
  VALORES_NA <- c(
    "No lo sé/Prefiero no contestar",
    "No lo sé/ Prefiero no contestar",
    "No lo sé / Prefiero no contestar",
    "No lo sé", "Prefiero no contestar", "No aplica", "NA"
  )

  # Mapa completo etiqueta de texto → código numérico para todos los cuestionarios.
  # HD     : hd_conocimiento (0-3), hd_habilidad (0-3), hd_frecuencia (0-4)
  # HI     : hi_frecuencia (0-4), hi_acuerdo (0-4)
  # HSXXI  : hsxxi_acuerdo (0-4), hsxxi_frecuencia (0-4)
  # CTXT   : ctx_frecuencia (0-3), ctx_receptor (0-4), ctx_acuerdo (0-4), ctx_binaria (0-1)
  MAPA_TEXTO_NUM <- c(
    # HD: conocimiento (0-3)
    "No sé / Nunca he oído hablar de esto"                                        = 0,
    "Conozco un poco el tema"                                                     = 1,
    "Sí, conozco bien este tema"                                                  = 2,
    "Totalmente, e incluso podría explicárselo a otras personas"                  = 3,
    # HD: habilidad (0-3)
    "No sé cómo hacerlo"                                                          = 0,
    "Puedo hacerlo con ayuda"                                                     = 1,
    "Puedo hacerlo por mi cuenta"                                                 = 2,
    "Puedo hacerlo con confianza y si es necesario puedo ayudar a otras personas" = 3,
    # HD/HSXXI/HI: frecuencia (0-4)
    "Nunca"                                                                       = 0,
    "Rara vez"                                                                    = 1,
    "Algunas veces"                                                               = 2,
    "Frecuentemente"                                                              = 3,
    "Siempre"                                                                     = 4,
    # HI: frecuencia propia (0-4)
    "Casi nunca"                                                                  = 0,
    "Algunas veces durante el semestre/año"                                       = 1,
    "1 a 3 veces al mes"                                                          = 2,
    "1 a 3 veces por semana"                                                      = 3,
    "Casi todos los días"                                                         = 4,
    # HI: acuerdo (0-4)
    "No estoy de acuerdo"                                                         = 0,
    "Algo de acuerdo"                                                             = 3,
    "De acuerdo"                                                                  = 2,
    "Muy de acuerdo"                                                              = 3,
    "Totalmente de acuerdo"                                                       = 4,
    # HSXXI: acuerdo (0-4)
    "Totalmente en desacuerdo"                                                    = 0,
    "Algo en desacuerdo"                                                          = 1,
    "Ni de acuerdo ni en desacuerdo"                                              = 2,
    # CTXT: frecuencia (0-3) — "Nunca"=0 y "Siempre"=4 ya definidos arriba
    "Pocas veces"                                                                 = 1,
    "Muchas veces"                                                                = 2,
    # CTXT: receptividad (0-4)
    "Nada receptivos"                                                             = 0,
    "Poco receptivos"                                                             = 1,
    "Indiferentes"                                                                = 2,
    "Medianamente receptivos"                                                     = 3,
    "Muy receptivos"                                                              = 4,
    # Binaria (0-1)
    "No"                                                                          = 0,
    "Sí"                                                                          = 1,
    "Si"                                                                          = 1
  )

  recodificar_col <- function(x) {
    if (is.numeric(x)) return(x)
    x_str <- trimws(as.character(x))
    # Convertir VALORES_NA a NA antes de cualquier otra cosa
    x_str[x_str %in% VALORES_NA] <- NA_character_
    num <- suppressWarnings(as.numeric(x_str))
    if (is.character(x) || is.factor(x)) {
      mapeado <- MAPA_TEXTO_NUM[x_str]
      return(as.numeric(ifelse(!is.na(mapeado), mapeado, num)))
    }
    num
  }

  dat_q <- as.data.frame(lapply(dat_q, recodificar_col))

  # Descartar columnas 100% NA (padres de opción múltiple sin sub-ítems)
  todo_na <- colMeans(is.na(dat_q)) == 1
  if (any(todo_na))
    cat(sprintf("  [INFO] Columnas 100%% NA descartadas: %s\n",
                paste(names(dat_q)[todo_na], collapse = ", ")))
  dat_q <- dat_q[, !todo_na, drop = FALSE]
  if (ncol(dat_q) == 0) { warning("Sin columnas con datos en: ", ruta); return(NULL) }

  cat(sprintf("  [%s] %d columnas Q disponibles en datos\n", ciclo, ncol(dat_q)))
  dat_q
}

#' Construye el catálogo de archivos disponibles para los dos ciclos.
#'
#' Nomenclatura esperada:
#'   2425/  CUESTIONARIO_FIGURA_momento.xlsx   (p.ej. HSXXI_EST_pre.xlsx)
#'   2526/  CUESTIONARIO_FIGURA_momento.xlsx

catalogo_archivos <- function() {
  # Patrón esperado: CUESTIONARIO_FIGURA_momento.xlsx (ej. CTXT_DIR_pre.xlsx)
  # Cuestionarios válidos: HD, HSXXI, HI, CTXT
  CUESTIONS_VALIDAS <- paste(CONFIG$cuestionarios, collapse = "|")

  leer_dir <- function(dir_ciclo, ciclo_etiq) {
    archivos <- list.files(dir_ciclo, pattern = "\\.xlsx$",
                           full.names = TRUE, ignore.case = TRUE)
    if (length(archivos) == 0) return(tibble::tibble())

    # Excluir archivos que no sean bases de datos de respuestas:
    # diccionarios, catálogos, archivos temporales (~$), etc.
    nombre_base <- tools::file_path_sans_ext(basename(archivos))
    es_valido <- stringr::str_detect(
      toupper(nombre_base),
      paste0("^(", CUESTIONS_VALIDAS, ")_")
    ) & !stringr::str_starts(nombre_base, "~")

    archivos <- archivos[es_valido]
    if (length(archivos) == 0) return(tibble::tibble())

    info <- tibble::tibble(ruta = archivos) %>%
      dplyr::mutate(
        nombre    = tools::file_path_sans_ext(basename(ruta)),
        partes    = stringr::str_split(nombre, "_"),
        cuestion  = purrr::map_chr(partes, ~ toupper(.x[1])),
        figura    = purrr::map_chr(partes, ~ toupper(.x[2])),
        momento   = purrr::map_chr(partes, ~ if (length(.x) >= 3) tolower(.x[3]) else "pre"),
        ciclo     = ciclo_etiq
      ) %>%
      dplyr::select(-partes, -nombre)
    info
  }

  dplyr::bind_rows(
    leer_dir(CONFIG$dir_2425, "2425"),
    leer_dir(CONFIG$dir_2526, "2526")
  )
}

# =============================================================================
# 3. DIAGNÓSTICO DE DATOS FALTANTES
# =============================================================================

diagnostico_NA <- function(datos, etiqueta) {
  pct_na <- colMeans(is.na(datos)) * 100
  df <- data.frame(
    Item       = names(pct_na),
    pct_NA     = round(pct_na, 1),
    n_validos  = colSums(!is.na(datos)),
    flag_NA    = ifelse(pct_na > CONFIG$umbral_NA_pct, "DATOS_INSUFICIENTES", "OK"),
    stringsAsFactors = FALSE
  )
  n_prob <- sum(df$flag_NA == "DATOS_INSUFICIENTES")
  cat(sprintf("  [NA] %s — %d ítems con >%d%% datos faltantes\n",
              etiqueta, n_prob, CONFIG$umbral_NA_pct))
  df
}

# =============================================================================
# 4. PIPELINE PSICOMÉTRICO CENTRAL
# =============================================================================

#' Ejecuta TCT + IRT (Rasch + GRM) + EFA + EGA para un data.frame de ítems.
#'
#' @param datos      data.frame con solo los ítems (numérico, escala inicia en 0)
#' @param etiqueta   Cadena identificadora para mensajes y archivos
#' @param max_item   Valor máximo teórico de la escala (ej. 3 para 0-3, 4 para 0-4).
#'                   Si NULL se deriva del máximo observado en los datos.
#' @return data.frame con todos los indicadores por ítem

analizar_items_psicometrico <- function(datos, etiqueta, max_item = NULL) {

  # Máximo teórico por ítem (escala inicia en 0).
  max_obs <- sapply(datos, function(x) {
    v <- max(x, na.rm = TRUE)
    if (is.infinite(v) || is.nan(v)) NA_real_ else v
  })

  max_por_item <- if (!is.null(max_item) && length(max_item) == 1) {
    setNames(rep(as.numeric(max_item), ncol(datos)), names(datos))
  } else {
    max_obs
  }

  MAX_ESCALA_LIKERT <- 10

  continuas    <- !is.na(max_por_item) & max_por_item > MAX_ESCALA_LIKERT
  problemas_max <- is.na(max_por_item) | max_por_item <= 0

  # --- Tabla descriptiva para TODOS los ítems (incluyendo continuos y sin datos) ---
  # Se calcula antes de filtrar para que aparezcan en la salida con su razón.
  desc_todos <- data.frame(
    Item        = names(datos),
    n_validos   = as.integer(colSums(!is.na(datos))),
    pct_NA      = round(colMeans(is.na(datos)) * 100, 1),
    media       = round(sapply(datos, mean, na.rm = TRUE), 3),
    max_observado = max_obs,
    tipo_variable = dplyr::case_when(
      continuas     ~ "Continua (max>10)",
      problemas_max ~ "Sin variabilidad",
      TRUE          ~ "Escala"
    ),
    stringsAsFactors = FALSE
  )

  if (any(continuas)) {
    cat(sprintf("  [AVISO] %d ítems continuos excluidos del análisis psicométrico (max > %d): %s\n",
                sum(continuas), MAX_ESCALA_LIKERT,
                paste(names(max_por_item)[continuas], collapse = ", ")))
  }

  # Ítems con max NA/0 (todo-NA o sin variabilidad)
  problemas_max <- is.na(max_por_item) | max_por_item <= 0
  if (any(problemas_max)) {
    cat(sprintf("  [AVISO] %d ítems con max NA o ≤0: %s\n",
                sum(problemas_max),
                paste(names(max_por_item)[problemas_max], collapse = ", ")))
    max_por_item[problemas_max] <- 1
  }

  # Excluir variables continuas de los datos antes de cualquier análisis
  items_excluir <- names(max_por_item)[continuas | problemas_max]
  datos <- datos[, !names(datos) %in% items_excluir, drop = FALSE]
  max_por_item <- max_por_item[names(datos)]

  if (ncol(datos) == 0) {
    warning(etiqueta, ": sin ítems tipo escala tras excluir variables continuas.")
    return(data.frame(Item = character(0)))
  }

  # Para mirt: máximo único = el mayor de todos los ítems válidos
  n_cat <- max(max_por_item, na.rm = TRUE) + 1

  cat(sprintf("  [INFO] Máximos por ítem (únicos): %s\n",
              paste(sort(unique(max_por_item)), collapse = ", ")))

  # Remover ítems con >20% NA antes de modelos
  diag_na <- diagnostico_NA(datos, etiqueta)
  items_ok <- diag_na$Item[diag_na$flag_NA == "OK"]
  datos_limpios <- datos[, items_ok, drop = FALSE]

  # Recuperar tipo_item del atributo (EXACTO / NUEVO / SIN_MATCH / SIN_CROSSWALK)
  tipo_item_attr <- attr(datos, "tipo_item")
  tipo_item_vec  <- if (!is.null(tipo_item_attr)) {
    tipo_item_attr[names(datos)]
  } else {
    setNames(rep("SIN_CROSSWALK", ncol(datos)), names(datos))
  }

  tabla <- data.frame(
    Item        = names(datos),
    tipo_item   = tipo_item_vec,            # EXACTO / NUEVO / SIN_MATCH
    max_escala  = max_por_item[names(datos)],
    pct_NA      = diag_na$pct_NA,
    flag_NA     = diag_na$flag_NA,
    stringsAsFactors = FALSE
  )

  if (ncol(datos_limpios) < 3) {
    warning(etiqueta, ": menos de 3 ítems con datos suficientes. Análisis omitido.")
    tabla$decision_final <- "INSUFICIENTE_DATOS"
    return(tabla)
  }

  # -------------------------------------------------------------------------
  # 4a. TCT
  # -------------------------------------------------------------------------
  cat("  TCT...\n")
  alpha_obj <- tryCatch(
    suppressWarnings(psych::alpha(datos_limpios, check.keys = FALSE)),
    error = function(e) { warning("TCT error: ", e$message); NULL }
  )

  if (!is.null(alpha_obj)) {
    media_item  <- colMeans(datos_limpios, na.rm = TRUE)
    # Índice de dificultad normalizado por ítem: media / max_item_propio
    # Esto es correcto cuando hay ítems con distinto máximo (0-3 y 0-4 en HD).
    max_limpios <- max_por_item[names(datos_limpios)]
    p_item      <- media_item / max_limpios
    ritc        <- alpha_obj$item.stats$r.drop
    alpha_drop  <- alpha_obj$alpha.drop[, "raw_alpha"]
    alpha_global <- alpha_obj$total$raw_alpha

    cat(sprintf("    Alpha = %.3f\n", alpha_global))

    tct_df <- data.frame(
      Item           = names(datos_limpios),
      media          = round(media_item, 3),
      sd             = round(apply(datos_limpios, 2, sd, na.rm = TRUE), 3),
      p_dificultad   = round(p_item, 3),
      ritc           = round(ritc, 3),
      alpha_sin_item = round(alpha_drop, 3),
      eliminar_TCT   = ifelse(
        ritc < CONFIG$umbral_ritc |
          p_item < CONFIG$umbral_dif_min |
          p_item > CONFIG$umbral_dif_max |
          alpha_drop > alpha_global + 0.01,
        "ELIMINAR", "Conservar"
      ),
      stringsAsFactors = FALSE
    )
    tabla <- dplyr::left_join(tabla, tct_df, by = "Item")
  }

  # Garantizar que datos_limpios es matriz numérica antes de IRT/EFA/EGA
  datos_limpios <- as.data.frame(lapply(datos_limpios, function(x) {
    v <- suppressWarnings(as.numeric(as.character(x)))
    v
  }))
  datos_limpios <- datos_limpios[, sapply(datos_limpios, function(x) !all(is.na(x))), drop = FALSE]

  if (ncol(datos_limpios) < 2) {
    warning(etiqueta, ": menos de 2 columnas numéricas tras limpieza — análisis omitido.")
    return(tabla)
  }

  # -------------------------------------------------------------------------
  # 4b. IRT: Rasch (PCM)
  # -------------------------------------------------------------------------
  cat("  Rasch...\n")
  tryCatch({
    mod_rasch <- mirt::mirt(datos_limpios, model = 1,
                            itemtype = "Rasch", verbose = FALSE)
    fit_r <- mirt::itemfit(mod_rasch, fit_statistics = "infit", na.rm = TRUE)
    params_r <- mirt::coef(mod_rasch, IRTpars = TRUE, simplify = TRUE)$items

    rasch_df <- data.frame(
      Item         = rownames(params_r),
      b_Rasch      = round(params_r[, "b"], 3),
      infit_MNSQ   = round(fit_r$infit,  3),
      outfit_MNSQ  = round(fit_r$outfit, 3),
      eliminar_Rasch = ifelse(
        fit_r$infit  > CONFIG$umbral_infit |
          fit_r$outfit > CONFIG$umbral_outfit |
          fit_r$infit  < 0.70,          # sobreajuste
        "ELIMINAR", "Conservar"
      ),
      stringsAsFactors = FALSE
    )
    tabla <- dplyr::left_join(tabla, rasch_df, by = "Item")
  }, error = function(e) warning("Rasch error: ", e$message))

  # -------------------------------------------------------------------------
  # 4c. IRT: GRM (Samejima)
  # -------------------------------------------------------------------------
  cat("  GRM...\n")
  tryCatch({
    max_por_item <- apply(datos_limpios, 2, max, na.rm = TRUE)
    grm_type <- if (all(max_por_item <= 1, na.rm = TRUE)) "2PL" else "graded"
    mod_grm <- mirt::mirt(datos_limpios, model = 1,
                          itemtype = grm_type, verbose = FALSE)
    coef_g   <- mirt::coef(mod_grm, IRTpars = TRUE, simplify = TRUE)$items
    # coef puede ser matriz (graded, múltiples ítems) o vector con nombre (2PL, 1 ítem)
    if (is.matrix(coef_g)) {
      items_g <- rownames(coef_g)
      a_vals  <- as.numeric(coef_g[, "a"])
    } else {
      items_g <- names(mirt::coef(mod_grm, IRTpars = TRUE))
      items_g <- items_g[items_g != "GroupPars"]
      a_vals  <- sapply(items_g, function(nm) {
        v <- mirt::coef(mod_grm, IRTpars = TRUE)[[nm]]
        if (is.matrix(v)) as.numeric(v[1, "a"]) else as.numeric(v["a"])
      })
    }
    info_g   <- mirt::iteminfo(mod_grm, Theta = matrix(0))

    grm_df <- data.frame(
      Item            = items_g,
      a_GRM           = round(a_vals, 3),
      info_theta0_GRM = round(as.numeric(info_g), 3),
      eliminar_GRM    = ifelse(a_vals < CONFIG$umbral_a_grm,
                               "ELIMINAR", "Conservar"),
      stringsAsFactors = FALSE
    )
    tabla <- dplyr::left_join(tabla, grm_df, by = "Item")

    # Guardar ICC (curvas características) y TIC (información del test)
    tryCatch({
      dir_fig <- file.path(CONFIG$dir_salida, "figuras")
      dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
      ruta_icc <- file.path(dir_fig, paste0(etiqueta, "_ICC_GRM.pdf"))
      pdf(ruta_icc, width = 12, height = 8)
      n_g <- min(ncol(datos_limpios), 24)
      for (i in seq(1, n_g, by = 6)) {
        idx <- i:min(i + 5, n_g)
        mirt::plot(mod_grm, type = "trace", which.items = idx,
                   main = paste0(etiqueta, " — ICC ítems ", min(idx), "–", max(idx)))
      }
      # Curva de información del test
      mirt::plot(mod_grm, type = "info",
                 main = paste0(etiqueta, " — Información del test (GRM)"))
      dev.off()
      cat("  [GRM] ICC guardado:", basename(ruta_icc), "\n")
    }, error = function(e) NULL)

  }, error = function(e) warning("GRM error: ", e$message))

  # -------------------------------------------------------------------------
  # 4d. EFA (1 factor para comunalidades; n_factores por análisis paralelo)
  # -------------------------------------------------------------------------
  cat("  EFA...\n")
  tryCatch({
    # Comunalidades unidemensionales (indicador de representatividad)
    efa1 <- psych::fa(datos_limpios, nfactors = 1, fm = "ml",
                      rotate = "none", cor = "poly", warnings = FALSE)
    comunalidades <- efa1$communality

    efa_df <- data.frame(
      Item            = names(comunalidades),
      comunalidad_EFA = round(comunalidades, 3),
      eliminar_EFA    = ifelse(comunalidades < CONFIG$umbral_comunalidad,
                               "ELIMINAR", "Conservar"),
      stringsAsFactors = FALSE
    )
    tabla <- dplyr::left_join(tabla, efa_df, by = "Item")
  }, error = function(e) warning("EFA error: ", e$message))

  # -------------------------------------------------------------------------
  # 4e. AFC — Análisis Factorial Confirmatorio (modelo unifactorial con lavaan)
  # Vota en la decisión final junto con TCT, GRM y EFA.
  # Falla graciosamente ante convergencia no lograda, N insuficiente o
  # matriz singular: en ese caso carga_AFC / R2_AFC quedan como NA.
  # -------------------------------------------------------------------------
  cat("  AFC...\n")
  tryCatch({
    items_afc  <- names(datos_limpios)
    modelo_afc <- paste0("F =~ ", paste(items_afc, collapse = " + "))
    fit_afc    <- lavaan::cfa(modelo_afc, data = datos_limpios,
                              ordered = TRUE, estimator = "WLSMV")
    params     <- lavaan::parameterEstimates(fit_afc, standardized = TRUE)
    cargas     <- params[params$op == "=~", ]

    afc_df <- data.frame(
      Item         = cargas$rhs,
      carga_AFC    = round(cargas$std.all, 3),
      R2_AFC       = round(cargas$std.all^2, 3),
      eliminar_AFC = ifelse(
        abs(cargas$std.all) < CONFIG$umbral_carga_AFC |
          cargas$std.all^2  < CONFIG$umbral_R2_AFC,
        "ELIMINAR", "Conservar"),
      stringsAsFactors = FALSE
    )
    tabla <- dplyr::left_join(tabla, afc_df, by = "Item")
    cat(sprintf("    AFC WLSMV: %d ítems con carga < %.2f\n",
                sum(afc_df$eliminar_AFC == "ELIMINAR", na.rm = TRUE),
                CONFIG$umbral_carga_AFC))
  }, error = function(e) warning("AFC error: ", e$message))

  # -------------------------------------------------------------------------
  # 4f. EGA (Psicometría de Redes) — solo descriptivo, no vota
  # -------------------------------------------------------------------------
  cat("  EGA...\n")
  tryCatch({
    ega_r <- EGAnet::EGA(datos_limpios, model = "glasso",
                         plot.EGA = FALSE, verbose = FALSE, seed = 123)

    # Guardar red EGA
    tryCatch({
      dir_fig <- file.path(CONFIG$dir_salida, "figuras")
      dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
      ruta_ega <- file.path(dir_fig, paste0(etiqueta, "_Red_EGA.png"))
      png(ruta_ega, width = 1800, height = 1400, res = 150)
      plot(ega_r, title = paste0(etiqueta, " — Red EGA (glasso)"))
      dev.off()
      cat("  [EGA] Red guardada:", basename(ruta_ega), "\n")
    }, error = function(e) NULL)

    boot_ega <- tryCatch(
      EGAnet::bootEGA(datos_limpios, model = "glasso",
                      iter = 100, plot.typicalStructure = FALSE,
                      verbose = FALSE, seed = 123),
      error = function(e) NULL
    )
    estab <- if (!is.null(boot_ega)) {
      tryCatch(EGAnet::itemStability(boot_ega), error = function(e) NULL)
    } else NULL
    est_vec <- if (!is.null(estab)) {
      tryCatch(estab$item.stability$empirical.dimensions, error = function(e) NULL)
    } else NULL
    if (is.null(est_vec)) stop("itemStability no disponible")

    ega_df <- data.frame(
      Item            = names(est_vec),
      estabilidad_EGA = round(est_vec, 3),
      eliminar_Red    = ifelse(est_vec < CONFIG$umbral_estabilidad,
                               "ELIMINAR", "Conservar"),
      stringsAsFactors = FALSE
    )
    tabla <- dplyr::left_join(tabla, ega_df, by = "Item")
  }, error = function(e) warning("EGA error: ", e$message))

  # -------------------------------------------------------------------------
  # 4g. Omega (psych) — fiabilidad compuesta del conjunto completo de ítems
  # Resultado a nivel escala; se guarda como atributo de la tabla.
  # -------------------------------------------------------------------------
  cat("  Omega...\n")
  omega_total_val <- tryCatch({
    om_full <- psych::omega(datos_limpios, nfactors = 1,
                            plot = FALSE, warnings = FALSE, fm = "ml")
    round(om_full$omega.tot, 3)
  }, error = function(e) { warning("Omega error: ", e$message); NA_real_ })
  attr(tabla, "omega_total") <- omega_total_val
  cat(sprintf("    omega_total = %s\n",
              if (is.na(omega_total_val)) "NA" else sprintf("%.3f", omega_total_val)))

  # -------------------------------------------------------------------------
  # 4h. Decisión integrada por votos
  # Métodos que votan: TCT, GRM, EFA, AFC (máx 4 votos).
  # Rasch y EGA son solo evidencia descriptiva — no votan.
  # ELIMINAR ≥4 | REVISAR =3 | Conservar ≤2
  # -------------------------------------------------------------------------
  cols_votan <- intersect(
    c("eliminar_TCT", "eliminar_GRM", "eliminar_EFA", "eliminar_AFC"),
    names(tabla)
  )

  if (length(cols_votan) > 0) {
    tabla$votos_eliminacion <- rowSums(
      sapply(cols_votan, function(col) {
        v <- tabla[[col]]
        ifelse(is.na(v), 0L, as.integer(v == "ELIMINAR"))
      }),
      na.rm = TRUE
    )

    tabla$decision_final <- dplyr::case_when(
      tabla$flag_NA == "DATOS_INSUFICIENTES" ~ "DATOS_INSUFICIENTES",
      tabla$votos_eliminacion >= 4           ~ "ELIMINAR",
      tabla$votos_eliminacion == 3           ~ "REVISAR",
      TRUE                                   ~ "Conservar"
    )
  }

  # Omega escala reducida (solo ítems Conservar)
  tryCatch({
    items_cons <- tabla$Item[!is.na(tabla$decision_final) &
                               tabla$decision_final == "Conservar" &
                               tabla$Item %in% names(datos_limpios)]
    if (length(items_cons) >= 3) {
      om_red <- psych::omega(datos_limpios[, items_cons, drop = FALSE],
                             nfactors = 1, plot = FALSE, warnings = FALSE, fm = "ml")
      omega_red_val <- round(om_red$omega.tot, 3)
      attr(tabla, "omega_reducida") <- omega_red_val
      cat(sprintf("  Omega escala reducida (%d ítems Conservar): %.3f\n",
                  length(items_cons), omega_red_val))
    } else {
      attr(tabla, "omega_reducida") <- NA_real_
    }
  }, error = function(e) {
    warning("Omega reducida error: ", e$message)
    attr(tabla, "omega_reducida") <- NA_real_
  })

  # -------------------------------------------------------------------------
  # 4i. Gráfico de perfil de ítems (ritc + dificultad + decisión)
  # -------------------------------------------------------------------------
  tryCatch({
    dir_fig <- file.path(CONFIG$dir_salida, "figuras")
    dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
    ruta_perfil <- file.path(dir_fig, paste0(etiqueta, "_Perfil_items.png"))

    n_items <- nrow(tabla)
    alto    <- max(600, n_items * 18)
    png(ruta_perfil, width = 1400, height = alto, res = 120)

    # Colores por decisión
    col_dec <- dplyr::case_when(
      tabla$decision_final == "ELIMINAR"           ~ "#CC0000",
      tabla$decision_final == "DATOS_INSUFICIENTES"~ "#888888",
      TRUE                                          ~ "#006600"
    )

    op <- par(mfrow = c(1, 2), mar = c(4, 6, 3, 1), oma = c(0, 0, 3, 0))

    # Panel izquierdo: ritc
    ritc_vals <- ifelse(is.na(tabla$ritc), 0, tabla$ritc)
    barplot(ritc_vals,
            names.arg = tabla$Item, horiz = TRUE, las = 1,
            col = col_dec, border = NA,
            xlab = "Correlación ítem-total (ritc)",
            main = "ritc por ítem",
            xlim = c(0, max(1, max(ritc_vals, na.rm = TRUE) * 1.1)))
    abline(v = CONFIG$umbral_ritc, lty = 2, col = "#856404", lwd = 1.5)

    # Panel derecho: dificultad
    dif_vals <- ifelse(is.na(tabla$p_dificultad), 0, tabla$p_dificultad)
    barplot(dif_vals,
            names.arg = tabla$Item, horiz = TRUE, las = 1,
            col = col_dec, border = NA,
            xlab = "Índice de dificultad (p)",
            main = "Dificultad por ítem",
            xlim = c(0, 1))
    abline(v = CONFIG$umbral_dif_min, lty = 2, col = "#856404", lwd = 1.5)
    abline(v = CONFIG$umbral_dif_max, lty = 2, col = "#856404", lwd = 1.5)

    mtext(paste0(etiqueta, " — Perfil de ítems"),
          outer = TRUE, cex = 1.2, font = 2)
    legend("topright",
           legend = c("Conservar", "ELIMINAR", "Sin datos"),
           fill   = c("#006600", "#CC0000", "#888888"),
           border = NA, bty = "n", cex = 0.8)

    par(op)
    dev.off()
    cat("  [Perfil] Gráfico guardado:", basename(ruta_perfil), "\n")
  }, error = function(e) NULL)

  # Alpha de la escala reducida
  items_conservar <- tabla$Item[tabla$decision_final == "Conservar" &
                                  tabla$Item %in% names(datos_limpios)]
  if (length(items_conservar) >= 3) {
    alpha_red <- tryCatch(
      suppressWarnings(psych::alpha(datos_limpios[, items_conservar], check.keys = FALSE))$total$raw_alpha,
      error = function(e) NA_real_
    )
    cat(sprintf("  Alpha escala reducida (%d ítems): %.3f\n",
                length(items_conservar), alpha_red))
    if (!is.na(alpha_red) && alpha_red < CONFIG$umbral_alpha) {
      warning("Alpha reducida por debajo del umbral en: ", etiqueta)
    }
    attr(tabla, "alpha_reducida") <- alpha_red
  }

  # --- Agregar ítems excluidos (continuos / sin variabilidad) a la tabla ---
  # Aparecen con indicadores NA y decision_final explicativa, para que figuren
  # en la salida y se pueda ver por qué no tienen análisis psicométrico.
  items_en_tabla <- tabla$Item
  items_excluidos <- desc_todos[!desc_todos$Item %in% items_en_tabla, ]
  if (nrow(items_excluidos) > 0) {
    filas_excl <- data.frame(
      Item          = items_excluidos$Item,
      tipo_item     = NA_character_,
      max_escala    = items_excluidos$max_observado,
      pct_NA        = items_excluidos$pct_NA,
      flag_NA       = ifelse(items_excluidos$pct_NA > CONFIG$umbral_NA_pct,
                             "DATOS_INSUFICIENTES", "OK"),
      media         = items_excluidos$media,
      sd            = NA_real_,
      p_dificultad  = NA_real_,
      ritc          = NA_real_,
      alpha_sin_item= NA_real_,
      b_Rasch       = NA_real_,
      infit_MNSQ    = NA_real_,
      outfit_MNSQ   = NA_real_,
      a_GRM             = NA_real_,
      info_theta0_GRM   = NA_real_,
      comunalidad_EFA   = NA_real_,
      carga_AFC         = NA_real_,
      R2_AFC            = NA_real_,
      eliminar_AFC      = NA_character_,
      estabilidad_EGA   = NA_real_,
      votos_eliminacion = NA_integer_,
      decision_final = dplyr::case_when(
        items_excluidos$tipo_variable == "Continua (max>10)" ~ "Variable continua",
        items_excluidos$pct_NA > CONFIG$umbral_NA_pct        ~ "DATOS_INSUFICIENTES",
        TRUE                                                  ~ "Sin variabilidad"
      ),
      stringsAsFactors = FALSE
    )
    # Añadir columnas eliminar_* que pueden estar en tabla pero no en filas_excl
    for (col in setdiff(names(tabla), names(filas_excl)))
      filas_excl[[col]] <- NA
    tabla <- dplyr::bind_rows(tabla, filas_excl[, names(tabla)])
  }

  tabla
}

# =============================================================================
# 4-B. PIPELINE DE CONTEXTO (CTXT)
# Análisis puramente descriptivo: sin supuestos reflectivos.
# No se aplican Alpha, Omega, Rasch, GRM, EFA, AFC ni EGA.
# =============================================================================

#' Genera tabla descriptiva por ítem para CTXT.
#'
#' @param datos     data.frame numérico con solo los ítems de contexto
#' @param etiqueta  cadena identificadora para mensajes de consola
#' @return data.frame con un ítem por fila y todos los indicadores descriptivos
#'         + columna decision_final: CONSERVAR | REVISAR | INFORMACIÓN LIMITADA

analizar_items_contexto <- function(datos, etiqueta) {

  n_total <- nrow(datos)
  cat(sprintf("  [CTXT] %d ítems | N = %d respondentes\n", ncol(datos), n_total))

  # Umbrales propios del pipeline de contexto
  UMBRAL_NA_CTXT    <- CONFIG$umbral_NA_pct   # 20 %
  UMBRAL_MODAL_CTXT <- 80                      # % categoría modal
  UMBRAL_CAT_CTXT   <- 2                       # mínimo de categorías usadas
  UMBRAL_SD_CTXT    <- 0.50                    # desviación estándar mínima

  resultado <- lapply(names(datos), function(item) {
    x       <- datos[[item]]
    x_val   <- x[!is.na(x)]
    n_val   <- length(x_val)
    pct_na  <- round(mean(is.na(x)) * 100, 1)
    pct_val <- round(n_val / n_total * 100, 1)

    # Estadísticos de posición y dispersión
    media <- if (n_val > 0) round(mean(x_val), 3) else NA_real_
    desv  <- if (n_val > 1) round(sd(x_val),  3) else NA_real_
    vari  <- if (n_val > 1) round(var(x_val), 3) else NA_real_

    # Frecuencias y categoría modal
    if (n_val > 0) {
      freq_tab     <- sort(table(x_val), decreasing = TRUE)
      n_cat_usadas <- length(freq_tab)
      cat_modal    <- as.numeric(names(freq_tab)[1])
      n_modal      <- as.integer(freq_tab[1])
      prop_modal   <- round(n_modal / n_val * 100, 1)
      freq_str     <- paste(
        mapply(function(cat, cnt) sprintf("%s=%d(%.0f%%)", cat, cnt,
                                         round(cnt / n_val * 100)),
               names(freq_tab), as.integer(freq_tab)),
        collapse = "; "
      )
      min_val <- min(x_val)
      max_val <- max(x_val)
    } else {
      n_cat_usadas <- 0L
      cat_modal    <- NA_real_
      prop_modal   <- NA_real_
      freq_str     <- "Sin datos válidos"
      min_val      <- NA_real_
      max_val      <- NA_real_
    }

    # Indicadores de calidad
    flag_na     <- pct_na > UMBRAL_NA_CTXT
    flag_modal  <- !is.na(prop_modal) && prop_modal > UMBRAL_MODAL_CTXT
    flag_pocas  <- n_cat_usadas < UMBRAL_CAT_CTXT
    flag_baja_v <- !is.na(desv) && desv < UMBRAL_SD_CTXT

    # Efectos techo y piso (categoría modal coincide con extremo y supera 50%)
    efecto_techo <- if (!is.na(cat_modal) && !is.na(max_val) &&
                         cat_modal == max_val && !is.na(prop_modal) && prop_modal > 50)
                      "Posible efecto techo" else "No"
    efecto_piso  <- if (!is.na(cat_modal) && !is.na(min_val) &&
                         cat_modal == min_val && !is.na(prop_modal) && prop_modal > 50)
                      "Posible efecto piso"  else "No"

    n_crit <- sum(flag_na, flag_modal, flag_pocas, flag_baja_v)

    decision <- dplyr::case_when(
      n_crit >= 2 ~ "INFORMACIÓN LIMITADA",
      n_crit == 1 ~ "REVISAR",
      TRUE        ~ "CONSERVAR"
    )

    data.frame(
      Item                 = item,
      pct_NA               = pct_na,
      pct_validos          = pct_val,
      n_validos            = n_val,
      media                = media,
      sd                   = desv,
      varianza             = vari,
      n_categorias_usadas  = n_cat_usadas,
      categoria_modal      = cat_modal,
      prop_modal_pct       = prop_modal,
      frecuencias          = freq_str,
      baja_variabilidad    = ifelse(flag_baja_v, "Sí", "No"),
      efecto_techo         = efecto_techo,
      efecto_piso          = efecto_piso,
      flag_NA_alto         = ifelse(flag_na,    "Sí", "No"),
      flag_modal_alta      = ifelse(flag_modal,  "Sí", "No"),
      flag_pocas_cat       = ifelse(flag_pocas,  "Sí", "No"),
      flag_baja_var        = ifelse(flag_baja_v, "Sí", "No"),
      n_criterios_prob     = n_crit,
      decision_final       = decision,
      stringsAsFactors     = FALSE
    )
  })

  tabla <- dplyr::bind_rows(resultado)

  n_cons  <- sum(tabla$decision_final == "CONSERVAR",           na.rm = TRUE)
  n_rev   <- sum(tabla$decision_final == "REVISAR",             na.rm = TRUE)
  n_info  <- sum(tabla$decision_final == "INFORMACIÓN LIMITADA", na.rm = TRUE)
  cat(sprintf("  [CTXT] Clasificación: %d CONSERVAR | %d REVISAR | %d INFORMACIÓN LIMITADA\n",
              n_cons, n_rev, n_info))

  # Gráfico de perfil de contexto: sd y prop_modal por ítem
  tryCatch({
    dir_fig <- file.path(CONFIG$dir_salida, "figuras")
    dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
    ruta_p  <- file.path(dir_fig, paste0(etiqueta, "_Perfil_CTXT.png"))

    n_it <- nrow(tabla)
    alto <- max(600, n_it * 16)
    png(ruta_p, width = 1400, height = alto, res = 120)

    col_dec <- dplyr::case_when(
      tabla$decision_final == "INFORMACIÓN LIMITADA" ~ "#888888",
      tabla$decision_final == "REVISAR"              ~ "#E6A817",
      TRUE                                           ~ "#006600"
    )
    op <- par(mfrow = c(1, 2), mar = c(4, 6, 3, 1), oma = c(0, 0, 3, 0))

    sd_vals <- ifelse(is.na(tabla$sd), 0, tabla$sd)
    barplot(sd_vals, names.arg = tabla$Item, horiz = TRUE, las = 1,
            col = col_dec, border = NA, xlab = "Desviación estándar",
            main = "SD por ítem")
    abline(v = UMBRAL_SD_CTXT, lty = 2, col = "#856404", lwd = 1.5)

    pm_vals <- ifelse(is.na(tabla$prop_modal_pct), 0, tabla$prop_modal_pct)
    barplot(pm_vals, names.arg = tabla$Item, horiz = TRUE, las = 1,
            col = col_dec, border = NA, xlab = "% categoría modal",
            main = "Concentración modal", xlim = c(0, 100))
    abline(v = UMBRAL_MODAL_CTXT, lty = 2, col = "#856404", lwd = 1.5)

    mtext(paste0(etiqueta, " — Perfil CTXT"), outer = TRUE, cex = 1.2, font = 2)
    legend("topright",
           legend = c("CONSERVAR", "REVISAR", "INFORMACIÓN LIMITADA"),
           fill   = c("#006600", "#E6A817", "#888888"),
           border = NA, bty = "n", cex = 0.8)
    par(op)
    dev.off()
    cat("  [CTXT] Perfil guardado:", basename(ruta_p), "\n")
  }, error = function(e) NULL)

  tabla
}

# =============================================================================
# 5. COMPARACIÓN ENTRE CICLOS
# =============================================================================

#' Combina tablas de dos ciclos y genera comparativo con semáforo.
#'
#' @param tabla_A  resultado de analizar_items_psicometrico() para ciclo 2425
#' @param tabla_B  resultado de analizar_items_psicometrico() para ciclo 2526
#' @return data.frame comparativo

#' Compara resultados psicométricos entre ciclos respetando el tipo de ítem.
#'
#' Produce un data.frame con tres grupos de filas claramente separados:
#'   1. EXACTO    — ítem existe en ambos ciclos: muestra indicadores lado a lado
#'                  + semáforo de consistencia + decisión_recomendada
#'   2. SIN_MATCH — ítem solo en 2425: indicadores del ciclo A, sin comparación
#'   3. NUEVO     — ítem solo en 2526: indicadores del ciclo B, sin comparación
#'
#' @param tabla_A  resultado de analizar_items_psicometrico() para ciclo 2425
#' @param tabla_B  resultado de analizar_items_psicometrico() para ciclo 2526

comparar_ciclos <- function(tabla_A, tabla_B, ciclo_A = "2425", ciclo_B = "2526") {

  sufA <- paste0("_", ciclo_A)
  sufB <- paste0("_", ciclo_B)

  indicadores <- c("tipo_item", "pct_NA", "p_dificultad", "ritc", "alpha_sin_item",
                   "b_Rasch", "infit_MNSQ", "outfit_MNSQ",
                   "a_GRM", "info_theta0_GRM", "comunalidad_EFA",
                   "carga_AFC", "R2_AFC",
                   "estabilidad_EGA", "votos_eliminacion", "decision_final")

  # Helper: extraer columnas disponibles con sufijo
  preparar <- function(tabla, sufijo) {
    cols <- intersect(indicadores, names(tabla))
    df   <- tabla[, c("Item", cols), drop = FALSE]
    names(df)[names(df) != "Item"] <- paste0(names(df)[names(df) != "Item"], sufijo)
    df
  }

  df_A <- preparar(tabla_A, sufA)
  df_B <- preparar(tabla_B, sufB)

  tipo_A_col <- paste0("tipo_item", sufA)
  tipo_B_col <- paste0("tipo_item", sufB)
  dec_A_col  <- paste0("decision_final", sufA)
  dec_B_col  <- paste0("decision_final", sufB)

  # --- Grupo 1: EXACTO — ítems presentes en ambos ciclos ---
  items_exacto_A <- tabla_A$Item[!is.na(tabla_A$tipo_item) & tabla_A$tipo_item == "EXACTO"]
  items_exacto_B <- tabla_B$Item[!is.na(tabla_B$tipo_item) & tabla_B$tipo_item == "EXACTO"]
  items_exacto   <- intersect(items_exacto_A, items_exacto_B)

  if (length(items_exacto) > 0) {
    comp_exacto <- dplyr::inner_join(
      df_A[df_A$Item %in% items_exacto, ],
      df_B[df_B$Item %in% items_exacto, ],
      by = "Item"
    )
    comp_exacto$grupo <- "EXACTO"

    # Semáforo de consistencia entre ciclos
    comp_exacto$consistencia <- dplyr::case_when(
      is.na(comp_exacto[[dec_A_col]]) | is.na(comp_exacto[[dec_B_col]]) ~ "Un ciclo sin dato",
      comp_exacto[[dec_A_col]] == "ELIMINAR" & comp_exacto[[dec_B_col]] == "ELIMINAR" ~ "Ambos: ELIMINAR",
      comp_exacto[[dec_A_col]] == "Conservar" & comp_exacto[[dec_B_col]] == "Conservar" ~ "Ambos: Conservar",
      TRUE ~ "Discordante"
    )
    comp_exacto$decision_recomendada <- dplyr::case_when(
      comp_exacto$consistencia == "Ambos: ELIMINAR"  ~ "ELIMINAR",
      comp_exacto$consistencia == "Ambos: Conservar" ~ "Conservar",
      comp_exacto$consistencia == "Discordante"      ~ "REVISAR (ciclos discordantes)",
      TRUE                                           ~ "REVISAR (dato faltante)"
    )
  } else {
    comp_exacto <- data.frame()
  }

  # --- Grupo 2: SIN_MATCH — solo en 2425 ---
  items_sin_match <- tabla_A$Item[!is.na(tabla_A$tipo_item) & tabla_A$tipo_item == "SIN_MATCH"]
  if (length(items_sin_match) > 0) {
    comp_sin_match <- df_A[df_A$Item %in% items_sin_match, ]
    comp_sin_match$grupo             <- "SIN_MATCH (solo 2425)"
    comp_sin_match$consistencia      <- "Solo 2425"
    comp_sin_match$decision_recomendada <- ifelse(
      !is.na(comp_sin_match[[dec_A_col]]) & comp_sin_match[[dec_A_col]] == "ELIMINAR",
      "ELIMINAR", "Conservar (revisar en 2526)")
  } else {
    comp_sin_match <- data.frame()
  }

  # --- Grupo 3: NUEVO — solo en 2526 ---
  items_nuevo <- tabla_B$Item[!is.na(tabla_B$tipo_item) & tabla_B$tipo_item == "NUEVO"]
  if (length(items_nuevo) > 0) {
    comp_nuevo <- df_B[df_B$Item %in% items_nuevo, ]
    comp_nuevo$grupo                <- "NUEVO (solo 2526)"
    comp_nuevo$consistencia         <- "Solo 2526"
    comp_nuevo$decision_recomendada <- ifelse(
      !is.na(comp_nuevo[[dec_B_col]]) & comp_nuevo[[dec_B_col]] == "ELIMINAR",
      "ELIMINAR", "Conservar (nuevo ítem)")
  } else {
    comp_nuevo <- data.frame()
  }

  # --- Grupo 4: SIN_CROSSWALK — en datos pero sin entrada en el crosswalk ---
  items_sc_A <- tabla_A$Item[!is.na(tabla_A$tipo_item) & tabla_A$tipo_item == "SIN_CROSSWALK"]
  items_sc_B <- tabla_B$Item[!is.na(tabla_B$tipo_item) & tabla_B$tipo_item == "SIN_CROSSWALK"]
  items_sc   <- union(items_sc_A, items_sc_B)

  if (length(items_sc) > 0) {
    sc_A <- if (length(items_sc_A) > 0) df_A[df_A$Item %in% items_sc_A, ] else data.frame()
    sc_B <- if (length(items_sc_B) > 0) df_B[df_B$Item %in% items_sc_B, ] else data.frame()
    comp_sc <- dplyr::bind_rows(sc_A, sc_B)
    comp_sc$grupo                <- "SIN_CROSSWALK"
    comp_sc$consistencia         <- "Fuera del crosswalk"
    comp_sc$decision_recomendada <- ifelse(
      !is.na(comp_sc[[dec_A_col]]) & comp_sc[[dec_A_col]] == "ELIMINAR" |
      !is.na(comp_sc[[dec_B_col]]) & comp_sc[[dec_B_col]] == "ELIMINAR",
      "REVISAR (sin validación)", "REVISAR (sin validación)")
  } else {
    comp_sc <- data.frame()
  }

  # Unir los cuatro grupos (bind_rows rellena con NA las columnas faltantes)
  comp_total <- dplyr::bind_rows(comp_exacto, comp_sin_match, comp_nuevo, comp_sc)

  # Reordenar: grupo + Item al frente (solo columnas que existan)
  cols_frente <- intersect(c("grupo", "Item", "consistencia", "decision_recomendada"),
                           names(comp_total))
  resto       <- setdiff(names(comp_total), cols_frente)
  comp_total  <- comp_total[, c(cols_frente, resto), drop = FALSE]

  n_ex <- nrow(comp_exacto)
  n_sm <- nrow(comp_sin_match)
  n_nv <- nrow(comp_nuevo)
  n_sc <- nrow(comp_sc)
  cat(sprintf("  [COMPARACION] %d EXACTO | %d SIN_MATCH | %d NUEVO | %d SIN_CROSSWALK\n",
              n_ex, n_sm, n_nv, n_sc))

  comp_total
}

# =============================================================================
# 6. EXPORTACIÓN A EXCEL CON FORMATO
# =============================================================================

COLORES <- list(
  eliminar      = list(fg = "#FFCCCC", font = "#CC0000"),
  conservar     = list(fg = "#CCFFCC", font = "#006600"),
  revisar       = list(fg = "#FFF3CC", font = "#856404"),
  insuf         = list(fg = "#E0E0E0", font = "#555555"),
  header        = list(fg = "#1F3864", font = "#FFFFFF"),
  exacto        = list(fg = "#FFFFFF", font = "#000000"),   # blanco — en ambos ciclos
  sin_match     = list(fg = "#FFF3CC", font = "#856404"),   # amarillo — solo 2425
  nuevo         = list(fg = "#DDEBF7", font = "#1F3864"),   # azul claro — solo 2526
  sin_crosswalk = list(fg = "#F2F2F2", font = "#666666")    # gris claro — fuera del crosswalk
)

# Aplica relleno por valor en col_decision
estilo_decision <- function(wb, hoja, tabla, col_decision) {
  reglas <- list(
    list(patron = "^ELIMINAR$",          col = COLORES$eliminar),
    list(patron = "^Conservar",          col = COLORES$conservar),
    list(patron = "^CONSERVAR$",         col = COLORES$conservar),   # CTXT
    list(patron = "^REVISAR",            col = COLORES$revisar),
    list(patron = "^INFORMACIÓN LIMITADA", col = COLORES$insuf),     # CTXT
    list(patron = "DATOS_INSUF",         col = COLORES$insuf)
  )
  for (r in reglas) {
    filas <- which(grepl(r$patron, tabla[[col_decision]])) + 1
    if (length(filas) == 0) next
    openxlsx::addStyle(wb, hoja,
      style = openxlsx::createStyle(fgFill = r$col$fg, fontColour = r$col$font,
                                    textDecoration = "bold"),
      rows = filas, cols = 1:ncol(tabla), gridExpand = TRUE)
  }
}

# Aplica relleno de fondo por grupo en la hoja comparativa
estilo_grupo <- function(wb, hoja, tabla) {
  if (!"grupo" %in% names(tabla)) return(invisible())
  reglas <- list(
    list(patron = "^EXACTO$",        col = COLORES$exacto),
    list(patron = "SIN_MATCH",       col = COLORES$sin_match),
    list(patron = "NUEVO",           col = COLORES$nuevo),
    list(patron = "SIN_CROSSWALK",   col = COLORES$sin_crosswalk)
  )
  for (r in reglas) {
    filas <- which(grepl(r$patron, tabla$grupo)) + 1
    if (length(filas) == 0) next
    openxlsx::addStyle(wb, hoja,
      style = openxlsx::createStyle(fgFill = r$col$fg, fontColour = r$col$font),
      rows = filas, cols = 1:ncol(tabla), gridExpand = TRUE)
  }
}

agregar_hoja_tabla <- function(wb, nombre_hoja, tabla, col_decision = "decision_final") {
  nombre_hoja <- substr(nombre_hoja, 1, 31)
  openxlsx::addWorksheet(wb, nombre_hoja)
  openxlsx::writeData(wb, nombre_hoja, tabla)

  # Encabezado
  openxlsx::addStyle(wb, nombre_hoja,
    style = openxlsx::createStyle(fgFill = COLORES$header$fg,
                                  fontColour = COLORES$header$font,
                                  textDecoration = "bold", halign = "center"),
    rows = 1, cols = 1:ncol(tabla), gridExpand = TRUE)
  openxlsx::setColWidths(wb, nombre_hoja, cols = 1:ncol(tabla), widths = "auto")

  # Colorear por grupo (EXACTO / SIN_MATCH / NUEVO) — hoja comparativa
  estilo_grupo(wb, nombre_hoja, tabla)

  # Colorear por decisión (sobreescribe el color de grupo en esa columna)
  if (col_decision %in% names(tabla))
    estilo_decision(wb, nombre_hoja, tabla, col_decision)
}

# =============================================================================
# 7. PIPELINE POR COMBINACIÓN — devuelve resultados, sin guardar Excel propio
# =============================================================================

#' Ejecuta TCT+IRT+EFA+EGA para una combinación cuestionario × figura × momento.
#' Devuelve lista nombrada por ciclo; cada elemento es el data.frame de analizar_items
#' con columna extra `momento`.

ejecutar_combinacion <- function(cuestion, figura, momento = "pre",
                                 catalogo, cw_df, max_item = NULL) {

  if (is.null(max_item) && cuestion %in% names(CONFIG$max_escala))
    max_item <- CONFIG$max_escala[[cuestion]]

  etiq_base <- paste0(cuestion, "_", figura, "_", momento)
  cat("\n", strrep("=", 70), "\n")
  cat("PROCESANDO:", etiq_base, "\n")
  cat(strrep("=", 70), "\n")

  reg <- catalogo %>%
    dplyr::filter(cuestion == !!cuestion,
                  figura   == !!figura,
                  momento  == !!momento)

  if (nrow(reg) == 0) {
    cat("  Sin archivos para:", etiq_base, "— omitido.\n")
    return(invisible(NULL))
  }

  # Códigos de ítems válidos según el crosswalk (para seleccionar columnas)
  codes_2425 <- cw_df$code_2425[!is.na(cw_df$code_2425) & cw_df$code_2425 != "NA"]
  codes_2526 <- cw_df$code_2526[!is.na(cw_df$code_2526) & cw_df$code_2526 != "NA"]

  resultados_ciclo <- list()

  for (i in seq_len(nrow(reg))) {
    ciclo <- reg$ciclo[i]
    ruta  <- reg$ruta[i]
    etiq  <- paste0(etiq_base, "_", ciclo)

    cat("\n  Ciclo:", ciclo, "— Archivo:", basename(ruta), "\n")
    datos_brutos <- cargar_base(ruta, ciclo)
    if (is.null(datos_brutos) || nrow(datos_brutos) < 30) {
      cat("  Datos insuficientes (N <30) — omitido.\n"); next
    }

    # Seleccionar solo columnas que están en el crosswalk para este ciclo
    if (ciclo == "2425") {
      cols_sel <- intersect(codes_2425, names(datos_brutos))
    } else {
      cols_sel <- intersect(codes_2526, names(datos_brutos))
    }

    if (length(cols_sel) == 0) {
      cat("  Sin columnas del crosswalk en los datos — omitido.\n"); next
    }

    datos <- datos_brutos[, cols_sel, drop = FALSE]
    cat(sprintf("  N sujetos: %d | N ítems crosswalk en datos: %d\n",
                nrow(datos), ncol(datos)))

    # Despachar al pipeline según el tipo de cuestionario
    res <- if (cuestion == "CTXT") {
      analizar_items_contexto(datos, etiq)
    } else {
      analizar_items_psicometrico(datos, etiq, max_item = max_item)
    }

    if (is.null(res) || nrow(res) == 0) {
      cat("  Sin ítems analizables — omitido.\n")
      next
    }
    res$ciclo    <- ciclo
    res$momento  <- momento
    resultados_ciclo[[paste0(ciclo, "_", momento)]] <- res
  }

  invisible(resultados_ciclo)
}

# =============================================================================
# 8. CONSTRUCCIÓN DE TABLA ANCHA Y GENERACIÓN DE EXCEL POR CUESTIONARIO
# =============================================================================

# Indicadores por ciclo+momento — Pipeline psicométrico (HD, HSXXI, HI)
INDICADORES_CM <- c(
  "pct_NA", "media", "p_dificultad", "ritc", "alpha_sin_item",
  "b_Rasch", "infit_MNSQ", "outfit_MNSQ",   # Rasch — descriptivo
  "a_GRM",                                   # GRM   — vota
  "comunalidad_EFA",                         # EFA   — vota
  "carga_AFC", "R2_AFC",                     # AFC   — vota
  "estabilidad_EGA",                         # EGA   — descriptivo
  "votos_eliminacion", "decision_final"
)

# Indicadores por ciclo+momento — Pipeline de contexto (CTXT)
INDICADORES_CM_CTXT <- c(
  "pct_NA", "pct_validos", "n_validos",
  "media", "sd", "varianza",
  "n_categorias_usadas", "categoria_modal", "prop_modal_pct",
  "baja_variabilidad", "efecto_techo", "efecto_piso",
  "flag_NA_alto", "flag_modal_alta", "flag_pocas_cat", "flag_baja_var",
  "n_criterios_prob", "decision_final"
)

#' Justificación para pipeline de CONTEXTO: lista flags activados por ciclo.
generar_justificacion_ctxt <- function(fila, claves_cm) {
  get1 <- function(nm) {
    v <- fila[[nm]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
  }

  dec_vals <- sapply(claves_cm, function(cm) get1(paste0("Decision_", cm)))

  if (all(is.na(dec_vals) | dec_vals == "Sin datos"))
    return("Sin datos en ningún ciclo disponible.")

  if (all(!is.na(dec_vals) & dec_vals == "CONSERVAR"))
    return("CONSERVAR en todos los ciclos disponibles.")

  razones <- c()
  for (cm in claves_cm) {
    flags <- c()
    if (!is.na(get1(paste0(cm, "_flag_NA_alto")))    && get1(paste0(cm, "_flag_NA_alto"))    == "Sí") flags <- c(flags, sprintf("NA>20%%(%.0f%%)", as.numeric(fila[[paste0(cm, "_pct_NA")]])))
    if (!is.na(get1(paste0(cm, "_flag_modal_alta"))) && get1(paste0(cm, "_flag_modal_alta")) == "Sí") flags <- c(flags, sprintf("modal>80%%(%.0f%%)", as.numeric(fila[[paste0(cm, "_prop_modal_pct")]])))
    if (!is.na(get1(paste0(cm, "_flag_pocas_cat")))  && get1(paste0(cm, "_flag_pocas_cat"))  == "Sí") flags <- c(flags, "< 2 categorías usadas")
    if (!is.na(get1(paste0(cm, "_flag_baja_var")))   && get1(paste0(cm, "_flag_baja_var"))   == "Sí") flags <- c(flags, sprintf("SD<0.50(%.2f)", as.numeric(fila[[paste0(cm, "_sd")]])))
    if (length(flags) > 0)
      razones <- c(razones, paste0("[", cm, ": ", paste(flags, collapse = ", "), "]"))
  }

  if (length(razones) == 0) return("Revisar por criterio de contenido.")
  paste(razones, collapse = " ")
}

#' Construye una fila de justificación a partir de los indicadores por ciclo.
generar_justificacion <- function(fila, claves_cm) {
  razones <- c()
  for (cm in claves_cm) {
    dec_col <- paste0(cm, "_decision_final")
    if (!dec_col %in% names(fila)) next
    dec <- fila[[dec_col]]
    if (is.na(dec) || dec != "ELIMINAR") next

    # Detectar qué criterios fallaron.
    # num1(): convierte a numérico escalar, devuelve NA si NULL/vacío/no-numérico.
    # Esto evita errores en if() cuando la columna no existe o apply() devuelve character.
    num1 <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (length(v) == 0L) NA_real_ else v[[1L]]
    }
    gt <- function(val, thr) { v <- num1(val); !is.na(v) && v > thr }
    lt <- function(val, thr) { v <- num1(val); !is.na(v) && v < thr }

    problemas <- c()
    if (gt(fila[[paste0(cm, "_pct_NA")]],          CONFIG$umbral_NA_pct))     problemas <- c(problemas, sprintf("NA=%.0f%%",  num1(fila[[paste0(cm, "_pct_NA")]])))
    if (lt(fila[[paste0(cm, "_ritc")]],             CONFIG$umbral_ritc))       problemas <- c(problemas, sprintf("ritc=%.2f",  num1(fila[[paste0(cm, "_ritc")]])))
    if (gt(fila[[paste0(cm, "_infit_MNSQ")]],       CONFIG$umbral_infit))      problemas <- c(problemas, sprintf("infit=%.2f", num1(fila[[paste0(cm, "_infit_MNSQ")]])))
    if (gt(fila[[paste0(cm, "_outfit_MNSQ")]],      CONFIG$umbral_outfit))     problemas <- c(problemas, sprintf("outfit=%.2f",num1(fila[[paste0(cm, "_outfit_MNSQ")]])))
    if (lt(fila[[paste0(cm, "_a_GRM")]],            CONFIG$umbral_a_grm))      problemas <- c(problemas, sprintf("a_GRM=%.2f", num1(fila[[paste0(cm, "_a_GRM")]])))
    if (lt(fila[[paste0(cm, "_comunalidad_EFA")]], CONFIG$umbral_comunalidad)) problemas <- c(problemas, sprintf("h2=%.2f",       num1(fila[[paste0(cm, "_comunalidad_EFA")]])))
    if (lt(fila[[paste0(cm, "_carga_AFC")]],       CONFIG$umbral_carga_AFC))   problemas <- c(problemas, sprintf("carga_AFC=%.2f", num1(fila[[paste0(cm, "_carga_AFC")]])))
    if (lt(fila[[paste0(cm, "_R2_AFC")]],          CONFIG$umbral_R2_AFC))      problemas <- c(problemas, sprintf("R2_AFC=%.2f",    num1(fila[[paste0(cm, "_R2_AFC")]])))
    if (lt(fila[[paste0(cm, "_estabilidad_EGA")]], CONFIG$umbral_estabilidad)) problemas <- c(problemas, sprintf("estab=%.2f",     num1(fila[[paste0(cm, "_estabilidad_EGA")]])))

    if (length(problemas) > 0)
      razones <- c(razones, paste0("[", cm, ": ", paste(problemas, collapse=", "), "]"))
  }
  # Verificar si algún ciclo reportó variable continua o sin datos
  dec_vals <- sapply(claves_cm, function(cm) {
    v <- fila[[paste0("Decision_", cm)]] %||% fila[[paste0(cm, "_decision_final")]]
    if (is.null(v)) NA_character_ else as.character(v)
  })
  if (any(!is.na(dec_vals) & dec_vals == "Variable continua"))
    return("Variable de conteo (días, horas, años): no aplica análisis psicométrico. Conservar o eliminar según criterio de contenido.")
  if (all(is.na(dec_vals) | dec_vals == "Sin datos"))
    return("Sin datos en ningún ciclo disponible: ítem no encontrado en los archivos de datos.")
  if (any(!is.na(dec_vals) & dec_vals == "REVISAR") && length(razones) > 0)
    return(paste("REVISAR —", paste(razones, collapse = " ")))
  if (length(razones) == 0) return("Conservar en todos los ciclos disponibles")
  paste(razones, collapse = " ")
}

#' Construye tabla ancha usando el crosswalk como lista maestra de ítems.
#' - Filas: cada ítem del crosswalk (code_2425 como identificador primario)
#' - Columnas: code_2425, code_2526, texto_2425, tipo_match,
#'             [indicadores por ciclo+momento], Decision_2425pre, Decision_2425post,
#'             Decision_2526pre, Decision_final, Justificacion
#'
#' La unión con resultados de análisis es:
#'   ciclo 2425 → por code_2425 (= Item en analizar_items)
#'   ciclo 2526 → por code_2526 (= Item en analizar_items)
construir_tabla_ancha <- function(resultados_lista, cw_df,
                                  indicadores = INDICADORES_CM,
                                  tipo = c("psicometrico", "ctxt")) {
  tipo <- match.arg(tipo)
  if (length(resultados_lista) == 0 || is.null(cw_df) || nrow(cw_df) == 0) return(NULL)

  # Lista maestra: todas las filas del crosswalk (code_2425 es la llave primaria;
  # para NUEVO items, code_2425 = NA y se usa code_2526 como llave)
  tabla <- cw_df[, c("code_2425", "code_2526", "texto_2425", "tipo_match"),
                 drop = FALSE]

  claves_cm <- names(resultados_lista)   # "2425_pre", "2426_post", "2526_pre", ...

  for (clave in claves_cm) {
    res      <- resultados_lista[[clave]]
    ciclo    <- sub("_.*", "", clave)                   # "2425" o "2526"
    cm_label <- gsub("_", "", clave)                    # "2425pre", "2526pre"

    cols_usar <- intersect(indicadores, names(res))
    bloque    <- res[, c("Item", cols_usar), drop = FALSE]
    names(bloque)[-1] <- paste0(cm_label, "_", names(bloque)[-1])

    if (ciclo == "2425") {
      # Unir por code_2425 = Item
      names(bloque)[1] <- "code_2425"
      tabla <- dplyr::left_join(tabla, bloque, by = "code_2425")
    } else {
      # Unir por code_2526 = Item
      names(bloque)[1] <- "code_2526"
      tabla <- dplyr::left_join(tabla, bloque, by = "code_2526")
    }
  }

  # Columnas de decisión por ciclo+momento para la tabla de salida
  dec_cols_cm <- paste0(gsub("_", "", claves_cm), "_decision_final")
  dec_cols_cm <- intersect(dec_cols_cm, names(tabla))

  # Decisión por ciclo (renombrar para claridad)
  for (dc in dec_cols_cm) {
    nuevo_nombre <- sub("_decision_final$", "", dc)  # "2425pre", "2526pre"
    nuevo_nombre <- paste0("Decision_", nuevo_nombre)
    names(tabla)[names(tabla) == dc] <- nuevo_nombre
  }
  dec_cols_final <- paste0("Decision_", gsub("_", "", claves_cm))
  dec_cols_final <- intersect(dec_cols_final, names(tabla))

  # Decision_final consolidada entre ciclos
  tabla$ciclos_datos <- rowSums(!is.na(tabla[, dec_cols_final, drop = FALSE]))

  if (tipo == "ctxt") {
    # Pipeline contexto: sin eliminación automática
    #   INFORMACIÓN LIMITADA → algún ciclo lo dice
    #   REVISAR              → algún ciclo lo dice (sin INFORMACIÓN LIMITADA)
    #   CONSERVAR            → todos los ciclos con datos lo dicen
    tabla$ciclos_info <- rowSums(
      sapply(dec_cols_final, function(d) {
        v <- tabla[[d]]
        ifelse(!is.na(v) & v == "INFORMACIÓN LIMITADA", 1L, 0L)
      }), na.rm = TRUE)
    tabla$ciclos_rev <- rowSums(
      sapply(dec_cols_final, function(d) {
        v <- tabla[[d]]
        ifelse(!is.na(v) & v == "REVISAR", 1L, 0L)
      }), na.rm = TRUE)

    tabla$Decision_final <- dplyr::case_when(
      tabla$ciclos_datos == 0   ~ "Sin datos",
      tabla$ciclos_info  >= 1   ~ "INFORMACIÓN LIMITADA",
      tabla$ciclos_rev   >= 1   ~ "REVISAR",
      TRUE                      ~ "CONSERVAR"
    )
    tabla$ciclos_info <- NULL
    tabla$ciclos_rev  <- NULL

    # Justificación CTXT: listar qué flags se activaron
    cm_labels <- gsub("_", "", claves_cm)
    tabla$Justificacion <- apply(tabla, 1, function(r) {
      generar_justificacion_ctxt(as.list(r), cm_labels)
    })

  } else {
    # Pipeline psicométrico: ELIMINAR | REVISAR | Conservar
    tabla$ciclos_elim <- rowSums(
      sapply(dec_cols_final, function(d) {
        v <- tabla[[d]]
        ifelse(!is.na(v) & v == "ELIMINAR", 1L, 0L)
      }), na.rm = TRUE)
    tabla$ciclos_rev  <- rowSums(
      sapply(dec_cols_final, function(d) {
        v <- tabla[[d]]
        ifelse(!is.na(v) & v == "REVISAR", 1L, 0L)
      }), na.rm = TRUE)

    tabla$Decision_final <- dplyr::case_when(
      tabla$ciclos_datos == 0                               ~ "Sin datos",
      tabla$ciclos_elim  >= ceiling(tabla$ciclos_datos / 2) ~ "ELIMINAR",
      tabla$ciclos_rev   >= 1                               ~ "REVISAR",
      TRUE                                                  ~ "Conservar"
    )
    tabla$ciclos_elim <- NULL
    tabla$ciclos_rev  <- NULL

    # Justificación psicométrica
    cm_labels <- gsub("_", "", claves_cm)
    tabla$Justificacion <- apply(tabla, 1, function(r) {
      generar_justificacion(as.list(r), cm_labels)
    })
  }

  tabla$ciclos_datos <- NULL

  tabla
}

#' Genera un Excel por cuestionario: una pestaña por figura educativa.
consolidar_cuestionario <- function(cuestion, catalogo) {

  cat("\n", strrep("#", 70), "\n")
  cat("CONSOLIDANDO CUESTIONARIO:", cuestion, "\n")
  cat(strrep("#", 70), "\n")

  figuras    <- CONFIG$figuras_por_cuestion[[cuestion]] %||% CONFIG$figuras
  momentos   <- unique(catalogo$momento[catalogo$cuestion == cuestion])
  max_item   <- CONFIG$max_escala[[cuestion]]

  wb <- openxlsx::createWorkbook()
  resumen_filas <- list()

  for (figura in figuras) {
    cat("\n  Figura:", figura, "\n")

    # Cargar crosswalk completo para esta figura (lista maestra de ítems)
    hoja_cw <- paste0(cuestion, "_", figura)
    cw_df   <- cargar_crosswalk_completo(hoja_cw)
    if (is.null(cw_df) || nrow(cw_df) == 0) {
      cat("  Sin entradas en crosswalk para", hoja_cw, "— pestaña omitida.\n")
      next
    }
    cat(sprintf("  [CROSSWALK] %d ítems validados (OK) para %s\n", nrow(cw_df), hoja_cw))

    # Recopilar resultados de todos los momentos disponibles
    resultados_figura <- list()

    for (momento in momentos) {
      res_cm <- ejecutar_combinacion(cuestion, figura, momento,
                                     catalogo, cw_df = cw_df, max_item = max_item)
      if (!is.null(res_cm) && length(res_cm) > 0)
        resultados_figura <- c(resultados_figura, res_cm)
    }

    # Seleccionar indicadores y tipo según el cuestionario
    es_ctxt       <- cuestion == "CTXT"
    ind_usar      <- if (es_ctxt) INDICADORES_CM_CTXT else INDICADORES_CM
    tipo_pipeline <- if (es_ctxt) "ctxt" else "psicometrico"

    if (length(resultados_figura) == 0) {
      cat("  Sin resultados de análisis para figura", figura,
          "— se incluye tabla de ítems sin indicadores.\n")
      tabla_ancha <- cw_df[, c("code_2425", "code_2526", "texto_2425", "tipo_match")]
      tabla_ancha$Decision_final <- "Sin datos"
      tabla_ancha$Justificacion  <- "No se encontraron archivos de datos para este ciclo"
    } else {
      tabla_ancha <- construir_tabla_ancha(resultados_figura, cw_df,
                                           indicadores = ind_usar,
                                           tipo = tipo_pipeline)
      if (is.null(tabla_ancha) || nrow(tabla_ancha) == 0) next
    }

    # Escribir pestaña
    agregar_hoja_tabla(wb,
                       nombre_hoja  = figura,
                       tabla        = tabla_ancha,
                       col_decision = "Decision_final")

    # Acumular para pestaña resumen
    n_total <- nrow(tabla_ancha)
    if (es_ctxt) {
      n_cons    <- sum(tabla_ancha$Decision_final == "CONSERVAR",           na.rm = TRUE)
      n_revisar <- sum(tabla_ancha$Decision_final == "REVISAR",             na.rm = TRUE)
      n_info    <- sum(tabla_ancha$Decision_final == "INFORMACIÓN LIMITADA", na.rm = TRUE)
      n_elim    <- 0L
      omega_tot <- NA_real_
      omega_red <- NA_real_
      resumen_filas[[figura]] <- data.frame(
        cuestionario       = cuestion,
        figura             = figura,
        total_items        = n_total,
        conservar          = n_cons,
        revisar            = n_revisar,
        informacion_limitada = n_info,
        stringsAsFactors   = FALSE
      )
      cat(sprintf("  [OK] %s: %d ítems | %d CONSERVAR | %d REVISAR | %d INFO LIMITADA\n",
                  figura, n_total, n_cons, n_revisar, n_info))
    } else {
      n_elim    <- sum(tabla_ancha$Decision_final == "ELIMINAR",  na.rm = TRUE)
      n_revisar <- sum(tabla_ancha$Decision_final == "REVISAR",   na.rm = TRUE)
      n_cons    <- sum(tabla_ancha$Decision_final == "Conservar", na.rm = TRUE)
      omega_tot <- NA_real_
      omega_red <- NA_real_
      for (res_k in resultados_figura) {
        ot <- attr(res_k, "omega_total")
        or <- attr(res_k, "omega_reducida")
        if (!is.null(ot) && !is.na(ot)) { omega_tot <- ot; omega_red <- or; break }
      }
      resumen_filas[[figura]] <- data.frame(
        cuestionario    = cuestion,
        figura          = figura,
        total_items     = n_total,
        a_eliminar      = n_elim,
        a_revisar       = n_revisar,
        a_conservar     = n_cons,
        pct_reduccion   = round(n_elim / n_total * 100, 1),
        omega_total     = omega_tot,
        omega_reducida  = omega_red,
        stringsAsFactors = FALSE
      )
      cat(sprintf("  [OK] %s: %d ítems | %d Eliminar | %d Revisar | %d Conservar\n",
                  figura, n_total, n_elim, n_revisar, n_cons))
    }
  }

  if (length(resumen_filas) == 0) {
    cat("  Sin datos para ninguna figura. Excel no generado.\n")
    return(invisible(NULL))
  }

  # Pestaña de resumen ejecutivo
  resumen_ejec <- dplyr::bind_rows(resumen_filas)
  agregar_hoja_tabla(wb, "Resumen_ejecutivo", resumen_ejec)

  # Guardar
  dir.create(CONFIG$dir_salida, showWarnings = FALSE, recursive = TRUE)
  archivo_out <- file.path(CONFIG$dir_salida, paste0(cuestion, "_reduccion.xlsx"))
  openxlsx::saveWorkbook(wb, archivo_out, overwrite = TRUE)
  cat("\n  Excel guardado:", archivo_out, "\n")

  invisible(wb)
}

# =============================================================================
# 8-B. REPORTE DE ESTRUCTURAS DE DIMENSIONES PARA AFC MULTIDIMENSIONAL
# =============================================================================
# HALLAZGO: el archivo crosswalk contiene la columna 'dimension' que asigna
# cada ítem a su dimensión teórica dentro del instrumento.  cargar_crosswalk_completo()
# ACTUALMENTE descarta esa columna (no la incluye en el data.frame devuelto).
#
# Para implementar AFC por dimensión (en lugar de unifactorial) bastaría:
#   1. Añadir 'dimension' al data.frame devuelto por cargar_crosswalk_completo()
#   2. En analizar_items_psicometrico(), construir el modelo CFA así:
#        dimensiones <- unique(cw_df$dimension)
#        lineas <- sapply(dimensiones, function(d) {
#          items_d <- cw_df$code_2425[cw_df$dimension == d]
#          paste0(d, " =~ ", paste(items_d, collapse=" + "))
#        })
#        modelo_multidim <- paste(lineas, collapse="\n")
#        fit_afc_multi <- lavaan::cfa(modelo_multidim, ...)
#   3. Extraer cargas por dimensión y calcular indicadores de ajuste global
#      (CFI, RMSEA, SRMR) para reportar si la estructura dimensional es adecuada.
#
# Esta función lee el crosswalk y reporta qué dimensiones existen por hoja.
# Ejecutar después de que el script esté cargado.

reportar_dimensiones_cw <- function() {
  if (!file.exists(CROSSWALK_PATH)) {
    cat("[REPORTE AFC] Crosswalk no encontrado:", CROSSWALK_PATH, "\n")
    return(invisible(NULL))
  }
  cw <- tryCatch(
    openxlsx::read.xlsx(CROSSWALK_PATH, sheet = "CROSSWALK_COMPLETO",
                        na.strings = c("", "NA")),
    error = function(e) NULL
  )
  if (is.null(cw)) { cat("[REPORTE AFC] No se pudo leer el crosswalk.\n"); return(invisible(NULL)) }

  tiene_dim <- "dimension" %in% names(cw)
  cat("\n", strrep("=", 70), "\n")
  cat("REPORTE: Estructuras de dimensiones disponibles para AFC multidimensional\n")
  cat(strrep("=", 70), "\n\n")

  if (!tiene_dim) {
    cat("  La columna 'dimension' NO existe en CROSSWALK_COMPLETO.\n")
    cat("  AFC multidimensional requiere añadir esa columna al crosswalk.\n\n")
    return(invisible(NULL))
  }

  cat("  La columna 'dimension' EXISTE. Estructura encontrada:\n\n")
  cw_ok <- cw[!is.na(cw$validado) & trimws(cw$validado) == "OK", ]

  hojas <- sort(unique(trimws(as.character(cw_ok$hoja))))
  for (h in hojas) {
    sub <- cw_ok[trimws(cw_ok$hoja) == h, ]
    dims <- sort(unique(trimws(as.character(sub$dimension[!is.na(sub$dimension)]))))
    cat(sprintf("  %-20s  %d ítems  |  Dimensiones (%d): %s\n",
                h, nrow(sub), length(dims),
                if (length(dims) == 0) "sin asignar" else paste(dims, collapse=", ")))
  }

  cat("\n  RECOMENDACIÓN:\n")
  cat("  - Preservar 'dimension' en cargar_crosswalk_completo() (añadir al data.frame).\n")
  cat("  - Pasar cw_df a analizar_items_psicometrico() y construir modelo CFA\n")
  cat("    por dimensión usando las líneas del model string de lavaan.\n")
  cat("  - Reportar ajuste global (CFI≥.90, RMSEA≤.08, SRMR≤.08) antes de\n")
  cat("    interpretar cargas por dimensión.\n")
  cat("  - NO implementado aún: solo diagnóstico de disponibilidad de estructura.\n\n")
  invisible(cw_ok)
}

# =============================================================================
# 9. PUNTO DE ENTRADA PRINCIPAL
# =============================================================================

cat("\nScript cargado. Construyendo catálogo de archivos...\n")
# Reporte de estructuras dimensionales disponibles para AFC multidimensional
reportar_dimensiones_cw()
catalogo <- catalogo_archivos()
cat(sprintf("Archivos encontrados: %d\n", nrow(catalogo)))
if (nrow(catalogo) > 0) print(catalogo[, c("ciclo","cuestion","figura","momento")])

# --- Ejecutar todos los cuestionarios encontrados ---
# Un archivo Excel por cuestionario; una pestaña por figura educativa.
# Columnas: indicadores por ciclo+momento (2425pre, 2425post, 2526pre)
#           + Decision final + Justificacion
#
# Descomenta para correr todo de una vez:
#
# cuestionarios_disponibles <- unique(catalogo$cuestion)
# for (cq in cuestionarios_disponibles) {
#   consolidar_cuestionario(cq, catalogo)
# }

# --- O ejecutar un cuestionario específico ---
# consolidar_cuestionario("CTXT",  catalogo)
# consolidar_cuestionario("HD",    catalogo)
# consolidar_cuestionario("HSXXI", catalogo)
# consolidar_cuestionario("HI",    catalogo)

cat("\nPróximos pasos:\n")
cat("  1. Verifica que catalogo tenga todos tus archivos (impreso arriba).\n")
cat("  2. Ajusta CONFIG$dir_datos / dir_salida si las rutas difieren.\n")
cat("  3. Descomenta el bloque 'combinaciones' en la sección 9 para correr todo.\n")
cat("  4. O ejecuta ejecutar_combinacion() individualmente por cuestionario.\n")
cat("  5. Al final, ejecuta consolidar_global() para el Excel maestro.\n")
