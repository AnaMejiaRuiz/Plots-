# =============================================================================
# ANÁLISIS PSICOMÉTRICO PARA REDUCCIÓN DE REACTIVOS
# Figuras Educativas: DOC, EST, DIR, PMF
# Cuestionarios: HD, HSXXI, HI, CTXT
# Ciclos: 2024-2025 (2425) y 2025-2026 (2526)
#
# Métodos:
#   1. Teoría Clásica de los Tests (TCT/CTT)
#   2. Teoría de Respuesta al Ítem (TRI/IRT) — Rasch, GRM
#   3. Análisis Factorial Exploratorio (AFE/EFA)
#   4. Psicometría de Redes (EGA)
#   5. Comparación entre ciclos y decisión integrada
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
  umbral_a_grm       = 0.50,  # discriminación GRM mínima
  umbral_comunalidad = 0.20,
  umbral_estabilidad = 0.50,  # EGA item stability
  umbral_votos       = 2      # mínimo de métodos que deben coincidir para eliminar
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
#' @param hoja  Ej. "CTXT_DIR", "HD_DOC", "SXXI_EST"
#' @return list con tres data.frames: $exacto, $nuevo, $sin_match
#'   $exacto   : code_2425, canon (= code_2526)  — presentes en ambos ciclos
#'   $nuevo    : canon (= code_2526)              — solo en 2526
#'   $sin_match: canon (= code_2425)              — solo en 2425
#'   Devuelve NULL si el crosswalk no existe o la hoja no se encuentra.

cargar_crosswalk <- function(hoja) {
  if (!file.exists(CROSSWALK_PATH)) {
    warning("Crosswalk no encontrado: ", CROSSWALK_PATH)
    return(NULL)
  }
  cw <- tryCatch(
    openxlsx::read.xlsx(CROSSWALK_PATH, sheet = "CROSSWALK_COMPLETO",
                        na.strings = c("", "NA")),
    error = function(e) { warning("Error leyendo crosswalk: ", e$message); NULL }
  )
  if (is.null(cw)) return(NULL)

  # Normalizar nombre de hoja
  cw$hoja <- stringr::str_replace_all(trimws(as.character(cw$hoja)), " ", "_")
  hoja    <- stringr::str_replace_all(trimws(hoja), " ", "_")

  sub_cw <- cw[cw$hoja == hoja & !is.na(cw$hoja), ]
  if (nrow(sub_cw) == 0) {
    warning("Hoja '", hoja, "' no encontrada en crosswalk.")
    return(NULL)
  }

  # Solo filas con validado == "OK" (excluir ELIMINAR y sin validar)
  ok <- !is.na(sub_cw$validado) & trimws(sub_cw$validado) == "OK"
  sub_cw <- sub_cw[ok, ]
  if (nrow(sub_cw) == 0) {
    warning("Ninguna fila con validado='OK' en hoja '", hoja, "'.")
    return(NULL)
  }

  # Normalizar tipo_match (el usuario puede escribir EXACTO, NUEVO, SIN_MATCH)
  sub_cw$tipo_match <- trimws(toupper(as.character(sub_cw$tipo_match)))

  # --- Ítems EXACTO: presentes en ambos ciclos ---
  exacto <- sub_cw[sub_cw$tipo_match == "EXACTO" &
                     !is.na(sub_cw$code_2425) & !is.na(sub_cw$code_2526), ]
  df_exacto <- data.frame(
    code_2425 = trimws(exacto$code_2425),
    canon     = trimws(exacto$code_2526),   # 2526 = nombre canónico
    stringsAsFactors = FALSE
  )

  # --- Ítems NUEVO: solo en 2526 ---
  nuevo <- sub_cw[sub_cw$tipo_match %in% c("NUEVO", "NUEVO_2526") &
                    !is.na(sub_cw$code_2526), ]
  df_nuevo <- data.frame(
    canon = trimws(nuevo$code_2526),
    stringsAsFactors = FALSE
  )

  # --- Ítems SIN_MATCH: solo en 2425 ---
  sin_match <- sub_cw[sub_cw$tipo_match == "SIN_MATCH" &
                        !is.na(sub_cw$code_2425), ]
  df_sin_match <- data.frame(
    canon = trimws(sin_match$code_2425),
    stringsAsFactors = FALSE
  )

  cat(sprintf("  [CROSSWALK] '%s': %d EXACTO | %d NUEVO(2526) | %d SIN_MATCH(2425)\n",
              hoja, nrow(df_exacto), nrow(df_nuevo), nrow(df_sin_match)))

  list(exacto = df_exacto, nuevo = df_nuevo, sin_match = df_sin_match)
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

cargar_base <- function(ruta, ciclo, mapa_cw = NULL) {
  dat <- tryCatch(
    readxl::read_excel(ruta, na = c("", "NA", "N/A")),
    error = function(e) { warning("No se pudo leer: ", ruta, "\n  ", e$message); NULL }
  )
  if (is.null(dat)) return(NULL)

  # --- Estandarizar nombres para ciclo 2425 ---
  # Prefijos observados: PRE_CTXT_DIR_Q1, POS_CTXT_DIR_Q10_LAPTOP, etc.
  if (ciclo == "2425") {
    extraido <- stringr::str_extract(names(dat), "Q\\d+.*$")
    extraido <- stringr::str_replace_all(extraido, "[^A-Za-z0-9_]", "_")
    names(dat) <- ifelse(is.na(extraido), paste0("VAR_", seq_along(extraido)), extraido)
  }

  # --- Seleccionar columnas Q, forzar numérico ---
  cols_q <- stringr::str_detect(names(dat), "^Q\\d+")
  dat_q  <- dat[, cols_q, drop = FALSE]
  dat_q  <- as.data.frame(lapply(dat_q,
               function(x) suppressWarnings(as.numeric(as.character(x)))))

  # --- Descartar columnas 100% NA (padres de opción múltiple) ---
  todo_na <- colMeans(is.na(dat_q)) == 1
  if (any(todo_na))
    cat(sprintf("  [INFO] Columnas padre 100%% NA descartadas: %s\n",
                paste(names(dat_q)[todo_na], collapse = ", ")))
  dat_q <- dat_q[, !todo_na, drop = FALSE]
  if (ncol(dat_q) == 0) { warning("Sin columnas con datos en: ", ruta); return(NULL) }

  # --- Aplicar crosswalk ---
  if (is.null(mapa_cw)) {
    # Sin crosswalk: devolver todas las columnas Q sin filtrar
    attr(dat_q, "tipo_item") <- setNames(rep("SIN_CROSSWALK", ncol(dat_q)), names(dat_q))
    return(dat_q)
  }

  if (ciclo == "2425") {
    # EXACTO: renombrar code_2425 → canon (code_2526)
    cols_exacto   <- mapa_cw$exacto$code_2425
    nombres_canon <- mapa_cw$exacto$canon
    # SIN_MATCH: conservar con nombre original (= canon para este ciclo)
    cols_sin_match <- mapa_cw$sin_match$canon

    cols_usar <- c(cols_exacto, cols_sin_match)
    cols_disponibles <- intersect(cols_usar, names(dat_q))

    if (length(cols_disponibles) == 0) {
      warning("Ninguna columna del crosswalk encontrada en: ", ruta)
      return(NULL)
    }

    dat_out <- dat_q[, cols_disponibles, drop = FALSE]

    # Renombrar EXACTO a nombre canónico
    idx_exacto <- match(mapa_cw$exacto$code_2425, names(dat_out))
    idx_valido <- !is.na(idx_exacto)
    names(dat_out)[idx_exacto[idx_valido]] <- mapa_cw$exacto$canon[idx_valido]

    # Etiqueta de tipo por columna (para la tabla de resultados)
    tipo_vec <- ifelse(names(dat_out) %in% mapa_cw$exacto$canon,
                       "EXACTO", "SIN_MATCH")
    attr(dat_out, "tipo_item") <- setNames(tipo_vec, names(dat_out))

    n_ex <- sum(tipo_vec == "EXACTO")
    n_sm <- sum(tipo_vec == "SIN_MATCH")
    cat(sprintf("  [2425] %d EXACTO + %d SIN_MATCH seleccionados (%d en datos)\n",
                n_ex, n_sm, ncol(dat_out)))

  } else {  # ciclo == "2526"
    # EXACTO: nombre ya es canónico (code_2526)
    cols_exacto <- mapa_cw$exacto$canon
    # NUEVO: solo existe en 2526
    cols_nuevo  <- mapa_cw$nuevo$canon

    cols_usar        <- c(cols_exacto, cols_nuevo)
    cols_disponibles <- intersect(cols_usar, names(dat_q))

    if (length(cols_disponibles) == 0) {
      warning("Ninguna columna del crosswalk encontrada en: ", ruta)
      return(NULL)
    }

    dat_out <- dat_q[, cols_disponibles, drop = FALSE]

    tipo_vec <- ifelse(names(dat_out) %in% cols_exacto, "EXACTO", "NUEVO")
    attr(dat_out, "tipo_item") <- setNames(tipo_vec, names(dat_out))

    n_ex <- sum(tipo_vec == "EXACTO")
    n_nv <- sum(tipo_vec == "NUEVO")
    cat(sprintf("  [2526] %d EXACTO + %d NUEVO seleccionados (%d en datos)\n",
                n_ex, n_nv, ncol(dat_out)))
  }

  dat_out
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

analizar_items <- function(datos, etiqueta, max_item = NULL) {

  # Máximo teórico por ítem (escala inicia en 0).
  # Si max_item es un escalar se aplica igual a todos; si es NULL se detecta
  # automáticamente por columna (necesario cuando hay mezcla 0-3 / 0-4).
  max_obs <- sapply(datos, function(x) {
    v <- max(x, na.rm = TRUE)
    if (is.infinite(v) || is.nan(v)) NA_real_ else v
  })

  max_por_item <- if (!is.null(max_item) && length(max_item) == 1) {
    setNames(rep(as.numeric(max_item), ncol(datos)), names(datos))
  } else {
    max_obs
  }

  # Umbral para detectar variables continuas (edad, días, horas, etc.)
  # Si el máximo observado supera este valor, el ítem se trata como continuo
  # y se excluye del pipeline psicométrico (TCT/IRT/EFA requieren escala acotada).
  MAX_ESCALA_LIKERT <- 10   # cualquier ítem con max > 10 se considera continuo

  continuas <- !is.na(max_por_item) & max_por_item > MAX_ESCALA_LIKERT
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
    psych::alpha(datos_limpios, check.keys = FALSE),
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

  # -------------------------------------------------------------------------
  # 4b. IRT: Rasch (PCM)
  # -------------------------------------------------------------------------
  cat("  Rasch...\n")
  tryCatch({
    mod_rasch <- mirt::mirt(datos_limpios, model = 1,
                            itemtype = "Rasch", verbose = FALSE)
    fit_r <- mirt::itemfit(mod_rasch, fit_statistics = "infit")
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
    mod_grm <- mirt::mirt(datos_limpios, model = 1,
                          itemtype = "graded", verbose = FALSE)
    params_g <- mirt::coef(mod_grm, IRTpars = TRUE, simplify = TRUE)$items
    info_g   <- mirt::iteminfo(mod_grm, Theta = matrix(0))

    grm_df <- data.frame(
      Item            = rownames(params_g),
      a_GRM           = round(params_g[, "a"], 3),
      info_theta0_GRM = round(as.numeric(info_g), 3),
      eliminar_GRM    = ifelse(params_g[, "a"] < CONFIG$umbral_a_grm,
                               "ELIMINAR", "Conservar"),
      stringsAsFactors = FALSE
    )
    tabla <- dplyr::left_join(tabla, grm_df, by = "Item")

    # Guardar ICC y TIC
    tryCatch({
      pdf(file.path(CONFIG$dir_salida,
                    paste0(etiqueta, "_ICC_GRM.pdf")), width = 12, height = 8)
      n_g <- min(ncol(datos_limpios), 24)
      for (i in seq(1, n_g, by = 6)) {
        idx <- i:min(i + 5, n_g)
        mirt::plot(mod_grm, type = "trace", which.items = idx,
                   main = paste0(etiqueta, " — ICC ítems ", min(idx), "–", max(idx)))
      }
      dev.off()
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
  # 4e. EGA (Psicometría de Redes)
  # -------------------------------------------------------------------------
  cat("  EGA...\n")
  tryCatch({
    ega_r <- EGAnet::EGA(datos_limpios, model = "glasso",
                         plot.EGA = FALSE, verbose = FALSE)
    estab  <- EGAnet::itemStability(ega_r)
    est_vec <- estab$item.stability$empirical.dimensions

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
  # 4f. Decisión integrada por votos
  # -------------------------------------------------------------------------
  cols_elim <- grep("^eliminar_", names(tabla), value = TRUE)

  if (length(cols_elim) > 0) {
    tabla$votos_eliminacion <- rowSums(
      sapply(cols_elim, function(col) {
        v <- tabla[[col]]
        ifelse(is.na(v), 0L, as.integer(v == "ELIMINAR"))
      }),
      na.rm = TRUE
    )

    tabla$decision_final <- dplyr::case_when(
      tabla$flag_NA == "DATOS_INSUFICIENTES" ~ "DATOS_INSUFICIENTES",
      tabla$votos_eliminacion >= CONFIG$umbral_votos ~ "ELIMINAR",
      TRUE ~ "Conservar"
    )
  }

  # Alpha de la escala reducida
  items_conservar <- tabla$Item[tabla$decision_final == "Conservar" &
                                  tabla$Item %in% names(datos_limpios)]
  if (length(items_conservar) >= 3) {
    alpha_red <- tryCatch(
      psych::alpha(datos_limpios[, items_conservar], check.keys = FALSE)$total$raw_alpha,
      error = function(e) NA_real_
    )
    cat(sprintf("  Alpha escala reducida (%d ítems): %.3f\n",
                length(items_conservar), alpha_red))
    if (!is.na(alpha_red) && alpha_red < CONFIG$umbral_alpha) {
      warning("Alpha reducida por debajo del umbral en: ", etiqueta)
    }
    attr(tabla, "alpha_reducida") <- alpha_red
  }

  tabla
}

# =============================================================================
# 5. COMPARACIÓN ENTRE CICLOS
# =============================================================================

#' Combina tablas de dos ciclos y genera comparativo con semáforo.
#'
#' @param tabla_A  resultado de analizar_items() para ciclo 2425
#' @param tabla_B  resultado de analizar_items() para ciclo 2526
#' @return data.frame comparativo

#' Compara resultados psicométricos entre ciclos respetando el tipo de ítem.
#'
#' Produce un data.frame con tres grupos de filas claramente separados:
#'   1. EXACTO    — ítem existe en ambos ciclos: muestra indicadores lado a lado
#'                  + semáforo de consistencia + decisión_recomendada
#'   2. SIN_MATCH — ítem solo en 2425: indicadores del ciclo A, sin comparación
#'   3. NUEVO     — ítem solo en 2526: indicadores del ciclo B, sin comparación
#'
#' @param tabla_A  resultado de analizar_items() para ciclo 2425
#' @param tabla_B  resultado de analizar_items() para ciclo 2526

comparar_ciclos <- function(tabla_A, tabla_B, ciclo_A = "2425", ciclo_B = "2526") {

  sufA <- paste0("_", ciclo_A)
  sufB <- paste0("_", ciclo_B)

  indicadores <- c("tipo_item", "pct_NA", "p_dificultad", "ritc", "alpha_sin_item",
                   "b_Rasch", "infit_MNSQ", "outfit_MNSQ",
                   "a_GRM", "info_theta0_GRM", "comunalidad_EFA",
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

  # Unir los tres grupos (bind_rows rellena con NA las columnas faltantes)
  comp_total <- dplyr::bind_rows(comp_exacto, comp_sin_match, comp_nuevo)

  # Reordenar: grupo + Item al frente (solo columnas que existan)
  cols_frente <- intersect(c("grupo", "Item", "consistencia", "decision_recomendada"),
                           names(comp_total))
  resto       <- setdiff(names(comp_total), cols_frente)
  comp_total  <- comp_total[, c(cols_frente, resto), drop = FALSE]

  n_ex <- nrow(comp_exacto)
  n_sm <- nrow(comp_sin_match)
  n_nv <- nrow(comp_nuevo)
  cat(sprintf("  [COMPARACION] %d EXACTO | %d SIN_MATCH | %d NUEVO\n", n_ex, n_sm, n_nv))

  comp_total
}

# =============================================================================
# 6. EXPORTACIÓN A EXCEL CON FORMATO
# =============================================================================

COLORES <- list(
  eliminar    = list(fg = "#FFCCCC", font = "#CC0000"),
  conservar   = list(fg = "#CCFFCC", font = "#006600"),
  revisar     = list(fg = "#FFF3CC", font = "#856404"),
  insuf       = list(fg = "#E0E0E0", font = "#555555"),
  header      = list(fg = "#1F3864", font = "#FFFFFF"),
  exacto      = list(fg = "#FFFFFF", font = "#000000"),   # blanco — ítems comparables
  sin_match   = list(fg = "#FFF3CC", font = "#856404"),   # amarillo — solo 2425
  nuevo       = list(fg = "#DDEBF7", font = "#1F3864")    # azul claro — solo 2526
)

# Aplica relleno por valor en col_decision
estilo_decision <- function(wb, hoja, tabla, col_decision) {
  reglas <- list(
    list(patron = "^ELIMINAR$",        col = COLORES$eliminar),
    list(patron = "^Conservar",        col = COLORES$conservar),
    list(patron = "^REVISAR",          col = COLORES$revisar),
    list(patron = "DATOS_INSUF",       col = COLORES$insuf)
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
    list(patron = "^EXACTO$",      col = COLORES$exacto),
    list(patron = "SIN_MATCH",     col = COLORES$sin_match),
    list(patron = "NUEVO",         col = COLORES$nuevo)
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
# 7. PIPELINE COMPLETO POR COMBINACIÓN CUESTIONARIO × FIGURA × MOMENTO
# =============================================================================

#' Ejecuta el análisis completo para una combinación y guarda el Excel.
#'
#' @param cuestion   "HD", "HSXXI", "HI", o "CTXT"
#' @param figura     "DIR", "DOC", "EST", o "PMF"
#' @param momento    "pre" o "post"
#' @param catalogo   data.frame devuelto por catalogo_archivos()
#' @param max_item   Máximo teórico de la escala (0 = inicio). NULL → derivar de datos.
#'                   Usar CONFIG$max_escala[[cuestion]] como valor por defecto.

ejecutar_combinacion <- function(cuestion, figura, momento = "pre",
                                 catalogo, max_item = NULL) {

  # Resolver max_item desde CONFIG si no se especifica
  if (is.null(max_item) && cuestion %in% names(CONFIG$max_escala)) {
    max_item <- CONFIG$max_escala[[cuestion]]
    cat(sprintf("  [INFO] max_item para %s desde CONFIG: %g\n", cuestion, max_item))
  }

  etiq_base <- paste0(cuestion, "_", figura, "_", momento)
  cat("\n", strrep("=", 70), "\n")
  cat("PROCESANDO:", etiq_base, "\n")
  cat(strrep("=", 70), "\n")

  # Cargar crosswalk para alinear códigos entre ciclos
  # La hoja del crosswalk usa formato CUESTIONARIO_FIGURA (ej. "CTXT_DIR")
  # HSXXI en las bases corresponde a SXXI en el diccionario
  hoja_cw <- paste0(gsub("HSXXI", "SXXI", cuestion), "_", figura)
  mapa_cw <- cargar_crosswalk(hoja_cw)
  if (!is.null(mapa_cw)) {
    n_match <- nrow(mapa_cw$exacto)
    cat(sprintf("  [CROSSWALK] Hoja '%s': %d EXACTO | %d NUEVO | %d SIN_MATCH\n",
                hoja_cw, nrow(mapa_cw$exacto), nrow(mapa_cw$nuevo), nrow(mapa_cw$sin_match)))
  } else {
    cat(sprintf("  [CROSSWALK] Sin mapa para '%s' — comparación usará códigos originales\n",
                hoja_cw))
  }

  # Buscar archivos en catálogo
  reg <- catalogo %>%
    dplyr::filter(cuestion == !!cuestion,
                  figura   == !!figura,
                  momento  == !!momento)

  if (nrow(reg) == 0) {
    cat("  Sin archivos para:", etiq_base, "— omitido.\n")
    return(invisible(NULL))
  }

  resultados_ciclo <- list()

  for (i in seq_len(nrow(reg))) {
    ciclo <- reg$ciclo[i]
    ruta  <- reg$ruta[i]
    etiq  <- paste0(etiq_base, "_", ciclo)

    cat("\n  Ciclo:", ciclo, "— Archivo:", basename(ruta), "\n")
    datos <- cargar_base(ruta, ciclo, mapa_cw = mapa_cw)
    if (is.null(datos) || nrow(datos) < 30) {
      cat("  Datos insuficientes (N <30) — omitido.\n"); next
    }
    cat("  N sujetos:", nrow(datos), "| N ítems:", ncol(datos), "\n")

    res <- analizar_items(datos, etiq, max_item = max_item)
    res$ciclo <- ciclo
    resultados_ciclo[[ciclo]] <- res
  }

  if (length(resultados_ciclo) == 0) return(invisible(NULL))

  # Crear workbook para esta combinación
  wb <- openxlsx::createWorkbook()

  for (ciclo in names(resultados_ciclo)) {
    agregar_hoja_tabla(wb,
                       nombre_hoja   = paste0(ciclo, "_items"),
                       tabla         = resultados_ciclo[[ciclo]],
                       col_decision  = "decision_final")
  }

  # Hoja comparativa si hay ambos ciclos
  if (all(c("2425", "2526") %in% names(resultados_ciclo))) {
    comp <- comparar_ciclos(resultados_ciclo[["2425"]],
                            resultados_ciclo[["2526"]])
    agregar_hoja_tabla(wb,
                       nombre_hoja  = "Comparacion_ciclos",
                       tabla        = comp,
                       col_decision = "decision_recomendada")
  }

  # Hoja resumen ejecutivo
  resumen_filas <- lapply(names(resultados_ciclo), function(ciclo) {
    t <- resultados_ciclo[[ciclo]]
    data.frame(
      ciclo          = ciclo,
      cuestionario   = cuestion,
      figura         = figura,
      momento        = momento,
      total_items    = nrow(t),
      datos_insuf    = sum(t$flag_NA == "DATOS_INSUFICIENTES", na.rm = TRUE),
      a_eliminar     = sum(t$decision_final == "ELIMINAR", na.rm = TRUE),
      a_conservar    = sum(t$decision_final == "Conservar", na.rm = TRUE),
      pct_reduccion  = round(
        sum(t$decision_final == "ELIMINAR", na.rm = TRUE) / nrow(t) * 100, 1
      ),
      ritc_promedio  = round(mean(t$ritc, na.rm = TRUE), 3),
      stringsAsFactors = FALSE
    )
  })
  resumen_ejec <- dplyr::bind_rows(resumen_filas)
  agregar_hoja_tabla(wb, "Resumen_ejecutivo", resumen_ejec)

  # Guardar Excel
  dir.create(CONFIG$dir_salida, showWarnings = FALSE, recursive = TRUE)
  archivo_out <- file.path(CONFIG$dir_salida, paste0(etiq_base, ".xlsx"))
  openxlsx::saveWorkbook(wb, archivo_out, overwrite = TRUE)
  cat("\n  Excel guardado:", archivo_out, "\n")

  invisible(resultados_ciclo)
}

# =============================================================================
# 8. CONSOLIDACIÓN GLOBAL
# =============================================================================

#' Reúne todos los archivos _items de resultados y produce un Excel maestro.

consolidar_global <- function(dir_salida = CONFIG$dir_salida) {
  archivos_xlsx <- list.files(dir_salida, pattern = "\\.xlsx$",
                              full.names = TRUE, recursive = FALSE)
  # Excluir el maestro si ya existe
  archivos_xlsx <- archivos_xlsx[!grepl("MAESTRO", archivos_xlsx)]

  if (length(archivos_xlsx) == 0) {
    cat("Sin archivos para consolidar.\n"); return(invisible(NULL))
  }

  todos <- purrr::map_dfr(archivos_xlsx, function(f) {
    hojas <- openxlsx::getSheetNames(f)
    hojas_comp <- hojas[grepl("_items|Comparacion", hojas)]
    purrr::map_dfr(hojas_comp, function(h) {
      df <- tryCatch(
        openxlsx::read.xlsx(f, sheet = h, na.strings = c("NA", "")),
        error = function(e) NULL
      )
      if (!is.null(df)) {
        df$Fuente <- basename(f)
        df$Hoja   <- h
      }
      df
    })
  })

  resumen_global <- todos %>%
    dplyr::filter(!is.na(decision_final)) %>%
    dplyr::group_by(Fuente, Hoja, ciclo) %>%
    dplyr::summarise(
      Total           = dplyr::n(),
      Eliminar        = sum(decision_final == "ELIMINAR", na.rm = TRUE),
      Conservar       = sum(decision_final == "Conservar", na.rm = TRUE),
      Datos_insuf     = sum(flag_NA == "DATOS_INSUFICIENTES", na.rm = TRUE),
      Pct_reduccion   = round(Eliminar / Total * 100, 1),
      ritc_promedio   = round(mean(ritc, na.rm = TRUE), 3),
      .groups = "drop"
    )

  wb_maestro <- openxlsx::createWorkbook()
  agregar_hoja_tabla(wb_maestro, "Todos_los_items", todos)
  agregar_hoja_tabla(wb_maestro, "Resumen_global", resumen_global)

  ruta_maestro <- file.path(dir_salida, "MAESTRO_reduccion_reactivos.xlsx")
  openxlsx::saveWorkbook(wb_maestro, ruta_maestro, overwrite = TRUE)
  cat("Excel maestro guardado:", ruta_maestro, "\n")

  invisible(list(items = todos, resumen = resumen_global))
}

# =============================================================================
# 9. PUNTO DE ENTRADA PRINCIPAL
# =============================================================================

cat("\nScript cargado. Construyendo catálogo de archivos...\n")
catalogo <- catalogo_archivos()
cat(sprintf("Archivos encontrados: %d\n", nrow(catalogo)))
if (nrow(catalogo) > 0) print(catalogo[, c("ciclo","cuestion","figura","momento")])

# --- Ejecutar todas las combinaciones encontradas en el catálogo ---
# max_item se resuelve automáticamente desde CONFIG$max_escala por cuestionario.
# Si un cuestionario mezcla tipos de escala (ej. CTXT con binarias y Likert),
# ejecuta por separado especificando max_item manualmente.
#
# Descomenta para correr todo de una vez:
#
# combinaciones <- catalogo %>%
#   dplyr::distinct(cuestion, figura, momento)
#
# for (i in seq_len(nrow(combinaciones))) {
#   ejecutar_combinacion(
#     cuestion  = combinaciones$cuestion[i],
#     figura    = combinaciones$figura[i],
#     momento   = combinaciones$momento[i],
#     catalogo  = catalogo
#     # max_item = NULL  → se toma de CONFIG$max_escala automáticamente
#   )
# }
# consolidar_global()

# --- O ejecutar una combinación específica con max_item explícito ---
# ejecutar_combinacion("HSXXI", "EST", "pre",  catalogo)           # max=4 (0-4)
# ejecutar_combinacion("HD",    "DOC", "pre",  catalogo)           # max=3 (0-3)
# ejecutar_combinacion("HI",    "DOC", "pre",  catalogo)           # max=4 (0-4)
# ejecutar_combinacion("CTXT",  "PMF", "pre",  catalogo)           # max=3 (0-3)
# ejecutar_combinacion("HD",    "EST", "pre",  catalogo, max_item = 4)  # si HD usa hd_frecuencia (0-4)

cat("\nPróximos pasos:\n")
cat("  1. Verifica que catalogo tenga todos tus archivos (impreso arriba).\n")
cat("  2. Ajusta CONFIG$dir_datos / dir_salida si las rutas difieren.\n")
cat("  3. Descomenta el bloque 'combinaciones' en la sección 9 para correr todo.\n")
cat("  4. O ejecuta ejecutar_combinacion() individualmente por cuestionario.\n")
cat("  5. Al final, ejecuta consolidar_global() para el Excel maestro.\n")
