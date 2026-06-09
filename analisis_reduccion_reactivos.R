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
  # 4e. EGA (Psicometría de Redes)
  # -------------------------------------------------------------------------
  cat("  EGA...\n")
  tryCatch({
    ega_r <- EGAnet::EGA(datos_limpios, model = "glasso",
                         plot.EGA = FALSE, verbose = FALSE)

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
                      verbose = FALSE),
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

  # -------------------------------------------------------------------------
  # 4g. Gráfico de perfil de ítems (ritc + dificultad + decisión)
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
# 7. PIPELINE POR COMBINACIÓN — devuelve resultados, sin guardar Excel propio
# =============================================================================

#' Ejecuta TCT+IRT+EFA+EGA para una combinación cuestionario × figura × momento.
#' Devuelve lista nombrada por ciclo; cada elemento es el data.frame de analizar_items
#' con columna extra `momento`.

ejecutar_combinacion <- function(cuestion, figura, momento = "pre",
                                 catalogo, max_item = NULL) {

  if (is.null(max_item) && cuestion %in% names(CONFIG$max_escala))
    max_item <- CONFIG$max_escala[[cuestion]]

  etiq_base <- paste0(cuestion, "_", figura, "_", momento)
  cat("\n", strrep("=", 70), "\n")
  cat("PROCESANDO:", etiq_base, "\n")
  cat(strrep("=", 70), "\n")

  hoja_cw <- paste0(gsub("HSXXI", "SXXI", cuestion), "_", figura)
  mapa_cw <- cargar_crosswalk(hoja_cw)
  if (!is.null(mapa_cw)) {
    cat(sprintf("  [CROSSWALK] '%s': %d EXACTO | %d NUEVO | %d SIN_MATCH\n",
                hoja_cw, nrow(mapa_cw$exacto), nrow(mapa_cw$nuevo), nrow(mapa_cw$sin_match)))
  } else {
    cat(sprintf("  [CROSSWALK] Sin mapa para '%s'\n", hoja_cw))
  }

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
    if (is.null(res) || nrow(res) == 0) {
      cat("  Sin ítems analizables (todos continuos o excluidos) — omitido.\n")
      next
    }
    res$ciclo   <- ciclo
    res$momento <- momento
    # Clave única para columnas en formato ancho: "2425pre", "2526pre", etc.
    res$clave_cm <- paste0(ciclo, momento)
    resultados_ciclo[[paste0(ciclo, "_", momento)]] <- res
  }

  invisible(resultados_ciclo)
}

# =============================================================================
# 8. CONSTRUCCIÓN DE TABLA ANCHA Y GENERACIÓN DE EXCEL POR CUESTIONARIO
# =============================================================================

# Indicadores que se replican por cada ciclo+momento
INDICADORES_CM <- c("pct_NA", "media", "p_dificultad", "ritc",
                     "alpha_sin_item", "b_Rasch", "infit_MNSQ", "outfit_MNSQ",
                     "a_GRM", "comunalidad_EFA", "estabilidad_EGA",
                     "decision_final")

#' Construye una fila de justificación a partir de los indicadores por ciclo.
generar_justificacion <- function(fila, claves_cm) {
  razones <- c()
  for (cm in claves_cm) {
    dec_col <- paste0(cm, "_decision_final")
    if (!dec_col %in% names(fila)) next
    dec <- fila[[dec_col]]
    if (is.na(dec) || dec != "ELIMINAR") next

    # Detectar qué criterios fallaron
    problemas <- c()
    pna   <- fila[[paste0(cm, "_pct_NA")]];       if (!is.na(pna)  && pna  > CONFIG$umbral_NA_pct)    problemas <- c(problemas, sprintf("NA=%.0f%%", pna))
    ritc  <- fila[[paste0(cm, "_ritc")]];          if (!is.na(ritc) && ritc < CONFIG$umbral_ritc)       problemas <- c(problemas, sprintf("ritc=%.2f", ritc))
    inf   <- fila[[paste0(cm, "_infit_MNSQ")]];   if (!is.na(inf)  && inf  > CONFIG$umbral_infit)      problemas <- c(problemas, sprintf("infit=%.2f", inf))
    out   <- fila[[paste0(cm, "_outfit_MNSQ")]];  if (!is.na(out)  && out  > CONFIG$umbral_outfit)     problemas <- c(problemas, sprintf("outfit=%.2f", out))
    agrm  <- fila[[paste0(cm, "_a_GRM")]];        if (!is.na(agrm) && agrm < CONFIG$umbral_a_grm)      problemas <- c(problemas, sprintf("a_GRM=%.2f", agrm))
    com   <- fila[[paste0(cm, "_comunalidad_EFA")]]; if (!is.na(com) && com < CONFIG$umbral_comunalidad) problemas <- c(problemas, sprintf("h2=%.2f", com))
    est   <- fila[[paste0(cm, "_estabilidad_EGA")]]; if (!is.na(est) && est < CONFIG$umbral_estabilidad) problemas <- c(problemas, sprintf("estab=%.2f", est))

    if (length(problemas) > 0)
      razones <- c(razones, paste0("[", cm, ": ", paste(problemas, collapse=", "), "]"))
  }
  if (length(razones) == 0) return("Conservar en todos los ciclos disponibles")
  paste(razones, collapse = " ")
}

#' Pivota lista de resultados por ciclo+momento a formato ancho (1 fila por ítem).
construir_tabla_ancha <- function(resultados_lista) {
  if (length(resultados_lista) == 0) return(NULL)

  # Universo de ítems: unión de todos los Item de todos los ciclos+momentos
  todos_items <- unique(unlist(lapply(resultados_lista, function(r) r$Item)))
  if (length(todos_items) == 0) return(NULL)

  # Columnas identificadoras (se toman del primer resultado disponible)
  primer <- resultados_lista[[which(sapply(resultados_lista, nrow) > 0)[1]]]
  cols_id <- intersect(c("Item", "tipo_item"), names(primer))

  tabla_base <- data.frame(Item = todos_items, stringsAsFactors = FALSE)

  # Añadir tipo_item (EXACTO/NUEVO/SIN_MATCH) desde cualquier ciclo que lo tenga
  tipo_map <- do.call(rbind, lapply(resultados_lista, function(r) {
    if ("tipo_item" %in% names(r)) r[, c("Item", "tipo_item")] else NULL
  }))
  if (!is.null(tipo_map)) {
    tipo_map <- tipo_map[!duplicated(tipo_map$Item), ]
    tabla_base <- dplyr::left_join(tabla_base, tipo_map, by = "Item")
  }

  claves_cm <- names(resultados_lista)  # e.g. "2425_pre", "2425_post", "2526_pre"

  # Añadir bloque de columnas por cada ciclo+momento
  for (clave in claves_cm) {
    res <- resultados_lista[[clave]]
    # Etiqueta compacta: "2425pre", "2526pre"
    cm_label <- gsub("_", "", clave)  # "2425_pre" → "2425pre"

    cols_usar <- intersect(INDICADORES_CM, names(res))
    bloque    <- res[, c("Item", cols_usar), drop = FALSE]
    names(bloque)[-1] <- paste0(cm_label, "_", names(bloque)[-1])

    tabla_base <- dplyr::left_join(tabla_base, bloque, by = "Item")
  }

  # Decisión final transversal: ELIMINAR si la mayoría de ciclos con datos dice ELIMINAR
  dec_cols <- paste0(gsub("_", "", claves_cm), "_decision_final")
  dec_cols  <- intersect(dec_cols, names(tabla_base))

  tabla_base$ciclos_con_datos <- rowSums(!is.na(
    tabla_base[, dec_cols, drop = FALSE]))

  tabla_base$ciclos_eliminar  <- rowSums(
    sapply(dec_cols, function(d) {
      v <- tabla_base[[d]]
      ifelse(!is.na(v) & v == "ELIMINAR", 1L, 0L)
    }), na.rm = TRUE)

  tabla_base$Decision <- dplyr::case_when(
    tabla_base$ciclos_con_datos == 0                                      ~ "Sin datos",
    tabla_base$ciclos_eliminar  >  tabla_base$ciclos_con_datos / 2        ~ "ELIMINAR",
    tabla_base$ciclos_eliminar  == tabla_base$ciclos_con_datos & tabla_base$ciclos_con_datos > 0 ~ "ELIMINAR",
    TRUE                                                                   ~ "Conservar"
  )

  # Justificación
  tabla_base$Justificacion <- apply(tabla_base, 1, function(r) {
    generar_justificacion(as.list(r), gsub("_", "", claves_cm))
  })

  # Quitar columnas auxiliares de conteo
  tabla_base$ciclos_con_datos <- NULL
  tabla_base$ciclos_eliminar  <- NULL

  tabla_base
}

#' Genera un Excel por cuestionario: una pestaña por figura educativa.
consolidar_cuestionario <- function(cuestion, catalogo) {

  cat("\n", strrep("#", 70), "\n")
  cat("CONSOLIDANDO CUESTIONARIO:", cuestion, "\n")
  cat(strrep("#", 70), "\n")

  figuras    <- CONFIG$figuras
  momentos   <- unique(catalogo$momento[catalogo$cuestion == cuestion])
  max_item   <- CONFIG$max_escala[[cuestion]]

  wb <- openxlsx::createWorkbook()
  resumen_filas <- list()

  for (figura in figuras) {
    cat("\n  Figura:", figura, "\n")

    # Recopilar resultados de todos los momentos disponibles
    resultados_figura <- list()

    for (momento in momentos) {
      res_cm <- ejecutar_combinacion(cuestion, figura, momento,
                                     catalogo, max_item = max_item)
      if (!is.null(res_cm) && length(res_cm) > 0)
        resultados_figura <- c(resultados_figura, res_cm)
    }

    if (length(resultados_figura) == 0) {
      cat("  Sin resultados para figura", figura, "— pestaña omitida.\n")
      next
    }

    # Tabla ancha: ítems × (indicadores por ciclo+momento)
    tabla_ancha <- construir_tabla_ancha(resultados_figura)
    if (is.null(tabla_ancha) || nrow(tabla_ancha) == 0) next

    # Escribir pestaña
    agregar_hoja_tabla(wb,
                       nombre_hoja  = figura,
                       tabla        = tabla_ancha,
                       col_decision = "Decision")

    # Acumular para pestaña resumen
    n_total   <- nrow(tabla_ancha)
    n_elim    <- sum(tabla_ancha$Decision == "ELIMINAR",  na.rm = TRUE)
    n_cons    <- sum(tabla_ancha$Decision == "Conservar", na.rm = TRUE)
    resumen_filas[[figura]] <- data.frame(
      cuestionario    = cuestion,
      figura          = figura,
      total_items     = n_total,
      a_eliminar      = n_elim,
      a_conservar     = n_cons,
      pct_reduccion   = round(n_elim / n_total * 100, 1),
      stringsAsFactors = FALSE
    )
    cat(sprintf("  [OK] %s: %d ítems | %d Eliminar | %d Conservar\n",
                figura, n_total, n_elim, n_cons))
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
# 9. PUNTO DE ENTRADA PRINCIPAL
# =============================================================================

cat("\nScript cargado. Construyendo catálogo de archivos...\n")
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
