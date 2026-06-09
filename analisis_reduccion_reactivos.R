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
# Los números de pregunta (Q1, Q2...) NO son estables entre ciclos:
#   - El mismo constructo tiene código distinto en 2424-2025 y 2025-2026
#   - En 2425 las opciones múltiples son sub-ítems (Q10_LAPTOP); en 2526
#     cada opción tiene su propio Q secuencial (Q14, Q15...)
#
# El crosswalk mapea código_2425 ↔ código_2526 por cuestionario/figura.
# Se genera automáticamente con similitud de texto y requiere validación experta.
#
# ARCHIVO: crosswalk_variables_2425_2526.xlsx (misma carpeta que este script)
# Columnas clave: hoja | code_2425 | code_2526 | tipo_match | validado

CROSSWALK_PATH <- file.path(
  dirname(sys.frame(1)$ofile %||% getwd()),
  "crosswalk_variables_2425_2526.xlsx"
)
# Fallback si no se detecta la ruta del script
if (!file.exists(CROSSWALK_PATH)) {
  CROSSWALK_PATH <- file.path(CONFIG$dir_datos, "..", "crosswalk_variables_2425_2526.xlsx")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Carga el crosswalk y devuelve tabla de mapeo para una hoja específica.
#' Filtra a ítems validados (validado == "OK" o vacío si aún no se revisó).
#'
#' @param hoja  Ej. "CTXT_DIR", "HD_DOC", "SXXI_EST"
#' @return data.frame con columnas code_2425, code_2526 (NA si sin equivalente)

cargar_crosswalk <- function(hoja) {
  if (!file.exists(CROSSWALK_PATH)) {
    warning("Crosswalk no encontrado: ", CROSSWALK_PATH,
            "\n  La comparación entre ciclos usará códigos crudos (puede ser incorrecta).")
    return(NULL)
  }
  cw <- tryCatch(
    openxlsx::read.xlsx(CROSSWALK_PATH, sheet = "CROSSWALK_COMPLETO",
                        na.strings = c("", "NA")),
    error = function(e) { warning("Error leyendo crosswalk: ", e$message); NULL }
  )
  if (is.null(cw)) return(NULL)

  # Normalizar nombre de hoja (espacios → guiones bajos)
  cw$hoja <- stringr::str_replace_all(cw$hoja, " ", "_")
  hoja    <- stringr::str_replace_all(hoja, " ", "_")

  sub_cw <- cw[cw$hoja == hoja, ]
  if (nrow(sub_cw) == 0) {
    warning("Hoja '", hoja, "' no encontrada en crosswalk.")
    return(NULL)
  }

  # Excluir filas marcadas como ELIMINAR y ítems sin match en ninguno de los dos ciclos
  excluir <- !is.na(sub_cw$validado) & sub_cw$validado == "ELIMINAR"
  sub_cw  <- sub_cw[!excluir, ]

  sub_cw[, c("code_2425", "code_2526", "tipo_match", "similitud")]
}

#' Renombra columnas de un data.frame usando el crosswalk para estandarizar
#' a nombres canónicos "CANON_Qxx" compartidos entre ciclos.
#'
#' Para cada par (code_2425, code_2526) se crea un nombre canónico basado
#' en el código 2526 (que es el ciclo de referencia más reciente).
#' Columnas sin equivalente en el otro ciclo conservan su código original.
#'
#' @param datos  data.frame con columnas nombradas por código de ciclo
#' @param ciclo  "2425" o "2526"
#' @param mapa   data.frame devuelto por cargar_crosswalk()
#' @return data.frame con columnas renombradas a nombres canónicos

aplicar_crosswalk <- function(datos, ciclo, mapa) {
  if (is.null(mapa)) return(datos)

  col_origen <- if (ciclo == "2425") "code_2425" else "code_2526"
  col_destino <- if (ciclo == "2425") "code_2526" else "code_2425"

  mapa_valido <- mapa[!is.na(mapa[[col_origen]]) & !is.na(mapa[[col_destino]]),]
  mapa_vec    <- setNames(mapa_valido[[col_destino]], mapa_valido[[col_origen]])

  nombres_nuevos <- names(datos)
  for (i in seq_along(nombres_nuevos)) {
    if (nombres_nuevos[i] %in% names(mapa_vec)) {
      nombres_nuevos[i] <- mapa_vec[[nombres_nuevos[i]]]
    }
  }
  names(datos) <- nombres_nuevos
  datos
}

# =============================================================================
# 3. CARGA Y ESTANDARIZACIÓN DE DATOS
# =============================================================================

#' Carga un archivo Excel de respuestas, estandariza nombres de columnas,
#' elimina columnas padre todo-NA (preguntas de opción múltiple contenedor),
#' y opcionalmente aplica el crosswalk para alinear con el otro ciclo.
#'
#' @param ruta       Ruta completa al archivo .xlsx
#' @param ciclo      "2425" o "2526"
#' @param mapa_cw    data.frame de cargar_crosswalk() o NULL para omitir
#' @return data.frame con columnas Qn/Qn_SUFIJO estandarizadas

cargar_base <- function(ruta, ciclo, mapa_cw = NULL) {
  dat <- tryCatch(
    readxl::read_excel(ruta, na = c("", "NA", "N/A")),
    error = function(e) {
      warning("No se pudo leer: ", ruta, "\n  ", e$message); return(NULL)
    }
  )
  if (is.null(dat)) return(NULL)

  # Estandarizar nombres para archivos del ciclo 2425.
  # Estructura observada en los archivos exportados:
  #   PRE_CTXT_PMF_Q45        → ítem simple     → Q45
  #   POS_CTXT_DIR_Q10        → padre opción múlt. (todo NA) → descartar
  #   POS_CTXT_DIR_Q10_LAPTOP → sub-ítem binario → Q10_LAPTOP
  # La misma lógica aplica a ambos prefijos (PRE_ y POS_).
  if (ciclo == "2425") {
    # Extraer todo lo que viene desde la primera Q seguida de dígito
    extraido <- stringr::str_extract(names(dat), "Q\\d+.*$")
    # Limpiar guiones/espacios residuales
    extraido <- stringr::str_replace_all(extraido, "[^A-Za-z0-9_]", "_")
    names(dat) <- ifelse(is.na(extraido),
                         paste0("VAR_", seq_along(extraido)),
                         extraido)
  }

  # Forzar numérico en todas las columnas Q (antes de filtrar,
  # para poder detectar columnas todo-NA provenientes de padres de opción múltiple)
  cols_q_idx <- stringr::str_detect(names(dat), "^Q\\d+")
  dat_q <- dat[, cols_q_idx, drop = FALSE]
  dat_q <- as.data.frame(lapply(dat_q, function(x) suppressWarnings(as.numeric(as.character(x)))))

  # Eliminar columnas donde el 100% son NA:
  # Corresponden a preguntas "padre" de opción múltiple cuya data real
  # está en los sub-ítems (Q10_LAPTOP, Q10_AUTO, etc.)
  todo_na <- colMeans(is.na(dat_q)) == 1
  if (any(todo_na)) {
    cat(sprintf("  [INFO] Columnas padre descartadas (100%% NA): %s\n",
                paste(names(dat_q)[todo_na], collapse = ", ")))
    dat_q <- dat_q[, !todo_na, drop = FALSE]
  }

  if (ncol(dat_q) == 0) {
    warning("Sin columnas con datos en: ", ruta); return(NULL)
  }

  # Aplicar crosswalk: renombrar códigos del ciclo al esquema canónico del otro
  # (solo cuando se proporciona mapa_cw)
  if (!is.null(mapa_cw)) {
    n_antes <- ncol(dat_q)
    dat_q   <- aplicar_crosswalk(dat_q, ciclo, mapa_cw)
    n_mapeados <- sum(names(dat_q) != names(dat_q))  # columnas renombradas
    cat(sprintf("  [CROSSWALK] %d/%d columnas renombradas al esquema canónico\n",
                sum(names(dat_q) != names(dat_q[, seq_len(n_antes)])), n_antes))
  }

  dat_q
}

#' Construye el catálogo de archivos disponibles para los dos ciclos.
#'
#' Nomenclatura esperada:
#'   2425/  CUESTIONARIO_FIGURA_momento.xlsx   (p.ej. HSXXI_EST_pre.xlsx)
#'   2526/  CUESTIONARIO_FIGURA_momento.xlsx

catalogo_archivos <- function() {
  leer_dir <- function(dir_ciclo, ciclo_etiq) {
    archivos <- list.files(dir_ciclo, pattern = "\\.xlsx$",
                           full.names = TRUE, ignore.case = TRUE)
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

  # Ítems cuyo máximo observado es NA (todo NA) o 0 (sin variabilidad)
  # se marcan como DATOS_INSUFICIENTES en diagnostico_NA; no crashear aquí.
  problemas_max <- is.na(max_por_item) | max_por_item <= 0
  if (any(problemas_max)) {
    cat(sprintf("  [AVISO] %d ítems con max NA o ≤0 (todo-NA o sin variabilidad): %s\n",
                sum(problemas_max),
                paste(names(max_por_item)[problemas_max], collapse = ", ")))
    # Asignar max=1 provisional para no bloquear el flujo; serán descartados
    # en la limpieza por NAs antes de los modelos.
    max_por_item[problemas_max] <- 1
  }

  # Para mirt: máximo único = el mayor de todos los ítems válidos
  n_cat <- max(max_por_item, na.rm = TRUE) + 1

  cat(sprintf("  [INFO] Máximos por ítem (únicos): %s\n",
              paste(sort(unique(max_por_item)), collapse = ", ")))

  # Remover ítems con >20% NA antes de modelos
  diag_na <- diagnostico_NA(datos, etiqueta)
  items_ok <- diag_na$Item[diag_na$flag_NA == "OK"]
  datos_limpios <- datos[, items_ok, drop = FALSE]

  tabla <- data.frame(
    Item        = names(datos),
    max_escala  = max_por_item[names(datos)],  # máximo teórico de cada ítem (0-3 o 0-4)
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

comparar_ciclos <- function(tabla_A, tabla_B, ciclo_A = "2425", ciclo_B = "2526") {

  sufA <- paste0("_", ciclo_A)
  sufB <- paste0("_", ciclo_B)

  indicadores_clave <- c("pct_NA", "p_dificultad", "ritc", "alpha_sin_item",
                         "b_Rasch", "infit_MNSQ", "outfit_MNSQ",
                         "a_GRM", "info_theta0_GRM", "comunalidad_EFA",
                         "estabilidad_EGA", "votos_eliminacion", "decision_final")

  cols_A <- intersect(indicadores_clave, names(tabla_A))
  cols_B <- intersect(indicadores_clave, names(tabla_B))

  df_A <- tabla_A[, c("Item", cols_A)]
  df_B <- tabla_B[, c("Item", cols_B)]

  names(df_A)[-1] <- paste0(names(df_A)[-1], sufA)
  names(df_B)[-1] <- paste0(names(df_B)[-1], sufB)

  comp <- dplyr::full_join(df_A, df_B, by = "Item")

  # Semáforo de consistencia
  dec_A_col <- paste0("decision_final", sufA)
  dec_B_col <- paste0("decision_final", sufB)

  comp$consistencia <- dplyr::case_when(
    is.na(comp[[dec_A_col]]) | is.na(comp[[dec_B_col]]) ~ "Solo un ciclo",
    comp[[dec_A_col]] == comp[[dec_B_col]] &
      comp[[dec_A_col]] == "ELIMINAR"    ~ "Ambos: ELIMINAR",
    comp[[dec_A_col]] == comp[[dec_B_col]] &
      comp[[dec_A_col]] == "Conservar"   ~ "Ambos: Conservar",
    TRUE                                 ~ "Discordante"
  )

  comp$decision_recomendada <- dplyr::case_when(
    comp$consistencia == "Ambos: ELIMINAR"  ~ "ELIMINAR",
    comp$consistencia == "Ambos: Conservar" ~ "Conservar",
    comp$consistencia == "Discordante"      ~ "REVISAR (ciclos discordantes)",
    TRUE                                    ~ "REVISAR (solo un ciclo)"
  )

  comp
}

# =============================================================================
# 6. EXPORTACIÓN A EXCEL CON FORMATO
# =============================================================================

COLORES <- list(
  eliminar   = list(fg = "#FFCCCC", font = "#CC0000"),
  conservar  = list(fg = "#CCFFCC", font = "#006600"),
  revisar    = list(fg = "#FFF3CC", font = "#856404"),
  insuf      = list(fg = "#E0E0E0", font = "#555555"),
  header     = list(fg = "#1F3864", font = "#FFFFFF")
)

estilo_decision <- function(wb, hoja, tabla, col_decision) {
  for (tipo in c("ELIMINAR", "Conservar", "REVISAR.*", "DATOS_INSUF.*")) {
    patron <- tipo
    col_def <- switch(
      gsub("\\.", "", tipo),
      "ELIMINAR"     = COLORES$eliminar,
      "Conservar"    = COLORES$conservar,
      "REVISAR"      = COLORES$revisar,
      "DATOSINSUF"   = COLORES$insuf,
      COLORES$revisar
    )
    filas <- which(grepl(patron, tabla[[col_decision]])) + 1
    if (length(filas) == 0) next
    estilo <- openxlsx::createStyle(
      fgFill       = col_def$fg,
      fontColour   = col_def$font,
      textDecoration = "bold"
    )
    openxlsx::addStyle(wb, hoja, style = estilo,
                       rows = filas, cols = 1:ncol(tabla),
                       gridExpand = TRUE)
  }
}

agregar_hoja_tabla <- function(wb, nombre_hoja, tabla, col_decision = "decision_final") {
  nombre_hoja <- substr(nombre_hoja, 1, 31)  # Excel: máx 31 chars
  openxlsx::addWorksheet(wb, nombre_hoja)
  openxlsx::writeData(wb, nombre_hoja, tabla)

  # Encabezado
  hdr_estilo <- openxlsx::createStyle(
    fgFill = COLORES$header$fg, fontColour = COLORES$header$font,
    textDecoration = "bold", halign = "center"
  )
  openxlsx::addStyle(wb, nombre_hoja, hdr_estilo,
                     rows = 1, cols = 1:ncol(tabla), gridExpand = TRUE)
  openxlsx::setColWidths(wb, nombre_hoja, cols = 1:ncol(tabla), widths = "auto")

  if (col_decision %in% names(tabla)) {
    estilo_decision(wb, nombre_hoja, tabla, col_decision)
  }
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
    n_match <- sum(!is.na(mapa_cw$code_2425) & !is.na(mapa_cw$code_2526))
    cat(sprintf("  [CROSSWALK] Hoja '%s': %d correspondencias cargadas\n",
                hoja_cw, n_match))
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
