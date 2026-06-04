# ==============================================================================
# FASE 6 — ANÁLISIS MULTINIVEL (MLM / HLM)
# ==============================================================================
# ESTRUCTURA JERÁRQUICA:
#   Modelo Estudiantes: alumno (L1) → escuela/CCT (L2)
#   Modelo Docentes:    docente (L1) → escuela/CCT (L2)
#
# VARIABLE DEPENDIENTE: PMP global del instrumento (0-100%) calculado en FASE 4
#   HD_DOC   → PMP Habilidades Digitales Docentes
#   HD_EST   → PMP Habilidades Digitales Estudiantes
#   HI_DOC   → PMP Habilidades de Implementación Docentes
#   HSXXI_DOC→ PMP Habilidades Siglo XXI Docentes (Likert + Dicotómico)
#   HSXXI_EST→ PMP Habilidades Siglo XXI Estudiantes (Likert + Dicotómico)
#
# SECUENCIA DE MODELOS (para cada instrumento):
#   M0: Modelo nulo         — estima ICC, justifica MLM
#   M1: Predictores L1      — variables del individuo (centradas CWC)
#   M2: Predictores L1 + L2 — añade contexto escolar (centradas CGM)
#   M3: + Tiempo (momento)  — efecto de la intervención PRE→POST
#   M4: Pendientes aleatorias— ¿varía el efecto entre escuelas?
#   M5: Interacciones cruzadas — ¿moderadores del cambio?
#
# SELECCIÓN DEL MEJOR MODELO: LRT (χ²) + AIC + BIC
# TAMAÑO DE EFECTO: d de Cohen sobre la diferencia PRE-POST (Feingold,2009)
# R²: Snijders & Bosker (1994) para L1 y L2
# ==============================================================================

# ---------------------------------------------------------------------------- #
# 0. PAQUETES
# ---------------------------------------------------------------------------- #
pkgs <- c("readxl","writexl","openxlsx","dplyr","tidyr","stringr","tibble",
          "ggplot2","scales","RColorBrewer","patchwork","ggrepel",
          "lme4","lmerTest","performance","effectsize","broom.mixed",
          "emmeans","MuMIn","insight")
for (p in pkgs) if (!requireNamespace(p,quietly=TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl);   library(writexl);  library(openxlsx)
  library(dplyr);    library(tidyr);    library(stringr); library(tibble)
  library(ggplot2);  library(scales);   library(RColorBrewer); library(patchwork)
  library(ggrepel)
  library(lme4);     library(lmerTest); library(performance)
  library(effectsize); library(broom.mixed)
  library(emmeans);  library(MuMIn); library(insight)
})
set.seed(42)

theme_f6 <- theme_minimal(base_size=11) +
  theme(plot.title=element_text(size=12,face="bold",hjust=0,margin=margin(b=4)),
        plot.subtitle=element_text(size=9,color="#555555"),
        plot.caption=element_text(size=7.5,color="#888888",hjust=1),
        plot.background=element_rect(fill="white",color=NA),
        panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold",size=9),
        legend.position="bottom")
theme_set(theme_f6)

COL_PRE  <- "#1A3A6C"
COL_POST <- "#F47B20"
COL_MOM  <- c(PRE=COL_PRE, POST=COL_POST)

seg <- function(e) tryCatch(e, error=function(x) NULL)
guardar_g <- function(g,path,w=10,h=6)
  if (!is.null(g)) tryCatch(ggsave(path,g,width=w,height=h,dpi=150,bg="white"),
                             error=function(x) NULL)

# ---------------------------------------------------------------------------- #
# 1. RUTAS  ← ADAPTAR
# ---------------------------------------------------------------------------- #
ruta_pre  <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/00_DATOS/Cuestionarios_independientes/"
ruta_post <- ruta_pre   # PRE y POST en la misma carpeta; cambiar si están separados
ruta_f4   <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/04_PMP/"       # PMP calculados
ruta_f5   <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/05_MULTINIVEL/"  # vars seleccionadas
ruta_sal  <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/06_MULTINIVEL/"
for (d in c("","TABLAS","GRAFICOS","MODELOS","POR_NIVEL","POR_ENTIDAD","POR_CCT"))
  dir.create(file.path(ruta_sal,d), showWarnings=FALSE, recursive=TRUE)

# ---------------------------------------------------------------------------- #
# 2. VALORES NA Y RECODIFICACIÓN
# ---------------------------------------------------------------------------- #
NA_VALS <- c("No lo sé/Prefiero no contestar","No lo sé/ Prefiero no contestar",
             "No lo sé / Prefiero no contestar","No lo sé",
             "Prefiero no contestar","No aplica","NA","")
limpiar <- function(x) { x[trimws(as.character(x)) %in% NA_VALS] <- NA; x }

COD_ALL <- list(
  acuerdo5    = c("Totalmente en desacuerdo"=0,"Algo en desacuerdo"=1,
                  "Ni de acuerdo ni en desacuerdo"=2,"Algo de acuerdo"=3,
                  "Totalmente de acuerdo"=4),
  frecuencia5 = c("Nunca"=0,"Rara vez"=1,"Algunas veces"=2,"Frecuentemente"=3,"Siempre"=4),
  frecuencia4 = c("Nunca"=0,"Pocas veces"=1,"Muchas veces"=2,"Siempre"=3),
  hi_freq5    = c("Casi nunca"=0,"Algunas veces durante el semestre/año"=1,
                  "1 a 3 veces al mes"=2,"1 a 3 veces por semana"=3,
                  "Casi todos los días"=4),
  receptor5   = c("Nada receptivos"=0,"Poco receptivos"=1,"Indiferentes"=2,
                  "Medianamente receptivos"=3,"Muy receptivos"=4),
  binaria     = c("No"=0,"Sí"=1),
  conocimiento4 = c("No sé / Nunca he oído hablar de esto"=0,"Conozco un poco el tema"=1,
                    "Sí, conozco bien este tema"=2,
                    "Totalmente, e incluso podría explicárselo a otras personas"=3),
  habilidad4  = c("No sé cómo hacerlo"=0,"Puedo hacerlo con ayuda"=1,
                  "Puedo hacerlo por mi cuenta"=2,
                  "Puedo hacerlo con confianza y si es necesario puedo ayudar a otras personas"=3)
)
recodificar <- function(vec) {
  v <- limpiar(as.character(vec))
  for (d in COD_ALL) {
    n_v <- sum(!is.na(v)); if (n_v==0) next
    if (sum(v[!is.na(v)] %in% names(d))/n_v >= 0.60) return(as.numeric(d[v]))
  }
  num <- suppressWarnings(as.numeric(v))
  if (mean(!is.na(num),na.rm=TRUE) >= 0.50) return(num)
  rep(NA_real_, length(v))
}

# ---------------------------------------------------------------------------- #
# 3. FUNCIÓN PMP (para calcular la variable dependiente in situ)
# ---------------------------------------------------------------------------- #
detect_cat_max <- function(vec) {
  v <- na.omit(suppressWarnings(as.numeric(vec)))
  if (length(v)==0) return(NA_integer_)
  mv <- max(v); if (mv<=1 && length(unique(v))<=2) return(1L)
  if (mv<=3) return(3L); return(4L)
}

calcular_pmp_individuo <- function(df, items, subscalas=NULL) {
  # PMP individual = promedio de (media_item_individuo / cat_max_item) × 100
  # Para cada persona: media de sus respuestas en cada ítem, normalizada
  items_ok <- intersect(items, names(df))
  cat_maxs  <- sapply(items_ok, function(c) detect_cat_max(df[[c]]))
  pmp_mat   <- mapply(function(col, cm) {
    v <- suppressWarnings(as.numeric(df[[col]]))
    if (is.na(cm)||cm==0) return(rep(NA_real_,nrow(df)))
    round(v/cm*100, 2)
  }, items_ok, cat_maxs, SIMPLIFY=TRUE)
  if (is.null(dim(pmp_mat))) return(rep(NA_real_,nrow(df)))
  round(rowMeans(pmp_mat, na.rm=TRUE), 2)
}

# ---------------------------------------------------------------------------- #
# 3b. EXTRAER GRADO DESDE FOLIO (estructura del proyecto)
# ---------------------------------------------------------------------------- #
# Formato del Folio: [CCT_num][Nivel_letra][Grado][Grupo_letra][Alumno_num]
# Ejemplos: 08P4A02 → Primaria Grado 4
#           03S1D04 → Secundaria Grado 1
#           01B1D02 → Bachillerato Grado 1
#
# El grado es el dígito INMEDIATAMENTE después de la letra de nivel (P/S/B)
# Se combina con Nivel para obtener un identificador único de grupo escolar:
#   "P_4" = Cuarto de primaria | "S_1" = Primero de secundaria | "B_1" = 1er año bachillerato
#
# Esta variable se usa como L2 en el modelo de 3 niveles de estudiantes:
#   L1 = alumno | L2 = grado dentro de escuela | L3 = escuela (CCT)

extraer_grado_folio <- function(folio_vec) {
  # Patrón: dígitos iniciales + letra nivel (P/S/B) + dígito de grado + ...
  grado_num <- regmatches(as.character(folio_vec),
                           regexpr("(?<=[PSB])[1-6]", as.character(folio_vec),
                                   perl=TRUE))
  # Si hay match, devolver el dígito; si no, NA
  result <- rep(NA_character_, length(folio_vec))
  matches <- regexpr("(?<=[PSB])[1-6]", as.character(folio_vec), perl=TRUE)
  result[matches > 0] <- regmatches(as.character(folio_vec), matches)
  result
}

# Construir etiqueta completa del grupo escolar (nivel + grado)
construir_grado_completo <- function(folio_vec, nivel_vec) {
  grado_raw <- extraer_grado_folio(folio_vec)
  nivel_lbl <- case_when(
    trimws(as.character(nivel_vec)) %in% c("P","Primaria","primaria")         ~ "P",
    trimws(as.character(nivel_vec)) %in% c("S","Secundaria","secundaria")     ~ "S",
    trimws(as.character(nivel_vec)) %in% c("B","Bachillerato","bachillerato",
                                            "Preparatoria","preparatoria")    ~ "B",
    TRUE ~ as.character(nivel_vec)
  )
  ifelse(!is.na(grado_raw) & !is.na(nivel_lbl),
         paste0(nivel_lbl, "_", grado_raw),
         NA_character_)
}

# ---------------------------------------------------------------------------- #
# 4. FUNCIONES ESTADÍSTICAS
# ---------------------------------------------------------------------------- #

# ── ICC desde modelo nulo (2 o 3 niveles) ────────────────────────────────────
# Docentes: 2 niveles → ICC_escuela
# Estudiantes: 3 niveles → ICC_escuela + ICC_grado_dentro_escuela
calcular_icc_modelo <- function(modelo) {
  if (is.null(modelo)) return(list(icc_escuela=NA, icc_grado=NA, var_total=NA))
  vc <- as.data.frame(VarCorr(modelo))
  var_res   <- vc$vcov[vc$grp=="Residual"]
  var_niv   <- vc[vc$grp!="Residual", , drop=FALSE]
  var_total <- sum(vc$vcov)
  if (is.na(var_total)||var_total==0)
    return(list(icc_escuela=NA, icc_grado=NA, var_total=NA))

  # Identificar componentes por nombre del grupo aleatorio
  # Nivel 3 (escuela): grp = "CCT"
  # Nivel 2 (grado dentro de escuela): grp = "CCT:Nivel" o "Nivel:CCT"
  grp_cct  <- var_niv[grepl("^CCT$", var_niv$grp), "vcov"]
  grp_grad <- var_niv[grepl(":", var_niv$grp), "vcov"]

  # Si solo hay un nivel de agrupación (docentes)
  if (length(grp_cct)==0 && nrow(var_niv)>=1)
    grp_cct <- var_niv$vcov[1]

  icc_escuela <- if (length(grp_cct)>0) round(grp_cct[1]/var_total, 4) else NA
  icc_grado   <- if (length(grp_grad)>0) round(grp_grad[1]/var_total, 4) else NA

  list(icc_escuela = icc_escuela,
       icc_grado   = icc_grado,
       var_escuela = if(length(grp_cct)>0)  round(grp_cct[1],3)  else NA,
       var_grado   = if(length(grp_grad)>0) round(grp_grad[1],3) else NA,
       var_res     = round(var_res,3),
       var_total   = round(var_total,3))
}

# ── R² Nakagawa & Schielzeth (2013) via MuMIn::r.squaredGLMM ─────────────────
# R²m (marginal)    = varianza explicada por efectos FIJOS (predictores)
# R²c (conditional) = varianza explicada por efectos fijos + aleatorios
# Funciona igual para modelos 2 y 3 niveles; no requiere que nulo y completo
# estén ajustados con el mismo método (REML vs ML) — evita el sesgo de Snijders
r2_multinivel <- function(modelo_completo, modelo_nulo=NULL) {
  if (is.null(modelo_completo)) return(list(r2_l1=NA_real_, r2_l2=NA_real_))
  tryCatch({
    r2 <- MuMIn::r.squaredGLMM(modelo_completo)
    # r.squaredGLMM puede devolver varias filas (métodos); tomar la primera
    list(r2_l1 = round(r2[1, "R2m"], 4),   # marginal  (efectos fijos)
         r2_l2 = round(r2[1, "R2c"], 4))    # conditional (fijos + aleatorios)
  }, error=function(e) list(r2_l1=NA_real_, r2_l2=NA_real_))
}

# ── Tabla de efectos fijos formateada ────────────────────────────────────────
tabla_efectos_fijos <- function(modelo, nombre_modelo) {
  if (is.null(modelo)) return(NULL)
  ct <- tryCatch(as.data.frame(coef(summary(modelo))), error=function(e)NULL)
  if (is.null(ct)) return(NULL)
  ct$predictor   <- rownames(ct)
  ct$modelo      <- nombre_modelo
  ct$coef        <- round(ct[,1], 3)
  ct$se          <- round(ct[,2], 3)
  # IC 95% (Wald)
  ct$ic_inf      <- round(ct$coef - 1.96*ct$se, 3)
  ct$ic_sup      <- round(ct$coef + 1.96*ct$se, 3)
  # p-valor (de lmerTest si disponible, si no de t)
  p_col <- grep("Pr|p.value|p-value", names(ct), ignore.case=TRUE, value=TRUE)[1]
  if (!is.na(p_col)) ct$p_valor <- round(ct[[p_col]], 4)
  else ct$p_valor <- round(2*(1-pnorm(abs(ct$coef/ct$se))), 4)
  ct$sig <- case_when(ct$p_valor<.001~"***",ct$p_valor<.01~"**",
                      ct$p_valor<.05~"*",ct$p_valor<.10~".",TRUE~"")
  ct[, c("modelo","predictor","coef","se","ic_inf","ic_sup","p_valor","sig")]
}

# ── Tabla de componentes de varianza ─────────────────────────────────────────
tabla_varianza <- function(modelo, nombre_modelo) {
  if (is.null(modelo)) return(NULL)
  vc <- as.data.frame(VarCorr(modelo)) %>%
    mutate(modelo=nombre_modelo, sd=round(sqrt(vcov),3), vcov=round(vcov,3))
  vc[, c("modelo","grp","var1","vcov","sd")]
}

# ── Comparación de modelos ────────────────────────────────────────────────────
comparar_modelos <- function(lista_modelos) {
  lista_ok <- Filter(Negate(is.null), lista_modelos)
  if (length(lista_ok) < 2) return(NULL)
  tryCatch({
    comp <- do.call(anova, lista_ok)
    df_comp <- as.data.frame(comp)
    df_comp$modelo <- names(lista_ok)
    df_comp$AIC    <- round(df_comp$AIC, 1)
    df_comp$BIC    <- round(df_comp$BIC, 1)
    df_comp
  }, error=function(e) NULL)
}

# ── Cohen's d para cambio PRE-POST ───────────────────────────────────────────
cohens_d_mlm <- function(pmp_pre, pmp_post) {
  # d_z Feingold (2009): mean(POST-PRE) / SD(POST-PRE) — sobre pares observados
  dif <- pmp_post - pmp_pre
  dif <- dif[!is.na(dif)]
  if (length(dif)<5) return(NA_real_)
  round(mean(dif)/sd(dif), 3)
}

cohens_d_ajustado <- function(modelo_tiempo, modelo_nulo) {
  # d ajustada (Hedges 2007; Pustejovsky & Tipton 2021):
  #   sigma = sqrt(var_total_nulo) = sqrt(sum de todos los componentes de varianza)
  #   Usa varianza TOTAL incondicional para no inflar la d ignorando la
  #   varianza entre escuelas (L2/L3). Delta viene de las medias marginales del modelo.
  if (is.null(modelo_tiempo) || is.null(modelo_nulo)) return(NA_real_)
  tryCatch({
    # Varianza total: suma de todos los componentes (VarCorr del modelo nulo)
    vc        <- as.data.frame(VarCorr(modelo_nulo))
    var_total <- sum(vc$vcov, na.rm=TRUE)
    if (is.na(var_total) || var_total <= 0) return(NA_real_)

    # Delta estimado por el modelo con momento (medias marginales PRE=0, POST=1)
    em    <- as.data.frame(emmeans(modelo_tiempo, ~momento,
                                   at=list(momento=c(0,1))))
    delta <- em$emmean[em$momento == 1] - em$emmean[em$momento == 0]
    if (length(delta) == 0 || is.na(delta)) return(NA_real_)

    round(delta / sqrt(var_total), 3)
  }, error=function(e) NA_real_)
}

# ── BLUPs (efectos aleatorios) por CCT ───────────────────────────────────────
extraer_blups <- function(modelo, nivel_grupo="CCT") {
  if (is.null(modelo)) return(NULL)
  re <- tryCatch(ranef(modelo)[[nivel_grupo]], error=function(e)NULL)
  if (is.null(re)) return(NULL)
  df <- as.data.frame(re)
  df$CCT <- rownames(re)
  df$intercepto_random <- round(df[["(Intercept)"]],3)
  # pendiente random (tiempo) si existe
  if ("momento" %in% names(df)) df$pendiente_random <- round(df$momento,3)
  df
}

# ---------------------------------------------------------------------------- #
# 5. CONFIGURACIÓN DE INSTRUMENTOS
# ---------------------------------------------------------------------------- #
# Variables seleccionadas en FASE 5 (solo las marcadas como INCLUIR)
# Se organizan por nivel para cada modelo

VARS_L1_EST <- c(
  # CTX_EST (individuo estudiante) — centrado CWC
  "Q17","Q18","Q19","Q38","Q40","Q41","Q42","Q43","Q46","Q49"
)
VARS_L1_DOC <- c(
  # CTX_DOC (individuo docente) — centrado CWC
  "Q9","Q12","Q17","Q20","Q24","Q44","Q45","Q46","Q47","Q48","Q49","Q50","Q51"
)
VARS_L2_EST_DIR <- c(
  # CTX_DIR agregado a escuela → modelo estudiantes — centrado CGM
  "Q32","Q37","Q38","Q39","Q40","Q42","Q43","Q65","Q66"
)
VARS_L2_EST_DOC_AGG <- c(
  # CTX_DOC agregado a escuela → modelo estudiantes — media escolar
  "Q49","Q50","Q54"
)
VARS_L2_DOC_DIR <- c(
  # CTX_DIR → modelo docentes — centrado CGM
  "Q32","Q37","Q38","Q39","Q40","Q42","Q65","Q66"
)

INSTRUMENTOS <- list(
  HD_DOC = list(
    figura="Docente",    tipo_modelo="docentes",
    archivo_pre="HD_DOC_pre.xlsx", archivo_post="HD_DOC_post.xlsx",
    folio="Folio", cct="CCT", nivel=NULL,
    items=paste0("Q",9:90),
    vars_l1=VARS_L1_DOC, vars_l2=VARS_L2_DOC_DIR),
  HD_EST = list(
    figura="Estudiante", tipo_modelo="estudiantes",
    archivo_pre="HD_EST_pre.xlsx", archivo_post="HD_EST_post.xlsx",
    folio="Folio", cct="CCT", nivel="Nivel",
    # nivel_anidamiento: col que forma el L2 dentro de escuela
    # HD_EST tiene "Nivel" (Primaria/Secundaria/Prepa); Grado se une desde CTX_EST
    nivel_anidamiento="Nivel",
    items=paste0("Q",16:97),
    vars_l1=VARS_L1_EST, vars_l2=VARS_L2_EST_DIR),
  HI_DOC = list(
    figura="Docente",    tipo_modelo="docentes",
    archivo_pre="HI_DOC_pre.xlsx", archivo_post="HI_DOC_post.xlsx",
    folio="Folio", cct="CCT", nivel=NULL,
    items=paste0("Q",8:49),
    vars_l1=VARS_L1_DOC, vars_l2=VARS_L2_DOC_DIR),
  HSXXI_DOC = list(
    figura="Docente",    tipo_modelo="docentes",
    archivo_pre="HSXXI_DOC_pre.xlsx", archivo_post="HSXXI_DOC_post.xlsx",
    folio="Folio", cct="CCT", nivel=NULL,
    items=paste0("Q",12:56),
    vars_l1=VARS_L1_DOC, vars_l2=VARS_L2_DOC_DIR),
  HSXXI_EST = list(
    figura="Estudiante", tipo_modelo="estudiantes",
    archivo_pre="HSXXI_EST_pre.xlsx", archivo_post="HSXXI_EST_post.xlsx",
    folio="Folio", cct="CCT", nivel="Nivel",
    nivel_anidamiento="Nivel",
    items=paste0("Q",16:60),
    vars_l1=VARS_L1_EST, vars_l2=VARS_L2_EST_DIR)
)

# CTX_DIR (para variables L2 de contexto escolar)
CTX_DIR_VARS <- c("CCT","Q32","Q37","Q38","Q39","Q40","Q42","Q43","Q65","Q66","Nivel")

# ---------------------------------------------------------------------------- #
# 6. FUNCIÓN AUXILIAR: CENTRADO DE VARIABLES
# ---------------------------------------------------------------------------- #

centrar_cwc <- function(df, cols, grupo_col) {
  # CWC: x_ij - x_barra_j  (centrado en la media del grupo)
  df <- df %>% group_by(.data[[grupo_col]]) %>%
    mutate(across(all_of(intersect(cols,names(.))),
                  ~. - mean(.,na.rm=TRUE),
                  .names="{.col}_cwc")) %>% ungroup()
  df
}

centrar_cgm <- function(df, cols) {
  # CGM: x_j - x_barra  (centrado en la gran media)
  for (col in intersect(cols, names(df))) {
    gm <- mean(df[[col]], na.rm=TRUE)
    df[[paste0(col,"_cgm")]] <- round(df[[col]] - gm, 3)
  }
  df
}

# ---------------------------------------------------------------------------- #
# 7. FUNCIÓN MAESTRA: análisis MLM para un instrumento
# ---------------------------------------------------------------------------- #
analizar_mlm <- function(nm) {
  cfg <- INSTRUMENTOS[[nm]]
  cat("\n", strrep("═",65), "\n  MLM:", nm, "|", cfg$figura, "\n")

  dir_nm <- file.path(ruta_sal, nm)
  for (d in c(dir_nm, file.path(dir_nm, c("TABLAS","GRAFICOS","MODELOS"))))
    dir.create(d, showWarnings=FALSE, recursive=TRUE)

  # ── Leer datos PRE y POST ─────────────────────────────────────────────────
  rp <- paste0(ruta_pre,  cfg$archivo_pre)
  rq <- paste0(ruta_post, cfg$archivo_post)
  if (!file.exists(rp)) {
    cat(sprintf("  !! Sin PRE — archivo no encontrado: %s\n", rp))
    return(invisible(NULL))
  }

  df_pre  <- read_excel(rp)
  df_post <- if (file.exists(rq)) read_excel(rq) else NULL
  if (is.null(df_post)) cat("  Sin POST — análisis transversal solo PRE\n")

  # ── Calcular PMP individual ───────────────────────────────────────────────
  items_pre  <- intersect(cfg$items, names(df_pre))
  df_pre_num <- as.data.frame(lapply(df_pre[,items_pre,drop=FALSE], recodificar))
  df_pre$PMP <- calcular_pmp_individuo(df_pre_num, items_pre)

  if (!is.null(df_post)) {
    items_post  <- intersect(cfg$items, names(df_post))
    df_post_num <- as.data.frame(lapply(df_post[,items_post,drop=FALSE], recodificar))
    df_post$PMP <- calcular_pmp_individuo(df_post_num, items_post)
  }

  cat(sprintf("  PRE: n=%d | PMP_pre=%.1f%% (SD=%.1f)\n",
              nrow(df_pre), mean(df_pre$PMP,na.rm=TRUE), sd(df_pre$PMP,na.rm=TRUE)))
  if (!is.null(df_post))
    cat(sprintf("  POST: n=%d | PMP_post=%.1f%% (SD=%.1f)\n",
                nrow(df_post), mean(df_post$PMP,na.rm=TRUE), sd(df_post$PMP,na.rm=TRUE)))

  # ── Construir datos en formato LARGO (pareado por Folio) ─────────────────
  # Para el análisis longitudinal PRE-POST dentro del MLM
  meta_cols <- intersect(c(cfg$folio,cfg$cct,cfg$nivel,"Grado","Grupo","Turno",
                            "Entidad"), names(df_pre))
  df_long <- NULL
  if (!is.null(df_post)) {
    df_pre_long  <- df_pre  %>% select(all_of(c(meta_cols,"PMP"))) %>%
      mutate(momento=0L, momento_lbl="PRE")
    df_post_long <- df_post %>% select(all_of(intersect(c(meta_cols,"PMP"),names(df_post)))) %>%
      mutate(momento=1L, momento_lbl="POST")
    df_long <- bind_rows(df_pre_long, df_post_long) %>%
      mutate(Entidad=substr(as.character(.data[[cfg$cct]]),1,2))
  } else {
    # Sin POST: construir df_long solo con PRE para poder estimar null model e ICC
    df_long <- df_pre %>% select(all_of(c(meta_cols,"PMP"))) %>%
      mutate(momento=0L, momento_lbl="PRE",
             Entidad=substr(as.character(.data[[cfg$cct]]),1,2))
  }

  # Extraer grado desde Folio para estudiantes (L2 del modelo de 3 niveles)
  if (cfg$tipo_modelo == "estudiantes" && cfg$folio %in% names(df_long)) {
    df_long$Grado_Folio <- construir_grado_completo(
      df_long[[cfg$folio]],
      if(!is.null(cfg$nivel) && cfg$nivel %in% names(df_long))
        df_long[[cfg$nivel]] else rep(NA,nrow(df_long)))
    n_grados <- n_distinct(na.omit(df_long$Grado_Folio))
    cat(sprintf("  Grados extraídos del Folio: %d grupos únicos → %s\n",
                n_grados,
                paste(sort(unique(na.omit(df_long$Grado_Folio))),collapse=", ")))
  }
  cat(sprintf("  Formato largo: %d observaciones\n", nrow(df_long)))

  # ── Agregar variables L2 del CTX_DIR ─────────────────────────────────────
  ruta_dir <- paste0(ruta_pre,"CTX_DIR_pre.xlsx")
  df_dir_l2 <- NULL
  if (file.exists(ruta_dir)) {
    df_dir_raw <- read_excel(ruta_dir)
    cols_dir  <- intersect(CTX_DIR_VARS, names(df_dir_raw))
    df_dir    <- df_dir_raw[, cols_dir, drop=FALSE]
    # Recodificar ítems ordinales
    for (col in setdiff(cols_dir, c("CCT","Nivel")))
      df_dir[[col]] <- recodificar(df_dir[[col]])
    # Agregar por CCT (media escolar)
    df_dir_l2 <- df_dir %>%
      group_by(CCT) %>%
      summarise(across(where(is.numeric), ~round(mean(.,na.rm=TRUE),3)),
                .groups="drop") %>%
      centrar_cgm(., intersect(cfg$vars_l2, names(.)))
  }

  # ── Leer variables L1 del cuestionario de contexto ───────────────────────
  # CTX_DOC para modelos docentes; CTX_EST para modelos estudiantes
  archivo_ctx_l1 <- if (cfg$tipo_modelo == "docentes")
    paste0(ruta_pre, "CTX_DOC_pre.xlsx")
  else
    paste0(ruta_pre, "CTX_EST_pre.xlsx")

  df_ctx_l1 <- NULL
  if (file.exists(archivo_ctx_l1)) {
    df_ctx_raw <- read_excel(archivo_ctx_l1)
    # Recodificar solo las columnas de ítems seleccionadas como L1
    cols_l1_disp <- intersect(cfg$vars_l1, names(df_ctx_raw))
    if (length(cols_l1_disp) > 0 && cfg$folio %in% names(df_ctx_raw)) {
      for (col in cols_l1_disp)
        df_ctx_raw[[col]] <- recodificar(df_ctx_raw[[col]])
      df_ctx_l1 <- df_ctx_raw[, c(cfg$folio, cols_l1_disp), drop=FALSE]
      cat(sprintf("  CTX L1 (%s): %d vars, %d registros\n",
                  basename(archivo_ctx_l1), length(cols_l1_disp), nrow(df_ctx_l1)))
    }
  } else {
    cat(sprintf("  Sin archivo CTX L1: %s\n", basename(archivo_ctx_l1)))
  }

  # ── Unir L2 al dataset largo ──────────────────────────────────────────────
  df_mlm <- df_long
  if (!is.null(df_dir_l2) && !is.null(df_mlm)) {
    df_mlm <- left_join(df_mlm, df_dir_l2, by=setNames("CCT",cfg$cct),
                        suffix=c("","_l2"))
  }

  # ── Unir variables L1 de contexto al dataset largo ────────────────────────
  if (!is.null(df_ctx_l1) && !is.null(df_mlm) && cfg$folio %in% names(df_mlm)) {
    df_mlm <- left_join(df_mlm, df_ctx_l1, by=cfg$folio, suffix=c("","_ctx"))
    cat(sprintf("  L1 CTX unido: %d cols en df_mlm\n", ncol(df_mlm)))
  }

  # Centrado CWC para predictores L1 disponibles
  if (!is.null(df_mlm)) {
    vars_l1_disp <- intersect(cfg$vars_l1, names(df_mlm))
    if (length(vars_l1_disp)>0 && cfg$cct %in% names(df_mlm))
      df_mlm <- centrar_cwc(df_mlm, vars_l1_disp, cfg$cct)
  }

  cct_col   <- cfg$cct
  nivel_col <- cfg$nivel

  # ── ESTRUCTURA DE ANIDAMIENTO ─────────────────────────────────────────────
  # Docentes:    L1=docente    | L2=escuela (CCT)                [2 niveles]
  # Estudiantes: L1=alumno     | L2=Nivel/Grado dentro escuela
  #                            | L3=escuela (CCT)               [3 niveles]
  # Fórmula 3 niveles: (1|CCT/Nivel) = (1|CCT) + (1|CCT:Nivel)

  es_estudiante  <- cfg$tipo_modelo == "estudiantes"
  # Para estudiantes: usar Grado_Folio como L2 (grado dentro de escuela)
  # Grado_Folio = [Nivel_letra]_[dígito] → ej. "P_4", "S_1", "B_2"
  # Esto crea el anidamiento: alumno → grado/nivel → escuela (3 niveles)
  niv_anid_col  <- if (es_estudiante && "Grado_Folio" %in% names(df_mlm) &&
                        n_distinct(na.omit(df_mlm$Grado_Folio)) > 1)
    "Grado_Folio"
  else if (es_estudiante && !is.null(cfg$nivel_anidamiento) &&
            cfg$nivel_anidamiento %in% names(df_mlm) &&
            n_distinct(na.omit(df_mlm[[cfg$nivel_anidamiento]])) > 1)
    cfg$nivel_anidamiento   # fallback: usar Nivel si no hay Grado_Folio
  else NULL

  tiene_anid_l2  <- !is.null(niv_anid_col)

  cat(sprintf("  Tipo: %s | Niveles de anidamiento: %d\n",
              cfg$tipo_modelo, if(tiene_anid_l2) 3L else 2L))

  cat("  Ajustando modelos...\n")
  MODELOS <- list()

  tiene_post <- !is.null(df_post)
  usar_largo <- !is.null(df_mlm) && nrow(df_mlm)>30 &&
                "PMP" %in% names(df_mlm) && sum(!is.na(df_mlm$PMP))>30

  # ── M0: Modelo nulo — descomposición de varianza ─────────────────────────
  if (usar_largo) {
    if (tiene_anid_l2) {
      # Modelo nulo 3 niveles: alumno / nivel-escuela / escuela
      fmla_nulo <- as.formula(
        sprintf("PMP ~ 1 + (1|%s/%s)", cct_col, niv_anid_col))
      MODELOS$M0_nulo_3niv <- seg(lmer(fmla_nulo, data=df_mlm, REML=TRUE,
                                        control=lmerControl(optimizer="bobyqa")))
      # También 2 niveles para comparar
      MODELOS$M0_nulo_2niv <- seg(lmer(
        as.formula(sprintf("PMP ~ 1 + (1|%s)", cct_col)),
        data=df_mlm, REML=TRUE,
        control=lmerControl(optimizer="bobyqa")))
    } else {
      MODELOS$M0_nulo <- seg(lmer(
        as.formula(sprintf("PMP ~ 1 + (1|%s)", cct_col)),
        data=df_mlm, REML=TRUE,
        control=lmerControl(optimizer="bobyqa")))
    }
  }

  # Seleccionar modelo nulo de referencia
  modelo_nulo_ref <- if (tiene_anid_l2) MODELOS$M0_nulo_3niv else MODELOS$M0_nulo
  icc_res <- calcular_icc_modelo(modelo_nulo_ref)

  if (tiene_anid_l2) {
    cat(sprintf("  ICC escuela (L3): %.3f | ICC nivel-escuela (L2): %.3f\n",
                coalesce(icc_res$icc_escuela,0),
                coalesce(icc_res$icc_grado,0)))
  } else {
    cat(sprintf("  ICC escuela (L2): %.3f\n",
                coalesce(icc_res$icc_escuela,0)))
  }

  # ── M1: + Tiempo (PRE→POST) ─── solo si hay datos POST ─────────────────────
  if (usar_largo && tiene_post) {
    if (tiene_anid_l2) {
      fmla_m1 <- as.formula(
        sprintf("PMP ~ momento + (1|%s/%s)", cct_col, niv_anid_col))
      MODELOS$M1_tiempo <- seg(lmer(fmla_m1, data=df_mlm, REML=FALSE,
                                     control=lmerControl(optimizer="bobyqa")))
    } else {
      MODELOS$M1_tiempo <- seg(lmer(
        as.formula(sprintf("PMP ~ momento + (1|%s)", cct_col)),
        data=df_mlm, REML=FALSE,
        control=lmerControl(optimizer="bobyqa")))
    }
  }

  # ── M2: + Nivel educativo (efecto fijo del nivel, además del anidamiento) ─
  if (usar_largo && !is.null(nivel_col) && nivel_col %in% names(df_mlm)) {
    if (tiene_anid_l2) {
      fmla_m2 <- as.formula(
        sprintf("PMP ~ %s + (1|%s/%s)",
                if(tiene_post) paste0("momento + as.factor(",nivel_col,")")
                else paste0("as.factor(",nivel_col,")"),
                cct_col, niv_anid_col))
      MODELOS$M2_nivel <- seg(lmer(fmla_m2, data=df_mlm, REML=FALSE,
                                    control=lmerControl(optimizer="bobyqa")))
    } else {
      MODELOS$M2_nivel <- seg(lmer(
        as.formula(sprintf("PMP ~ %s + (1|%s)",
                           if(tiene_post) paste0("momento + as.factor(",nivel_col,")")
                           else paste0("as.factor(",nivel_col,")"),
                           cct_col)),
        data=df_mlm, REML=FALSE,
        control=lmerControl(optimizer="bobyqa")))
    }
  }

  # ── M3: + Predictores L2/L3 del director (contexto escolar) ───────────────
  vars_l2_cgm <- paste0(intersect(cfg$vars_l2, names(df_mlm)),"_cgm")
  vars_l2_cgm_ok <- intersect(vars_l2_cgm, names(df_mlm))
  vars_l2_cgm_ok <- vars_l2_cgm_ok[sapply(vars_l2_cgm_ok, function(v) {
    vv <- df_mlm[[v]]; sum(!is.na(vv))>20 && sd(vv,na.rm=TRUE)>0
  })]

  if (usar_largo && length(vars_l2_cgm_ok)>0) {
    pred_l2 <- paste(vars_l2_cgm_ok[seq_len(min(5,length(vars_l2_cgm_ok)))],
                     collapse="+")
    pred_fijo <- if (tiene_post) paste0("momento + ", pred_l2) else pred_l2
    if (tiene_anid_l2) {
      fmla_m3 <- as.formula(
        sprintf("PMP ~ %s + (1|%s/%s)", pred_fijo, cct_col, niv_anid_col))
    } else {
      fmla_m3 <- as.formula(
        sprintf("PMP ~ %s + (1|%s)", pred_fijo, cct_col))
    }
    MODELOS$M3_l2 <- seg(lmer(fmla_m3, data=df_mlm, REML=FALSE,
                                control=lmerControl(optimizer="bobyqa")))
  }

  # ── M4: Pendientes aleatorias del tiempo ── solo si hay POST ─────────────
  if (usar_largo && tiene_post && !is.null(MODELOS$M1_tiempo)) {
    n_l3 <- n_distinct(na.omit(df_mlm[[cct_col]]))
    if (n_l3 >= 10) {
      if (tiene_anid_l2) {
        fmla_m4 <- as.formula(
          sprintf("PMP ~ momento + (momento|%s) + (1|%s:%s)",
                  cct_col, cct_col, niv_anid_col))
      } else {
        fmla_m4 <- as.formula(
          sprintf("PMP ~ momento + (momento|%s)", cct_col))
      }
      MODELOS$M4_pendientes <- seg(lmer(fmla_m4, data=df_mlm, REML=FALSE,
                                         control=lmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e6))))
    }
  }

  # ── M5: Interacción cruzada tiempo × nivel educativo ── solo si hay POST ──
  if (usar_largo && tiene_post && !is.null(nivel_col) && nivel_col %in% names(df_mlm)) {
    if (tiene_anid_l2) {
      fmla_m5 <- as.formula(
        sprintf("PMP ~ momento * as.factor(%s) + (1|%s/%s)",
                nivel_col, cct_col, niv_anid_col))
    } else {
      fmla_m5 <- as.formula(
        sprintf("PMP ~ momento * as.factor(%s) + (1|%s)",
                nivel_col, cct_col))
    }
    MODELOS$M5_interaccion <- seg(lmer(fmla_m5, data=df_mlm, REML=FALSE,
                                        control=lmerControl(optimizer="bobyqa")))
  }

  # ── M5b: Interacción tiempo × infraestructura L2 ── solo si hay POST ──────
  if (usar_largo && tiene_post && "Q32_cgm" %in% names(df_mlm)) {
    if (tiene_anid_l2) {
      fmla_m5b <- as.formula(
        sprintf("PMP ~ momento * Q32_cgm + (1|%s/%s)", cct_col, niv_anid_col))
    } else {
      fmla_m5b <- as.formula(sprintf("PMP ~ momento * Q32_cgm + (1|%s)", cct_col))
    }
    MODELOS$M5b_interaccion_l2 <- seg(lmer(fmla_m5b, data=df_mlm, REML=FALSE,
                                            control=lmerControl(optimizer="bobyqa")))
  }

  cat(sprintf("  Modelos ajustados: %d\n", length(MODELOS)))

  # ── SELECCIÓN DEL MEJOR MODELO ────────────────────────────────────────────
  modelos_ok <- Filter(Negate(is.null), MODELOS)
  # Referencia al modelo nulo para comparaciones
  modelo_nulo_ref <- if(!is.null(MODELOS$M0_nulo_3niv)) MODELOS$M0_nulo_3niv
                     else if(!is.null(MODELOS$M0_nulo_2niv)) MODELOS$M0_nulo_2niv
                     else MODELOS$M0_nulo

  # Comparación solo entre modelos ML (excluir nulos REML para que anova() no mezcle)
  modelos_ml <- Filter(function(m) {
    tryCatch(!isREML(m), error=function(e) FALSE)
  }, modelos_ok)

  comp_modelos <- if (length(modelos_ml) >= 2) {
    tryCatch({
      do.call(anova, modelos_ml) %>% as.data.frame() %>%
        mutate(modelo=names(modelos_ml),
               AIC=round(AIC,1), BIC=round(BIC,1)) %>%
        select(modelo, everything())
    }, error=function(e) NULL)
  } else NULL

  # Seleccionar mejor modelo: por BIC entre modelos ML; si no hay, el más complejo
  mejor_modelo <- if (!is.null(comp_modelos) && nrow(comp_modelos) > 0) {
    nm_mejor <- comp_modelos$modelo[which.min(comp_modelos$BIC)]
    modelos_ml[[nm_mejor]]
  } else if (length(modelos_ml) > 0) {
    modelos_ml[[length(modelos_ml)]]   # más complejo disponible
  } else {
    modelo_nulo_ref   # fallback: solo hay modelo nulo
  }

  cat(sprintf("  Mejor modelo: %s\n",
              if(!is.null(comp_modelos) && nrow(comp_modelos)>0)
                comp_modelos$modelo[which.min(comp_modelos$BIC)]
              else if(length(modelos_ml)>0) names(modelos_ml)[length(modelos_ml)]
              else "M0_nulo (sin predictores)"))

  # ── EXTRAER RESULTADOS ────────────────────────────────────────────────────

  # Efectos fijos de todos los modelos
  ef_fijos <- do.call(rbind, lapply(names(modelos_ok),
    function(nm_m) tabla_efectos_fijos(modelos_ok[[nm_m]], nm_m)))

  # Varianza de todos los modelos
  varianzas <- do.call(rbind, lapply(names(modelos_ok),
    function(nm_m) tabla_varianza(modelos_ok[[nm_m]], nm_m)))

  # R² del mejor modelo vs nulo (Snijders & Bosker, 1994)
  # Solo tiene sentido cuando el mejor modelo tiene predictores adicionales al nulo
  modelo_nulo_r2 <- if(!is.null(MODELOS$M0_nulo_3niv)) MODELOS$M0_nulo_3niv
                   else if(!is.null(MODELOS$M0_nulo_2niv)) MODELOS$M0_nulo_2niv
                   else MODELOS$M0_nulo
  # R² solo tiene sentido cuando el mejor modelo tiene predictores (no solo intercepto)
  n_fe_mejor <- if(!is.null(mejor_modelo)) length(fixef(mejor_modelo)) else 0
  n_fe_nulo  <- if(!is.null(modelo_nulo_r2)) length(fixef(modelo_nulo_r2)) else 0
  r2_res <- if (n_fe_mejor > n_fe_nulo)
    r2_multinivel(mejor_modelo)
  else
    list(r2_l1=NA_real_, r2_l2=NA_real_)   # No aplica: mejor modelo = nulo

  # d de Cohen — dos versiones
  # d_cruda  (Feingold, 2009): sobre diferencias individuales observadas (PRE→POST pareado)
  # d_ajust  (multinivel):     delta estimado por modelo / SD residual del nulo
  d_cohen <- tryCatch({
    if (!is.null(df_long) && cfg$folio %in% names(df_long) && tiene_post) {
      df_paired <- df_long %>%
        filter(!is.na(PMP), momento_lbl %in% c("PRE","POST")) %>%
        select(all_of(c(cfg$folio,"momento_lbl","PMP"))) %>%
        pivot_wider(names_from=momento_lbl, values_from=PMP,
                    values_fn=mean) %>%
        filter(!is.na(PRE), !is.na(POST))
      if (nrow(df_paired) >= 5)
        cohens_d_mlm(df_paired$PRE, df_paired$POST)
      else NA_real_
    } else NA_real_
  }, error=function(e) NA_real_)

  d_cohen_aj <- cohens_d_ajustado(MODELOS$M1_tiempo, modelo_nulo_ref)

  # BLUPs por CCT
  blups_cct <- extraer_blups(mejor_modelo, cct_col)
  if (!is.null(blups_cct)) {
    blups_cct$Entidad <- substr(as.character(blups_cct$CCT), 1, 2)
    blups_cct$instrumento <- nm
  }

  # Medias marginales estimadas por nivel (si aplica)
  emmeans_nivel <- NULL
  if (!is.null(nivel_col) && !is.null(MODELOS$M2_nivel)) {
    emmeans_nivel <- tryCatch(
      as.data.frame(emmeans(MODELOS$M2_nivel,
                             specs=c("momento", nivel_col),
                             at=list(momento=c(0,1)))),
      error=function(e) NULL)
  }

  # ── GUARDAR TABLAS ────────────────────────────────────────────────────────
  cat("  Guardando tablas...\n")
  wb <- createWorkbook()
  add_ws <- function(nm_ws, dat) {
    if (!is.null(dat)&&nrow(dat)>0)
      tryCatch({addWorksheet(wb,nm_ws); writeData(wb,nm_ws,dat)},error=function(e)NULL)
  }

  # Resumen ejecutivo
  df_resumen_exec <- data.frame(
    instrumento    = nm,
    figura         = cfg$figura,
    niveles_anid   = if(tiene_anid_l2) 3L else 2L,
    N_pre          = nrow(df_pre),
    N_post         = if(!is.null(df_post)) nrow(df_post) else NA,
    PMP_pre        = round(mean(df_pre$PMP,na.rm=TRUE),2),
    PMP_post       = if(!is.null(df_post)) round(mean(df_post$PMP,na.rm=TRUE),2) else NA,
    delta_pp       = if(!is.null(df_post))
      round(mean(df_post$PMP,na.rm=TRUE)-mean(df_pre$PMP,na.rm=TRUE),2) else NA,
    d_Cohen_crudo  = d_cohen,    # Feingold (2009): sobre diferencias individuales obs.
    d_Cohen_ajust  = d_cohen_aj, # Multinivel: delta_modelo / SD_residual_nulo
    ICC_escuela    = coalesce(icc_res$icc_escuela, NA_real_),
    ICC_grado      = coalesce(icc_res$icc_grado,   NA_real_),
    R2_marginal    = r2_res$r2_l1,   # efectos fijos (Nakagawa & Schielzeth 2013)
    R2_condicional = r2_res$r2_l2,   # efectos fijos + aleatorios
    n_modelos      = length(modelos_ok),
    stringsAsFactors=FALSE)
  add_ws("Resumen_Ejecutivo", df_resumen_exec)

  if (!is.null(comp_modelos)) add_ws("Comparacion_Modelos", comp_modelos)
  if (!is.null(ef_fijos))    add_ws("Efectos_Fijos_Todos", ef_fijos)
  if (!is.null(varianzas))   add_ws("Varianza_Componentes", varianzas)
  if (!is.null(blups_cct))   add_ws("BLUPs_CCT", blups_cct)
  if (!is.null(emmeans_nivel)) add_ws("Medias_Marginales_Nivel", emmeans_nivel)

  # Tabla de efectos fijos del mejor modelo (limpia y lista para reporte)
  ef_mejor <- tabla_efectos_fijos(mejor_modelo, "MEJOR MODELO")
  if (!is.null(ef_mejor)) add_ws("Efectos_Mejor_Modelo", ef_mejor)

  tryCatch(saveWorkbook(wb, file.path(dir_nm,"TABLAS",
             paste0(nm,"_MLM_resultados.xlsx")),overwrite=TRUE),error=function(e)NULL)

  # CSVs individuales
  if (!is.null(blups_cct)) write.csv(blups_cct,
    file.path(dir_nm,"TABLAS",paste0(nm,"_BLUPs_CCT.csv")), row.names=FALSE)
  if (!is.null(ef_mejor)) write.csv(ef_mejor,
    file.path(dir_nm,"TABLAS",paste0(nm,"_efectos_fijos.csv")), row.names=FALSE)
  write.csv(df_resumen_exec,
    file.path(dir_nm,"TABLAS",paste0(nm,"_resumen.csv")), row.names=FALSE)

  # Guardar modelos como RDS
  saveRDS(modelos_ok, file.path(dir_nm,"MODELOS",paste0(nm,"_modelos.rds")))
  saveRDS(mejor_modelo, file.path(dir_nm,"MODELOS",paste0(nm,"_mejor_modelo.rds")))

  # ── GRÁFICOS ──────────────────────────────────────────────────────────────
  cat("  Generando gráficos...\n")
  tit <- function(v) paste(v,"—",nm)
  gg  <- function(p) file.path(dir_nm,"GRAFICOS",p)

  # G1: PMP PRE vs POST (violin + boxplot + puntos) — puntajes CRUDOS
  if (!is.null(df_long)) {
    momentos_presentes <- unique(df_long$momento_lbl)
    df_viol <- df_long %>% filter(!is.na(PMP)) %>%
      mutate(momento_f=factor(momento_lbl, levels=c("PRE","POST")))
    n_momentos <- n_distinct(df_viol$momento_f)
    g1 <- ggplot(df_viol, aes(x=momento_f, y=PMP, fill=momento_f)) +
      geom_violin(alpha=0.35, width=0.8, color=NA) +
      geom_boxplot(width=0.25, outlier.size=0.8, alpha=0.85,
                   color="grey30", linewidth=0.5) +
      stat_summary(fun=mean, geom="point", shape=18,
                   size=4, color="white") +
      stat_summary(fun=mean, geom="text",
                   aes(label=sprintf("%.1f%%",after_stat(y))),
                   vjust=-1.2, size=3.5, fontface="bold") +
      scale_fill_manual(values=COL_MOM, name="Momento") +
      scale_y_continuous(labels=label_number(suffix="%"), limits=c(0,105)) +
      labs(title=tit("Distribución PMP — PRE vs POST (puntajes crudos)"),
           subtitle=sprintf("n_pre=%d | n_post=%d | d_Cohen=%.2f",
                            nrow(df_pre), if(!is.null(df_post))nrow(df_post) else 0,
                            ifelse(is.na(d_cohen),0,d_cohen)),
           x=NULL, y="PMP (%)") +
      theme(legend.position="none",
            axis.text.x=element_text(size=12,face="bold"))
    guardar_g(g1, gg("G1_violin_PMP.png"), 7, 6)
  }

  # G1b: Medias marginales AJUSTADAS PRE vs POST (emmeans del modelo M1_tiempo)
  # Estas medias controlan la estructura de anidamiento escolar → son las
  # estimaciones ajustadas por el modelo multinivel
  modelo_tiempo <- MODELOS$M1_tiempo
  if (!is.null(modelo_tiempo) && tiene_post) {
    df_em_pre_post <- tryCatch({
      em <- as.data.frame(emmeans(modelo_tiempo, ~momento,
                                   at=list(momento=c(0,1))))
      em$momento_lbl <- ifelse(em$momento==0,"PRE","POST")
      em$momento_f   <- factor(em$momento_lbl, levels=c("PRE","POST"))
      em
    }, error=function(e) NULL)

    if (!is.null(df_em_pre_post)) {
      # Guardar tabla de medias ajustadas
      add_ws("Medias_Ajustadas_PRE_POST", df_em_pre_post)
      write.csv(df_em_pre_post,
        file.path(dir_nm,"TABLAS",paste0(nm,"_medias_ajustadas.csv")),
        row.names=FALSE)

      delta_aj <- round(diff(df_em_pre_post$emmean[order(df_em_pre_post$momento)]),2)
      g1b <- ggplot(df_em_pre_post,
                    aes(x=momento_f, y=emmean, fill=momento_f, color=momento_f)) +
        geom_col(width=0.55, alpha=0.85, color=NA) +
        geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                      width=0.18, linewidth=1.2, color="grey30") +
        geom_text(aes(label=sprintf("%.1f%%", emmean)),
                  vjust=-0.6, size=5, fontface="bold",
                  color=c(COL_PRE, COL_POST)) +
        annotate("text", x=1.5, y=max(df_em_pre_post$upper.CL)+5,
                 label=sprintf("Δ = %+.1f pp  (d_aj=%.2f | d_crudo=%.2f)",
                               delta_aj,
                               ifelse(is.na(d_cohen_aj),0,d_cohen_aj),
                               ifelse(is.na(d_cohen),0,d_cohen)),
                 size=4.5, fontface="bold", color="#333333") +
        scale_fill_manual(values=COL_MOM)  +
        scale_y_continuous(labels=label_number(suffix="%"), limits=c(0,110)) +
        labs(title=tit("Medias marginales ajustadas — PRE vs POST"),
             subtitle="Estimadas por el modelo multinivel M1 | IC 95% | controlando estructura escolar",
             x=NULL, y="PMP estimado (%)") +
        theme(legend.position="none",
              axis.text.x=element_text(size=13,face="bold",color=c(COL_PRE,COL_POST)))
      guardar_g(g1b, gg("G1b_medias_ajustadas_PRE_POST.png"), 7, 6)
    }
  }

  # G2: ICC y componentes de varianza (modelo nulo)
  modelo_nulo_g2 <- if(!is.null(MODELOS$M0_nulo_3niv)) MODELOS$M0_nulo_3niv
                    else if(!is.null(MODELOS$M0_nulo_2niv)) MODELOS$M0_nulo_2niv
                    else MODELOS$M0_nulo
  if (!is.null(modelo_nulo_g2)) {
    vc_n <- as.data.frame(VarCorr(modelo_nulo_g2))
    icc_esc <- coalesce(icc_res$icc_escuela, 0)
    df_var <- data.frame(
      componente=c(paste0("Entre escuelas (L2)\nCCT"),
                   "Dentro escuelas (L1)\nResidual"),
      varianza=c(vc_n$vcov[vc_n$grp!="Residual"][1],
                  vc_n$vcov[vc_n$grp=="Residual"]),
      pct=round(c(icc_esc,(1-icc_esc))*100,1)
    )
    g2 <- ggplot(df_var, aes(x="", y=varianza, fill=componente)) +
      geom_col(color="white", linewidth=0.8) +
      geom_text(aes(label=sprintf("%.1f%%\n(%s)",pct,componente)),
                position=position_stack(vjust=0.5),
                size=3.5, fontface="bold", color="white") +
      coord_polar(theta="y") +
      scale_fill_manual(values=c(COL_POST,COL_PRE)) +
      labs(title=tit("Descomposición de varianza (Modelo Nulo)"),
           subtitle=sprintf("ICC = %.3f → %.1f%% de varianza entre escuelas",
                            icc_esc, icc_esc*100)) +
      theme_void() + theme(plot.title=element_text(size=11,face="bold",hjust=0.5),
                            plot.subtitle=element_text(size=9,color="#555",hjust=0.5),
                            legend.position="none")
    guardar_g(g2, gg("G2_varianza_ICC.png"), 7, 7)
  }

  # G3: Efectos fijos del mejor modelo (coefplot)
  if (!is.null(ef_mejor) && nrow(ef_mejor)>1) {
    df_ef <- ef_mejor %>% filter(predictor != "(Intercept)") %>%
      mutate(sig_color=ifelse(p_valor<.05,"Significativo","No significativo"))
    if (nrow(df_ef)>0) {
      g3 <- ggplot(df_ef, aes(x=reorder(predictor,coef), y=coef, color=sig_color)) +
        geom_hline(yintercept=0, linetype="dashed", color="#888888", linewidth=0.5) +
        geom_errorbar(aes(ymin=ic_inf, ymax=ic_sup), width=0.3, linewidth=0.8) +
        geom_point(size=4, alpha=0.95) +
        geom_text(aes(label=sig), vjust=-0.9, size=4.5, color="grey30") +
        scale_color_manual(values=c("Significativo"=COL_POST,
                                     "No significativo"="#AAAAAA"),
                           name="Significancia") +
        coord_flip() +
        labs(title=tit("Efectos fijos — Mejor modelo"),
             subtitle="IC 95% Wald | * p<.05 | ** p<.01 | *** p<.001",
             x=NULL, y="Coeficiente (puntos porcentuales de PMP)") +
        theme(axis.text.y=element_text(size=8.5,face="bold"))
      guardar_g(g3, gg("G3_coefplot.png"), 10, max(4,nrow(df_ef)*0.5+2))
    }
  }

  # G4: BLUPs por CCT (ranking de escuelas)
  if (!is.null(blups_cct) && nrow(blups_cct)>0) {
    df_blup <- blups_cct %>%
      mutate(color=case_when(intercepto_random>2~"Alto",
                              intercepto_random< -2~"Bajo",TRUE~"Promedio"),
             CCT=factor(CCT, levels=CCT[order(intercepto_random)]))
    g4 <- ggplot(df_blup, aes(x=CCT, y=intercepto_random, fill=color)) +
      geom_col(width=0.75, alpha=0.88, color="white", linewidth=0.2) +
      geom_hline(yintercept=0, linewidth=0.5, color="#444") +
      geom_hline(yintercept=c(-2,2), linetype="dashed",
                 color="#888888", linewidth=0.4) +
      scale_fill_manual(values=c(Alto="#1A9641",Bajo="#D7191C",Promedio="#CCCCCC"),
                        name="Desempeño") +
      coord_flip() +
      labs(title=tit("Efectos aleatorios por escuela (BLUPs)"),
           subtitle="Desviación de cada escuela respecto a la media general | Líneas ±2 puntos",
           x="CCT", y="Efecto aleatorio (pp de PMP)",
           caption="BLUPs = Best Linear Unbiased Predictors | Estimaciones Bayes empírico") +
      theme(axis.text.y=element_text(size=ifelse(nrow(df_blup)>40,5,7.5),
                                      face="bold"))
    guardar_g(g4, gg("G4_BLUPs_CCT.png"), 10, max(7,nrow(df_blup)*0.22+2))
  }

  # G5: BLUPs por Entidad (agregado)
  if (!is.null(blups_cct) && "Entidad" %in% names(blups_cct) &&
      n_distinct(blups_cct$Entidad)>1) {
    df_ent <- blups_cct %>%
      group_by(Entidad) %>%
      summarise(blup_medio=round(mean(intercepto_random,na.rm=TRUE),3),
                blup_sd   =round(sd(intercepto_random,na.rm=TRUE),3),
                n_escuelas=n(), .groups="drop")
    g5 <- ggplot(df_ent, aes(x=reorder(Entidad,blup_medio), y=blup_medio)) +
      geom_errorbar(aes(ymin=blup_medio-blup_sd, ymax=blup_medio+blup_sd),
                    width=0.4, color="#888888", linewidth=0.7) +
      geom_point(aes(size=n_escuelas, color=blup_medio)) +
      geom_hline(yintercept=0, linetype="dashed", color="#666", linewidth=0.5) +
      geom_text(aes(label=sprintf("%.1f\n(n=%d)",blup_medio,n_escuelas)),
                vjust=-0.7, size=3, fontface="bold") +
      scale_color_gradient2(low="#D7191C",mid="grey",high="#1A9641",
                             midpoint=0,name="BLUP medio") +
      scale_size_continuous(range=c(3,8),name="N escuelas") +
      coord_flip() +
      labs(title=tit("Efectos por entidad (promedio de BLUPs)"),
           subtitle="Cada punto = entidad | Barras = ±1 SD entre escuelas de la entidad",
           x="Entidad (clave)", y="BLUP promedio (pp de PMP)") +
      theme(axis.text.y=element_text(size=9,face="bold"))
    guardar_g(g5, gg("G5_BLUPs_Entidad.png"), 10, 6)
    write.csv(df_ent, file.path(dir_nm,"TABLAS",paste0(nm,"_BLUPs_Entidad.csv")),
              row.names=FALSE)
  }

  # G6: Medias marginales por nivel × momento (emmeans)
  if (!is.null(emmeans_nivel)) {
    tryCatch({
      df_em <- emmeans_nivel %>%
        mutate(momento_lbl=ifelse(momento==0,"PRE","POST"),
               momento_f=factor(momento_lbl,levels=c("PRE","POST")))
      g6 <- ggplot(df_em, aes(x=.data[[nivel_col]], y=emmean,
                               fill=momento_f, color=momento_f)) +
        geom_col(position=position_dodge(0.75),width=0.7,alpha=0.85,
                 linewidth=0.3,color="white") +
        geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),
                      position=position_dodge(0.75),width=0.3,linewidth=0.7)+
        geom_text(aes(label=sprintf("%.1f%%",emmean)),
                  position=position_dodge(0.75),vjust=-0.4,size=3,fontface="bold")+
        scale_fill_manual(values=COL_MOM,name="Momento")+
        scale_color_manual(values=COL_MOM,guide="none")+
        scale_y_continuous(limits=c(0,105),labels=label_number(suffix="%"))+
        labs(title=tit("PMP estimado por nivel educativo y momento"),
             subtitle="Medias marginales estimadas (emmeans) | IC 95% ajustado",
             x="Nivel educativo",y="PMP estimado (%)")+
        theme(axis.text.x=element_text(size=10,face="bold"))
      guardar_g(g6, gg("G6_emmeans_nivel.png"), 9, 6)
    }, error=function(e) NULL)
  }

  # G7: Cambio PRE-POST por CCT (dumbbell)
  if (!is.null(df_long) && !is.null(blups_cct)) {
    df_dumb <- df_long %>% filter(!is.na(PMP)) %>%
      group_by(.data[[cct_col]], momento_lbl) %>%
      summarise(pmp_medio=round(mean(PMP,na.rm=TRUE),1),.groups="drop") %>%
      pivot_wider(names_from=momento_lbl,values_from=pmp_medio) %>%
      filter(!is.na(PRE)&!is.na(POST)) %>%
      mutate(delta=round(POST-PRE,1),
             color_d=case_when(delta>3~"Mejora",delta< -3~"Deterioro",TRUE~"Estable"))
    n_ccts_plot <- min(40, nrow(df_dumb))
    df_dumb_p <- df_dumb %>%
      arrange(desc(abs(delta))) %>% head(n_ccts_plot) %>%
      mutate(CCT_lbl=factor(.data[[cct_col]],
                             levels=.data[[cct_col]][order(PRE)]))
    g7 <- ggplot(df_dumb_p) +
      geom_segment(aes(x=PRE,xend=POST,
                        y=CCT_lbl,yend=CCT_lbl,color=color_d),
                   linewidth=1.5,alpha=0.6)+
      geom_point(aes(x=PRE, y=CCT_lbl), color=COL_PRE, size=3)+
      geom_point(aes(x=POST,y=CCT_lbl), color=COL_POST,size=3)+
      geom_text(aes(x=POST,y=CCT_lbl,
                     label=sprintf("%+.1f",delta)),
                hjust=-0.2,size=2.8,fontface="bold")+
      scale_color_manual(values=c(Mejora="#1A9641",Deterioro="#D7191C",Estable="#CCCCCC"),
                         name="Cambio")+
      scale_x_continuous(limits=c(0,108),labels=label_number(suffix="%"))+
      labs(title=tit(paste("Cambio PRE→POST por CCT (top",n_ccts_plot,"mayor delta)")),
           subtitle="Punto azul=PRE | Punto naranja=POST | Etiqueta=delta en pp",
           x="PMP medio (%)",y="CCT")+
      theme(axis.text.y=element_text(size=7))
    guardar_g(g7, gg("G7_dumbbell_CCT.png"),
              11, max(6,n_ccts_plot*0.28+2))
    write.csv(df_dumb %>% rename(CCT=.data[[cct_col]]),
              file.path(dir_nm,"TABLAS",paste0(nm,"_cambio_CCT.csv")),row.names=FALSE)
  }

  n_g <- length(list.files(file.path(dir_nm,"GRAFICOS"),"*.png"))
  cat(sprintf("  ✓ %s | %d modelos | %d gráficos\n", nm, length(modelos_ok), n_g))
  invisible(list(resumen=df_resumen_exec, ef_fijos=ef_fijos, varianzas=varianzas,
                 comp_modelos=comp_modelos, blups=blups_cct, mejor=mejor_modelo))
}

# ---------------------------------------------------------------------------- #
# 8. EJECUCIÓN PRINCIPAL
# ---------------------------------------------------------------------------- #
cat("\n", strrep("#",65), "\n")
cat("  FASE 6 — ANÁLISIS MULTINIVEL (MLM / HLM)\n")
cat("  Software: lme4 + lmerTest | Estimación: REML (nulo) / ML (comparación)\n")
cat("  Centrado: CWC (L1) + CGM (L2) | Selección: BIC mínimo\n")
cat("  Inicio:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("#",65), "\n")

RESULTADOS_MLM <- list()
for (nm in names(INSTRUMENTOS))
  RESULTADOS_MLM[[nm]] <- tryCatch(analizar_mlm(nm), error=function(e) {
    cat("\n!! ERROR en",nm,":",conditionMessage(e),"\n"); NULL
  })

# ---------------------------------------------------------------------------- #
# 9. RESUMEN GLOBAL
# ---------------------------------------------------------------------------- #
cat("\n\n", strrep("═",65), "\n  RESUMEN GLOBAL MLM\n", strrep("═",65), "\n")

df_resumen_global <- do.call(rbind, lapply(RESULTADOS_MLM, function(r) r$resumen))
if (!is.null(df_resumen_global)) {
  print(df_resumen_global %>%
          select(instrumento,figura,PMP_pre,PMP_post,delta_pp,
                 d_Cohen_crudo,d_Cohen_ajust,ICC_escuela,R2_marginal,R2_condicional),
        row.names=FALSE)
  write.csv(df_resumen_global,
            file.path(ruta_sal,"TABLAS","00_RESUMEN_MLM_Global.csv"), row.names=FALSE)
}

# Gráfico comparativo global: delta PRE-POST por instrumento
if (!is.null(df_resumen_global) && sum(!is.na(df_resumen_global$delta_pp))>0) {
  g_comp <- ggplot(df_resumen_global%>%filter(!is.na(delta_pp)),
                    aes(x=reorder(instrumento,delta_pp),
                        y=delta_pp, fill=ifelse(delta_pp>0,"Mejora","Sin cambio"))) +
    geom_col(width=0.7,alpha=0.9,color="white",linewidth=0.3) +
    geom_text(aes(label=sprintf("%+.1f pp\n(d=%.2f)",delta_pp,
                                 ifelse(is.na(d_Cohen),0,d_Cohen))),
              hjust=ifelse(df_resumen_global$delta_pp[!is.na(df_resumen_global$delta_pp)]>=0,-0.1,1.1),
              size=3.5,fontface="bold")+
    geom_hline(yintercept=0,linewidth=0.5,color="#444")+
    scale_fill_manual(values=c(Mejora="#1A9641","Sin cambio"="#FDAE61"),guide="none")+
    scale_y_continuous(labels=label_number(suffix=" pp"))+
    coord_flip()+
    labs(title="Cambio PRE→POST por instrumento (puntos porcentuales de PMP)",
         subtitle="d = Cohen's d sobre la distribución de diferencias individuales",
         x=NULL, y="Δ PMP (pp)")+
    theme(axis.text.y=element_text(size=10,face="bold"))
  guardar_g(g_comp, file.path(ruta_sal,"GRAFICOS","00_comparativo_delta.png"), 11, 6)
}

cat("\n  Archivos en:", ruta_sal, "\n")
cat("  Fin:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n\n")

# ---------------------------------------------------------------------------- #
# NOTAS METODOLÓGICAS
# ---------------------------------------------------------------------------- #
# ESTIMACIÓN:
#   REML → modelos nulos y finales (estimación de varianza)
#   ML   → comparación de modelos (LRT) — Bates et al. (2015)
#
# SELECCIÓN DE MODELO:
#   BIC mínimo: penaliza más la complejidad → parsimonia (Raftery, 1995)
#   LRT (χ²) para diferencias significativas entre modelos anidados
#
# CENTRADO (Enders & Tofighi, 2007):
#   CWC (L1): elimina la parte "between" del predictor L1
#             → interpreta el efecto "within-school"
#   CGM (L2): el coeficiente es el efecto de la variable de nivel escuela
#             para una escuela con valor promedio en L1
#
# TAMAÑO DE EFECTO:
#   d de Cohen (Feingold, 2009): sobre distribución de diferencias (POST-PRE)
#   < 0.20 = negligible | 0.20-0.49 = pequeño | 0.50-0.79 = mediano | ≥ 0.80 = grande
#
# R² MULTINIVEL (Snijders & Bosker, 1994):
#   R²_L1 = proporción de varianza L1 explicada por predictores
#   R²_L2 = proporción de varianza L2 explicada por predictores L2
#
# BLUPs (Best Linear Unbiased Predictors):
#   Efectos aleatorios específicos de cada escuela (Bayes empírico)
#   = desviación de la escuela respecto a la media fija del modelo
#   Útiles para ranking de escuelas con "shrinkage" (más confiables que medias crudas)
#
# CONVERGENCIA:
#   Optimizador bobyqa (Bates et al.) — más robusto para modelos complejos
#   Si no converge con pendientes aleatorias, se mantiene solo intercepto aleatorio
# ==============================================================================
