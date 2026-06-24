# ==============================================================================
# FASE 1 — ANÁLISIS PSICOMÉTRICO COMPLETO
# Programa de Formación Escolar (PFE)
# ==============================================================================
# SALIDAS POR CUESTIONARIO (6 módulos):
#   [1] Calidad de datos      → %missing, N total/nivel/CCT, efecto piso/techo
#   [2] Confiabilidad         → α Cronbach + ω McDonald (global + subscala)
#   [3] Validez de constructo → AFC (lavaan): CFI, TLI, RMSEA, SRMR
#   [4] Validez convergente   → AVE, CR (Composite Reliability)
#   [5] Validez discriminante → Fornell-Larcker + HTMT
#   [6] Invarianza de medición→ configural / métrica / escalar (lavaan)
#
# ANÁLISIS COMPLEMENTARIOS (detección de ítems débiles):
#   TCT → r ítem-total corregida, α sin ítem
#   AFE → policórica, análisis paralelo, oblimin
#   IRT → GRM (politómicos), 2PL (binarios)
#   REDUND → CIP, pares r ≥ 0.85
#
# TIPO DE VARIABLE → ANÁLISIS:
#   Ordinal politómica (HD/HI/HSXXI Likert) → policórica, WLSMV, GRM
#   Binaria (Sí/No en CTX)                  → tetracórica, WLSMV, 2PL
#   MC/Conocimiento objetivo (mc_cols)       → % correcto; EXCLUIDOS de CFA/IRT
#
# ESTIMADOR AFC: WLSMV (ordered=TRUE en lavaan)
#   Apropiado para Likert ordinal; no asume normalidad ni escala de intervalo.
#   Fallback a DWLS si n < 100.
#
# CUESTIONARIOS DE CONTEXTO (CTXT_DOC, CTXT_EST):
#   Módulos [3]-[6] NO APLICAN. Los ítems CTX son descriptivos o formativos:
#   miden prácticas, frecuencias y percepciones de fenómenos externos, no
#   indicadores reflectivos de un constructo latente. Aplicar AFC produciría
#   índices sin sentido conceptual (Bollen & Bauldry, 2011).
#   Análisis disponibles para CTX: calidad de datos, α/ω descriptivo, TCT,
#   redundancia inter-ítem y correlación con outcome (Fase 5).
# ==============================================================================

# ---------------------------------------------------------------------------- #
# 0. PAQUETES
# ---------------------------------------------------------------------------- #
pkgs <- c("readxl","writexl","dplyr","tidyr","stringr","ggplot2","scales",
          "psych","mirt","GPArotation","lavaan","semTools","semPlot")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl);  library(writexl)
  library(dplyr);   library(tidyr);   library(stringr)
  library(ggplot2); library(scales)
  library(psych);   library(mirt);    library(GPArotation)
  library(lavaan);  library(semTools)
  if (requireNamespace("semPlot", quietly = TRUE)) library(semPlot)
})
set.seed(42)
theme_set(theme_minimal(base_size = 11))
seg <- function(e) tryCatch(e, error = function(x) NULL)

# ---------------------------------------------------------------------------- #
# 1. RUTAS  <- ADAPTAR
# ---------------------------------------------------------------------------- #
ruta_datos  <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/00_DATOS/Cuestionarios_independientes/"
ruta_salida <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/01_ANALISIS_PSICOMETRICO/"
dir.create(ruta_salida, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------- #
# 2. VALORES NA
# ---------------------------------------------------------------------------- #
VALORES_NA <- c(
  "No lo sé/Prefiero no contestar","No lo sé/ Prefiero no contestar",
  "No lo sé / Prefiero no contestar","No lo sé",
  "Prefiero no contestar","No aplica","NA",""
)

# ---------------------------------------------------------------------------- #
# 3. DICCIONARIOS DE CODIFICACIÓN
# ---------------------------------------------------------------------------- #
COD <- list(
  # hd_habilidad_mixto PRIMERO: 16 ítems HD_EST con piso de conocimiento + cats 1-3 habilidad
  hd_habilidad_mixto = c(
    "No sé / Nunca he oído hablar de esto"                                        = 0,
    "No sé cómo hacerlo"                                                          = 0,
    "Puedo hacerlo con ayuda"                                                     = 1,
    "Puedo hacerlo por mi cuenta"                                                 = 2,
    "Puedo hacerlo con confianza y si es necesario puedo ayudar a otras personas" = 3
  ),
  hd_conocimiento = c(
    "No sé / Nunca he oído hablar de esto" = 0, "Conozco un poco el tema" = 1,
    "Sí, conozco bien este tema" = 2,
    "Totalmente, e incluso podría explicárselo a otras personas" = 3
  ),
  hd_habilidad = c(
    "No sé cómo hacerlo" = 0, "Puedo hacerlo con ayuda" = 1,
    "Puedo hacerlo por mi cuenta" = 2,
    "Puedo hacerlo con confianza y si es necesario puedo ayudar a otras personas" = 3
  ),
  hd_frecuencia   = c("Nunca"=0,"Rara vez"=1,"Algunas veces"=2,"Frecuentemente"=3,"Siempre"=4),
  hi_frecuencia   = c("Casi nunca"=0,"Algunas veces durante el semestre/año"=1,
                      "1 a 3 veces al mes"=2,"1 a 3 veces por semana"=3,"Casi todos los días"=4),
  hi_acuerdo      = c("No estoy de acuerdo"=0,"Algo de acuerdo"=1,"De acuerdo"=2,
                      "Muy de acuerdo"=3,"Totalmente de acuerdo"=4),
  hsxxi_acuerdo   = c("Totalmente en desacuerdo"=0,"Algo en desacuerdo"=1,
                      "Ni de acuerdo ni en desacuerdo"=2,"Algo de acuerdo"=3,
                      "Totalmente de acuerdo"=4),
  hsxxi_frecuencia= c("Nunca"=0,"Rara vez"=1,"Algunas veces"=2,"Frecuentemente"=3,"Siempre"=4),
  ctx_frecuencia  = c("Nunca"=0,"Pocas veces"=1,"Muchas veces"=2,"Siempre"=3),
  ctx_receptor    = c("Nada receptivos"=0,"Poco receptivos"=1,"Indiferentes"=2,
                      "Medianamente receptivos"=3,"Muy receptivos"=4),
  ctx_acuerdo     = c("Totalmente en desacuerdo"=0,"Algo en desacuerdo"=1,
                      "Ni de acuerdo ni en desacuerdo"=2,"Algo de acuerdo"=3,
                      "Totalmente de acuerdo"=4),
  ctx_binaria     = c("No"=0,"Sí"=1)
)

recodificar <- function(vec) {
  v <- as.character(vec)
  v[trimws(v) %in% VALORES_NA | is.na(v)] <- NA
  n_v <- sum(!is.na(v)); if (n_v == 0) return(rep(NA_real_, length(v)))
  for (d in COD) {
    if (sum(v[!is.na(v)] %in% names(d), na.rm = TRUE) / n_v >= 0.60)
      return(as.numeric(d[v]))
  }
  num <- suppressWarnings(as.numeric(v))
  if (mean(!is.na(num), na.rm = TRUE) >= 0.50) return(num)
  rep(NA_real_, length(v))
}
recodificar_df <- function(df) as.data.frame(lapply(df, recodificar), check.names = FALSE)

seleccionar_validas <- function(df, min_obs = 10, min_cats = 2) {
  keep <- vapply(df, function(col) {
    v <- na.omit(col); if (length(v) < min_obs || length(unique(v)) < min_cats) return(FALSE)
    !is.na(sd(v)) && sd(v) > 0
  }, logical(1))
  df[, keep, drop = FALSE]
}

# ---------------------------------------------------------------------------- #
# 4. CONFIGURACIÓN DE INSTRUMENTOS
# ---------------------------------------------------------------------------- #
INSTRUMENTOS <- list(
  HD_DOC = list(
    archivo = "HD_DOC.xlsx",
    id_cols = c("Fecha","Q1","Q2","CCT","Folio","Nombre","Coordinador","Facilitador","Duracion","Momento"),
    items   = paste0("Q", 9:90), mc_cols = NULL,
    subscalas = list(Info_Alfab=paste0("Q",9:20), Comunic_Col=paste0("Q",21:43),
                     Gen_Contenido=paste0("Q",44:59), Seguridad=paste0("Q",60:75),
                     Resol_Prob=paste0("Q",76:90)),
    descripcion = "Habilidades Digitales — Docentes"
  ),
  HD_EST = list(
    archivo = "HD_EST.xlsx",
    id_cols = c("Fecha","Q1","Q2","CCT","Folio","Nombre","Coordinador","Facilitador","Duracion","Momento"),
    items   = paste0("Q", 16:97), mc_cols = NULL,
    subscalas = list(Info_Alfab=paste0("Q",16:27), Comunic_Col=paste0("Q",28:50),
                     Gen_Contenido=paste0("Q",51:66), Seguridad=paste0("Q",67:82),
                     Resol_Prob=paste0("Q",83:97)),
    descripcion = "Habilidades Digitales — Estudiantes"
  ),
  HI_DOC = list(
    archivo = "HI_DOC.xlsx",
    id_cols = c("Fecha","Q1","Q2","CCT","Folio","Nombre","Coordinador","Facilitador","Duracion","Momento"),
    items   = paste0("Q", 8:49), mc_cols = NULL,
    # HI es UNIDIMENSIONAL conceptualmente (no tiene subdimensiones teóricas).
    # Sin embargo, mezcla dos tipos de escala de respuesta:
    #   - Escala de FRECUENCIA (hi_frecuencia, 0-4): Q8-Q13, Q17-Q27, Q31-Q35, Q39-Q46
    #   - Escala de ACUERDO   (hi_acuerdo,    0-4): Q14-Q16, Q28-Q30, Q36-Q38, Q47-Q49
    # Para el AFC se definen dos grupos de ítems por escala dentro del mismo
    # factor latente. lavaan trata cada grupo con sus propios umbrales ordenados
    # (ordered=TRUE), lo que permite la estimación WLSMV sin confundir escalas.
    # La validez discriminante NO aplica (instrumento unidimensional).
    # La invarianza SÍ aplica: mismo modelo por nivel educativo.
    subscalas = list(
      HI_Frecuencia = c(paste0("Q", 8:13),  paste0("Q", 17:27),
                        paste0("Q", 31:35),  paste0("Q", 39:46)),
      HI_Acuerdo    = c(paste0("Q", 14:16),  paste0("Q", 28:30),
                        paste0("Q", 36:38),  paste0("Q", 47:49))
    ),
    es_unidimensional = TRUE,   # para reportar como 1 factor en el resumen
    descripcion = "Habilidades de Implementación — Docentes"
  ),
  HSXXI_DOC = list(
    archivo = "HSXXI_DOC.xlsx",
    id_cols = c("Fecha","Q1","Q2","CCT","Folio","Nombre","Coordinador","Facilitador","Duracion","Momento"),
    items   = paste0("Q", 12:56), mc_cols = paste0("Q", 57:70),
    subscalas = list(Pens_Critico=paste0("Q",12:22), Trabajo_Equipo=paste0("Q",23:31),
                     Comunicacion=paste0("Q",32:36), Creatividad=paste0("Q",37:41),
                     Alfab_Info=paste0("Q",42:46), Alfab_Tecnologico=paste0("Q",47:51),
                     Resol_Problemas=paste0("Q",52:56)),
    descripcion = "Habilidades Siglo XXI — Docentes"
  ),
  HSXXI_EST = list(
    archivo = "HSXXI_EST.xlsx",
    id_cols = c("Fecha","Q1","Q2","CCT","Folio","Nombre","Coordinador","Facilitador","Duracion","Momento"),
    items   = paste0("Q", 16:60), mc_cols = paste0("Q", 61:74),
    subscalas = list(Pens_Critico=paste0("Q",16:26), Trabajo_Equipo=paste0("Q",27:35),
                     Comunicacion=paste0("Q",36:40), Creatividad=paste0("Q",41:45),
                     Alfab_Info=paste0("Q",46:50), Alfab_Tecnologico=paste0("Q",51:55),
                     Resol_Problemas=paste0("Q",56:60)),
    descripcion = "Habilidades Siglo XXI — Estudiantes"
  ),
  CTXT_DOC = list(
    archivo = "CTXT_DOC.xlsx",
    id_cols = c("Fecha","Q1","Q2","CCT","Escuela","Folio","Nombre","Coordinador","Facilitador","Duracion","Momento"),
    items   = c("Q30","Q31","Q45","Q46","Q47","Q48","Q49","Q50","Q51","Q52","Q53","Q54"),
    mc_cols = NULL,
    subscalas = list(Liderazgo_Dir=c("Q30","Q31"),
                     Practica_Docente=c("Q45","Q46","Q47","Q48"),
                     Actitudes_TecEd=c("Q49","Q50","Q51","Q52","Q53","Q54")),
    es_contexto = TRUE,   # ítems descriptivos/formativos: no aplica AFC/AVE/CR/invarianza
    descripcion = "Contexto — Docentes"
  ),
  CTXT_EST = list(
    archivo = "CTXT_EST.xlsx",
    id_cols = c("Fecha","Q1","Q2","CCT","Escuela","Folio","Nombre","Grado","Grupo","Turno",
                "Coordinador","Facilitador","Lista","Duracion","Momento"),
    items   = c(paste0("Q", 23:37), "Q49"), mc_cols = NULL,
    subscalas = list(Clima_Escolar=paste0("Q",23:29),
                     Practica_Doc_Obs=paste0("Q",30:37),
                     Actitud_Aula="Q49"),
    es_contexto = TRUE,   # ítems descriptivos/formativos: no aplica AFC/AVE/CR/invarianza
    descripcion = "Contexto — Estudiantes"
  )
)

# ---------------------------------------------------------------------------- #
# 5. LECTURA Y DERIVACIÓN DE NIVEL
# ---------------------------------------------------------------------------- #
leer_pre <- function(archivo) {
  df <- read_excel(paste0(ruta_datos, archivo), col_types = "text")
  names(df) <- trimws(names(df))
  if (!"Momento" %in% names(df)) stop("Columna 'Momento' ausente en: ", archivo)
  df_pre <- df %>% filter(Momento == "pre")
  if (!"Nivel" %in% names(df_pre) && "CCT" %in% names(df_pre)) {
    df_pre <- df_pre %>%
      mutate(.tipo = str_extract(CCT, "(?<=\\d{2})[A-Z]{3}"),
             Nivel = case_when(
               .tipo %in% c("DPR","EPR","EPB","DPB") ~ "PRIMARIA",
               .tipo %in% c("DES","DTV","DST","EST")  ~ "SECUNDARIA",
               .tipo %in% c("DCT","ETC","ECB","EMS","CBT") ~ "BACHILLERATO",
               TRUE ~ "OTRO")) %>% select(-.tipo)
  }
  cat("    ", archivo, "| n_pre:", nrow(df_pre), "\n")
  df_pre
}

# ---------------------------------------------------------------------------- #
# 6. MÓDULO 1: CALIDAD DE DATOS
# ---------------------------------------------------------------------------- #
modulo_calidad <- function(df_raw, df_num, items_disp, nombre) {
  N <- nrow(df_num)
  miss <- data.frame(
    instrumento = nombre,
    item        = items_disp,
    n_valido    = sapply(items_disp, function(c) sum(!is.na(df_num[[c]]))),
    n_total     = N,
    pct_missing = sapply(items_disp, function(c) round(mean(is.na(df_num[[c]]))*100,1)),
    media       = sapply(items_disp, function(c) round(mean(df_num[[c]],na.rm=TRUE),3)),
    sd          = sapply(items_disp, function(c) round(sd(df_num[[c]],na.rm=TRUE),3)),
    piso        = sapply(items_disp, function(c) {
      v <- na.omit(df_num[[c]]); if(length(v)==0) NA else round(mean(v==min(v)),3)}),
    techo       = sapply(items_disp, function(c) {
      v <- na.omit(df_num[[c]]); if(length(v)==0) NA else round(mean(v==max(v)),3)}),
    n_cats      = sapply(items_disp, function(c) length(unique(na.omit(df_num[[c]])))),
    stringsAsFactors = FALSE
  )
  miss$flag_miss  <- ifelse(miss$pct_missing > 25, "FLAG NA>25%",  "")
  miss$flag_piso  <- ifelse(miss$piso  > 0.40, "FLAG piso>40%", "")
  miss$flag_techo <- ifelse(miss$techo > 0.40, "FLAG techo>40%","")

  n_nivel <- if ("Nivel" %in% names(df_raw))
    as.data.frame(table(Nivel=df_raw$Nivel, useNA="always"), stringsAsFactors=FALSE)
  else data.frame(Nivel="Sin_nivel", Freq=N)

  n_cct <- if ("CCT" %in% names(df_raw))
    df_raw %>% count(CCT, name="N") %>% mutate(Entidad=substr(as.character(CCT),1,2))
  else NULL

  resumen <- data.frame(instrumento=nombre, N_total=N, n_items=length(items_disp),
    pct_NA_global=round(mean(is.na(df_num))*100,1),
    items_miss25=sum(miss$pct_missing>25), items_piso=sum(miss$piso>0.40,na.rm=TRUE),
    items_techo=sum(miss$techo>0.40,na.rm=TRUE), stringsAsFactors=FALSE)

  list(resumen=resumen, por_item=miss, por_nivel=n_nivel, por_cct=n_cct)
}

# ---------------------------------------------------------------------------- #
# 7. MÓDULO 2: CONFIABILIDAD (alpha + omega)
# ---------------------------------------------------------------------------- #
modulo_confiabilidad <- function(df_num, subscalas, nombre) {
  calc_1 <- function(datos, etiqueta) {
    dc <- seleccionar_validas(datos, min_obs=10, min_cats=2)
    dc <- dc[rowSums(!is.na(dc)) >= ncol(dc)*0.5, , drop=FALSE]
    if (ncol(dc)<2 || nrow(dc)<10) return(NULL)
    ra <- seg(psych::alpha(dc, warnings=FALSE, check.keys=TRUE)); if(is.null(ra)) return(NULL)
    rw <- seg(suppressWarnings(psych::omega(dc, nfactors=1, plot=FALSE)))
    items <- data.frame(subscala=etiqueta, item=rownames(ra$item.stats),
      media=round(ra$item.stats$mean,3), sd=round(ra$item.stats$sd,3),
      r_it=round(ra$item.stats$r.drop,3),
      alpha_si=round(ra$alpha.drop[,"raw_alpha"],3), stringsAsFactors=FALSE)
    items$flag_r_it <- ifelse(items$r_it < 0.25, "FLAG r_it<0.25", "")
    list(
      tabla = data.frame(instrumento=nombre, subscala=etiqueta, n=nrow(dc), n_items=ncol(dc),
        alpha=round(ra$total$raw_alpha,3), alpha_std=round(ra$total$std.alpha,3),
        omega=if(!is.null(rw)) round(rw$omega.tot,3) else NA,
        interp=dplyr::case_when(ra$total$raw_alpha>=.90~"Excelente(>=.90)",
          ra$total$raw_alpha>=.80~"Bueno(.80-.89)", ra$total$raw_alpha>=.70~"Aceptable(.70-.79)",
          ra$total$raw_alpha>=.60~"Cuestionable(.60-.69)", TRUE~"Pobre(<.60)"),
        stringsAsFactors=FALSE),
      items = items
    )
  }
  items_g <- intersect(unique(unlist(subscalas)), names(df_num))
  rg <- calc_1(df_num[,items_g,drop=FALSE], "GLOBAL")
  rs <- Filter(Negate(is.null), lapply(names(subscalas), function(s) {
    cols <- intersect(subscalas[[s]], names(df_num))
    if(length(cols)<2) return(NULL); calc_1(df_num[,cols,drop=FALSE], s)
  }))
  list(
    tabla     = dplyr::bind_rows(c(list(rg$tabla), lapply(rs,`[[`,"tabla"))),
    items_tct = dplyr::bind_rows(c(list(rg$items), lapply(rs,`[[`,"items")))
  )
}

# ---------------------------------------------------------------------------- #
# 8. MÓDULO 3: VALIDEZ DE CONSTRUCTO — AFC (WLSMV)
# ---------------------------------------------------------------------------- #
construir_sintaxis <- function(subscalas, items_disp) {
  lineas <- lapply(names(subscalas), function(s) {
    cols <- intersect(subscalas[[s]], items_disp)
    if (length(cols)<2) return(NULL)
    paste0(s," =~ ", paste(cols, collapse=" + "))
  })
  paste(Filter(Negate(is.null), lineas), collapse="\n")
}

modulo_afc <- function(df_num, subscalas, nombre, es_unidimensional = FALSE) {
  items_disp <- intersect(unique(unlist(subscalas)), names(df_num))
  subs_ok    <- Filter(function(s) length(intersect(s,items_disp))>=2, subscalas)
  if (length(subs_ok)<1) { cat("    [AFC] Insuficientes subs.\n"); return(NULL) }

  # Para instrumentos unidimensionales con múltiples tipos de escala (HI_DOC):
  # todos los ítems cargan en un único factor latente "HI_Global", aunque se
  # hayan definido dos grupos por escala para facilitar la convergencia WLSMV.
  if (es_unidimensional) {
    todos_items <- intersect(unique(unlist(subscalas)), names(df_num))
    syntax <- paste0("HI_Global =~ ", paste(todos_items, collapse = " + "))
    cat(sprintf("    [AFC] Modelo UNIDIMENSIONAL: 1 factor, %d items\n", length(todos_items)))
  } else {
    syntax <- construir_sintaxis(subs_ok, items_disp)
  }
  df_m   <- df_num[, items_disp, drop=FALSE]
  df_m   <- df_m[rowSums(!is.na(df_m)) >= length(items_disp)*0.5, , drop=FALSE]
  n      <- nrow(df_m)
  est    <- if (n>=100) "WLSMV" else "DWLS"
  cat(sprintf("    [AFC] n=%d estimador=%s factores=%d\n", n, est, length(subs_ok)))

  fit <- seg(lavaan::cfa(model=syntax, data=df_m, ordered=TRUE, estimator=est,
                          missing="pairwise", std.lv=TRUE))
  if (is.null(fit)) { cat("    [AFC] No convergió.\n"); return(NULL) }

  fi <- tryCatch(lavaan::fitMeasures(fit,
    c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),
    error=function(e) NULL)
  if (is.null(fi)) return(NULL)

  cfi=round(fi["cfi"],3); tli=round(fi["tli"],3)
  rmsea=round(fi["rmsea"],3); srmr=round(fi["srmr"],3)
  interp <- dplyr::case_when(
    cfi>=.95 & rmsea<=.06 & srmr<=.08 ~ "Ajuste excelente",
    cfi>=.90 & rmsea<=.08 & srmr<=.10 ~ "Ajuste aceptable",
    cfi>=.85                            ~ "Ajuste marginal",
    TRUE                                ~ "Ajuste pobre")

  tabla_fi <- data.frame(instrumento=nombre, n=n, estimador=est,
    n_factores=if(es_unidimensional) 1L else length(subs_ok), chi2=round(fi["chisq"],2), df=round(fi["df"],0),
    p=round(fi["pvalue"],4), CFI=cfi, TLI=tli, RMSEA=rmsea,
    RMSEA_li=round(fi["rmsea.ci.lower"],3), RMSEA_ls=round(fi["rmsea.ci.upper"],3),
    SRMR=srmr, interpretacion=interp, stringsAsFactors=FALSE)

  cargas <- tryCatch({
    sp <- lavaan::standardizedSolution(fit)
    sp_f <- sp[sp$op=="=~", c("lhs","rhs","est.std","se","z","pvalue")]
    names(sp_f)[1:2] <- c("factor","item"); sp_f
  }, error=function(e) NULL)

  cat(sprintf("    [AFC] CFI=%.3f TLI=%.3f RMSEA=%.3f SRMR=%.3f — %s\n",
              cfi, tli, rmsea, srmr, interp))
  list(tabla_fi=tabla_fi, cargas=cargas, syntax=syntax, obj=fit, n=n)
}

# ---------------------------------------------------------------------------- #
# 9. MÓDULO 4: VALIDEZ CONVERGENTE (AVE, CR)
# ---------------------------------------------------------------------------- #
modulo_convergente <- function(res_afc, nombre) {
  if (is.null(res_afc)||is.null(res_afc$cargas)) return(NULL)
  p <- res_afc$cargas
  dplyr::bind_rows(lapply(unique(p$factor), function(f) {
    lam <- p$est.std[p$factor==f]; if(length(lam)<2) return(NULL)
    l2  <- lam^2; err <- 1-l2
    AVE <- round(mean(l2)/(mean(l2)+mean(err)),3)
    CR  <- round((sum(lam))^2/((sum(lam))^2+sum(err)),3)
    data.frame(instrumento=nombre, factor=f, n_items=length(lam),
      carga_media=round(mean(lam),3), carga_min=round(min(lam),3),
      carga_max=round(max(lam),3), AVE=AVE, CR=CR,
      flag_AVE=ifelse(AVE<0.50,"FLAG AVE<.50",""),
      flag_CR=ifelse(CR<0.70,"FLAG CR<.70",""),
      interp_AVE=ifelse(AVE>=0.50,"Convergente (AVE>=.50)","No convergente (AVE<.50)"),
      interp_CR=ifelse(CR>=0.70,"Aceptable (CR>=.70)",ifelse(CR>=0.60,"Cuestionable","Pobre")),
      stringsAsFactors=FALSE)
  }))
}

# ---------------------------------------------------------------------------- #
# 10. MÓDULO 5: VALIDEZ DISCRIMINANTE (Fornell-Larcker + HTMT)
# ---------------------------------------------------------------------------- #
modulo_discriminante <- function(res_afc, tabla_ave, nombre) {
  if (is.null(res_afc)||is.null(res_afc$obj)||is.null(tabla_ave)) return(NULL)
  phi <- tryCatch(round(lavaan::inspect(res_afc$obj,"cor.lv"),3), error=function(e) NULL)
  if (is.null(phi)) { cat("    [Discriminante] Sin phi.\n"); return(NULL) }
  facs <- rownames(phi); if (length(facs)<2) return(NULL)
  ave  <- setNames(tabla_ave$AVE, tabla_ave$factor)

  fl <- expand.grid(F1=facs,F2=facs,stringsAsFactors=FALSE) %>% filter(F1<F2) %>%
    mutate(r=phi[cbind(F1,F2)], r2=round(r^2,3),
           sqAVE1=round(sqrt(coalesce(ave[F1],NA_real_)),3),
           sqAVE2=round(sqrt(coalesce(ave[F2],NA_real_)),3),
           flag_FL=ifelse(sqAVE1>abs(r)&sqAVE2>abs(r),"","FLAG: sqAVE < r"))

  htmt_df <- tryCatch({
    hm <- as.matrix(semTools::htmt(res_afc$obj))
    expand.grid(F1=rownames(hm),F2=colnames(hm),stringsAsFactors=FALSE) %>% filter(F1<F2) %>%
      mutate(HTMT=round(hm[cbind(F1,F2)],3),
             flag_HTMT=ifelse(HTMT>=.90,"FLAG HTMT>=.90",ifelse(HTMT>=.85,"AVISO HTMT>=.85","")))
  }, error=function(e) NULL)

  tabla <- if (!is.null(htmt_df)) left_join(fl, htmt_df, by=c("F1","F2")) else fl
  cat(sprintf("    [Discriminante] pares=%d FL_fail=%d\n",
              nrow(tabla), sum(tabla$flag_FL!="",na.rm=TRUE)))
  list(tabla=tabla, phi=phi, htmt=htmt_df)
}

# ---------------------------------------------------------------------------- #
# 11. MÓDULO 6: INVARIANZA DE MEDICIÓN
# ---------------------------------------------------------------------------- #
modulo_invarianza <- function(df_raw, df_num, res_afc, nombre) {
  if (is.null(res_afc)||is.null(res_afc$syntax)) return(NULL)
  if (!"Nivel" %in% names(df_raw)) { cat("    [Invarianza] Sin Nivel.\n"); return(NULL) }
  its <- intersect(unique(unlist(regmatches(res_afc$syntax,
              gregexpr("[Qq]\\d+",res_afc$syntax)))), names(df_num))
  df_inv <- cbind(df_num[,its,drop=FALSE], Nivel=df_raw$Nivel)
  df_inv <- df_inv[!is.na(df_inv$Nivel),]
  grps   <- names(which(table(df_inv$Nivel)>=50))
  if (length(grps)<2) { cat("    [Invarianza] <2 grupos n>=50.\n"); return(NULL) }
  df_inv <- df_inv[df_inv$Nivel %in% grps,]
  cat(sprintf("    [Invarianza] grupos: %s\n", paste(grps,collapse=", ")))

  aj <- function(ge) seg(lavaan::cfa(res_afc$syntax, data=df_inv, group="Nivel",
    group.equal=ge, ordered=TRUE, estimator=if(nrow(df_inv)>=100)"WLSMV" else "DWLS",
    missing="pairwise", std.lv=TRUE))
  m_c <- aj(character(0)); m_m <- aj("loadings"); m_e <- aj(c("loadings","intercepts"))

  ext <- function(mod, tipo) {
    if (is.null(mod)) return(data.frame(tipo=tipo,CFI=NA,TLI=NA,RMSEA=NA,SRMR=NA))
    fi <- tryCatch(lavaan::fitMeasures(mod,c("cfi","tli","rmsea","srmr")),
                   error=function(e) c(cfi=NA,tli=NA,rmsea=NA,srmr=NA))
    data.frame(instrumento=nombre, tipo=tipo, n_grupos=length(grps),
      CFI=round(fi["cfi"],3), TLI=round(fi["tli"],3),
      RMSEA=round(fi["rmsea"],3), SRMR=round(fi["srmr"],3), stringsAsFactors=FALSE)
  }
  tab <- dplyr::bind_rows(ext(m_c,"Configural"), ext(m_m,"Metrica"), ext(m_e,"Escalar"))
  tab$dCFI   <- c(NA, round(tab$CFI[2]-tab$CFI[1],3),   round(tab$CFI[3]-tab$CFI[2],3))
  tab$dRMSEA <- c(NA, round(tab$RMSEA[2]-tab$RMSEA[1],3),round(tab$RMSEA[3]-tab$RMSEA[2],3))
  tab$decision <- dplyr::case_when(
    is.na(tab$dCFI)                                 ~ "Modelo base",
    tab$dCFI > -0.010 & tab$dRMSEA < 0.015         ~ "Invarianza soportada",
    tab$dCFI <= -0.010 | tab$dRMSEA >= 0.015        ~ "Invarianza parcial/falla",
    TRUE                                             ~ "No evaluado")
  cat("    [Invarianza]\n"); print(tab[,c("tipo","CFI","RMSEA","dCFI","dRMSEA","decision")],row.names=FALSE)
  list(tabla=tab)
}

# ---------------------------------------------------------------------------- #
# 12. ANÁLISIS COMPLEMENTARIOS (TCT, AFE, IRT, REDUNDANCIA)
# ---------------------------------------------------------------------------- #
analisis_tct <- function(datos, nombre) {
  dc <- seleccionar_validas(datos,10,2)
  dc <- dc[rowSums(!is.na(dc))>=ncol(dc)*0.5,,drop=FALSE]
  if(ncol(dc)<2||nrow(dc)<10) return(NULL)
  ra <- seg(psych::alpha(dc,warnings=FALSE,check.keys=TRUE)); if(is.null(ra)) return(NULL)
  rw <- seg(suppressWarnings(psych::omega(dc,nfactors=1,plot=FALSE)))
  its <- data.frame(item=rownames(ra$item.stats), media=round(ra$item.stats$mean,3),
    sd=round(ra$item.stats$sd,3), r_it=round(ra$item.stats$r.drop,3),
    alpha_si=round(ra$alpha.drop[,"raw_alpha"],3), stringsAsFactors=FALSE)
  its$FLAG_TCT <- ifelse(its$r_it<0.25,"FLAG r_it<0.25","")
  list(alpha=round(ra$total$raw_alpha,3), alpha_std=round(ra$total$std.alpha,3),
       omega=if(!is.null(rw)) round(rw$omega.tot,3) else NA,
       n=nrow(dc), n_items=ncol(dc), items=its)
}

analisis_afe <- function(datos, nombre, nfactors_manual=NULL) {
  dc <- seleccionar_validas(datos,20,2)
  dc <- dc[rowSums(!is.na(dc))>=ncol(dc)*0.5,,drop=FALSE]
  n <- nrow(dc); ni <- ncol(dc)
  if(n<50||ni<3){ cat("    [AFE] n=",n,"insuf\n"); return(NULL) }
  mat <- tryCatch(suppressWarnings(psych::polychoric(dc,correct=0.1)$rho),
                  error=function(e) cor(dc,use="pairwise.complete.obs"))
  mat <- (mat+t(mat))/2; diag(mat) <- 1
  kmo <- tryCatch(psych::KMO(mat)$MSA, error=function(e) NA)
  ap  <- tryCatch(suppressWarnings(psych::fa.parallel(dc,fm="ml",fa="fa",plot=FALSE,
                  n.iter=20,cor="poly",show.legend=FALSE,sim=TRUE)), error=function(e) NULL)
  nf  <- if(!is.null(nfactors_manual)) nfactors_manual
         else if(!is.null(ap)&&!is.na(ap$nfact)) max(1,ap$nfact) else 1
  nf  <- min(nf,ni-1,max(1,floor(n/5)))
  fa  <- tryCatch(suppressWarnings(psych::fa(mat,nfactors=nf,rotate="oblimin",fm="ml",n.obs=n)),
                  error=function(e) NULL); if(is.null(fa)) return(NULL)
  lm  <- as.data.frame(unclass(fa$loadings)); fc <- names(lm)
  lm_r <- as.data.frame(lapply(lm,function(x) round(x,3))); rownames(lm_r) <- rownames(lm)
  cargas <- data.frame(item=rownames(lm_r),lm_r,communalidad=round(fa$communality,3),
    unicidad=round(fa$uniquenesses,3),stringsAsFactors=FALSE,check.names=FALSE)
  cargas$carga_max <- apply(cargas[,fc,drop=FALSE],1,function(x) round(max(abs(x)),3))
  cargas$FLAG_AFE  <- ifelse(cargas$carga_max<0.35|cargas$communalidad<0.20,
                             "FLAG carga<0.35 o h2<0.20","")
  var_ac <- if(!is.null(fa$Vaccounted)&&"Cumulative Var"%in%rownames(fa$Vaccounted))
              round(fa$Vaccounted["Cumulative Var",nf]*100,1) else NA
  list(kmo=kmo, nfactors=nf, var_acum=var_ac, cargas=cargas, fa_obj=fa, n=n)
}

analisis_irt <- function(datos, nombre) {
  dc <- seleccionar_validas(datos,30,2); dc <- dc[complete.cases(dc),,drop=FALSE]
  n <- nrow(dc); ni <- ncol(dc)
  if(n<80||ni<2){ cat("    [IRT] n=",n,"insuf\n"); return(NULL) }
  dm    <- as.matrix(dc)
  tipos <- ifelse(apply(dm,2,function(x) length(unique(x)))<=2,"2PL","graded")
  mod   <- tryCatch(suppressWarnings(mirt::mirt(dm,1,itemtype=tipos,
                    SE=TRUE,verbose=FALSE,technical=list(NCYCLES=2000))), error=function(e) NULL)
  if(is.null(mod)) return(NULL)
  params <- tryCatch({
    co <- mirt::coef(mod,IRTpars=TRUE,simplify=TRUE)$items
    df_p <- as.data.frame(co); df_p$item <- rownames(co)
    nc <- vapply(df_p,is.numeric,logical(1)); df_p[nc] <- lapply(df_p[nc],round,3)
    df_p$FLAG_IRT <- ifelse(df_p$a1<0.50,"FLAG discrim<0.50","")
    df_p[,c("item",setdiff(names(df_p),"item"))]
  }, error=function(e) NULL)
  fit <- tryCatch({
    fi <- as.data.frame(mirt::itemfit(mod,fit_stats="S_X2",na.rm=TRUE))
    fi$FLAG_misfit <- ifelse(!is.na(fi$p.S_X2)&fi$p.S_X2<0.01,"FLAG misfit p<.01",""); fi
  }, error=function(e) NULL)
  list(modelo=mod, params=params, fit=fit, n=n)
}

analisis_redundancia <- function(datos, nombre, ua=0.85, um=0.70) {
  dc <- seleccionar_validas(datos,10,2); ni <- ncol(dc); if(ni<2) return(NULL)
  mat <- tryCatch(suppressWarnings(psych::polychoric(dc,correct=0.1)$rho),
                  error=function(e) cor(dc,use="pairwise.complete.obs"))
  mat <- (mat+t(mat))/2
  mc  <- mat; diag(mc) <- NA; cip <- round(mean(mc,na.rm=TRUE),3)
  its <- colnames(dc)
  pares <- do.call(rbind, unlist(lapply(seq_len(ni-1), function(i)
    lapply(seq(i+1,ni), function(j) data.frame(item_A=its[i],item_B=its[j],
      r=round(mat[i,j],3),
      nivel=dplyr::case_when(is.na(mat[i,j])~"Sin dato",mat[i,j]>=ua~"ALTO",
                             mat[i,j]>=um~"MODERADO",TRUE~"Normal"),
      stringsAsFactors=FALSE))), recursive=FALSE))
  pares <- pares[order(-pares$r,na.last=TRUE),]
  fl_a  <- unique(c(pares$item_A[pares$r>=ua],pares$item_B[pares$r>=ua]))
  res   <- data.frame(subscala=nombre,n_items=ni,CIP=cip,
    CIP_interp=dplyr::case_when(cip<0.15~"Heterogeneo",cip<=0.50~"Optimo",TRUE~"Redundante"),
    pares_alto=sum(pares$r>=ua,na.rm=TRUE),pares_medio=sum(pares$r>=um&pares$r<ua,na.rm=TRUE),
    items_flag=if(length(fl_a)>0) paste(fl_a,collapse=", ") else "Ninguno",
    stringsAsFactors=FALSE)
  diag(mat) <- 1
  df_h <- data.frame(A=rep(its,each=ni),B=rep(its,ni),r=as.vector(mat))
  df_h$A <- factor(df_h$A,levels=its); df_h$B <- factor(df_h$B,levels=rev(its))
  df_h$lbl <- ifelse(!is.na(df_h$r)&abs(df_h$r)>=um&df_h$A!=df_h$B,sprintf("%.2f",df_h$r),"")
  g_h <- ggplot(df_h,aes(A,B,fill=r))+geom_tile(color="white",linewidth=.25)+
    geom_text(aes(label=lbl),size=ifelse(ni<=12,2.8,2),color="white",fontface="bold")+
    scale_fill_gradient2(low="#2166AC",mid="white",high="#D7191C",midpoint=0,limits=c(-1,1),name="r")+
    labs(title=paste("Inter-item —",nombre),subtitle=sprintf("CIP=%.3f | pares r>=%.2f: %d",cip,ua,res$pares_alto))+
    theme(axis.text.x=element_text(angle=45,hjust=1,size=ifelse(ni<=15,8,6)),
          axis.text.y=element_text(size=ifelse(ni<=15,8,6)))
  list(cip=cip, resumen=res, pares=pares, items_flag=fl_a, heatmap=g_h, mat=mat)
}

tabla_flags <- function(tct, afe, irt, items, redund=NULL) {
  base <- data.frame(item=items, stringsAsFactors=FALSE)
  if (!is.null(tct)&&!is.null(tct$items))
    base <- merge(base, tct$items[,c("item","media","sd","r_it","FLAG_TCT")], by="item", all.x=TRUE)
  if (!is.null(afe)&&!is.null(afe$cargas))
    base <- merge(base, afe$cargas[,c("item","carga_max","communalidad","FLAG_AFE")], by="item", all.x=TRUE)
  if (!is.null(irt)&&!is.null(irt$params))
    base <- merge(base, irt$params[,c("item","a1","FLAG_IRT")], by="item", all.x=TRUE)
  if (!is.null(irt)&&!is.null(irt$fit)) {
    cols_f <- intersect(c("item","p.S_X2","RMSEA.S_X2","FLAG_misfit"),names(irt$fit))
    base <- merge(base, irt$fit[,cols_f,drop=FALSE], by="item", all.x=TRUE)
  }
  base$FLAG_REDUND <- ifelse(!is.null(redund)&&length(redund$items_flag)>0&
                               base$item%in%redund$items_flag,"FLAG r>=0.85","")
  fc <- intersect(c("FLAG_TCT","FLAG_AFE","FLAG_IRT","FLAG_misfit","FLAG_REDUND"),names(base))
  base$n_flags <- rowSums(sapply(base[,fc,drop=FALSE],function(x) !is.na(x)&nchar(trimws(x))>0),na.rm=TRUE)
  base$DECISION <- ifelse(base$n_flags>=2,"ROJO ELIMINAR",
                   ifelse(base$n_flags==1,"AMARILLO REVISAR","VERDE CONSERVAR"))
  base
}

# ---------------------------------------------------------------------------- #
# 13. FUNCIÓN MAESTRA
# ---------------------------------------------------------------------------- #
analizar_instrumento <- function(cfg, nombre) {
  cat("\n",strrep("=",65),"\n  INSTRUMENTO:",nombre," | ",cfg$descripcion,"\n",strrep("=",65),"\n")
  dir_inst <- file.path(ruta_salida, nombre)
  dir.create(dir_inst, showWarnings=FALSE, recursive=TRUE)

  df_raw     <- leer_pre(cfg$archivo)
  items_disp <- intersect(cfg$items, names(df_raw))
  df_num     <- recodificar_df(df_raw[, items_disp, drop=FALSE])

  # -- [1] CALIDAD --
  cat("\n  [1] Calidad de datos...\n")
  cal <- modulo_calidad(df_raw, df_num, items_disp, nombre)
  write.csv(cal$resumen,   file.path(dir_inst,"1_calidad_resumen.csv"),   row.names=FALSE)
  write.csv(cal$por_item,  file.path(dir_inst,"1_calidad_por_item.csv"),  row.names=FALSE)
  write.csv(cal$por_nivel, file.path(dir_inst,"1_N_por_nivel.csv"),       row.names=FALSE)
  if (!is.null(cal$por_cct))
    write.csv(cal$por_cct, file.path(dir_inst,"1_N_por_CCT.csv"),         row.names=FALSE)
  cat(sprintf("    N=%d | NA=%.1f%% | items_miss25=%d | piso_flag=%d | techo_flag=%d\n",
    cal$resumen$N_total, cal$resumen$pct_NA_global,
    cal$resumen$items_miss25, cal$resumen$items_piso, cal$resumen$items_techo))

  # -- [2] CONFIABILIDAD --
  cat("\n  [2] Confiabilidad (alpha + omega)...\n")
  conf <- modulo_confiabilidad(df_num, cfg$subscalas, nombre)
  write.csv(conf$tabla,     file.path(dir_inst,"2_confiabilidad.csv"),   row.names=FALSE)
  write.csv(conf$items_tct, file.path(dir_inst,"2_TCT_items.csv"),       row.names=FALSE)
  fg <- conf$tabla[conf$tabla$subscala=="GLOBAL",]
  cat(sprintf("    GLOBAL: alpha=%.3f omega=%.3f — %s\n",
    coalesce(fg$alpha,NA_real_), coalesce(fg$omega,NA_real_), coalesce(fg$interp,"?")))

  # -- [3]-[6] AFC / AVE-CR / Discriminante / Invarianza --
  # Los cuestionarios de contexto (es_contexto=TRUE) usan ítems descriptivos/
  # formativos → AFC reflectivo no tiene sustento conceptual (Bollen & Bauldry, 2011)
  es_ctx <- isTRUE(cfg$es_contexto)
  if (es_ctx) {
    cat("\n  [3-6] NO APLICA: cuestionario de contexto (ítems formativos/descriptivos)\n")
    cat("        AFC, AVE, CR, discriminante e invarianza requieren estructura latente.\n")
    afc <- NULL; ave_cr <- NULL; discr <- NULL; inv <- NULL
  }

  if (!es_ctx) {
  cat("\n  [3] Validez de constructo (AFC WLSMV)...\n")
  afc <- modulo_afc(df_num, cfg$subscalas, nombre,
                    es_unidimensional = isTRUE(cfg$es_unidimensional))
  if (!is.null(afc)) {
    write.csv(afc$tabla_fi, file.path(dir_inst,"3_AFC_indices.csv"),  row.names=FALSE)
    write.csv(afc$cargas,   file.path(dir_inst,"3_AFC_cargas.csv"),   row.names=FALSE)
    # Diagrama AFC con ggplot2: cargas estandarizadas por factor
    tryCatch({
      if (!is.null(afc$cargas) && nrow(afc$cargas) > 0) {
        df_d <- afc$cargas %>%
          mutate(
            item      = factor(item, levels = rev(unique(item))),
            sig_lbl   = dplyr::case_when(
              pvalue < .001 ~ "***", pvalue < .01 ~ "**",
              pvalue < .05  ~ "*",   pvalue < .10 ~ ".",
              TRUE ~ ""),
            color_b   = ifelse(est.std >= 0.50, "#1A3A6C", "#F47B20"),
            lbl       = sprintf("%.3f%s", est.std, sig_lbl),
            hjust_val = ifelse(est.std >= 0, -0.08, 1.08)
          )
        n_facts <- n_distinct(df_d$factor)
        g_afc <- ggplot(df_d, aes(x = est.std, y = item, fill = color_b)) +
          geom_col(width = 0.72, alpha = 0.88, color = "white", linewidth = 0.3) +
          geom_vline(xintercept = c(0.50, 0.70), linetype = "dashed",
                     color = c("#F47B20","#1A9641"), linewidth = 0.5, alpha = 0.7) +
          geom_text(aes(label = lbl, hjust = hjust_val),
                    size = 2.8, fontface = "bold", color = "grey20") +
          scale_fill_identity() +
          scale_x_continuous(limits = c(0, 1.05),
                             breaks = c(0, 0.25, 0.50, 0.70, 1.00),
                             labels = c("0","0.25","0.50","0.70","1.00")) +
          facet_wrap(~factor, scales = "free_y", ncol = min(3, n_facts)) +
          labs(
            title    = paste("Cargas factoriales estandarizadas —", nombre),
            subtitle = sprintf("AFC WLSMV | n=%d | CFI=%.3f | TLI=%.3f | RMSEA=%.3f | SRMR=%.3f",
                               afc$n,
                               afc$tabla_fi$CFI[1], afc$tabla_fi$TLI[1],
                               afc$tabla_fi$RMSEA[1], afc$tabla_fi$SRMR[1]),
            caption  = "Línea naranja = λ≥0.50 | Línea verde = λ≥0.70 | *** p<.001 ** p<.01 * p<.05",
            x = "Carga estandarizada (λ)", y = NULL
          ) +
          theme_minimal(base_size = 10) +
          theme(
            plot.title       = element_text(size = 12, face = "bold", hjust = 0),
            plot.subtitle    = element_text(size = 8.5, color = "#555555"),
            plot.caption     = element_text(size = 7.5, color = "#888888", hjust = 1),
            plot.background  = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#EEEEEE"),
            panel.grid.minor   = element_blank(),
            strip.text       = element_text(face = "bold", size = 9,
                                            color = "white"),
            strip.background = element_rect(fill = "#1A3A6C", color = NA),
            axis.text.y      = element_text(size = 7.5, face = "bold"),
            axis.text.x      = element_text(size = 8),
            legend.position  = "none"
          )
        n_items_total <- nrow(df_d)
        alto <- max(5, n_items_total * 0.28 + 2)
        ancho <- min(14, n_facts * 4 + 2)
        ggsave(file.path(dir_inst, "3_AFC_diagrama.png"),
               g_afc, width = ancho, height = alto, dpi = 180, bg = "white")
        cat(sprintf("    [AFC] Diagrama guardado (%dx%.0f cm, %d items, %d factores)\n",
                    round(ancho*2.54), round(alto*2.54), n_items_total, n_facts))
      }
    }, error = function(e) cat("    [AFC] Diagrama no generado:", conditionMessage(e), "\n"))
  }

  # -- [4] CONVERGENTE --
  cat("\n  [4] Validez convergente (AVE, CR)...\n")
  ave_cr <- modulo_convergente(afc, nombre)
  if (!is.null(ave_cr)) {
    write.csv(ave_cr, file.path(dir_inst,"4_convergente_AVE_CR.csv"), row.names=FALSE)
    cat(sprintf("    AVE>=.50: %d/%d | CR>=.70: %d/%d\n",
      sum(ave_cr$AVE>=0.50), nrow(ave_cr), sum(ave_cr$CR>=0.70), nrow(ave_cr)))
  }

  # -- [5] DISCRIMINANTE --
  # Para instrumentos unidimensionales (HI_DOC) la validez discriminante
  # no aplica: no existen pares de factores que comparar. modulo_discriminante()
  # devuelve NULL automáticamente cuando el modelo tiene < 2 factores.
  cat("\n  [5] Validez discriminante (Fornell-Larcker + HTMT)...\n")
  discr <- modulo_discriminante(afc, ave_cr, nombre)
  if (!is.null(discr)) {
    write.csv(discr$tabla, file.path(dir_inst,"5_discriminante_FL_HTMT.csv"), row.names=FALSE)
    write.csv(as.data.frame(discr$phi), file.path(dir_inst,"5_correlaciones_factores.csv"), row.names=TRUE)
  } else {
    cat("    [Discriminante] No aplica: <2 factores (instrumento unidimensional)\n")
  }

  # -- [6] INVARIANZA --
  cat("\n  [6] Invarianza de medicion...\n")
  inv <- modulo_invarianza(df_raw, df_num, afc, nombre)
  if (!is.null(inv))
    write.csv(inv$tabla, file.path(dir_inst,"6_invarianza.csv"), row.names=FALSE)

  } # fin if (!es_ctx)

  # -- COMPLEMENTARIOS por subscala -- (aplican a TODOS los instrumentos)
  # Para instrumentos unidimensionales (es_unidimensional=TRUE) los grupos de
  # ítems definidos en subscalas son agrupaciones de escala de respuesta, no
  # subdimensiones teóricas. Se colapsan en una única entrada para que TCT/AFE/
  # IRT/Redundancia reflejen correctamente la estructura de 1 factor.
  cat("\n  [Comp] TCT, AFE, IRT, Redundancia por subscala...\n")
  subs_comp <- if (isTRUE(cfg$es_unidimensional)) {
    etq <- paste0(nombre, "_Global")
    setNames(list(unique(unlist(cfg$subscalas))), etq)
  } else {
    cfg$subscalas
  }
  todas_flags <- list(); redund_res <- list()
  for (sub in names(subs_comp)) {
    tryCatch({
      cols <- intersect(subs_comp[[sub]], names(df_num))
      if (length(cols)<2) next
      df_s <- df_num[,cols,drop=FALSE]
      sdir <- file.path(dir_inst, sub); dir.create(sdir, showWarnings=FALSE)
      tct <- analisis_tct(df_s, sub)
      afe <- if (length(cols)>=4) analisis_afe(df_s, sub) else NULL
      irt <- analisis_irt(df_s, sub)
      rdn <- analisis_redundancia(df_s, sub)
      fl  <- tabla_flags(tct, afe, irt, cols, rdn); fl$subscala <- sub
      todas_flags[[sub]] <- fl
      if (!is.null(tct))  write.csv(tct$items,  file.path(sdir,"TCT_items.csv"),    row.names=FALSE)
      if (!is.null(afe))  write.csv(afe$cargas,  file.path(sdir,"AFE_cargas.csv"),  row.names=FALSE)
      if (!is.null(irt)&&!is.null(irt$params)) write.csv(irt$params, file.path(sdir,"IRT_params.csv"), row.names=FALSE)
      if (!is.null(irt)&&!is.null(irt$fit))    write.csv(irt$fit,    file.path(sdir,"IRT_ajuste.csv"), row.names=FALSE)
      if (!is.null(rdn)) {
        write.csv(rdn$pares,   file.path(sdir,"REDUND_pares.csv"),   row.names=FALSE)
        write.csv(rdn$resumen, file.path(sdir,"REDUND_resumen.csv"), row.names=FALSE)
        redund_res[[sub]] <- rdn$resumen
        if (!is.null(rdn$heatmap)) seg(
          ggsave(file.path(sdir,"REDUND_heatmap.png"), rdn$heatmap,
                 width=max(6,length(cols)*0.55+1), height=max(5,length(cols)*0.5+1), dpi=150))
      }
      write.csv(fl, file.path(sdir,"DECISION_items.csv"), row.names=FALSE)
      cat(sprintf("    %-22s alpha=%.3f omega=%.3f CIP=%.3f | ROJO=%d AMARILLO=%d\n",
        sub, coalesce(tct$alpha,NA_real_), coalesce(tct$omega,NA_real_),
        coalesce(rdn$cip,NA_real_),
        sum(fl$DECISION=="ROJO ELIMINAR",na.rm=TRUE),
        sum(fl$DECISION=="AMARILLO REVISAR",na.rm=TRUE)))
    }, error=function(e) cat("    !! ERROR:",sub,":",conditionMessage(e),"\n"))
  }

  # Global complementario
  tct_g <- seg(analisis_tct(df_num, nombre))
  afe_g <- seg(analisis_afe(df_num, nombre,
    nfactors_manual = if(isTRUE(cfg$es_unidimensional)) 1L else length(cfg$subscalas)))
  irt_g <- if(nrow(df_num)>=200&&ncol(seleccionar_validas(df_num,30))<=50)
              seg(analisis_irt(df_num,nombre)) else NULL
  rdn_g <- seg(analisis_redundancia(df_num, nombre))
  fl_g  <- tabla_flags(tct_g, afe_g, irt_g, items_disp, rdn_g)
  fl_g$subscala <- "GLOBAL"
  flags_todas <- dplyr::bind_rows(c(todas_flags, list(fl_g)))
  write.csv(flags_todas, file.path(dir_inst,"DECISION_items_GLOBAL.csv"), row.names=FALSE)
  if (!is.null(afe_g)) write.csv(afe_g$cargas, file.path(dir_inst,"AFE_global_cargas.csv"), row.names=FALSE)
  if (length(redund_res)>0) write.csv(dplyr::bind_rows(redund_res),
    file.path(dir_inst,"REDUND_resumen_subscalas.csv"), row.names=FALSE)
  if (!is.null(rdn_g)&&!is.null(rdn_g$heatmap)&&length(items_disp)<=60)
    seg(ggsave(file.path(dir_inst,"REDUND_heatmap_global.png"), rdn_g$heatmap,
               width=max(8,length(items_disp)*0.35+2), height=max(7,length(items_disp)*0.30+2), dpi=150))

  # MC/VF — solo frecuencias
  if (!is.null(cfg$mc_cols)&&length(cfg$mc_cols)>0) {
    mc_d <- intersect(cfg$mc_cols, names(df_raw))
    if (length(mc_d)>0) {
      mc_f <- dplyr::bind_rows(lapply(mc_d, function(col) {
        tb <- table(df_raw[[col]],useNA="always")
        data.frame(item=col,respuesta=names(tb),n=as.integer(tb),
                   pct=round(as.numeric(prop.table(tb))*100,1),stringsAsFactors=FALSE)
      }))
      write.csv(mc_f, file.path(dir_inst,"MC_frecuencias.csv"), row.names=FALSE)
      cat("    MC/VF:",length(mc_d),"items -> MC_frecuencias.csv (requieren clave)\n")
    }
  }
  cat("  >> Resultados en:", dir_inst, "\n")
  invisible(list(nombre=nombre, calidad=cal, conf=conf$tabla, afc=if(!is.null(afc)) afc$tabla_fi else NULL,
                 ave_cr=ave_cr, discr=if(!is.null(discr)) discr$tabla else NULL,
                 inv=if(!is.null(inv)) inv$tabla else NULL, flags=flags_todas))
}

# ---------------------------------------------------------------------------- #
# 14. CTXT_DIR (n~78; solo calidad + TCT; sin AFC/invarianza)
# ---------------------------------------------------------------------------- #
analizar_ctx_dir <- function() {
  cat("\n",strrep("=",65),"\n  CTXT_DIR — descriptivo (n~78; sin AFC ni IRT)\n",strrep("=",65),"\n")
  df_raw  <- leer_pre("CTXT_DIR.xlsx")
  dir_out <- file.path(ruta_salida,"CTXT_DIR")
  dir.create(dir_out, showWarnings=FALSE, recursive=TRUE)
  sus_ord <- intersect(paste0("Q",37:43), names(df_raw))
  if (length(sus_ord)>0) {
    df_ord <- recodificar_df(df_raw[,sus_ord,drop=FALSE])
    cal    <- modulo_calidad(df_raw, df_ord, sus_ord, "CTXT_DIR")
    write.csv(cal$por_item,  file.path(dir_out,"1_calidad_por_item.csv"),  row.names=FALSE)
    write.csv(cal$por_nivel, file.path(dir_out,"1_N_por_nivel.csv"),       row.names=FALSE)
    tct <- seg(analisis_tct(df_ord,"CTXT_DIR_ord"))
    if (!is.null(tct)) {
      write.csv(tct$items, file.path(dir_out,"2_TCT_items.csv"), row.names=FALSE)
      cat("  Q37-43: alpha=",tct$alpha," omega=",tct$omega,"\n")
    }
    rdn <- seg(analisis_redundancia(df_ord,"CTXT_DIR_ord"))
    if (!is.null(rdn)) {
      write.csv(rdn$pares,   file.path(dir_out,"REDUND_pares.csv"),   row.names=FALSE)
      write.csv(rdn$resumen, file.path(dir_out,"REDUND_resumen.csv"), row.names=FALSE)
    }
    cat("  NOTA: n~78 insuficiente para AFC confiable (minimo recomendado n>=200 con WLSMV).\n")
  }
  sus_bin <- intersect(paste0("Q",21:28), names(df_raw))
  if (length(sus_bin)>0) {
    pb <- dplyr::bind_rows(lapply(sus_bin, function(col) {
      v <- df_raw[[col]]
      data.frame(item=col, p_si=round(mean(v=="Si",na.rm=TRUE),3), n=sum(!is.na(v)), stringsAsFactors=FALSE)
    }))
    write.csv(pb, file.path(dir_out,"Binarias_proporciones.csv"), row.names=FALSE)
  }
  cat("  >> Resultados en:", dir_out, "\n")
}

# ---------------------------------------------------------------------------- #
# 15. EJECUCIÓN PRINCIPAL
# ---------------------------------------------------------------------------- #
cat("\n",strrep("#",65),"\n")
cat("  FASE 1 — ANÁLISIS PSICOMÉTRICO\n")
cat("  Modulos: [1]Calidad [2]Confiabilidad [3]AFC-WLSMV [4]AVE/CR [5]Discriminante [6]Invarianza\n")
cat("  Complementarios: TCT | AFE | IRT-GRM | Redundancia\n")
cat("  Estimador AFC: WLSMV (ordered=TRUE) — variables ordinales\n")
cat("  Inicio:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n",strrep("#",65),"\n")

RESULTADOS <- list()
for (nm in names(INSTRUMENTOS))
  RESULTADOS[[nm]] <- tryCatch(
    analizar_instrumento(INSTRUMENTOS[[nm]], nm),
    error=function(e){ cat("\n!! ERROR en",nm,":",conditionMessage(e),"\n"); NULL })
analizar_ctx_dir()

# ---------------------------------------------------------------------------- #
# 16. RESUMEN GLOBAL (6 modulos, un instrumento por fila)
# ---------------------------------------------------------------------------- #
cat("\n\n",strrep("=",65),"\n  RESUMEN GLOBAL\n",strrep("=",65),"\n\n")
res_global <- dplyr::bind_rows(lapply(names(RESULTADOS), function(nm) {
  r <- RESULTADOS[[nm]]; if(is.null(r)) return(NULL)
  afc_r <- if(!is.null(r$afc))    r$afc    else data.frame(CFI=NA,TLI=NA,RMSEA=NA,SRMR=NA,interpretacion=NA_character_)
  if (!"interpretacion" %in% names(afc_r)) afc_r$interpretacion <- NA_character_
  conf_g<- if(!is.null(r$conf))   r$conf[r$conf$subscala=="GLOBAL",,drop=FALSE] else data.frame(alpha=NA,omega=NA)
  ave_g <- if(!is.null(r$ave_cr)) r$ave_cr else data.frame(AVE=NA,CR=NA)
  disc_g<- if(!is.null(r$discr))  r$discr  else data.frame(flag_FL=NA)
  inv_g <- if(!is.null(r$inv))    r$inv    else data.frame(tipo=NA,decision=NA)
  cal_g <- if(!is.null(r$calidad))r$calidad$resumen else data.frame(N_total=NA,pct_NA_global=NA)
  es_ctx_r <- isTRUE(INSTRUMENTOS[[nm]]$es_contexto)
  data.frame(
    instrumento=nm,
    tipo        = ifelse(es_ctx_r, "Contexto (descriptivo)", "Psicometrico"),
    N=coalesce(cal_g$N_total[1],NA_integer_),
    pct_NA=coalesce(cal_g$pct_NA_global[1],NA_real_),
    alpha=coalesce(conf_g$alpha[1],NA_real_), omega=coalesce(conf_g$omega[1],NA_real_),
    CFI    = ifelse(es_ctx_r, NA_real_, coalesce(afc_r$CFI[1],   NA_real_)),
    TLI    = ifelse(es_ctx_r, NA_real_, coalesce(afc_r$TLI[1],   NA_real_)),
    RMSEA  = ifelse(es_ctx_r, NA_real_, coalesce(afc_r$RMSEA[1], NA_real_)),
    SRMR   = ifelse(es_ctx_r, NA_real_, coalesce(afc_r$SRMR[1],  NA_real_)),
    nota_3_6 = ifelse(es_ctx_r,
      "No aplica: items formativos/descriptivos",
      coalesce(afc_r$interpretacion[1], "")),
    AVE_min= ifelse(es_ctx_r, NA_real_, round(min(ave_g$AVE,na.rm=TRUE),3)),
    AVE_ok = ifelse(es_ctx_r, NA_integer_, sum(ave_g$AVE>=0.50,na.rm=TRUE)),
    CR_min = ifelse(es_ctx_r, NA_real_,  round(min(ave_g$CR,na.rm=TRUE),3)),
    FL_fail  = ifelse(es_ctx_r | is.null(r$discr), NA_integer_,
                      sum(disc_g$flag_FL != "", na.rm = TRUE)),
    nota_discr = dplyr::case_when(
      es_ctx_r        ~ "No aplica (CTX)",
      is.null(r$discr)~ "No aplica (1 factor)",
      TRUE            ~ ""),
    inv_metrica=ifelse(es_ctx_r, "No aplica",
      coalesce(inv_g$decision[inv_g$tipo=="Metrica"][1], NA_character_)),
    inv_escalar=ifelse(es_ctx_r, "No aplica",
      coalesce(inv_g$decision[inv_g$tipo=="Escalar"][1], NA_character_)),
    stringsAsFactors=FALSE)
}))
if (!is.null(res_global)&&nrow(res_global)>0) {
  print(res_global, row.names=FALSE)
  write.csv(res_global, file.path(ruta_salida,"00_RESUMEN_GLOBAL_6modulos.csv"), row.names=FALSE)
}
lista_flags <- Filter(Negate(is.null), lapply(names(RESULTADOS), function(nm) {
  r <- RESULTADOS[[nm]]
  if (!is.null(r)&&!is.null(r$flags)) { r$flags$instrumento <- nm; r$flags }
}))
if (length(lista_flags)>0)
  write.csv(dplyr::bind_rows(lista_flags),
            file.path(ruta_salida,"00_TODOS_flags.csv"), row.names=FALSE)
cat("\n  Fin:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n  Resultados en:",ruta_salida,"\n\n")
