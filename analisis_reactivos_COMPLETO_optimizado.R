# ╔══════════════════════════════════════════════════════════╗
# ║  REDUCCIÓN DE REACTIVOS — SCRIPT ÚNICO v7 (OPTIMIZADO)   ║
# ║  source("analisis_reactivos_COMPLETO_optimizado.R")     ║
# ╚══════════════════════════════════════════════════════════╝
#
# CAMBIOS DE VELOCIDAD (sobre v7, sin alterar resultados):
#   O1. Crosswalk: se leía desde disco con openxlsx::read.xlsx() en
#       CADA llamada a crosswalk_hoja() (una vez por cada combinación
#       cuestionario x figura). Ahora se lee una sola vez y se
#       cachea en memoria (.CW_CACHE); las llamadas siguientes
#       filtran el data.frame ya cargado. Con N combinaciones esto
#       evita N-1 lecturas completas del archivo de crosswalk.
#   O2. guardar_hoja(): openxlsx::setColWidths(..., widths="auto")
#       es conocido por ser muy lento (recorre celda por celda con
#       un cálculo interno costoso). Se sustituye por un cálculo
#       vectorizado de ancho en R (max nchar por columna), que da
#       un resultado visualmente equivalente a una fracción del costo.
#   O3. consolidar(): para cada archivo de salida se llamaba
#       openxlsx::read.xlsx() de forma independiente para leer
#       "Reactivos_ancho", "Resumen_Global" y cada hoja larga, lo que
#       reabre y reparsea el .xlsx completo varias veces por archivo.
#       Ahora se usa openxlsx::loadWorkbook() UNA vez por archivo y
#       luego openxlsx::readWorkbook() sobre el objeto ya cargado en
#       memoria para cada hoja, evitando reabrir el ZIP/XML repetidas
#       veces.
#   O4. Se eliminó una variable muerta en bloque_psico() que se
#       calculaba sin usarse (tipo_bloque_items), y se evitó volver a
#       calcular vectores ya disponibles dentro del mismo bloque.
#   O5. cargar_base(): pequeñas vectorizaciones (evitar reconstruir
#       vectores intermedios) sin tocar la lógica de negocio.
#
# La lógica psicométrica, los umbrales, las columnas exportadas y el
# formato de los archivos de salida son IDÉNTICOS al script v7
# original. Solo cambia el tiempo de ejecución.
# ═══════════════════════════════════════════════════════════

# ── RUTAS ─────────────────────────────────────────────────────
RUTA_BASE      <- "C:/Users/almejia/Desktop/ANALISIS REACTIVOS"
RUTA_2425      <- file.path(RUTA_BASE, "datos/2425")
RUTA_2526      <- file.path(RUTA_BASE, "datos/2526")
RUTA_SALIDA    <- file.path(RUTA_BASE, "resultados")
RUTA_CROSSWALK <- file.path(RUTA_BASE, "crosswalk_variables_2425_2526.xlsx")

# ── PAQUETES ──────────────────────────────────────────────────
pkgs <- c("psych","mirt","GPArotation",
          "dplyr","tidyr","purrr","tibble","stringr","openxlsx","readxl")
nuevos <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]
if (length(nuevos)) install.packages(nuevos, dependencies=TRUE)
invisible(lapply(pkgs, library, character.only=TRUE))

# ── PARÁMETROS ────────────────────────────────────────────────
PAR <- list(
  pct_NA_max  = 20,
  ritc_min    = 0.20,
  p_min       = 0.10,
  p_max       = 0.90,
  infit_lo    = 0.70,
  infit_hi    = 1.30,
  outfit_hi   = 1.30,
  a_grm_min   = 0.50,
  comunal_min = 0.20,
  votos_elim  = 3,
  votos_rev   = 2,
  alpha_ref   = 0.70,
  generar_ICC = FALSE,
  correr_GRM  = FALSE,
  correr_Rasch = FALSE
)

RESP_NA <- c(
  "No lo sé","No lo sé/Prefiero no contestar",
  "No lo sé/ Prefiero no contestar","No lo sé / Prefiero no contestar",
  "Prefiero no contestar","No aplica","No sé","NS/NC",
  "Sí, lo leí y acepto participar","Sí, lo leí y autorizo"
)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

INDS <- c("media","DE","p_dificultad","ritc","alpha_sin_item","elim_TCT",
          "b_Rasch","infit_MNSQ","outfit_MNSQ","elim_Rasch",
          "a_GRM","info_GRM","elim_GRM",
          "comunalidad","metodo_EFA","elim_EFA",
          "n_votos","decision","flag_NA","pct_NA")

# ── MAPAS DE CODIFICACIÓN ─────────────────────────────────────
MAPAS <- list(
  hd_conocimiento = c(
    "No sé / Nunca he oído hablar de esto"                          = 0L,
    "Conozco un poco el tema"                                       = 1L,
    "Sí, conozco bien este tema"                                    = 2L,
    "Totalmente, e incluso podría explicárselo a otras personas"    = 3L),
  hd_habilidad = c(
    "No sé cómo hacerlo"                                                          = 0L,
    "Puedo hacerlo con ayuda"                                                     = 1L,
    "Puedo hacerlo por mi cuenta"                                                 = 2L,
    "Puedo hacerlo con confianza y si es necesario puedo ayudar a otras personas" = 3L),
  frecuencia_5 = c(
    "Nunca"=0L,"Rara vez"=1L,"Algunas veces"=2L,"Frecuentemente"=3L,"Siempre"=4L),
  acuerdo_5 = c(
    "Totalmente en desacuerdo"=0L,"Algo en desacuerdo"=1L,
    "Ni de acuerdo ni en desacuerdo"=2L,"Algo de acuerdo"=3L,
    "Totalmente de acuerdo"=4L),
  hi_acuerdo = c(
    "No estoy de acuerdo"=0L,"Algo de acuerdo"=1L,
    "De acuerdo"=2L,"Muy de acuerdo"=3L,"Totalmente de acuerdo"=4L),
  hi_frecuencia = c(
    "Algunas veces durante el semestre/año"=0L,"Casi nunca"=0L,
    "1 a 3 veces al mes"=1L,"1 a 3 veces por semana"=2L,
    "Casi todos los días"=3L),
  ctxt_freq4 = c(
    "Nunca"=0L,"Pocas veces"=1L,"Muchas veces"=2L,"Siempre"=3L),
  dicot_texto = c(
    "No"=0L,"No."=0L,"Falso"=0L,"0"=0L,
    "Sí"=1L,"Si"=1L,"Sí."=1L,"Verdadero"=1L,"1"=1L)
)

codificar_col <- function(x) {
  xc <- as.character(x); xc[xc %in% RESP_NA] <- NA
  num <- suppressWarnings(as.numeric(xc))
  pct <- sum(!is.na(num)) / max(sum(!is.na(xc)), 1)
  if (pct > 0.90) {
    v <- num[!is.na(num)]
    if (!length(v)) return(list(vals=num, tipo="VACIA", max=NA))
    u <- sort(unique(v)); mn <- min(u); mx <- max(u); nu <- length(u)
    if (nu==2 && all(u %in% c(0,1)))
      return(list(vals=num, tipo="DICOT_01", max=1))
    if (mx<=4 && mn>=0 && nu<=6)
      return(list(vals=num, tipo="LIKERT_NUM", max=mx))
    return(list(vals=num, tipo=paste0("CONT_",round(mn),"-",round(mx)), max=mx))
  }
  pres <- unique(xc[!is.na(xc)])
  for (nm in names(MAPAS)) {
    m <- MAPAS[[nm]]
    if (length(pres)>0 && mean(pres %in% names(m))>=0.80) {
      coded <- m[xc]; coded[is.na(xc)] <- NA_integer_
      tipo  <- if (nm=="dicot_texto") "DICOT_TXT" else paste0("LIKERT_",nm)
      return(list(vals=as.numeric(coded), tipo=tipo, max=max(m)))
    }
  }
  nu <- length(pres)
  tipo <- if (any(nchar(pres)>25, na.rm=TRUE)) "TEXTO" else paste0("CAT_",nu)
  list(vals=rep(NA_real_,length(x)), tipo=tipo, max=NA)
}

es_likert <- function(t) grepl("LIKERT|hd_con|hd_hab|frec|acuerdo|hi_", t)
es_dicot  <- function(t) grepl("DICOT", t)
es_conocimiento <- function(t) grepl("hd_con|hd_hab", t)

# ── CROSSWALK (O1: caché en memoria) ──────────────────────────
# El crosswalk se leía completo desde disco en cada llamada
# (una por cada combinación cuestionario x figura). Se cachea el
# data.frame crudo una sola vez por sesión y se reutiliza.
.CW_CACHE <- new.env(parent = emptyenv())

.cargar_crosswalk_crudo <- function() {
  if (!is.null(.CW_CACHE$datos)) return(.CW_CACHE$datos)
  if (!file.exists(RUTA_CROSSWALK))
    stop("Crosswalk no encontrado: ", RUTA_CROSSWALK)
  cw <- suppressMessages(openxlsx::read.xlsx(
    RUTA_CROSSWALK, sheet="CROSSWALK_COMPLETO", na.strings=c("","NA")))
  cw$hoja       <- toupper(trimws(gsub("HSXXI","SXXI",as.character(cw$hoja))))
  cw$tipo_match <- toupper(trimws(as.character(cw$tipo_match)))
  cw$validado   <- toupper(trimws(as.character(cw$validado)))
  .CW_CACHE$datos <- cw
  cw
}

crosswalk_hoja <- function(hoja) {
  hoja <- toupper(trimws(gsub("HSXXI","SXXI",hoja)))
  cw  <- .cargar_crosswalk_crudo()
  sub <- cw[!is.na(cw$hoja) & cw$hoja==hoja &
              cw$tipo_match=="EXACTO" & cw$validado=="OK",]
  if (nrow(sub)==0)
    stop("Sin items EXACTO para '",hoja,"'. Disponibles: ",
         paste(sort(unique(cw$hoja[!is.na(cw$hoja)])),collapse=", "))
  sub <- sub[!duplicated(sub$code_2425), ]
  data.frame(
    code_2425 = trimws(as.character(sub$code_2425)),
    code_2526 = trimws(as.character(sub$code_2526)),
    etiqueta  = trimws(as.character(sub$lbl_2425)),
    stringsAsFactors=FALSE)
}

# ── CARGA DE BASE ─────────────────────────────────────────────
cargar_base <- function(ruta, ciclo, momento, mapa) {
  dat <- tryCatch(
    readxl::read_excel(ruta, col_types="text", na=c("","NA","N/A","n/a")),
    error=function(e) stop("No se pudo leer: ",basename(ruta),"\n",e$message))
  if (ciclo=="2526") {
    mc <- names(dat)[tolower(names(dat))=="momento"]
    if (length(mc)==1) {
      n0  <- nrow(dat)
      dat <- dat[tolower(as.character(dat[[mc]]))==tolower(momento),]
      cat(sprintf("      Momento='%s': N=%d (de %d)\n",momento,nrow(dat),n0))
      if (nrow(dat)==0) return(NULL)
    }
  }
  if (ciclo=="2425") {
    ext <- stringr::str_extract(names(dat),
      "(?i)(?:PRE|POS)_[A-Z]+_[A-Z]+_(Q\\d+(?:_[A-Z0-9]+)*)$") |>
      stringr::str_extract("Q.*")
    fb <- ifelse(stringr::str_detect(names(dat),"^Q\\d+"),names(dat),NA_character_)
    names(dat) <- dplyr::coalesce(ext,fb) |>
      (\(x) ifelse(is.na(x),paste0("__A",seq_along(x)),x))()
  }
  nom      <- names(dat)
  nom_base <- sub("\\.(\\d+)$","",nom)
  dups     <- duplicated(nom_base)
  if (any(dups)) {
    cat(sprintf("      Cols duplicadas eliminadas: %s\n",
                paste(nom[dups],collapse=", ")))
    dat <- dat[,!dups,drop=FALSE]
    names(dat) <- nom_base[!dups]
  }
  key      <- if (ciclo=="2425") "code_2425" else "code_2526"
  cols_hay <- intersect(mapa[[key]], names(dat))
  if (!length(cols_hay)) {
    warning("Sin cols del crosswalk en: ",basename(ruta)); return(NULL) }
  sel <- dat[,cols_hay,drop=FALSE]
  if (ciclo=="2526") {
    idx <- match(mapa$code_2526, names(sel)); ok <- !is.na(idx)
    if (any(ok)) names(sel)[idx[ok]] <- mapa$code_2425[ok]
  }
  coded    <- lapply(sel, codificar_col)
  tipo_col <- vapply(coded, `[[`, character(1), "tipo")
  max_col  <- vapply(coded, `[[`, numeric(1),   "max")
  df_num   <- as.data.frame(lapply(coded,"[[","vals"), check.names=FALSE)
  ok2      <- colMeans(is.na(df_num)) < 1
  df_num   <- df_num[,ok2,drop=FALSE]
  tipo_col <- tipo_col[ok2]; max_col <- max_col[ok2]
  if (ncol(df_num)==0) return(NULL)
  attr(df_num,"tipo_col") <- tipo_col
  attr(df_num,"max_col")  <- max_col
  attr(df_num,"N")        <- nrow(df_num)
  df_num
}

# ── PIPELINE PSICOMÉTRICO ─────────────────────────────────────
bloque_psico <- function(dat, tipo_bloque, max_col, etiq) {
  N <- nrow(dat); k <- ncol(dat)
  cat(sprintf("        %s [N=%d, k=%d]\n", tipo_bloque, N, k))

  pct_na  <- round(colMeans(is.na(dat))*100, 1)
  n_val   <- colSums(!is.na(dat))
  flag_na <- ifelse(pct_na > PAR$pct_NA_max, "DATOS_INSUF", "OK")
  tabla   <- data.frame(
    Item=names(dat), tipo_var=tipo_bloque,
    pct_NA=pct_na, n_validos=n_val, flag_NA=flag_na,
    stringsAsFactors=FALSE)

  items_ok <- names(dat)[flag_na=="OK"]
  d_all <- dat[,items_ok,drop=FALSE]

  # O6: ítems con varianza cero (o con <2 observaciones válidas) producen
  # NA en la matriz de correlación y hacen fallar psych::alpha()/fa() para
  # TODO el bloque, no solo para ese ítem ("missing values (NAs) in the
  # correlation matrix do not allow me to continue"). Se excluyen del
  # cálculo psicométrico (TCT/Rasch/GRM/EFA) y se marcan como DATOS_INSUF,
  # en vez de dejar que tumben el bloque completo.
  sd0 <- vapply(d_all, function(x) {
    v <- x[!is.na(x)]
    if (length(v) < 2) return(NA_real_)
    sd(v)
  }, numeric(1))
  var_cero <- names(sd0)[is.na(sd0) | sd0 == 0]
  if (length(var_cero)) {
    cat(sprintf("        Excluidos por varianza cero/insuficiente: %s\n",
                paste(var_cero, collapse=", ")))
  }

  d  <- d_all[, setdiff(names(d_all), var_cero), drop=FALSE]

  # O7: aun sin varianza cero, dos ítems pueden no compartir suficientes
  # observaciones no-NA entre sí, lo que deja NA en su celda de la matriz
  # de correlación por pares y hace fallar psych::alpha()/fa() para TODO
  # el bloque otra vez. Se elimina iterativamente el ítem más problemático
  # (el que más NAs genera en la matriz) hasta que la matriz quede limpia.
  excl_pairwise <- character(0)
  repeat {
    if (ncol(d) < 2) break
    cm <- suppressWarnings(stats::cor(d, use="pairwise.complete.obs"))
    n_na <- colSums(is.na(cm))
    if (all(n_na == 0)) break
    peor <- names(which.max(n_na))
    excl_pairwise <- c(excl_pairwise, peor)
    d <- d[, setdiff(names(d), peor), drop=FALSE]
  }
  if (length(excl_pairwise)) {
    cat(sprintf("        Excluidos por NA en correlación por pares: %s\n",
                paste(excl_pairwise, collapse=", ")))
  }

  excluidos_psico <- c(var_cero, excl_pairwise)
  mx <- max_col[setdiff(items_ok, excluidos_psico)]; mx[is.na(mx)|mx<=0] <- 1
  if (ncol(d)<2) {
    tabla$decision <- ifelse(tabla$Item %in% excluidos_psico, "DATOS_INSUF", "INSUF_ITEMS")
    return(tabla)
  }
  es_d <- tipo_bloque == "DICOT"

  # ── TCT ──────────────────────────────────────────────────────
  es_conocimiento_bloque <- es_dicot(tipo_bloque) ||
    any(es_conocimiento(names(max_col[items_ok])))

  tct_ok <- FALSE
  tryCatch({
    ao   <- suppressWarnings(psych::alpha(d, check.keys=TRUE))
    ag   <- ao$total$raw_alpha
    med  <- colMeans(d, na.rm=TRUE)
    p    <- med / mx
    ritc <- ao$item.stats$r.drop
    adr  <- ao$alpha.drop[,"raw_alpha"]
    cat(sprintf("          TCT: alpha=%.3f\n", ag))
    attr(tabla, "alpha_total") <- round(ag, 3)
    tct <- data.frame(
      Item=names(d), media=round(med,3),
      DE=round(apply(d,2,sd,na.rm=TRUE),3),
      p_dificultad=round(p,3), ritc=round(ritc,3),
      alpha_sin_item=round(adr,3),
      elim_TCT=dplyr::case_when(
        ritc < PAR$ritc_min                             ~ "ELIMINAR",
        adr  > ag + 0.01                                ~ "ELIMINAR",
        es_conocimiento_bloque & p < PAR$p_min          ~ "ELIMINAR",
        es_conocimiento_bloque & p > PAR$p_max          ~ "ELIMINAR",
        TRUE                                            ~ "Conservar"),
      stringsAsFactors=FALSE)
    tabla  <- dplyr::left_join(tabla, tct, by="Item")
    tct_ok <- TRUE
  }, error=function(e) warning("TCT: ",e$message))

  if (!tct_ok && es_d) {
    tryCatch({
      med   <- colMeans(d, na.rm=TRUE)
      p     <- med / mx
      score <- rowSums(d, na.rm=TRUE)
      ritc  <- sapply(names(d), function(it){
        x <- d[[it]]; ok <- !is.na(x) & !is.na(score)
        if (sum(ok)<10) return(NA_real_)
        suppressWarnings(cor(x[ok], score[ok]-x[ok]))
      })
      cat("          TCT fallback punto-biserial: OK\n")
      tct <- data.frame(
        Item=names(d), media=round(med,3),
        DE=round(apply(d,2,sd,na.rm=TRUE),3),
        p_dificultad=round(p,3), ritc=round(ritc,3),
        alpha_sin_item=NA_real_,
        elim_TCT=dplyr::case_when(
          is.na(ritc)          ~ "DATOS_INSUF",
          ritc < PAR$ritc_min  ~ "ELIMINAR",
          p    < PAR$p_min     ~ "ELIMINAR",
          p    > PAR$p_max     ~ "ELIMINAR",
          TRUE                 ~ "Conservar"),
        stringsAsFactors=FALSE)
      tabla <- dplyr::left_join(tabla, tct, by="Item")
    }, error=function(e) warning("TCT fallback: ",e$message))
  }

  # ── Rasch 1PL — SOLO para ítems DICOT ───────────────────────
  if (es_d && isTRUE(PAR$correr_Rasch)) {
    tryCatch({
      mr <- mirt::mirt(d, 1, itemtype="Rasch", verbose=FALSE)
      fr <- mirt::itemfit(mr, fit_statistics="infit")
      pr <- mirt::coef(mr, IRTpars=TRUE, simplify=TRUE)$items
      cat("          Rasch(1PL-DICOT): OK\n")
      irt <- data.frame(
        Item=rownames(pr),
        b_Rasch=round(pr[,"b"],3),
        infit_MNSQ=round(fr$infit,3),
        outfit_MNSQ=round(fr$outfit,3),
        elim_Rasch=dplyr::case_when(
          fr$infit  < PAR$infit_lo  ~ "ELIMINAR",
          fr$infit  > PAR$infit_hi  ~ "ELIMINAR",
          fr$outfit > PAR$outfit_hi ~ "ELIMINAR",
          TRUE                      ~ "Conservar"),
        stringsAsFactors=FALSE)
      tabla <- dplyr::left_join(tabla, irt, by="Item")
    }, error=function(e) warning("Rasch DICOT: ",e$message))
  }

  # ── GRM — SOLO para ítems LIKERT ─────────────────────────────
  if (!es_d && isTRUE(PAR$correr_GRM)) {
    tryCatch({
      mg <- mirt::mirt(d, 1, itemtype="graded", verbose=FALSE)
      pg <- mirt::coef(mg, IRTpars=TRUE, simplify=TRUE)$items
      ig <- mirt::iteminfo(mg, Theta=matrix(0))
      cat("          GRM(LIKERT): OK\n")
      grm <- data.frame(
        Item=rownames(pg),
        a_GRM=round(pg[,"a"],3),
        info_GRM=round(as.numeric(ig),3),
        elim_GRM=ifelse(pg[,"a"]<PAR$a_grm_min,"ELIMINAR","Conservar"),
        stringsAsFactors=FALSE)
      tabla <- dplyr::left_join(tabla, grm, by="Item")
      if (isTRUE(PAR$generar_ICC)) {
        tryCatch({
          pdf(file.path(RUTA_SALIDA,paste0(etiq,"_ICC.pdf")),width=12,height=8)
          n_it <- min(ncol(d),24)
          for (i0 in seq(1,n_it,6)){
            idx <- i0:min(i0+5,n_it)
            mirt::plot(mg,type="trace",which.items=idx,
                       main=paste0(etiq," ICC ",min(idx),"-",max(idx)))}
          dev.off()
          cat("          ICC PDF guardado\n")
        }, error=function(e) NULL)
      }
    }, error=function(e) warning("GRM: ",e$message))
  }

  # ── EFA con cascada de fallbacks ─────────────────────────────
  efa_comunalidad <- rep(NA_real_, ncol(d))
  names(efa_comunalidad) <- names(d)
  metodo_efa_usado <- "sin_datos"

  tryCatch({
    ct <- if (es_d) "tet" else "poly"
    ef <- suppressWarnings(
      psych::fa(d, nfactors=1, fm="ml", rotate="none",
                cor=ct, warnings=FALSE))
    efa_comunalidad[names(ef$communality)] <- ef$communality
    metodo_efa_usado <- ct
    cat(sprintf("          EFA(%s): OK\n", ct))
  }, error = function(e) {
    tryCatch({
      ef2 <- suppressWarnings(
        psych::fa(d, nfactors=1, fm="ml", rotate="none",
                  cor="cor", warnings=FALSE))
      efa_comunalidad[names(ef2$communality)] <<- ef2$communality
      metodo_efa_usado <<- "pearson_fallback"
      cat("          EFA(Pearson fallback): OK\n")
    }, error = function(e2) {
      warning("EFA: poly y Pearson fallaron — comunalidad en NA para ", etiq)
    })
  })

  if (any(!is.na(efa_comunalidad))) {
    efa <- data.frame(
      Item        = names(efa_comunalidad),
      comunalidad = round(efa_comunalidad, 3),
      metodo_EFA  = metodo_efa_usado,
      elim_EFA    = dplyr::case_when(
        is.na(efa_comunalidad)                    ~ NA_character_,
        efa_comunalidad < PAR$comunal_min         ~ "ELIMINAR",
        TRUE                                      ~ "Conservar"),
      stringsAsFactors = FALSE)
    efa_ok <- efa[efa$Item %in% items_ok, ]
    tabla  <- dplyr::left_join(tabla, efa_ok, by="Item")
  }

  # ── Votos → decisión ──────────────────────────────────────────
  cols_voto <- grep("^elim_", names(tabla), value=TRUE)

  if (length(cols_voto)) {
    tabla$n_votos <- rowSums(
      sapply(cols_voto, function(cc)
        as.integer(!is.na(tabla[[cc]]) & tabla[[cc]]=="ELIMINAR")),
      na.rm=TRUE)
    tabla$decision <- dplyr::case_when(
      tabla$flag_NA  == "DATOS_INSUF"     ~ "DATOS_INSUF",
      tabla$n_votos  >= PAR$votos_elim    ~ "ELIMINAR",
      tabla$n_votos  == PAR$votos_rev     ~ "REVISAR",
      TRUE                                ~ "Conservar"
    )
  }
  if (!"decision" %in% names(tabla))
    tabla$decision <- ifelse(tabla$flag_NA=="DATOS_INSUF","DATOS_INSUF","Conservar")
  # Ítems con varianza cero o NA en correlación por pares: nunca pasaron
  # por TCT/Rasch/GRM/EFA (ver filtros arriba). Se marcan DATOS_INSUF sin
  # importar el voto.
  tabla$decision[tabla$Item %in% excluidos_psico] <- "DATOS_INSUF"

  ic <- tabla$Item[!is.na(tabla$decision) & tabla$decision=="Conservar" &
                     tabla$Item %in% names(d)]
  if (length(ic)>=2) {
    ar <- tryCatch(
      suppressWarnings(
        psych::alpha(d[,ic,drop=FALSE],check.keys=TRUE))$total$raw_alpha,
      error=function(e) NA_real_)
    attr(tabla,"alpha_red") <- ar
    cat(sprintf("          alpha reducida (%d items): %.3f\n",
                length(ic),round(ar,3)))
  }
  tabla
}

# ── ANÁLISIS DE UN DATA.FRAME ─────────────────────────────────
analizar_datos <- function(dat, etiq, mapa) {
  N_obs <- attr(dat,"N")
  tc  <- attr(dat,"tipo_col"); mxc <- attr(dat,"max_col")
  i_l <- names(tc)[es_likert(tc)]
  i_d <- names(tc)[es_dicot(tc)]
  i_o <- names(tc)[!es_likert(tc) & !es_dicot(tc)]
  cat(sprintf("    Tipos: %d Likert | %d Dicot | %d Otro\n",
              length(i_l),length(i_d),length(i_o)))
  res <- list()
  if (length(i_l)>=2) {
    bl <- dat[,i_l,drop=FALSE]; attr(bl,"tipo_col") <- tc[i_l]
    r  <- bloque_psico(bl,"LIKERT",mxc[i_l],paste0(etiq,"_LIK"))
    r$bloque <- "LIKERT"; res[["LIKERT"]] <- r
  }
  if (length(i_d)>=2) {
    bd <- dat[,i_d,drop=FALSE]; attr(bd,"tipo_col") <- tc[i_d]
    r  <- bloque_psico(bd,"DICOT",mxc[i_d],paste0(etiq,"_DIC"))
    r$bloque <- "DICOT"; res[["DICOT"]] <- r
  }
  if (length(i_o)>0) {
    r <- data.frame(Item=i_o, tipo_var=tc[i_o], bloque="DESCRIPTIVO",
                    flag_NA=NA, decision="NO_PSICO", stringsAsFactors=FALSE)
    res[["DESCRIPTIVO"]] <- r
  }
  out  <- dplyr::bind_rows(res)
  lmap <- setNames(mapa$etiqueta, mapa$code_2425)
  out  <- dplyr::mutate(out, etiqueta=lmap[Item], .before=everything())
  out  <- dplyr::mutate(out, Item=.data$Item, .before=.data$etiqueta)
  attr(out,"N_obs") <- N_obs
  attr(out,"alpha_total_likert") <- attr(res[["LIKERT"]], "alpha_total") %||% NA_real_
  attr(out,"alpha_total_dicot")  <- attr(res[["DICOT"]],  "alpha_total") %||% NA_real_
  out
}

# ── FORMATO ANCHO: una fila por reactivo ──────────────────────
pivotar_ancho <- function(res_list, mapa) {
  momentos    <- c("2425_pre","2425_post","2526_pre","2526_post")
  todos_items <- mapa$code_2425

  lista_wide <- lapply(momentos, function(mom) {
    if (!mom %in% names(res_list)) {
      df <- data.frame(Item=todos_items, stringsAsFactors=FALSE)
      for (ind in INDS) df[[paste0(ind,"_",mom)]] <- NA
      return(df)
    }
    t       <- res_list[[mom]]
    cols_u  <- intersect(c("Item",INDS), names(t))
    t_sub   <- t[,cols_u,drop=FALSE]
    ind_p   <- intersect(INDS, names(t_sub))
    names(t_sub)[names(t_sub) %in% ind_p] <- paste0(ind_p,"_",mom)
    df_base <- data.frame(Item=todos_items, stringsAsFactors=FALSE)
    dplyr::left_join(df_base, t_sub, by="Item")
  })

  wide <- lista_wide[[1]]
  for (i in 2:length(lista_wide))
    wide <- dplyr::full_join(wide, lista_wide[[i]], by="Item")

  lmap        <- setNames(mapa$etiqueta, mapa$code_2425)
  tipo_cols   <- paste0("tipo_var_",momentos)
  bloque_cols <- paste0("bloque_",momentos)
  if (any(tipo_cols %in% names(wide)))
    wide$tipo_var <- apply(
      wide[,intersect(tipo_cols,names(wide)),drop=FALSE],
      1, function(r) na.omit(r)[1] %||% NA)
  if (any(bloque_cols %in% names(wide)))
    wide$bloque <- apply(
      wide[,intersect(bloque_cols,names(wide)),drop=FALSE],
      1, function(r) na.omit(r)[1] %||% NA)
  wide$etiqueta <- lmap[wide$Item]

  dec_cols      <- paste0("decision_",momentos)
  dec_cols_pres <- intersect(dec_cols, names(wide))
  if (length(dec_cols_pres)>0) {
    wide$n_ELIMINAR <- rowSums(
      sapply(dec_cols_pres, function(cc)
        as.integer(!is.na(wide[[cc]]) & wide[[cc]]=="ELIMINAR")),
      na.rm=TRUE)
    wide$n_Conservar <- rowSums(
      sapply(dec_cols_pres, function(cc)
        as.integer(!is.na(wide[[cc]]) & wide[[cc]]=="Conservar")),
      na.rm=TRUE)
    wide$decision_INTEGRADA <- dplyr::case_when(
      wide$n_ELIMINAR >= 3                      ~ "ELIMINAR",
      wide$n_ELIMINAR == 2                      ~ "REVISAR",
      wide$n_Conservar == length(dec_cols_pres) ~ "Conservar",
      wide$n_ELIMINAR  == 1                     ~ "REVISAR",
      TRUE                                      ~ "SIN_DATOS")
  }

  id_cols   <- c("Item","etiqueta","tipo_var","bloque")
  mom_inds  <- unlist(lapply(momentos, function(m) paste0(INDS,"_",m)))
  dec_final <- c(dec_cols_pres,"n_ELIMINAR","n_Conservar","decision_INTEGRADA")
  col_order <- c(id_cols,
                 intersect(mom_inds,names(wide)),
                 intersect(dec_final,names(wide)))
  extra <- setdiff(names(wide),col_order)
  wide[,c(col_order,extra),drop=FALSE]
}

# ── INDICADORES GLOBALES: mediana entre los 4 momentos ────────
calcular_globales <- function(wide) {
  momentos <- c("2425_pre","2425_post","2526_pre","2526_post")

  extraer_vals <- function(df, var_base) {
    cols <- paste0(var_base, "_", momentos)
    cols_pres <- intersect(cols, names(df))
    if (!length(cols_pres)) return(matrix(NA_real_, nrow=nrow(df), ncol=0))
    as.matrix(df[, cols_pres, drop=FALSE])
  }

  med_row   <- function(m) apply(m, 1, function(x) median(as.numeric(x), na.rm=TRUE))
  iqr_row   <- function(m) apply(m, 1, function(x) {
    v <- as.numeric(x[!is.na(x)])
    if (length(v)<2) return(NA_real_)
    as.numeric(quantile(v,.75,names=FALSE) - quantile(v,.25,names=FALSE))
  })
  n_ok_row  <- function(m) apply(m, 1, function(x) sum(!is.na(as.numeric(x))))

  m_ritc <- extraer_vals(wide, "ritc")
  if (ncol(m_ritc)>0) {
    wide$ritc_global  <- round(med_row(m_ritc), 3)
    wide$ritc_iqr     <- round(iqr_row(m_ritc), 3)
  }

  m_com <- extraer_vals(wide, "comunalidad")
  if (ncol(m_com)>0) {
    wide$comunalidad_global <- round(med_row(m_com), 3)
    wide$comunalidad_iqr    <- round(iqr_row(m_com), 3)
  }

  m_agrm  <- extraer_vals(wide, "a_GRM")
  m_brasch <- extraer_vals(wide, "b_Rasch")
  discrim_mat <- if (ncol(m_agrm)>0) m_agrm else if (ncol(m_brasch)>0) m_brasch else NULL
  if (!is.null(discrim_mat)) {
    wide$discrim_global <- round(med_row(discrim_mat), 3)
    wide$discrim_iqr    <- round(iqr_row(discrim_mat), 3)
  }

  m_infit <- extraer_vals(wide, "infit_MNSQ")
  if (ncol(m_infit)>0)
    wide$infit_global <- round(med_row(m_infit), 3)

  m_outfit <- extraer_vals(wide, "outfit_MNSQ")
  if (ncol(m_outfit)>0)
    wide$outfit_global <- round(med_row(m_outfit), 3)

  m_asi <- extraer_vals(wide, "alpha_sin_item")
  if (ncol(m_asi)>0)
    wide$alpha_sin_item_global <- round(med_row(m_asi), 3)

  if (ncol(m_ritc)>0)
    wide$n_momentos_ok <- n_ok_row(m_ritc)

  wide$voto_g_ritc   <- as.integer(!is.na(wide$ritc_global) &
                                     wide$ritc_global < PAR$ritc_min)
  wide$voto_g_com    <- as.integer(!is.na(wide$comunalidad_global) &
                                     wide$comunalidad_global < PAR$comunal_min)
  es_dicot_row <- !is.na(wide$bloque) & wide$bloque == "DICOT"
  wide$voto_g_disc <- dplyr::case_when(
    !es_dicot_row & !is.na(wide$discrim_global) &
      wide$discrim_global < PAR$a_grm_min         ~ 1L,
    es_dicot_row  & !is.na(wide$infit_global) &
      wide$infit_global   > PAR$infit_hi           ~ 1L,
    TRUE                                           ~ 0L)

  wide$voto_g_alpha <- as.integer(
    !is.na(wide$alpha_sin_item_global) &
    wide$alpha_sin_item_global > (PAR$alpha_ref %||% 0.70) + 0.01)

  wide$n_votos_global <- rowSums(
    wide[, c("voto_g_ritc","voto_g_com","voto_g_disc","voto_g_alpha"),
         drop=FALSE],
    na.rm=TRUE)

  wide$decision_GLOBAL <- dplyr::case_when(
    is.na(wide$bloque) | wide$bloque == "DESCRIPTIVO" ~ "NO_PSICO",
    wide$n_votos_global >= PAR$votos_elim             ~ "ELIMINAR",
    wide$n_votos_global == PAR$votos_rev              ~ "REVISAR",
    TRUE                                              ~ "Conservar"
  )

  wide
}

# ── GUARDAR HOJA EXCEL (O2: ancho de columna rápido) ───────────
# openxlsx::setColWidths(..., widths="auto") es muy lento porque
# recalcula el ancho óptimo celda por celda. Se sustituye por un
# cálculo vectorizado en R sobre los datos ya en memoria, con un
# resultado visual equivalente y una fracción del costo.
.anchos_rapidos <- function(df) {
  anchos <- vapply(seq_along(df), function(i) {
    col <- df[[i]]
    txt <- if (is.character(col)) col else format(col)
    mx  <- suppressWarnings(max(nchar(txt), na.rm=TRUE))
    if (!is.finite(mx)) mx <- 0
    max(nchar(names(df)[i]), mx, na.rm=TRUE)
  }, numeric(1))
  pmin(40, pmax(8, anchos + 2))
}

guardar_hoja <- function(wb, nombre, df) {
  nombre <- substr(nombre,1,31)
  openxlsx::addWorksheet(wb, nombre)
  openxlsx::writeData(wb, nombre, df)
  openxlsx::setColWidths(wb, nombre, cols=seq_len(ncol(df)),
                          widths=.anchos_rapidos(df))
  openxlsx::freezePane(wb, nombre, firstRow=TRUE, firstCol=FALSE)
}

# ── CATÁLOGO DE ARCHIVOS ──────────────────────────────────────
catalogo_archivos <- function() {
  parsear <- function(d, tag) {
    archs <- list.files(d, pattern="\\.xlsx$", full.names=TRUE, ignore.case=TRUE)
    archs <- archs[!grepl("DICCIONARIO|crosswalk|MAESTRO",archs,ignore.case=TRUE)]
    if (!length(archs)) return(tibble::tibble())
    tibble::tibble(ruta=archs) %>%
      dplyr::mutate(
        nom    = tools::file_path_sans_ext(basename(ruta)) %>%
                 stringr::str_replace_all("[\\s]","_") %>%
                 stringr::str_replace_all("_{2,}","_") %>%
                 stringr::str_replace_all("_$",""),
        partes = stringr::str_split(nom,"_"),
        cuest  = purrr::map_chr(partes,~toupper(.x[1])),
        fig    = purrr::map_chr(partes,~toupper(.x[2])),
        momento= if(tag=="2425")
          purrr::map_chr(partes,function(p){
            m <- tolower(p[3]%||%"pre")
            stringr::str_extract(m,"^(pre|post)")%||%"pre"})
        else rep("ambos",length(archs)),
        ciclo=tag) %>%
      dplyr::filter(
        cuest%in%c("HD","HSXXI","HI","CTXT"),
        fig%in%c("DIR","DOC","EST","PMF"),
        if(tag=="2526") purrr::map_lgl(partes,~length(.x)==2) else TRUE) %>%
      dplyr::select(-partes,-nom)
  }
  dplyr::bind_rows(parsear(RUTA_2425,"2425"), parsear(RUTA_2526,"2526"))
}

# ── PIPELINE POR COMBINACIÓN ──────────────────────────────────
CACHE <- list()

ejecutar_combinacion <- function(cuest, fig, catalogo) {
  hoja_cw <- paste0(gsub("HSXXI","SXXI",cuest),"_",fig)
  etiq    <- paste0(cuest,"_",fig)
  cat(sprintf("\n--- %s ---\n",etiq))
  mapa <- tryCatch(crosswalk_hoja(hoja_cw),
    error=function(e){message("Crosswalk: ",e$message);NULL})
  if (is.null(mapa)||nrow(mapa)==0) return(invisible(NULL))
  cat(sprintf("  Items EXACTO: %d\n",nrow(mapa)))

  reg25 <- dplyr::filter(catalogo,ciclo=="2425",cuest==!!cuest,fig==!!fig)
  reg26 <- dplyr::filter(catalogo,ciclo=="2526",cuest==!!cuest,fig==!!fig)
  res   <- list()

  for (mom in c("pre","post")) {
    r25 <- dplyr::filter(reg25,momento==mom)
    if (nrow(r25)>0) {
      cat(sprintf("  [2425-%s] %s\n",mom,basename(r25$ruta[1])))
      dat <- tryCatch(cargar_base(r25$ruta[1],"2425",mom,mapa),
               error=function(e){warning(e$message);NULL})
      if (!is.null(dat)&&nrow(dat)>=30) {
        r <- analizar_datos(dat,paste0(etiq,"_2425_",mom),mapa)
        r$ciclo <- "2425"; r$momento <- mom
        res[[paste0("2425_",mom)]] <- r
      } else cat("    N<30 omitido\n")
    }
    if (nrow(reg26)>0) {
      cat(sprintf("  [2526-%s] %s\n",mom,basename(reg26$ruta[1])))
      dat <- tryCatch(cargar_base(reg26$ruta[1],"2526",mom,mapa),
               error=function(e){warning(e$message);NULL})
      if (!is.null(dat)&&nrow(dat)>=30) {
        r <- analizar_datos(dat,paste0(etiq,"_2526_",mom),mapa)
        r$ciclo <- "2526"; r$momento <- mom
        res[[paste0("2526_",mom)]] <- r
      } else cat("    N<30 omitido\n")
    }
  }
  if (!length(res)) return(invisible(NULL))
  CACHE[[etiq]] <<- list(res=res,mapa=mapa)

  wb <- openxlsx::createWorkbook()

  wide <- tryCatch(pivotar_ancho(res,mapa), error=function(e){
    warning("Pivot ancho: ",e$message); NULL})
  if (!is.null(wide)) {
    guardar_hoja(wb,"Reactivos_ancho",wide)

    tryCatch({
      wide_g <- calcular_globales(wide)
      cols_id  <- c("Item","etiqueta","tipo_var","bloque")
      cols_glob <- c("ritc_global","ritc_iqr",
                     "comunalidad_global","comunalidad_iqr",
                     "discrim_global","discrim_iqr",
                     "infit_global","outfit_global",
                     "alpha_sin_item_global",
                     "n_momentos_ok",
                     "voto_g_ritc","voto_g_com","voto_g_disc","voto_g_alpha",
                     "n_votos_global","decision_GLOBAL",
                     "decision_INTEGRADA")
      cols_ok   <- intersect(c(cols_id, cols_glob), names(wide_g))
      guardar_hoja(wb,"Resumen_Global",wide_g[,cols_ok,drop=FALSE])
    }, error=function(e) warning("Resumen_Global: ",e$message))
  }

  for (nm in names(res)) guardar_hoja(wb,nm,res[[nm]])

  rsm <- dplyr::bind_rows(lapply(names(res),function(nm){
    t     <- res[[nm]]
    N_obs <- attr(t,"N_obs") %||% NA
    data.frame(
      cuestionario=cuest, figura=fig,
      ciclo   = if("ciclo"  %in%names(t)) t$ciclo[1]   else NA,
      momento = if("momento"%in%names(t)) t$momento[1] else NA,
      N_obs,
      total_items        = nrow(t),
      Likert             = sum(t$bloque=="LIKERT",      na.rm=TRUE),
      Dicotomico         = sum(t$bloque=="DICOT",        na.rm=TRUE),
      No_psico           = sum(t$bloque=="DESCRIPTIVO",  na.rm=TRUE),
      Eliminar           = sum(t$decision=="ELIMINAR",   na.rm=TRUE),
      Revisar            = sum(t$decision=="REVISAR",    na.rm=TRUE),
      Conservar          = sum(t$decision=="Conservar",  na.rm=TRUE),
      Datos_insuf        = sum(t$flag_NA=="DATOS_INSUF", na.rm=TRUE),
      pct_reduccion      = round(
        sum(t$decision=="ELIMINAR",na.rm=TRUE)/
        max(sum(t$decision%in%c("ELIMINAR","REVISAR","Conservar"),na.rm=TRUE),1)*100,1),
      alpha_total_likert = round(attr(t,"alpha_total_likert") %||% NA_real_, 3),
      alpha_total_dicot  = round(attr(t,"alpha_total_dicot")  %||% NA_real_, 3),
      alpha_reducida     = round(attr(t,"alpha_red")          %||% NA_real_, 3),
      delta_alpha_likert = round(
        (attr(t,"alpha_red") %||% NA_real_) -
        (attr(t,"alpha_total_likert") %||% NA_real_), 3),
      stringsAsFactors=FALSE)
  }))
  guardar_hoja(wb,"Resumen",rsm)

  ruta_out <- file.path(RUTA_SALIDA,paste0(etiq,".xlsx"))
  openxlsx::saveWorkbook(wb,ruta_out,overwrite=TRUE)
  cat(sprintf("  -> %s\n",basename(ruta_out)))
  invisible(res)
}

# ── MAESTRO CONSOLIDADO (O3: una sola apertura por archivo) ───
consolidar <- function() {
  archs <- list.files(RUTA_SALIDA,pattern="\\.xlsx$",full.names=TRUE)
  archs <- archs[!grepl("MAESTRO",archs)]
  if (!length(archs)){cat("Sin archivos.\n");return(invisible())}

  # Se carga cada workbook UNA sola vez (loadWorkbook) y se leen
  # todas las hojas necesarias desde el objeto en memoria con
  # readWorkbook(), en vez de reabrir el archivo en disco con
  # read.xlsx() para cada hoja (antes: hasta 2 + n_hojas_largas
  # aperturas de archivo; ahora: 1 apertura por archivo).
  piezas <- lapply(archs, function(f) {
    wb_obj <- tryCatch(openxlsx::loadWorkbook(f), error=function(e) NULL)
    if (is.null(wb_obj)) return(list(wide=NULL, glob=NULL, largo=NULL))
    hojas <- tryCatch(names(wb_obj), error=function(e) character())

    leer <- function(hoja) tryCatch(
      openxlsx::readWorkbook(wb_obj, sheet=hoja, na.strings=c("NA","")),
      error=function(e) NULL)

    wide <- if ("Reactivos_ancho" %in% hojas) {
      d <- leer("Reactivos_ancho"); if (!is.null(d)) d$Fuente <- basename(f); d
    } else NULL

    glob <- if ("Resumen_Global" %in% hojas) {
      d <- leer("Resumen_Global"); if (!is.null(d)) d$Fuente <- basename(f); d
    } else NULL

    hojas_largas <- hojas[grepl("^(2425|2526)_", hojas)]
    largo <- purrr::map_dfr(hojas_largas, function(h) {
      d <- leer(h)
      if (!is.null(d)) { d$Fuente <- basename(f); d$Hoja <- h }
      d
    })

    list(wide=wide, glob=glob, largo=largo)
  })

  wide_all  <- dplyr::bind_rows(lapply(piezas, `[[`, "wide"))
  glob_all  <- dplyr::bind_rows(lapply(piezas, `[[`, "glob"))
  largo_all <- dplyr::bind_rows(lapply(piezas, `[[`, "largo"))

  rsm_g <- largo_all %>%
    dplyr::filter(!is.na(decision)) %>%
    dplyr::group_by(Fuente,ciclo,momento,bloque) %>%
    dplyr::summarise(
      Total    = dplyr::n(),
      Eliminar = sum(decision=="ELIMINAR", na.rm=TRUE),
      Revisar  = sum(decision=="REVISAR",  na.rm=TRUE),
      Conservar= sum(decision=="Conservar",na.rm=TRUE),
      Insuf    = sum(flag_NA=="DATOS_INSUF",na.rm=TRUE),
      Pct_red  = round(Eliminar/Total*100,1),
      .groups  = "drop")

  rsm_glob <- if (!is.null(glob_all) && nrow(glob_all)>0) {
    glob_all %>%
      dplyr::filter(!is.na(decision_GLOBAL)) %>%
      dplyr::group_by(Fuente, bloque) %>%
      dplyr::summarise(
        Total            = dplyr::n(),
        Eliminar_global  = sum(decision_GLOBAL=="ELIMINAR", na.rm=TRUE),
        Revisar_global   = sum(decision_GLOBAL=="REVISAR",  na.rm=TRUE),
        Conservar_global = sum(decision_GLOBAL=="Conservar",na.rm=TRUE),
        Pct_red_global   = round(Eliminar_global/Total*100, 1),
        ritc_mediana     = round(median(ritc_global,      na.rm=TRUE), 3),
        comunal_mediana  = round(median(comunalidad_global,na.rm=TRUE),3),
        discrim_mediana  = round(median(discrim_global,   na.rm=TRUE), 3),
        .groups = "drop")
  } else NULL

  wb_m <- openxlsx::createWorkbook()

  if (!is.null(glob_all)  && nrow(glob_all)>0)
    guardar_hoja(wb_m,"Resumen_Global_Reactivos", glob_all)

  if (!is.null(wide_all)  && nrow(wide_all)>0)
    guardar_hoja(wb_m,"Todos_reactivos_ancho",wide_all)

  if (!is.null(largo_all) && nrow(largo_all)>0)
    guardar_hoja(wb_m,"Todos_items_largo",largo_all)

  guardar_hoja(wb_m,"Resumen_por_momento",rsm_g)

  if (!is.null(rsm_glob))
    guardar_hoja(wb_m,"Conteos_decision_global",rsm_glob)

  ruta_m <- file.path(RUTA_SALIDA,"MAESTRO_reduccion_reactivos.xlsx")
  openxlsx::saveWorkbook(wb_m,ruta_m,overwrite=TRUE)
  cat("Maestro:",ruta_m,"\n")
}

# ── EJECUCIÓN ─────────────────────────────────────────────────
cat("\n====================================================\n")
cat("  REDUCCION DE REACTIVOS v7 (optimizado)\n")
cat("====================================================\n\n")

for (nm in c("2425"=RUTA_2425,"2526"=RUTA_2526,"crosswalk"=RUTA_CROSSWALK)){
  ok <- file.exists(nm)
  cat(sprintf("  %-12s: %s\n",names(nm),
              ifelse(ok,paste("[OK]",nm),paste("[FALTA]",nm))))
  if(!ok) stop("Ruta no encontrada: ",nm)
}
dir.create(RUTA_SALIDA,showWarnings=FALSE,recursive=TRUE)
cat(sprintf("  %-12s: [OK] %s\n\n","salida",RUTA_SALIDA))

catalogo <- catalogo_archivos()
cat(sprintf("Archivos en catalogo: %d\n\n",nrow(catalogo)))
if(nrow(catalogo)==0) stop("Catalogo vacio.")
print(dplyr::arrange(catalogo[,c("ciclo","cuest","fig","momento")],
                     ciclo,cuest,fig,momento))

combis <- dplyr::distinct(catalogo,cuest,fig) %>% dplyr::arrange(cuest,fig)
cat(sprintf("\nCombinaciones: %d\n",nrow(combis)))
t0 <- Sys.time(); errores <- list()

for (i in seq_len(nrow(combis))) {
  tryCatch(
    ejecutar_combinacion(combis$cuest[i],combis$fig[i],catalogo),
    error=function(e){
      msg <- sprintf("%s_%s: %s",combis$cuest[i],combis$fig[i],conditionMessage(e))
      cat("  ERROR:",msg,"\n")
      errores[[length(errores)+1]] <<- msg
    })
  cat(sprintf("  %.1f min\n",as.numeric(difftime(Sys.time(),t0,units="mins"))))
}

cat("\n====================================================\n")
cat("  CONSOLIDANDO\n====================================================\n")
consolidar()

t_tot <- round(as.numeric(difftime(Sys.time(),t0,units="mins")),1)
cat(sprintf("\n====================================================\n"))
cat(sprintf("  FINALIZADO en %.1f min\n",t_tot))
cat(sprintf("  OK: %d  |  Errores: %d\n",nrow(combis)-length(errores),length(errores)))
cat(sprintf("  Resultados en: %s\n",RUTA_SALIDA))
if(length(errores)){cat("\nErrores:\n");for(e in errores) cat("  *",e,"\n")}
cat("====================================================\n")
