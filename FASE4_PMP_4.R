# ==============================================================================
# FASE 4 — PORCENTAJE DEL MÁXIMO POSIBLE (PMP)
# Escala homologada 0–100% para comparar ítems, dimensiones e instrumentos
# ==============================================================================
# METODOLOGÍA:
#   PMP_ítem  = (media_ítem / cat_max_ítem) × 100
#   PMP_dim   = media(PMP de todos los ítems de la dimensión)
#   PMP_global= media(PMP de todos los ítems del instrumento)
#
# NIVELES DE ANÁLISIS:
#   A. Por ítem (heatmap de PMP)
#   B. Por dimensión / subscala
#   C. Global del instrumento
#   D. Por nivel educativo, CCT y entidad (A, B y C estratificados)
#   E. Comparativo entre instrumentos y figuras
#
# COLORES: PRE = #1A3A6C (azul oscuro) | POST = #F47B20 (naranja fuerte)
# ==============================================================================

# ---------------------------------------------------------------------------- #
# 0. PAQUETES
# ---------------------------------------------------------------------------- #
pkgs <- c("readxl","writexl","openxlsx","dplyr","tidyr","stringr",
          "tibble","forcats","ggplot2","scales","RColorBrewer",
          "patchwork","ggrepel")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl);  library(writexl);  library(openxlsx)
  library(dplyr);   library(tidyr);    library(stringr)
  library(tibble);  library(forcats)
  library(ggplot2); library(scales);   library(RColorBrewer)
  library(patchwork); library(ggrepel)
})
set.seed(42)

theme_f4 <- theme_minimal(base_size=11) +
  theme(plot.title       = element_text(size=12, face="bold", hjust=0, margin=margin(b=4)),
        plot.subtitle    = element_text(size=9, color="#555555", margin=margin(b=8)),
        plot.caption     = element_text(size=7.5, color="#888888", hjust=1),
        plot.background  = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.major = element_line(color="#EEEEEE"),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face="bold", size=9),
        legend.position  = "bottom",
        legend.title     = element_text(size=9, face="bold"),
        legend.text      = element_text(size=8),
        axis.text        = element_text(size=8))
theme_set(theme_f4)

COL_PRE   <- "#1A3A6C"
COL_POST  <- "#F47B20"
COL_MOM   <- c(PRE=COL_PRE, POST=COL_POST)
PAL_NIVEL <- c("#E63946","#457B9D","#2A9D8F","#F4A261","#8338EC","#3A86FF",
               "#06D6A0","#FFB703","#FB8500","#8ECAE6","#219EBC","#023047")

UMBRAL_BAJO  <- 40
UMBRAL_MEDIO <- 60
UMBRAL_ALTO  <- 80

seg <- function(e) tryCatch(e, error=function(x) NULL)
guardar_g <- function(g, path, w=10, h=6)
  if (!is.null(g)) tryCatch(ggsave(path, g, width=w, height=h, dpi=150, bg="white"),
                             error=function(x) NULL)

# ---------------------------------------------------------------------------- #
# 1. RUTAS  ← ADAPTAR
# ---------------------------------------------------------------------------- #
ruta_pre  <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/00_DATOS/Cuestionarios_independientes/"
ruta_post <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/00_DATOS/bases_post/"
ruta_sal  <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/04_PMP/"
dir.create(ruta_sal, showWarnings=FALSE, recursive=TRUE)

# ---------------------------------------------------------------------------- #
# 2. RECODIFICACIÓN
# ---------------------------------------------------------------------------- #
VALORES_NA <- c("No lo sé/Prefiero no contestar","No lo sé/ Prefiero no contestar",
                "No lo sé / Prefiero no contestar","No lo sé",
                "Prefiero no contestar","No aplica","NA","")
COD <- list(
  hd_conocimiento = c("No sé / Nunca he oído hablar de esto"=0,"Conozco un poco el tema"=1,
    "Sí, conozco bien este tema"=2,
    "Totalmente, e incluso podría explicárselo a otras personas"=3),
  hd_habilidad    = c("No sé cómo hacerlo"=0,"Puedo hacerlo con ayuda"=1,
    "Puedo hacerlo por mi cuenta"=2,
    "Puedo hacerlo con confianza y si es necesario puedo ayudar a otras personas"=3),
  hd_frecuencia   = c("Nunca"=0,"Rara vez"=1,"Algunas veces"=2,"Frecuentemente"=3,"Siempre"=4),
  hi_frecuencia   = c("Casi nunca"=0,"Algunas veces durante el semestre/año"=1,
    "1 a 3 veces al mes"=2,"1 a 3 veces por semana"=3,"Casi todos los días"=4),
  hi_acuerdo      = c("No estoy de acuerdo"=0,"Algo de acuerdo"=1,"De acuerdo"=2,
    "Muy de acuerdo"=3,"Totalmente de acuerdo"=4),
  hsxxi_acuerdo   = c("Totalmente en desacuerdo"=0,"Algo en desacuerdo"=1,
    "Ni de acuerdo ni en desacuerdo"=2,"Algo de acuerdo"=3,"Totalmente de acuerdo"=4),
  hsxxi_frecuencia= c("Nunca"=0,"Rara vez"=1,"Algunas veces"=2,"Frecuentemente"=3,"Siempre"=4),
  ctx_frecuencia  = c("Nunca"=0,"Pocas veces"=1,"Muchas veces"=2,"Siempre"=3),
  ctx_receptor    = c("Nada receptivos"=0,"Poco receptivos"=1,"Indiferentes"=2,
    "Medianamente receptivos"=3,"Muy receptivos"=4),
  ctx_acuerdo     = c("Totalmente en desacuerdo"=0,"Algo en desacuerdo"=1,
    "Ni de acuerdo ni en desacuerdo"=2,"Algo de acuerdo"=3,"Totalmente de acuerdo"=4),
  ctx_binaria     = c("No"=0,"Sí"=1)
)
recodificar <- function(vec) {
  v <- as.character(vec); v[v %in% VALORES_NA] <- NA
  for (d in COD) {
    n_v <- sum(!is.na(v)); if (n_v==0) next
    if (sum(v[!is.na(v)] %in% names(d))/n_v >= 0.60) return(as.numeric(d[v]))
  }
  num <- suppressWarnings(as.numeric(v))
  if (mean(!is.na(num), na.rm=TRUE) >= 0.50) return(num)
  rep(NA_real_, length(v))
}
recodificar_df <- function(df) mutate(df, across(everything(), recodificar))

# ---------------------------------------------------------------------------- #
# 3. DETECCIÓN DE CAT_MAX POR ÍTEM
# ---------------------------------------------------------------------------- #
detect_cat_max <- function(vec) {
  v <- na.omit(vec); if (length(v)==0) return(NA_integer_)
  mv <- max(v, na.rm=TRUE)
  if (mv<=1 && length(unique(v))<=2) return(1L)
  if (mv<=3) return(3L)
  return(4L)
}
detectar_cat_max_df <- function(df, items)
  sapply(setNames(items,items), function(c)
    if (c %in% names(df)) detect_cat_max(df[[c]]) else NA_integer_)

# ---------------------------------------------------------------------------- #
# 4. CÁLCULO DE PMP
# ---------------------------------------------------------------------------- #
pmp_vec <- function(vec, cat_max) {
  v <- vec[!is.na(vec)]
  if (length(v)==0 || is.na(cat_max) || cat_max==0) return(NA_real_)
  round(mean(v) / cat_max * 100, 2)
}

pmp_ic <- function(vec, cat_max, B=500) {
  v <- vec[!is.na(vec)]
  if (length(v)<5 || is.na(cat_max)) return(c(lb=NA_real_, ub=NA_real_))
  boots <- replicate(B, mean(sample(v, length(v), replace=TRUE)) / cat_max * 100)
  c(lb=round(quantile(boots, 0.025), 2), ub=round(quantile(boots, 0.975), 2))
}

# Tabla PMP por ítem
calc_pmp_items <- function(df, items, mom, cat_maxs_vec) {
  do.call(rbind, lapply(items, function(col) {
    if (!col %in% names(df)) return(NULL)
    cm <- cat_maxs_vec[col]
    if (is.na(cm)) return(NULL)
    v  <- df[[col]][!is.na(df[[col]])]
    if (length(v)==0) return(NULL)
    ic <- pmp_ic(v, cm)
    data.frame(item=col, n_valid=length(v), cat_max=cm,
               pmp=pmp_vec(v, cm), lb95=ic["lb"], ub95=ic["ub"],
               nivel_pmp=cut(pmp_vec(v,cm),
                              breaks=c(-Inf,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,Inf),
                              labels=c("Bajo","Medio","Alto","Muy alto")),
               momento=mom, stringsAsFactors=FALSE)
  }))
}

# Tabla PMP por dimensión (media de PMP de ítems)
calc_pmp_dim <- function(df, subscalas, mom, cat_maxs_vec) {
  do.call(rbind, lapply(names(subscalas), function(sub) {
    cols   <- intersect(subscalas[[sub]], names(df))
    pmps   <- sapply(cols, function(c) {
      cm <- cat_maxs_vec[c]; if (is.na(cm)) return(NA_real_)
      pmp_vec(df[[c]], cm)
    })
    pmps <- pmps[!is.na(pmps)]
    if (length(pmps)==0) return(NULL)
    n_val <- sum(sapply(cols, function(c) sum(!is.na(df[[c]]))))
    data.frame(dimension=sub, n_items=length(cols),
               n_resp_total=n_val,
               pmp=round(mean(pmps), 2),
               sd_pmp=round(sd(pmps), 2),
               pmp_min=round(min(pmps), 2),
               pmp_max=round(max(pmps), 2),
               nivel_pmp=as.character(cut(mean(pmps),
                 breaks=c(-Inf,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,Inf),
                 labels=c("Bajo","Medio","Alto","Muy alto"))),
               momento=mom, stringsAsFactors=FALSE)
  }))
}

# PMP global (media de PMP de todos los ítems)
calc_pmp_global <- function(df, items, mom, cat_maxs_vec) {
  pmps <- sapply(intersect(items, names(df)), function(c) {
    cm <- cat_maxs_vec[c]; if (is.na(cm)) return(NA_real_)
    pmp_vec(df[[c]], cm)
  })
  pmps <- pmps[!is.na(pmps)]
  if (length(pmps)==0) return(NULL)
  data.frame(nivel="GLOBAL", n_items=length(pmps),
             pmp=round(mean(pmps), 2),
             sd_pmp=round(sd(pmps), 2),
             pmp_min=round(min(pmps), 2),
             pmp_max=round(max(pmps), 2),
             momento=mom, stringsAsFactors=FALSE)
}

# ---------------------------------------------------------------------------- #
# 4B. CÁLCULO GENÉRICO POR GRUPO (nivel / entidad / CCT)
# ---------------------------------------------------------------------------- #
# `grupo_vec` = vector con la variable de agrupación (misma longitud que df)
# `grupo_lbl` = nombre de la columna en el resultado (e.g. "nivel","entidad","cct")

calc_pmp_dim_grupo <- function(df, subscalas, mom, grupo_vec, cat_maxs_vec,
                                grupo_lbl = "nivel") {
  grupos <- unique(as.character(grupo_vec)[!is.na(grupo_vec)])
  do.call(rbind, lapply(names(subscalas), function(sub) {
    cols <- intersect(subscalas[[sub]], names(df))
    do.call(rbind, lapply(grupos, function(gv) {
      idx  <- which(as.character(grupo_vec) == gv)
      pmps <- sapply(cols, function(c) {
        cm <- cat_maxs_vec[c]; if (is.na(cm)) return(NA_real_)
        pmp_vec(df[[c]][idx], cm)
      })
      pmps <- pmps[!is.na(pmps)]
      if (length(pmps)==0) return(NULL)
      row <- data.frame(dimension=sub, n_items=length(cols),
                        n_resp=length(idx),
                        pmp=round(mean(pmps),2),
                        sd_pmp=round(sd(pmps),2),
                        nivel_pmp=as.character(cut(mean(pmps),
                          breaks=c(-Inf,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,Inf),
                          labels=c("Bajo","Medio","Alto","Muy alto"))),
                        momento=mom, stringsAsFactors=FALSE)
      row[[grupo_lbl]] <- gv
      row
    }))
  }))
}

calc_pmp_global_grupo <- function(df, items, mom, grupo_vec, cat_maxs_vec,
                                   grupo_lbl = "nivel") {
  grupos <- unique(as.character(grupo_vec)[!is.na(grupo_vec)])
  do.call(rbind, lapply(grupos, function(gv) {
    idx  <- which(as.character(grupo_vec) == gv)
    pmps <- sapply(intersect(items, names(df)), function(c) {
      cm <- cat_maxs_vec[c]; if (is.na(cm)) return(NA_real_)
      pmp_vec(df[[c]][idx], cm)
    })
    pmps <- pmps[!is.na(pmps)]
    if (length(pmps)==0) return(NULL)
    row <- data.frame(n_items=length(pmps),
                      n_resp=length(idx),
                      pmp=round(mean(pmps),2),
                      sd_pmp=round(sd(pmps),2),
                      momento=mom, stringsAsFactors=FALSE)
    row[[grupo_lbl]] <- gv
    row
  }))
}

# Alias para compatibilidad con funciones de nivel (renombra columna → "nivel")
calc_pmp_dim_nivel    <- function(df, s, m, nv, cm) calc_pmp_dim_grupo(df, s, m, nv, cm, "nivel")
calc_pmp_global_nivel <- function(df, i, m, nv, cm) calc_pmp_global_grupo(df, i, m, nv, cm, "nivel")

# ---------------------------------------------------------------------------- #
# 5. CONFIGURACIÓN INSTRUMENTOS
# ---------------------------------------------------------------------------- #
# nivel_col   : columna de nivel educativo (NULL si no aplica)
# entidad_col : columna de entidad/estado  (NULL si no existe en el cuestionario)
# cct_col     : columna de CCT             (NULL si no existe)
# ← ADAPTAR los nombres de columna según tus archivos reales
INSTRUMENTOS <- list(
  HD_DOC = list(figura="Docente",     archivo_pre="HD_DOC_pre.xlsx",
                archivo_post="HD_DOC_post.xlsx",
                nivel_col=NULL,   entidad_col="Entidad",  cct_col="CCT",
                items=paste0("Q",9:90),
                subscalas=list(Info_Alfab=paste0("Q",9:20),
                               Comunic_Col=paste0("Q",21:43),
                               Gen_Contenido=paste0("Q",44:59),
                               Seguridad=paste0("Q",60:75),
                               Resol_Prob=paste0("Q",76:90)),
                descripcion="Habilidades Digitales — Docentes"),
  HD_EST = list(figura="Estudiante",  archivo_pre="HD_EST_pre.xlsx",
                archivo_post="HD_EST_post.xlsx",
                nivel_col="Nivel",  entidad_col="Entidad",  cct_col="CCT",
                items=paste0("Q",16:97),
                subscalas=list(Info_Alfab=paste0("Q",16:27),
                               Comunic_Col=paste0("Q",28:50),
                               Gen_Contenido=paste0("Q",51:66),
                               Seguridad=paste0("Q",67:82),
                               Resol_Prob=paste0("Q",83:97)),
                descripcion="Habilidades Digitales — Estudiantes"),
  HI_DOC = list(figura="Docente",     archivo_pre="HI_DOC_pre.xlsx",
                archivo_post="HI_DOC_post.xlsx",
                nivel_col=NULL,   entidad_col="Entidad",  cct_col="CCT",
                items=paste0("Q",8:49),
                subscalas=list(Pensamiento_Critico=paste0("Q",8:16),
                               Colaboracion=paste0("Q",17:22),
                               Comunicacion_Aut=paste0("Q",23:30),
                               Creatividad=paste0("Q",31:38),
                               Tecnologia=paste0("Q",39:49)),
                descripcion="Habilidades de Implementación — Docentes"),
  HSXXI_DOC = list(figura="Docente",  archivo_pre="HSXXI_DOC_pre.xlsx",
                   archivo_post="HSXXI_DOC_post.xlsx",
                   nivel_col=NULL,   entidad_col="Entidad",  cct_col="CCT",
                   items=paste0("Q",12:56),
                   subscalas=list(Pens_Critico=paste0("Q",12:22),
                                  Trabajo_Equipo=paste0("Q",23:31),
                                  Comunicacion=paste0("Q",32:36),
                                  Creatividad=paste0("Q",37:41),
                                  Alfab_Info=paste0("Q",42:46),
                                  Alfab_Tecnologico=paste0("Q",47:51),
                                  Resol_Problemas=paste0("Q",52:56)),
                   descripcion="Habilidades Siglo XXI — Docentes"),
  HSXXI_EST = list(figura="Estudiante",archivo_pre="HSXXI_EST_pre.xlsx",
                   archivo_post="HSXXI_EST_post.xlsx",
                   nivel_col="Nivel", entidad_col="Entidad",  cct_col="CCT",
                   items=paste0("Q",16:60),
                   subscalas=list(Pens_Critico=paste0("Q",16:26),
                                  Trabajo_Equipo=paste0("Q",27:35),
                                  Comunicacion=paste0("Q",36:40),
                                  Creatividad=paste0("Q",41:45),
                                  Alfab_Info=paste0("Q",46:50),
                                  Alfab_Tecnologico=paste0("Q",51:55),
                                  Resol_Problemas=paste0("Q",56:60)),
                   descripcion="Habilidades Siglo XXI — Estudiantes")
)

# ---------------------------------------------------------------------------- #
# 6. PALETA SEMÁNTICA DE NIVEL PMP
# ---------------------------------------------------------------------------- #
PAL_NIVEL_PMP <- c("Bajo"="#D7191C","Medio"="#FDAE61",
                    "Alto"="#A6D96A","Muy alto"="#1A9641")
col_nivel_pmp <- function(pmp_val) {
  if (is.na(pmp_val)) return("#CCCCCC")
  if (pmp_val < UMBRAL_BAJO)  return(PAL_NIVEL_PMP["Bajo"])
  if (pmp_val < UMBRAL_MEDIO) return(PAL_NIVEL_PMP["Medio"])
  if (pmp_val < UMBRAL_ALTO)  return(PAL_NIVEL_PMP["Alto"])
  return(PAL_NIVEL_PMP["Muy alto"])
}

# ---------------------------------------------------------------------------- #
# 7. VISUALIZACIONES GLOBALES
# ---------------------------------------------------------------------------- #

prep_dim <- function(df_dim)
  df_dim %>% mutate(dim_lbl=str_replace_all(dimension,"_"," "),
                    momento=factor(momento,levels=c("PRE","POST")))

# ── 01. Barras PMP por dimensión (PRE vs POST) ───────────────────────────────
viz_barras_dim <- function(df_dim, titulo) {
  if (is.null(df_dim)||nrow(df_dim)==0) return(NULL)
  df_p <- prep_dim(df_dim)
  ggplot(df_p, aes(x=reorder(dim_lbl,pmp), y=pmp, fill=momento)) +
    geom_col(position=position_dodge(0.75), width=0.7,
             alpha=0.9, color="white", linewidth=0.3) +
    geom_errorbar(aes(ymin=pmp_min, ymax=pmp_max),
                  position=position_dodge(0.75), width=0.3,
                  linewidth=0.5, alpha=0.5) +
    geom_text(aes(label=sprintf("%.1f%%",pmp)),
              position=position_dodge(0.75), hjust=-0.1,
              size=3.2, fontface="bold") +
    geom_hline(yintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed", color=c("#D7191C","#F4A261","#1A9641"),
               linewidth=0.5) +
    scale_fill_manual(values=COL_MOM, name="Momento") +
    scale_y_continuous(limits=c(0,108), labels=label_number(suffix="%"),
                        breaks=seq(0,100,20)) +
    coord_flip() +
    labs(title=titulo,
         subtitle="PMP por dimensión | Líneas: umbrales Bajo (40%) / Medio (60%) / Alto (80%)",
         x=NULL, y="PMP (%)",
         caption="Barras = PMP medio | Bigotes = rango entre ítems de la dimensión") +
    theme(axis.text.y=element_text(size=9,face="bold"))
}

# ── 02. ★ Bullet chart ───────────────────────────────────────────────────────
viz_bullet <- function(df_dim, titulo) {
  if (is.null(df_dim)||nrow(df_dim)==0) return(NULL)
  df_p <- prep_dim(df_dim) %>%
    mutate(color_barra=sapply(pmp, col_nivel_pmp))
  ggplot(df_p, aes(y=reorder(dim_lbl, pmp))) +
    annotate("rect", xmin=0,  xmax=UMBRAL_BAJO,  ymin=-Inf,ymax=Inf, fill="#FFE6E6", alpha=0.4) +
    annotate("rect", xmin=UMBRAL_BAJO, xmax=UMBRAL_MEDIO, ymin=-Inf,ymax=Inf, fill="#FFF3E0", alpha=0.4) +
    annotate("rect", xmin=UMBRAL_MEDIO,xmax=UMBRAL_ALTO,  ymin=-Inf,ymax=Inf, fill="#F1F8E9", alpha=0.4) +
    annotate("rect", xmin=UMBRAL_ALTO, xmax=100,          ymin=-Inf,ymax=Inf, fill="#E8F5E9", alpha=0.4) +
    geom_col(data=df_p%>%filter(momento=="PRE"),  aes(x=pmp), fill=COL_PRE,  alpha=0.3, width=0.65) +
    geom_col(data=df_p%>%filter(momento=="POST"), aes(x=pmp), fill=COL_POST, alpha=0.9, width=0.3) +
    geom_vline(xintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed", linewidth=0.5, color=c("#D7191C","#F4A261","#1A9641")) +
    geom_text(data=df_p%>%filter(momento=="POST"),
              aes(x=pmp, label=sprintf("%.1f%%",pmp)), hjust=-0.15, size=3.2, color=COL_POST, fontface="bold") +
    geom_text(data=df_p%>%filter(momento=="PRE"),
              aes(x=pmp-2, label=sprintf("%.1f%%",pmp)), hjust=1.1, size=2.8, color=COL_PRE, alpha=0.8) +
    scale_x_continuous(limits=c(0,112), labels=label_number(suffix="%"),
                        breaks=c(0,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,100)) +
    labs(title=titulo,
         subtitle="Barra amplia=PRE (azul) | Barra estrecha=POST (naranja) | Franjas=umbrales",
         x="PMP (%)", y=NULL,
         caption=sprintf("Umbrales: Bajo<%.0f%% | Medio %.0f-%.0f%% | Alto %.0f-%.0f%% | Muy alto>%.0f%%",
                          UMBRAL_BAJO,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_MEDIO,UMBRAL_ALTO,UMBRAL_ALTO)) +
    theme(axis.text.y=element_text(size=9,face="bold"), panel.grid.major.y=element_blank())
}

# ── 03. Heatmap ítem × PMP ───────────────────────────────────────────────────
viz_heatmap_items <- function(df_items, titulo, max_items=50) {
  if (is.null(df_items)||nrow(df_items)==0) return(NULL)
  its <- unique(df_items$item)
  if (length(its)>max_items) its <- its[round(seq(1,length(its),length.out=max_items))]
  df_p <- df_items %>% filter(item %in% its) %>%
    mutate(momento=factor(momento,levels=c("PRE","POST")),
           item=factor(item,levels=rev(its)))
  ggplot(df_p, aes(x=momento, y=item, fill=pmp)) +
    geom_tile(color="white", linewidth=0.35) +
    geom_text(aes(label=ifelse(!is.na(pmp), sprintf("%.0f",pmp),"")),
              size=2.5, color="white", fontface="bold") +
    scale_fill_gradient2(low="#D7191C", mid="#FFFFBF", high="#1A9641",
                          midpoint=50, limits=c(0,100), na.value="grey85", name="PMP (%)") +
    labs(title=titulo, subtitle="Rojo=bajo (<50%) | Amarillo=medio | Verde=alto (>50%)",
         x="Momento", y="Ítem") +
    theme(axis.text.x=element_text(size=11,face="bold",color=c(COL_PRE,COL_POST)),
          axis.text.y=element_text(size=6.5), panel.grid=element_blank(), legend.position="right")
}

# ── 04. ★ Radar PMP ──────────────────────────────────────────────────────────
viz_radar_pmp <- function(df_dim, titulo, color_pre=COL_PRE, color_post=COL_POST) {
  if (is.null(df_dim)||nrow(df_dim)==0) return(NULL)
  dims <- unique(str_replace_all(df_dim$dimension,"_"," "))
  if (length(dims)<3) return(NULL)
  n_d    <- length(dims)
  angles <- seq(0, 2*pi, length.out=n_d+1)[-(n_d+1)]
  df_c <- df_dim %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           dim_idx=match(dim_lbl, dims), ang=angles[dim_idx],
           x=pmp*cos(ang), y=pmp*sin(ang))
  df_cl <- bind_rows(df_c, df_c%>%filter(dim_idx==1)%>%
                       mutate(ang=angles[1],x=pmp*cos(angles[1]),y=pmp*sin(angles[1])))
  df_ejes <- data.frame(ang=angles, x_l=112*cos(angles), y_l=112*sin(angles), label=dims)
  circ_r  <- c(20,40,60,80,100)
  df_circ <- do.call(rbind, lapply(circ_r, function(r) {
    th <- seq(0,2*pi,length.out=180)
    data.frame(x=r*cos(th), y=r*sin(th), r=r)
  }))
  ggplot() +
    geom_path(data=df_circ, aes(x,y,group=r), color="#EEEEEE",linewidth=0.5) +
    geom_segment(data=data.frame(ang=angles),
                 aes(x=0,y=0,xend=103*cos(ang),yend=103*sin(ang)),
                 color="#DDDDDD", linewidth=0.4) +
    geom_text(data=data.frame(r=circ_r,x=circ_r*cos(0.3),y=circ_r*sin(0.3)),
              aes(x,y,label=paste0(r,"%")), size=2.5, color="#AAAAAA") +
    geom_path(data=data.frame(x=UMBRAL_BAJO*cos(seq(0,2*pi,length.out=180)),
                               y=UMBRAL_BAJO*sin(seq(0,2*pi,length.out=180))),
              aes(x,y), color="#D7191C", linetype="dashed", linewidth=0.5, alpha=0.6, inherit.aes=FALSE) +
    geom_path(data=data.frame(x=UMBRAL_ALTO*cos(seq(0,2*pi,length.out=180)),
                               y=UMBRAL_ALTO*sin(seq(0,2*pi,length.out=180))),
              aes(x,y), color="#1A9641", linetype="dashed", linewidth=0.5, alpha=0.6, inherit.aes=FALSE) +
    geom_polygon(data=df_cl%>%filter(momento=="POST"),
                 aes(x,y), fill=color_post, alpha=0.25, color=color_post, linewidth=1.4) +
    geom_polygon(data=df_cl%>%filter(momento=="PRE"),
                 aes(x,y), fill=color_pre, alpha=0.18, color=color_pre, linewidth=1.4) +
    geom_point(data=df_c, aes(x,y,color=momento,shape=momento), size=3.5, alpha=0.95) +
    geom_text_repel(data=df_c%>%filter(momento=="POST"),
                    aes(x,y,label=sprintf("%.1f%%",pmp),color=momento),
                    size=3, fontface="bold", max.overlaps=20, segment.size=0.3) +
    geom_text(data=df_ejes, aes(x_l,y_l,label=label),
              size=3.2, fontface="bold", color="#333333", lineheight=0.85) +
    scale_color_manual(values=c(PRE=color_pre,POST=color_post),name="Momento") +
    scale_shape_manual(values=c(PRE=16,POST=17), name="Momento") +
    coord_equal(xlim=c(-130,130), ylim=c(-130,130)) +
    labs(title=titulo,
         subtitle="Radio = PMP (%) | Rojo punteado = umbral bajo (40%) | Verde = alto (80%)",
         caption="Azul oscuro = PRE | Naranja = POST") +
    theme_void() +
    theme(plot.title=element_text(size=12,face="bold",hjust=0.5,margin=margin(b=4)),
          plot.subtitle=element_text(size=9,color="#555555",hjust=0.5),
          plot.caption=element_text(size=7.5,color="#888888",hjust=1),
          plot.background=element_rect(fill="white",color=NA),
          legend.position="bottom", legend.title=element_text(size=9,face="bold"),
          plot.margin=margin(10,10,10,10))
}

# ── 05. ★ Spoke chart PMP ────────────────────────────────────────────────────
viz_spoke_pmp <- function(df_dim, titulo) {
  if (is.null(df_dim)||nrow(df_dim)==0) return(NULL)
  df_p <- prep_dim(df_dim) %>% mutate(x_pos=as.numeric(factor(dim_lbl)))
  n_d <- n_distinct(df_p$dim_lbl)
  ggplot(df_p, aes(x=factor(x_pos), y=pmp, fill=momento)) +
    geom_col(position=position_dodge(0.78), width=0.72, alpha=0.9, color="white", linewidth=0.4) +
    geom_text(aes(label=sprintf("%.1f%%",pmp)), position=position_dodge(0.78),
              hjust=-0.1, size=3, fontface="bold", color="grey20") +
    geom_hline(yintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed", linewidth=0.4,
               color=c("#D7191C","#F4A261","#1A9641"), alpha=0.6) +
    scale_fill_manual(values=COL_MOM, name="Momento") +
    scale_x_discrete(labels=setNames(unique(df_p$dim_lbl), as.character(seq_len(n_d)))) +
    scale_y_continuous(limits=c(-20,115), breaks=c(0,40,60,80,100),
                        labels=paste0(c(0,40,60,80,100),"%")) +
    coord_polar(theta="x", start=0) +
    labs(title=titulo,
         subtitle="Barras desde el centro | PMP 0–100% | Azul=PRE | Naranja=POST",
         x=NULL, y=NULL,
         caption=sprintf("Líneas: %.0f%% (bajo) / %.0f%% (medio) / %.0f%% (alto)",
                          UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO)) +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.subtitle=element_text(size=9,color="#555555",hjust=0.5),
          plot.caption=element_text(size=7.5,color="#888888",hjust=1),
          plot.background=element_rect(fill="white",color=NA),
          panel.grid=element_line(color="#EEEEEE"),
          axis.text.x=element_text(size=8.5,face="bold"),
          axis.text.y=element_text(size=7,color="#AAAAAA"),
          legend.position="bottom", legend.title=element_text(size=9,face="bold"))
}

# ── 06. ★ Termómetro / gauge por dimensión ──────────────────────────────────
viz_termometro <- function(df_dim, titulo) {
  if (is.null(df_dim)||nrow(df_dim)==0) return(NULL)
  df_p <- prep_dim(df_dim) %>% mutate(color_nivel=sapply(pmp, col_nivel_pmp))
  ggplot(df_p, aes(x=momento, y=pmp, fill=momento)) +
    geom_col(aes(y=100), fill="#F5F5F5", width=0.6) +
    geom_col(width=0.6, alpha=0.9, color="white", linewidth=0.3) +
    geom_hline(data=data.frame(y=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO)),
               aes(yintercept=y), linetype="dashed", linewidth=0.45,
               color=c("#D7191C","#F4A261","#1A9641"), inherit.aes=FALSE) +
    geom_text(aes(label=sprintf("%.1f%%",pmp), y=pmp+3), size=3.2, fontface="bold") +
    scale_fill_manual(values=COL_MOM, name="Momento") +
    scale_y_continuous(limits=c(0,108), labels=label_number(suffix="%"),
                        breaks=c(0,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,100)) +
    facet_wrap(~dim_lbl, nrow=1) +
    labs(title=titulo,
         subtitle="Cada panel = una dimensión | Valor = PMP (%) | PRE azul / POST naranja",
         x=NULL, y="PMP (%)") +
    theme(strip.text=element_text(size=8,face="bold"),
          axis.text.x=element_text(size=9,face="bold",color=c(COL_PRE,COL_POST)),
          legend.position="none")
}

# ── 07. Slope chart PMP PRE→POST ─────────────────────────────────────────────
viz_slope_pmp <- function(df_dim, titulo) {
  if (is.null(df_dim)||nrow(df_dim)==0) return(NULL)
  df_p <- prep_dim(df_dim)
  ggplot(df_p, aes(x=momento, y=pmp, group=dim_lbl, color=dim_lbl)) +
    geom_line(linewidth=1.3, alpha=0.85) +
    geom_point(size=4.5, alpha=0.95) +
    geom_text_repel(data=df_p%>%filter(momento=="POST"),
                    aes(label=sprintf("%s\n%.1f%%",dim_lbl,pmp)),
                    nudge_x=0.15, size=2.8, hjust=0, direction="y",
                    segment.color="grey70", max.overlaps=20) +
    geom_text_repel(data=df_p%>%filter(momento=="PRE"),
                    aes(label=sprintf("%.1f%%",pmp)),
                    nudge_x=-0.15, size=2.8, hjust=1, direction="y",
                    segment.color="grey70", max.overlaps=20) +
    geom_hline(yintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed", linewidth=0.4,
               color=c("#D7191C","#F4A261","#1A9641"), alpha=0.6) +
    scale_y_continuous(labels=label_number(suffix="%"), limits=c(0,105)) +
    scale_color_brewer(palette="Set2") +
    labs(title=titulo, subtitle="Cambio de PMP por dimensión PRE → POST",
         x=NULL, y="PMP (%)") +
    theme(legend.position="none",
          axis.text.x=element_text(size=12,face="bold",color=c(COL_PRE,COL_POST)),
          plot.margin=margin(5,80,5,10))
}

# ── 08. Delta barras (POST − PRE en pp) ──────────────────────────────────────
viz_delta_pmp <- function(df_dim, titulo) {
  if (is.null(df_dim)||nrow(df_dim)==0||!"POST" %in% df_dim$momento) return(NULL)
  df_d <- df_dim %>%
    select(dimension,momento,pmp) %>%
    pivot_wider(names_from=momento,values_from=pmp) %>%
    mutate(delta=round(coalesce(POST,0)-coalesce(PRE,0),2),
           dim_lbl=str_replace_all(dimension,"_"," "),
           color=case_when(delta>2~"Mejora",delta< -2~"Deterioro",TRUE~"Sin cambio"))
  ggplot(df_d, aes(x=reorder(dim_lbl,delta), y=delta, fill=color)) +
    geom_col(width=0.72, alpha=0.9, color="white", linewidth=0.3) +
    geom_hline(yintercept=0, color="#444444", linewidth=0.5) +
    geom_text(aes(label=sprintf("%+.1f pp",delta), hjust=ifelse(delta>=0,-0.1,1.1)),
              size=3.2, fontface="bold") +
    scale_fill_manual(values=c(Mejora="#1A9641",Deterioro="#D7191C","Sin cambio"="#F4A261"),
                      name="Dirección") +
    scale_y_continuous(labels=label_number(suffix=" pp"), breaks=pretty(c(-20,20),n=8)) +
    coord_flip() +
    labs(title=titulo, subtitle="Diferencia POST − PRE en puntos porcentuales de PMP",
         x=NULL, y="Δ PMP (pp)", caption=">2 pp = Mejora | <-2 pp = Deterioro") +
    theme(axis.text.y=element_text(size=9,face="bold"))
}

# ── 09. Dot-plot con IC 95% por ítem ────────────────────────────────────────
viz_dotplot_ic <- function(df_items, subscalas, titulo, max_items=30) {
  if (is.null(df_items)||nrow(df_items)==0) return(NULL)
  its <- unique(df_items$item)
  if (length(its)>max_items) its <- its[round(seq(1,length(its),length.out=max_items))]
  df_p <- df_items %>% filter(item %in% its) %>%
    mutate(momento=factor(momento,levels=c("PRE","POST")),
           item=factor(item,levels=rev(its)))
  ggplot(df_p, aes(x=pmp, y=item, color=momento)) +
    geom_linerange(aes(xmin=lb95, xmax=ub95), linewidth=0.6, alpha=0.5) +
    geom_point(size=2.8, alpha=0.9) +
    geom_vline(xintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed", linewidth=0.4,
               color=c("#D7191C","#F4A261","#1A9641"), alpha=0.6) +
    scale_color_manual(values=COL_MOM, name="Momento") +
    scale_x_continuous(limits=c(0,105), labels=label_number(suffix="%"),
                        breaks=c(0,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,100)) +
    labs(title=titulo, subtitle="Punto = PMP | Segmento = IC 95% bootstrap (B=500)",
         x="PMP (%)", y="Ítem") +
    theme(axis.text.y=element_text(size=6.5))
}

# ── 10. ★ Waffle PMP global ──────────────────────────────────────────────────
viz_waffle_pmp <- function(df_global, titulo) {
  if (is.null(df_global)||nrow(df_global)==0) return(NULL)
  df_p <- df_global %>% mutate(momento=factor(momento,levels=c("PRE","POST")))
  do.call(rbind, lapply(unique(df_p$momento), function(mom) {
    pmp_val <- df_p$pmp[df_p$momento==mom]
    if (length(pmp_val)==0) return(NULL)
    pmp_val <- pmp_val[1]
    n_fill  <- max(0, min(100, round(pmp_val)))
    data.frame(col=rep(1:10,times=10)[seq_len(100)],
               row=rep(1:10,each=10)[seq_len(100)],
               lleno=seq_len(100) <= n_fill, momento=mom)
  })) -> df_waf
  if (is.null(df_waf)||nrow(df_waf)==0) return(NULL)
  df_waf <- df_waf %>% mutate(momento=factor(momento,levels=c("PRE","POST")))
  ggplot(df_waf, aes(x=col, y=row, fill=interaction(lleno,momento))) +
    geom_tile(color="white", linewidth=0.7, width=0.9, height=0.9) +
    scale_fill_manual(
      values=c("FALSE.PRE"="#EEEEEE","TRUE.PRE"=COL_PRE,
               "FALSE.POST"="#EEEEEE","TRUE.POST"=COL_POST), guide="none") +
    facet_wrap(~momento, ncol=2) +
    coord_equal() +
    geom_text(data=df_p, aes(x=5.5, y=-0.5, label=sprintf("PMP = %.1f%%",pmp)),
              size=4.5, fontface="bold", inherit.aes=FALSE, color=c(COL_PRE,COL_POST)) +
    labs(title=titulo, subtitle="Cada cuadro = 1% | Azul=PRE | Naranja=POST",
         caption="Cuadros coloreados = % PMP alcanzado") +
    theme_void() +
    theme(plot.title=element_text(size=12,face="bold",hjust=0.5,margin=margin(b=4)),
          plot.subtitle=element_text(size=9,color="#555555",hjust=0.5),
          plot.caption=element_text(size=7.5,color="#888888",hjust=1),
          plot.background=element_rect(fill="white",color=NA),
          strip.text=element_text(size=11,face="bold",margin=margin(b=4)),
          plot.margin=margin(10,10,10,10))
}

# ── 11. Ridgeline distribución de PMP por ítem ──────────────────────────────
viz_ridgeline_pmp <- function(df_items, titulo) {
  if (is.null(df_items)||nrow(df_items)==0) return(NULL)
  df_p <- df_items %>% mutate(momento=factor(momento,levels=c("PRE","POST")))
  ggplot(df_p, aes(x=pmp, fill=momento, color=momento)) +
    geom_density(alpha=0.38, linewidth=0.9, adjust=1.2) +
    geom_rug(alpha=0.15, linewidth=0.35, sides="b") +
    geom_vline(xintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed", linewidth=0.45,
               color=c("#D7191C","#F4A261","#1A9641"), alpha=0.7) +
    scale_fill_manual(values=COL_MOM, name="Momento") +
    scale_color_manual(values=COL_MOM, name="Momento") +
    scale_x_continuous(limits=c(-5,105), labels=label_number(suffix="%"),
                        breaks=c(0,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,100)) +
    labs(title=titulo, subtitle="Distribución de PMP entre ítems | Azul=PRE | Naranja=POST",
         x="PMP (%)", y="Densidad",
         caption="Líneas punteadas: umbrales de referencia") +
    theme(legend.position="top")
}

# ---------------------------------------------------------------------------- #
# 8. VISUALIZACIONES POR GRUPO (nivel / entidad / CCT)
# ---------------------------------------------------------------------------- #
# Todas las funciones reciben un df con columna `nivel` (que puede contener
# valores de nivel educativo, entidad o CCT — renombrado antes de llamar).

# ── N01. ★ Radar PMP superpuesto por grupo ───────────────────────────────────
viz_radar_nivel_pmp <- function(df_dim_niv, titulo) {
  if (is.null(df_dim_niv)||nrow(df_dim_niv)==0) return(NULL)
  dims   <- unique(str_replace_all(df_dim_niv$dimension,"_"," "))
  if (length(dims)<3) return(NULL)
  n_d    <- length(dims)
  angles <- seq(0,2*pi,length.out=n_d+1)[-(n_d+1)]
  niveles<- unique(df_dim_niv$nivel)
  n_niv  <- length(niveles)
  pal_niv<- setNames(PAL_NIVEL[seq_len(min(n_niv,length(PAL_NIVEL)))], niveles[seq_len(min(n_niv,length(PAL_NIVEL)))])

  df_c <- df_dim_niv %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           dim_idx=match(dim_lbl,dims), ang=angles[dim_idx],
           x=pmp*cos(ang), y=pmp*sin(ang))
  df_cl <- bind_rows(df_c, df_c%>%filter(dim_idx==1)%>%
                       mutate(ang=angles[1],x=pmp*cos(angles[1]),y=pmp*sin(angles[1])))
  df_ejes <- data.frame(ang=angles, x_l=112*cos(angles), y_l=112*sin(angles), label=dims)
  circ_r  <- c(20,40,60,80,100)
  df_circ <- do.call(rbind, lapply(circ_r, function(r) {
    th <- seq(0,2*pi,length.out=180)
    data.frame(x=r*cos(th),y=r*sin(th),r=r)
  }))
  ggplot() +
    geom_path(data=df_circ,aes(x,y,group=r),color="#EEEEEE",linewidth=0.4) +
    geom_segment(data=data.frame(ang=angles),
                 aes(x=0,y=0,xend=103*cos(ang),yend=103*sin(ang)),
                 color="#DDDDDD",linewidth=0.4) +
    geom_text(data=data.frame(r=circ_r,x=circ_r*cos(0.3),y=circ_r*sin(0.3)),
              aes(x,y,label=paste0(r,"%")),size=2.3,color="#AAAAAA") +
    geom_polygon(data=df_cl,
                 aes(x,y,color=nivel,fill=nivel,linetype=momento,
                     group=interaction(nivel,momento)),
                 alpha=0.08, linewidth=1.1) +
    geom_point(data=df_c, aes(x,y,color=nivel,shape=momento), size=2.8, alpha=0.9) +
    geom_text(data=df_ejes, aes(x_l,y_l,label=label),
              size=3,fontface="bold",color="#333333",lineheight=0.85) +
    scale_color_manual(values=pal_niv, name="Grupo") +
    scale_fill_manual(values=pal_niv, guide="none") +
    scale_linetype_manual(values=c(PRE="dashed",POST="solid"), name="Momento") +
    scale_shape_manual(values=c(PRE=1,POST=16), name="Momento") +
    coord_equal(xlim=c(-135,135),ylim=c(-135,135)) +
    labs(title=titulo, subtitle="PMP (%) | Un polígono por grupo | Punteado=PRE | Sólido=POST") +
    theme_void() +
    theme(plot.title=element_text(size=12,face="bold",hjust=0.5,margin=margin(b=4)),
          plot.subtitle=element_text(size=9,color="#555555",hjust=0.5),
          plot.background=element_rect(fill="white",color=NA),
          legend.position="bottom", legend.title=element_text(size=9,face="bold"),
          plot.margin=margin(10,10,10,10))
}

# ── N02. ★ Spoke chart PMP facetado por grupo ────────────────────────────────
viz_spoke_nivel_pmp <- function(df_dim_niv, titulo) {
  if (is.null(df_dim_niv)||nrow(df_dim_niv)==0) return(NULL)
  df_p <- df_dim_niv %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           momento=factor(momento,levels=c("PRE","POST")),
           x_pos=as.numeric(factor(dim_lbl)))
  n_d <- n_distinct(df_p$dim_lbl)
  ggplot(df_p, aes(x=factor(x_pos),y=pmp,fill=momento)) +
    geom_col(position=position_dodge(0.78),width=0.72,alpha=0.9,color="white",linewidth=0.35) +
    geom_text(aes(label=sprintf("%.0f%%",pmp)),
              position=position_dodge(0.78),hjust=-0.1,size=2.6,fontface="bold",color="grey20") +
    geom_hline(yintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed",linewidth=0.4,
               color=c("#D7191C","#F4A261","#1A9641"),alpha=0.6) +
    scale_fill_manual(values=COL_MOM,name="Momento") +
    scale_x_discrete(labels=setNames(unique(df_p$dim_lbl), as.character(seq_len(n_d)))) +
    scale_y_continuous(limits=c(-20,115),breaks=c(0,40,60,80,100),
                        labels=paste0(c(0,40,60,80,100),"%")) +
    coord_polar(theta="x",start=0) +
    facet_wrap(~nivel,ncol=3) +
    labs(title=titulo,
         subtitle="PMP (%) | Barras polares por grupo | Azul=PRE | Naranja=POST",
         x=NULL,y=NULL) +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.subtitle=element_text(size=9,color="#555555",hjust=0.5),
          plot.background=element_rect(fill="white",color=NA),
          strip.text=element_text(face="bold",size=9),
          panel.grid=element_line(color="#EEEEEE"),
          axis.text.x=element_text(size=7.5,face="bold"),
          axis.text.y=element_text(size=6,color="#AAAAAA"),
          legend.position="bottom")
}

# ── N03. Barras agrupadas grupo × dimensión ──────────────────────────────────
viz_barras_nivel_dim <- function(df_dim_niv, titulo) {
  if (is.null(df_dim_niv)||nrow(df_dim_niv)==0) return(NULL)
  df_p <- df_dim_niv %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           momento=factor(momento,levels=c("PRE","POST")))
  ggplot(df_p, aes(x=dim_lbl, y=pmp, fill=momento)) +
    geom_col(position=position_dodge(0.75),width=0.7,alpha=0.9,color="white",linewidth=0.3) +
    geom_text(aes(label=sprintf("%.0f%%",pmp)),
              position=position_dodge(0.75),vjust=-0.3,size=2.5,fontface="bold") +
    geom_hline(yintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed",linewidth=0.4,
               color=c("#D7191C","#F4A261","#1A9641"),alpha=0.6) +
    scale_fill_manual(values=COL_MOM,name="Momento") +
    scale_y_continuous(limits=c(0,108),labels=label_number(suffix="%")) +
    facet_wrap(~nivel,ncol=3) +
    labs(title=titulo, subtitle="PMP (%) por dimensión y grupo | Azul=PRE | Naranja=POST",
         x=NULL,y="PMP (%)") +
    theme(axis.text.x=element_text(angle=30,hjust=1,size=7),
          strip.text=element_text(face="bold",size=9), legend.position="top")
}

# ── N04. Heatmap grupo × dimensión ───────────────────────────────────────────
viz_heatmap_nivel_pmp <- function(df_dim_niv, titulo) {
  if (is.null(df_dim_niv)||nrow(df_dim_niv)==0) return(NULL)
  df_p <- df_dim_niv %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           momento=factor(momento,levels=c("PRE","POST")))
  ggplot(df_p, aes(x=nivel, y=reorder(dim_lbl,-as.numeric(factor(dim_lbl))), fill=pmp)) +
    geom_tile(color="white",linewidth=0.5,height=0.85,width=0.85) +
    geom_text(aes(label=sprintf("%.1f%%",pmp)), size=3.2,color="white",fontface="bold") +
    scale_fill_gradient2(low="#D7191C",mid="#FFFFBF",high="#1A9641",
                          midpoint=50,limits=c(0,100),name="PMP (%)") +
    facet_wrap(~momento,ncol=2) +
    labs(title=titulo,
         subtitle="Rojo<50% (bajo) | Amarillo~50% (medio) | Verde>50% (alto)",
         x="Grupo",y=NULL) +
    theme(axis.text.x=element_text(angle=25,hjust=1,size=8,face="bold"),
          axis.text.y=element_text(size=9,face="bold"),
          panel.grid=element_blank(),legend.position="right",
          strip.text=element_text(size=10,face="bold"))
}

# ── N05. ★ Dumbbell PRE-POST por grupo ───────────────────────────────────────
viz_dumbbell_nivel_pmp <- function(df_dim_niv, titulo) {
  if (is.null(df_dim_niv)||nrow(df_dim_niv)==0) return(NULL)
  df_seg <- df_dim_niv %>%
    select(dimension,nivel,momento,pmp) %>%
    pivot_wider(names_from=momento,values_from=pmp) %>%
    filter(!is.na(PRE)&!is.na(POST)) %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "))
  df_t <- df_dim_niv %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           momento=factor(momento,levels=c("PRE","POST")))
  niveles <- unique(df_t$nivel)
  pal_niv <- setNames(PAL_NIVEL[seq_len(min(length(niveles),length(PAL_NIVEL)))], niveles[seq_len(min(length(niveles),length(PAL_NIVEL)))])
  ggplot() +
    geom_segment(data=df_seg,
                 aes(x=PRE,xend=POST,
                     y=reorder(dim_lbl,-PRE),yend=reorder(dim_lbl,-PRE),
                     color=nivel),linewidth=1.8,alpha=0.5) +
    geom_point(data=df_t%>%filter(momento=="PRE"),
               aes(x=pmp,y=dim_lbl,color=nivel),
               shape=21,size=4.5,fill="white",stroke=1.6) +
    geom_point(data=df_t%>%filter(momento=="POST"),
               aes(x=pmp,y=dim_lbl,color=nivel), shape=16,size=4.5,alpha=0.95) +
    geom_text(data=df_t%>%filter(momento=="POST"),
              aes(x=pmp,y=dim_lbl,label=sprintf("%.0f%%",pmp),color=nivel),
              vjust=-0.8,size=2.8,fontface="bold") +
    scale_color_manual(values=pal_niv,name="Grupo") +
    scale_x_continuous(limits=c(0,108),labels=label_number(suffix="%"),
                        breaks=c(0,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,100)) +
    facet_wrap(~nivel,ncol=3) +
    labs(title=titulo, subtitle="Círculo hueco=PRE | Punto sólido=POST | PMP (%)",
         x="PMP (%)",y=NULL) +
    theme(legend.position="none",
          axis.text.y=element_text(size=8,face="bold"),
          strip.text=element_text(face="bold",size=9))
}

# ── N06. ★ Bump chart ranking por grupo ──────────────────────────────────────
viz_bump_nivel_pmp <- function(df_dim_niv, titulo) {
  if (is.null(df_dim_niv)||nrow(df_dim_niv)==0) return(NULL)
  df_all <- df_dim_niv %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           momento=factor(momento,levels=c("PRE","POST"))) %>%
    group_by(nivel,momento) %>%
    mutate(ranking=rank(-pmp,ties.method="first")) %>% ungroup()
  ggplot(df_all, aes(x=momento,y=ranking,group=dim_lbl,color=dim_lbl)) +
    geom_line(linewidth=1.2,alpha=0.8) +
    geom_point(size=4,alpha=0.9) +
    geom_text(data=df_all%>%filter(momento=="POST"),
              aes(label=sprintf("%s\n%.0f%%",dim_lbl,pmp)),
              nudge_x=0.06,size=2.4,hjust=0) +
    scale_y_reverse(breaks=seq_len(n_distinct(df_all$dim_lbl))) +
    scale_color_brewer(palette="Set2") +
    facet_wrap(~nivel,ncol=3) +
    labs(title=titulo,
         subtitle="Ranking de dimensiones por PMP (%) | PRE → POST | 1 = mayor PMP",
         x=NULL,y="Ranking") +
    theme(legend.position="none",
          axis.text.x=element_text(size=10,face="bold",color=c(COL_PRE,COL_POST)),
          strip.text=element_text(face="bold",size=9),
          plot.margin=margin(5,60,5,10))
}

# ── CCT01. Heatmap CCT × dimensión (resumen compacto) ────────────────────────
# Para CCT pueden ser muchas escuelas: limitamos a top/bottom o todas con texto pequeño
viz_heatmap_cct_pmp <- function(df_dim_cct, titulo, max_cct=40) {
  if (is.null(df_dim_cct)||nrow(df_dim_cct)==0) return(NULL)
  # Ordenar CCT por PMP global descendente y limitar
  orden_cct <- df_dim_cct %>%
    group_by(nivel) %>%
    summarise(pmp_med=mean(pmp,na.rm=TRUE), .groups="drop") %>%
    arrange(desc(pmp_med)) %>%
    pull(nivel)
  if (length(orden_cct) > max_cct) orden_cct <- orden_cct[seq_len(max_cct)]

  df_p <- df_dim_cct %>%
    filter(nivel %in% orden_cct) %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           nivel=factor(nivel, levels=rev(orden_cct)),
           momento=factor(momento,levels=c("PRE","POST")))
  ggplot(df_p, aes(x=dim_lbl, y=nivel, fill=pmp)) +
    geom_tile(color="white",linewidth=0.4) +
    geom_text(aes(label=sprintf("%.0f",pmp)), size=2.2, color="white", fontface="bold") +
    scale_fill_gradient2(low="#D7191C",mid="#FFFFBF",high="#1A9641",
                          midpoint=50,limits=c(0,100),name="PMP (%)") +
    facet_wrap(~momento,ncol=2) +
    labs(title=titulo,
         subtitle=sprintf("Top %d CCT por PMP global | Rojo<50%% | Verde>50%%", max_cct),
         x="Dimensión", y="CCT") +
    theme(axis.text.x=element_text(angle=30,hjust=1,size=7,face="bold"),
          axis.text.y=element_text(size=6),
          panel.grid=element_blank(),legend.position="right",
          strip.text=element_text(size=10,face="bold"))
}

# ── CCT02. Barras top/bottom CCT por PMP global ──────────────────────────────
viz_top_bottom_cct <- function(df_glob_cct, titulo, n_top=10) {
  if (is.null(df_glob_cct)||nrow(df_glob_cct)==0) return(NULL)
  # Tomar PMP promedio por CCT (media entre PRE y POST si ambos existen)
  resumen <- df_glob_cct %>%
    group_by(cct) %>%
    summarise(pmp_med=mean(pmp,na.rm=TRUE), n_resp=max(n_resp,na.rm=TRUE), .groups="drop") %>%
    arrange(desc(pmp_med))
  n_total <- nrow(resumen)
  if (n_total <= n_top*2) {
    df_sel <- resumen %>% mutate(grupo="Todos")
  } else {
    df_top <- resumen %>% slice_head(n=n_top) %>% mutate(grupo="Top")
    df_bot <- resumen %>% slice_tail(n=n_top) %>% mutate(grupo="Bottom")
    df_sel <- bind_rows(df_top, df_bot)
  }
  ggplot(df_sel, aes(x=reorder(cct,pmp_med), y=pmp_med, fill=grupo)) +
    geom_col(width=0.75, alpha=0.9, color="white", linewidth=0.3) +
    geom_text(aes(label=sprintf("%.1f%%",pmp_med)), hjust=-0.1, size=3, fontface="bold") +
    geom_hline(yintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed", linewidth=0.4,
               color=c("#D7191C","#F4A261","#1A9641"), alpha=0.7) +
    scale_fill_manual(values=c(Top="#1A9641",Bottom="#D7191C",Todos="#457B9D"),
                      name=NULL) +
    scale_y_continuous(limits=c(0,108), labels=label_number(suffix="%")) +
    coord_flip() +
    labs(title=titulo,
         subtitle=sprintf("PMP global promedio por CCT | n total = %d centros", n_total),
         x="CCT", y="PMP global (%)",
         caption="Media entre PRE y POST cuando ambos están disponibles") +
    theme(axis.text.y=element_text(size=7,face="bold"), legend.position="top")
}

# ── ENT01. Barras PMP por entidad ────────────────────────────────────────────
viz_barras_entidad_pmp <- function(df_dim_ent, titulo) {
  if (is.null(df_dim_ent)||nrow(df_dim_ent)==0) return(NULL)
  df_p <- df_dim_ent %>%
    mutate(dim_lbl=str_replace_all(dimension,"_"," "),
           momento=factor(momento,levels=c("PRE","POST")))
  ggplot(df_p, aes(x=reorder(nivel,pmp), y=pmp, fill=momento)) +
    geom_col(position=position_dodge(0.75),width=0.7,alpha=0.9,color="white",linewidth=0.3) +
    geom_text(aes(label=sprintf("%.0f%%",pmp)),
              position=position_dodge(0.75),hjust=-0.1,size=2.8,fontface="bold") +
    geom_hline(yintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
               linetype="dashed",linewidth=0.4,
               color=c("#D7191C","#F4A261","#1A9641"),alpha=0.6) +
    scale_fill_manual(values=COL_MOM,name="Momento") +
    scale_y_continuous(limits=c(0,108),labels=label_number(suffix="%")) +
    facet_wrap(~dim_lbl) +
    coord_flip() +
    labs(title=titulo,
         subtitle="PMP por entidad y dimensión | Azul=PRE | Naranja=POST",
         x="Entidad", y="PMP (%)") +
    theme(axis.text.y=element_text(size=8,face="bold"),
          strip.text=element_text(face="bold",size=8),
          legend.position="top")
}

# ── ENT02. Delta PMP por entidad (POST − PRE) ─────────────────────────────────
viz_delta_entidad_pmp <- function(df_glob_ent, titulo) {
  if (is.null(df_glob_ent)||nrow(df_glob_ent)==0||!"POST" %in% df_glob_ent$momento) return(NULL)
  df_d <- df_glob_ent %>%
    select(entidad,momento,pmp) %>%
    pivot_wider(names_from=momento,values_from=pmp) %>%
    filter(!is.na(PRE)&!is.na(POST)) %>%
    mutate(delta=round(POST-PRE,2),
           color=case_when(delta>2~"Mejora",delta< -2~"Deterioro",TRUE~"Sin cambio"))
  if (nrow(df_d)==0) return(NULL)
  ggplot(df_d, aes(x=reorder(entidad,delta), y=delta, fill=color)) +
    geom_col(width=0.75,alpha=0.9,color="white",linewidth=0.3) +
    geom_hline(yintercept=0,color="#444444",linewidth=0.5) +
    geom_text(aes(label=sprintf("%+.1f pp",delta), hjust=ifelse(delta>=0,-0.1,1.1)),
              size=3,fontface="bold") +
    scale_fill_manual(values=c(Mejora="#1A9641",Deterioro="#D7191C","Sin cambio"="#F4A261"),
                      name="Dirección") +
    scale_y_continuous(labels=label_number(suffix=" pp")) +
    coord_flip() +
    labs(title=titulo, subtitle="Diferencia POST − PRE en PMP global por entidad",
         x=NULL, y="Δ PMP (pp)") +
    theme(axis.text.y=element_text(size=9,face="bold"))
}

# ---------------------------------------------------------------------------- #
# 9. FUNCIÓN AUXILIAR: CALCULAR Y GUARDAR UN DESAGREGADO
# ---------------------------------------------------------------------------- #
# Abstrae la lógica repetida para nivel / entidad / CCT.
# `grupo_lbl` : nombre del grupo ("nivel","entidad","cct")
# `carpeta`   : subcarpeta de salida (e.g. "POR_NIVEL")
# `prefijo`   : prefijo de archivo (e.g. "N","ENT","CCT")
# `max_grupos_radar`: si hay más grupos que esto, no generar radar (muy denso)

procesar_grupo <- function(nm, cfg, df_raw_pre, df_raw_post,
                            df_pre, df_post,
                            items_disp, cm_pre, cm_post_completo,
                            grupo_lbl, carpeta, prefijo,
                            max_grupos_radar=12,
                            max_cct=40) {
  # Extraer vectores del grupo
  col_grupo <- cfg[[paste0(grupo_lbl,"_col")]]
  if (is.null(col_grupo)) return(invisible(NULL))

  gv_pre  <- if (col_grupo %in% names(df_raw_pre))  df_raw_pre[[col_grupo]]  else NULL
  gv_post <- if (!is.null(df_raw_post) && col_grupo %in% names(df_raw_post))
               df_raw_post[[col_grupo]] else NULL

  if (is.null(gv_pre)) {
    cat(sprintf("  ⚠ Columna '%s' no encontrada en PRE — omitiendo %s\n", col_grupo, grupo_lbl))
    return(invisible(NULL))
  }
  n_grupos <- n_distinct(na.omit(as.character(gv_pre)))
  if (n_grupos < 2) {
    cat(sprintf("  ⚠ %s: solo %d valor único — omitiendo\n", grupo_lbl, n_grupos))
    return(invisible(NULL))
  }
  cat(sprintf("  %s: %d grupos detectados\n", toupper(grupo_lbl), n_grupos))

  # Calcular PMP por dimensión y global
  d_dim_g <- tryCatch(
    bind_rows(
      calc_pmp_dim_grupo(df_pre,  cfg$subscalas, "PRE",  gv_pre,  cm_pre,           grupo_lbl),
      if (!is.null(df_post) && !is.null(gv_post))
        calc_pmp_dim_grupo(df_post, cfg$subscalas, "POST", gv_post, cm_post_completo, grupo_lbl)
    ),
    error=function(e){cat("  [dim_",grupo_lbl,"] Error:",conditionMessage(e),"\n");NULL}
  )
  d_glob_g <- tryCatch(
    bind_rows(
      calc_pmp_global_grupo(df_pre,  items_disp, "PRE",  gv_pre,  cm_pre,           grupo_lbl),
      if (!is.null(df_post) && !is.null(gv_post))
        calc_pmp_global_grupo(df_post, intersect(items_disp,names(df_post)),
                               "POST", gv_post, cm_post_completo, grupo_lbl)
    ),
    error=function(e){cat("  [glob_",grupo_lbl,"] Error:",conditionMessage(e),"\n");NULL}
  )

  if (is.null(d_dim_g) || nrow(d_dim_g)==0) {
    cat(sprintf("  ⚠ Sin datos de PMP por %s\n", grupo_lbl))
    return(invisible(NULL))
  }

  # Renombrar columna de grupo a "nivel" para reutilizar viz_* existentes
  d_dim_niv  <- d_dim_g  %>% rename(nivel=all_of(grupo_lbl))
  d_glob_niv <- d_glob_g %>% rename(nivel=all_of(grupo_lbl))

  # Directorio de salida
  dir_sal <- file.path(ruta_sal, nm, carpeta)
  dir.create(dir_sal, showWarnings=FALSE, recursive=TRUE)
  g_path  <- function(f) file.path(dir_sal, f)

  tit <- function(v) paste(v,"—",nm, sprintf("(por %s)", toupper(grupo_lbl)))
  n_niv  <- n_distinct(d_dim_niv$nivel)
  nd     <- n_distinct(d_dim_niv$dimension)
  has_post <- "POST" %in% d_dim_niv$momento

  # Gráficos — adaptados según tipo de grupo
  if (grupo_lbl == "cct") {
    # Para CCT: heatmap compacto + top/bottom
    tryCatch(guardar_g(
      seg(viz_heatmap_cct_pmp(d_dim_niv, tit("Heatmap PMP"), max_cct=max_cct)),
      g_path(paste0(prefijo,"01_heatmap_cct.png")), 16, max(8, min(max_cct,n_niv)*0.22+3)
    ), error=function(e) NULL)

    if (!is.null(d_glob_niv)) {
      # Renombrar "nivel" → "cct" en d_glob_niv para viz_top_bottom_cct
      d_for_top <- d_glob_niv %>% rename(cct=nivel)
      tryCatch(guardar_g(
        seg(viz_top_bottom_cct(d_for_top, tit("Top/Bottom PMP global"))),
        g_path(paste0(prefijo,"02_top_bottom_cct.png")), 11, 8
      ), error=function(e) NULL)
    }

    # Heatmap nivel×dimensión (compacto) también funciona
    tryCatch(guardar_g(
      seg(viz_heatmap_nivel_pmp(d_dim_niv, tit("Heatmap PMP dimensión × CCT"))),
      g_path(paste0(prefijo,"03_heatmap_dim_cct.png")), max(14, n_niv*0.4+4), 9
    ), error=function(e) NULL)

  } else {
    # Para nivel o entidad: suite completa de gráficos por grupo

    # N01/E01 Radar (solo si grupos ≤ max_grupos_radar)
    if (n_niv <= max_grupos_radar) {
      tryCatch(guardar_g(
        seg(viz_radar_nivel_pmp(d_dim_niv, tit(paste0("★ Radar PMP por ", grupo_lbl)))),
        g_path(paste0(prefijo,"01_radar.png")), 12, 12
      ), error=function(e) cat(sprintf("  %s01 radar ERROR: %s\n", prefijo, conditionMessage(e))))
    }

    # N02/E02 Spoke
    tryCatch(guardar_g(
      seg(viz_spoke_nivel_pmp(d_dim_niv, tit(paste0("★ Spoke PMP por ", grupo_lbl)))),
      g_path(paste0(prefijo,"02_spoke.png")), 14, max(6, ceiling(n_niv/3)*5+2)
    ), error=function(e) NULL)

    # N03/E03 Barras
    tryCatch(guardar_g(
      seg(viz_barras_nivel_dim(d_dim_niv, tit(paste0("Barras PMP ", grupo_lbl, " × dimensión")))),
      g_path(paste0(prefijo,"03_barras.png")), max(10, n_niv*2.5+2), max(6, nd*0.9+2)
    ), error=function(e) NULL)

    # N04/E04 Heatmap
    tryCatch(guardar_g(
      seg(viz_heatmap_nivel_pmp(d_dim_niv, tit(paste0("Heatmap PMP ", grupo_lbl, " × dimensión")))),
      g_path(paste0(prefijo,"04_heatmap.png")), max(13, n_niv*1.2+2), max(7, nd*0.8+2)
    ), error=function(e) NULL)

    if (has_post) {
      # N05/E05 Dumbbell
      tryCatch(guardar_g(
        seg(viz_dumbbell_nivel_pmp(d_dim_niv, tit(paste0("★ Dumbbell PMP por ", grupo_lbl)))),
        g_path(paste0(prefijo,"05_dumbbell.png")), max(10, n_niv*3+2), max(5, nd*0.6+2)
      ), error=function(e) NULL)

      # N06/E06 Bump
      tryCatch(guardar_g(
        seg(viz_bump_nivel_pmp(d_dim_niv, tit(paste0("★ Bump ranking PMP por ", grupo_lbl)))),
        g_path(paste0(prefijo,"06_bump.png")), max(10, n_niv*3+2), max(5, nd*0.5+2)
      ), error=function(e) NULL)

      # E07 Delta global por entidad (solo para entidad)
      if (grupo_lbl == "entidad") {
        d_glob_ent <- d_glob_niv %>% rename(entidad=nivel)
        tryCatch(guardar_g(
          seg(viz_delta_entidad_pmp(d_glob_ent, tit("Delta PMP global por entidad"))),
          g_path(paste0(prefijo,"07_delta_global.png")), max(10, n_niv*0.6+4), 7
        ), error=function(e) NULL)
      }
    }
  }

  # Devolver tablas para Excel
  list(d_dim=d_dim_g, d_glob=d_glob_g)
}

# ---------------------------------------------------------------------------- #
# 10. FUNCIÓN MAESTRA POR INSTRUMENTO
# ---------------------------------------------------------------------------- #
analizar_pmp <- function(nm) {
  cfg <- INSTRUMENTOS[[nm]]
  cat("\n", strrep("═",65),"\n  INSTRUMENTO:", nm, "|", cfg$figura,"\n")

  for (d in file.path(ruta_sal, nm, c("","TABLAS","GRAFICOS","POR_NIVEL","POR_ENTIDAD","POR_CCT")))
    dir.create(d, showWarnings=FALSE, recursive=TRUE)

  rp <- paste0(ruta_pre,  cfg$archivo_pre)
  rq <- paste0(ruta_post, cfg$archivo_post)
  if (!file.exists(rp)) { cat("  Sin PRE\n"); return(invisible(NULL)) }

  df_raw_pre  <- read_excel(rp)
  df_raw_post <- if (file.exists(rq)) read_excel(rq) else NULL
  if (is.null(df_raw_post)) cat("  ⚠ Sin POST — solo PRE\n")

  items_disp <- intersect(cfg$items, names(df_raw_pre))
  df_pre  <- recodificar_df(df_raw_pre[, items_disp, drop=FALSE])
  df_post <- if (!is.null(df_raw_post))
    recodificar_df(df_raw_post[, intersect(items_disp,names(df_raw_post)), drop=FALSE])
  else NULL

  N_pre  <- nrow(df_pre)
  N_post <- if (!is.null(df_post)) nrow(df_post) else 0

  cm_pre  <- detectar_cat_max_df(df_pre, items_disp)
  cm_post <- if (!is.null(df_post))
    detectar_cat_max_df(df_post, intersect(items_disp,names(df_post))) else cm_pre

  tab_cm <- table(cm_pre[!is.na(cm_pre)])
  cat(sprintf("  N PRE=%d | POST=%d | Ítems: %s\n", N_pre, N_post,
              paste(paste0("0-",names(tab_cm),"=",as.integer(tab_cm),"íts"), collapse=", ")))

  # cm completo para POST (base en pre, actualiza con obs)
  cm_post_completo <- cm_pre
  if (!is.null(df_post)) {
    cm_post_obs <- detectar_cat_max_df(df_post, intersect(items_disp, names(df_post)))
    cm_post_completo[names(cm_post_obs)] <- cm_post_obs
  }

  # ── CALCULAR PMP GLOBAL ────────────────────────────────────────────────────
  cat("  Calculando PMP...\n")
  d_items <- bind_rows(calc_pmp_items(df_pre, items_disp,"PRE", cm_pre),
                        if(!is.null(df_post)) calc_pmp_items(df_post,
                           intersect(items_disp,names(df_post)),"POST",cm_post))
  d_dims  <- bind_rows(calc_pmp_dim(df_pre, cfg$subscalas,"PRE", cm_pre),
                        if(!is.null(df_post)) calc_pmp_dim(df_post,cfg$subscalas,"POST",cm_post))
  d_glob  <- bind_rows(calc_pmp_global(df_pre, items_disp,"PRE", cm_pre),
                        if(!is.null(df_post)) calc_pmp_global(df_post,
                           intersect(items_disp,names(df_post)),"POST",cm_post))

  if (!is.null(d_glob))
    for (i in seq_len(nrow(d_glob)))
      cat(sprintf("  PMP %s: %.1f%%\n", d_glob$momento[i], d_glob$pmp[i]))

  # ── CALCULAR PMP POR NIVEL / ENTIDAD / CCT ────────────────────────────────
  res_nivel   <- procesar_grupo(nm, cfg, df_raw_pre, df_raw_post, df_pre, df_post,
                                 items_disp, cm_pre, cm_post_completo,
                                 "nivel",   "POR_NIVEL",   "N")
  res_entidad <- procesar_grupo(nm, cfg, df_raw_pre, df_raw_post, df_pre, df_post,
                                 items_disp, cm_pre, cm_post_completo,
                                 "entidad", "POR_ENTIDAD", "E")
  res_cct     <- procesar_grupo(nm, cfg, df_raw_pre, df_raw_post, df_pre, df_post,
                                 items_disp, cm_pre, cm_post_completo,
                                 "cct",     "POR_CCT",     "C", max_cct=40)

  # ── TABLAS EXCEL ─────────────────────────────────────────────────────────
  wb <- createWorkbook()
  add_ws <- function(nm_ws, dat) {
    if (!is.null(dat) && nrow(dat)>0)
      tryCatch({addWorksheet(wb,nm_ws); writeData(wb,nm_ws,dat)}, error=function(e)NULL)
  }
  add_ws("PMP_Global",    d_glob)
  add_ws("PMP_Dimension", d_dims)
  add_ws("PMP_Item",      d_items)
  if (!is.null(res_nivel$d_glob))   add_ws("PMP_Global_xNivel",     res_nivel$d_glob)
  if (!is.null(res_nivel$d_dim))    add_ws("PMP_Dimension_xNivel",  res_nivel$d_dim)
  if (!is.null(res_entidad$d_glob)) add_ws("PMP_Global_xEntidad",   res_entidad$d_glob)
  if (!is.null(res_entidad$d_dim))  add_ws("PMP_Dimension_xEntidad",res_entidad$d_dim)
  if (!is.null(res_cct$d_glob))     add_ws("PMP_Global_xCCT",       res_cct$d_glob)
  if (!is.null(res_cct$d_dim))      add_ws("PMP_Dimension_xCCT",    res_cct$d_dim)

  tryCatch(saveWorkbook(wb,
             file.path(ruta_sal,nm,"TABLAS",paste0(nm,"_PMP.xlsx")),
             overwrite=TRUE), error=function(e)NULL)

  # ── GRÁFICOS GLOBALES ────────────────────────────────────────────────────
  cat("  Generando gráficos globales...\n")
  tit <- function(v) paste(v,"—",nm)
  g   <- function(p) file.path(ruta_sal, nm, "GRAFICOS", p)
  has_post <- !is.null(d_dims) && "POST" %in% d_dims$momento

  guardar_g(seg(viz_barras_dim(d_dims, tit("PMP por dimensión"))),
            g("01_barras_dim.png"), 10, 6)
  guardar_g(seg(viz_bullet(d_dims, tit("★ Bullet chart — PMP vs umbrales"))),
            g("02_bullet.png"), 10, max(4,n_distinct(d_dims$dimension)*0.6+2))
  guardar_g(seg(viz_heatmap_items(d_items, tit("Heatmap PMP por ítem"))),
            g("03_heatmap_items.png"), 10, max(8,min(50,nrow(d_items%>%distinct(item)))*0.22+2))
  guardar_g(seg(viz_radar_pmp(d_dims, tit("★ Radar PMP"))),
            g("04_radar.png"), 10, 10)
  guardar_g(seg(viz_spoke_pmp(d_dims, tit("★ Spoke chart PMP"))),
            g("05_spoke.png"), 11, 11)
  guardar_g(seg(viz_termometro(d_dims, tit("★ Termómetros por dimensión"))),
            g("06_termometro.png"), max(10,n_distinct(d_dims$dimension)*1.8), 5)
  if (has_post) {
    guardar_g(seg(viz_slope_pmp(d_dims, tit("Slope chart PMP"))),
              g("07_slope.png"), 11, 7)
    guardar_g(seg(viz_delta_pmp(d_dims, tit("Delta PMP (POST − PRE)"))),
              g("08_delta.png"), 10, 6)
  }
  guardar_g(seg(viz_dotplot_ic(d_items, cfg$subscalas, tit("Dot-plot PMP + IC 95%"))),
            g("09_dotplot_ic.png"), 10, max(6,min(30,nrow(d_items%>%distinct(item)))*0.22+2))
  guardar_g(seg(viz_waffle_pmp(d_glob, tit("★ Waffle PMP global"))),
            g("10_waffle.png"), 11, 6)
  guardar_g(seg(viz_ridgeline_pmp(d_items, tit("Ridgeline distribución PMP"))),
            g("11_ridgeline.png"), 10, 6)

  n_g <- sum(sapply(c("GRAFICOS","POR_NIVEL","POR_ENTIDAD","POR_CCT"), function(d)
    length(list.files(file.path(ruta_sal,nm,d),"*.png"))))
  cat(sprintf("  ✓ %s completado | %d gráficos\n", nm, n_g))

  invisible(list(d_items=d_items, d_dims=d_dims, d_glob=d_glob,
                 d_dim_nivel   =res_nivel$d_dim,    d_glob_nivel   =res_nivel$d_glob,
                 d_dim_entidad =res_entidad$d_dim,  d_glob_entidad =res_entidad$d_glob,
                 d_dim_cct     =res_cct$d_dim,      d_glob_cct     =res_cct$d_glob))
}

# ---------------------------------------------------------------------------- #
# 11. GRÁFICOS COMPARATIVOS ENTRE INSTRUMENTOS
# ---------------------------------------------------------------------------- #
graficos_comparativos <- function(resultados) {
  cat("\n  Generando comparativos entre instrumentos...\n")

  d_master_glob <- do.call(rbind, lapply(names(resultados), function(nm) {
    r <- resultados[[nm]]
    if (is.null(r)||is.null(r$d_glob)) return(NULL)
    r$d_glob %>% mutate(instrumento=nm, figura=INSTRUMENTOS[[nm]]$figura)
  }))
  d_master_dim <- do.call(rbind, lapply(names(resultados), function(nm) {
    r <- resultados[[nm]]
    if (is.null(r)||is.null(r$d_dims)) return(NULL)
    r$d_dims %>% mutate(instrumento=nm, figura=INSTRUMENTOS[[nm]]$figura)
  }))
  # Tablas comparativas por entidad y CCT
  d_master_ent <- do.call(rbind, lapply(names(resultados), function(nm) {
    r <- resultados[[nm]]
    if (is.null(r)||is.null(r$d_glob_entidad)) return(NULL)
    r$d_glob_entidad %>% mutate(instrumento=nm)
  }))
  d_master_cct <- do.call(rbind, lapply(names(resultados), function(nm) {
    r <- resultados[[nm]]
    if (is.null(r)||is.null(r$d_glob_cct)) return(NULL)
    r$d_glob_cct %>% mutate(instrumento=nm)
  }))

  if (!is.null(d_master_glob)) {
    write.csv(d_master_glob, file.path(ruta_sal,"00_PMP_Global_comparativo.csv"),      row.names=FALSE)
    write.csv(d_master_dim,  file.path(ruta_sal,"00_PMP_Dimensiones_comparativo.csv"), row.names=FALSE)
  }
  if (!is.null(d_master_ent) && nrow(d_master_ent)>0)
    write.csv(d_master_ent, file.path(ruta_sal,"00_PMP_Global_xEntidad_comparativo.csv"), row.names=FALSE)
  if (!is.null(d_master_cct) && nrow(d_master_cct)>0)
    write.csv(d_master_cct, file.path(ruta_sal,"00_PMP_Global_xCCT_comparativo.csv"),     row.names=FALSE)

  # C01: Radar comparativo
  if (!is.null(d_master_glob) && n_distinct(d_master_glob$instrumento) >= 3) {
    insts  <- unique(d_master_glob$instrumento); n_i <- length(insts)
    angles <- seq(0,2*pi,length.out=n_i+1)[-(n_i+1)]
    df_c <- d_master_glob %>%
      mutate(inst_idx=match(instrumento,insts), ang=angles[inst_idx],
             x=pmp*cos(ang), y=pmp*sin(ang))
    df_cl <- bind_rows(df_c, df_c%>%filter(inst_idx==1)%>%
                         mutate(ang=angles[1],x=pmp*cos(angles[1]),y=pmp*sin(angles[1])))
    df_ejes <- data.frame(ang=angles,x_l=114*cos(angles),y_l=114*sin(angles),label=insts)
    circ_r  <- c(20,40,60,80,100)
    df_circ <- do.call(rbind, lapply(circ_r, function(r) {
      th <- seq(0,2*pi,length.out=180)
      data.frame(x=r*cos(th),y=r*sin(th),r=r)
    }))
    g_c01 <- ggplot() +
      geom_path(data=df_circ,aes(x,y,group=r),color="#EEEEEE",linewidth=0.5) +
      geom_segment(data=data.frame(ang=angles),
                   aes(x=0,y=0,xend=103*cos(ang),yend=103*sin(ang)),
                   color="#DDDDDD",linewidth=0.4) +
      geom_text(data=data.frame(r=circ_r,x=circ_r*cos(0.3),y=circ_r*sin(0.3)),
                aes(x,y,label=paste0(r,"%")),size=2.5,color="#AAAAAA") +
      geom_path(data=data.frame(x=UMBRAL_BAJO*cos(seq(0,2*pi,length.out=180)),
                                 y=UMBRAL_BAJO*sin(seq(0,2*pi,length.out=180))),
                aes(x,y), color="#D7191C", linetype="dashed",
                linewidth=0.5, alpha=0.6, inherit.aes=FALSE) +
      geom_path(data=data.frame(x=UMBRAL_ALTO*cos(seq(0,2*pi,length.out=180)),
                                 y=UMBRAL_ALTO*sin(seq(0,2*pi,length.out=180))),
                aes(x,y), color="#1A9641", linetype="dashed",
                linewidth=0.5, alpha=0.6, inherit.aes=FALSE) +
      geom_polygon(data=df_cl%>%filter(momento=="POST"),
                   aes(x,y),fill=COL_POST,alpha=0.22,color=COL_POST,linewidth=1.3) +
      geom_polygon(data=df_cl%>%filter(momento=="PRE"),
                   aes(x,y),fill=COL_PRE,alpha=0.18,color=COL_PRE,linewidth=1.3) +
      geom_point(data=df_c,aes(x,y,color=momento,shape=momento),size=3.5,alpha=0.95) +
      geom_text_repel(data=df_c%>%filter(momento=="POST"),
                      aes(x,y,label=sprintf("%.1f%%",pmp)),
                      size=3,fontface="bold",color=COL_POST,max.overlaps=20) +
      geom_text(data=df_ejes,aes(x_l,y_l,label=label),
                size=3.5,fontface="bold",color="#333333",lineheight=0.85) +
      scale_color_manual(values=COL_MOM,name="Momento") +
      scale_shape_manual(values=c(PRE=16,POST=17),name="Momento") +
      coord_equal(xlim=c(-135,135),ylim=c(-135,135)) +
      labs(title="★ Radar PMP — Comparativo entre instrumentos",
           subtitle="PMP global por instrumento | Azul=PRE | Naranja=POST",
           caption="Rojo punteado=40% (bajo) | Verde=80% (alto)") +
      theme_void() +
      theme(plot.title=element_text(size=13,face="bold",hjust=0.5),
            plot.subtitle=element_text(size=10,color="#555555",hjust=0.5),
            plot.caption=element_text(size=8,color="#888888",hjust=1),
            plot.background=element_rect(fill="white",color=NA),
            legend.position="bottom", legend.title=element_text(size=10,face="bold"))
    guardar_g(g_c01, file.path(ruta_sal,"C01_radar_comparativo.png"), 12, 12)
  }

  # C02: Dot-plot comparativo
  if (!is.null(d_master_glob)) {
    g_c02 <- ggplot(d_master_glob%>%mutate(momento=factor(momento,levels=c("PRE","POST"))),
                     aes(x=pmp, y=reorder(instrumento,pmp), color=momento)) +
      geom_segment(data=d_master_glob%>%select(instrumento,momento,pmp)%>%
                     pivot_wider(names_from=momento,values_from=pmp)%>%
                     filter(!is.na(PRE)&!is.na(POST)),
                   aes(x=PRE,xend=POST,y=reorder(instrumento,PRE),yend=reorder(instrumento,PRE)),
                   color="#CCCCCC",linewidth=2.5,inherit.aes=FALSE) +
      geom_point(size=5,alpha=0.95) +
      geom_text(aes(label=sprintf("%.1f%%",pmp)),vjust=-0.8,size=3.5,fontface="bold") +
      geom_vline(xintercept=c(UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO),
                 linetype="dashed",linewidth=0.5,
                 color=c("#D7191C","#F4A261","#1A9641"),alpha=0.7) +
      scale_color_manual(values=COL_MOM,name="Momento") +
      scale_x_continuous(limits=c(0,105),labels=label_number(suffix="%"),
                          breaks=c(0,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_ALTO,100)) +
      labs(title="PMP global — Comparativo entre instrumentos y figuras",
           subtitle="Círculo=PRE (azul) | Punto=POST (naranja) | Segmento=cambio",
           x="PMP (%)",y=NULL) +
      theme(axis.text.y=element_text(size=10,face="bold"))
    guardar_g(g_c02, file.path(ruta_sal,"C02_dotplot_comparativo.png"), 11, 6)
  }

  # C03: Heatmap instrumento × dimensión
  if (!is.null(d_master_dim) && nrow(d_master_dim)>0) {
    df_h <- d_master_dim %>%
      mutate(dim_lbl=str_replace_all(dimension,"_"," "),
             momento=factor(momento,levels=c("PRE","POST")))
    g_c03 <- ggplot(df_h, aes(x=instrumento,
                                y=reorder(dim_lbl,-as.numeric(factor(dim_lbl))),
                                fill=pmp)) +
      geom_tile(color="white",linewidth=0.5,height=0.85,width=0.85) +
      geom_text(aes(label=sprintf("%.0f%%",pmp)),size=2.8,color="white",fontface="bold") +
      scale_fill_gradient2(low="#D7191C",mid="#FFFFBF",high="#1A9641",
                            midpoint=50,limits=c(0,100),name="PMP (%)") +
      facet_wrap(~momento,ncol=2) +
      labs(title="Heatmap PMP — instrumento × dimensión",
           subtitle="Rojo<50% | Amarillo~50% | Verde>50%",
           x="Instrumento",y=NULL) +
      theme(axis.text.x=element_text(angle=25,hjust=1,size=8,face="bold"),
            axis.text.y=element_text(size=8,face="bold"),
            panel.grid=element_blank(),legend.position="right",
            strip.text=element_text(size=10,face="bold"))
    guardar_g(g_c03, file.path(ruta_sal,"C03_heatmap_comparativo.png"), 14, 10)
  }

  # C04: Heatmap comparativo por entidad (todos los instrumentos)
  if (!is.null(d_master_ent) && nrow(d_master_ent)>0) {
    df_ent <- d_master_ent %>%
      mutate(momento=factor(momento,levels=c("PRE","POST")))
    g_c04 <- ggplot(df_ent, aes(x=instrumento, y=reorder(entidad,pmp), fill=pmp)) +
      geom_tile(color="white",linewidth=0.4,height=0.85,width=0.85) +
      geom_text(aes(label=sprintf("%.0f%%",pmp)),size=2.5,color="white",fontface="bold") +
      scale_fill_gradient2(low="#D7191C",mid="#FFFFBF",high="#1A9641",
                            midpoint=50,limits=c(0,100),name="PMP (%)") +
      facet_wrap(~momento,ncol=2) +
      labs(title="Heatmap PMP global — instrumento × entidad",
           subtitle="Rojo<50% | Verde>50%",
           x="Instrumento",y="Entidad") +
      theme(axis.text.x=element_text(angle=25,hjust=1,size=8,face="bold"),
            axis.text.y=element_text(size=8,face="bold"),
            panel.grid=element_blank(),legend.position="right",
            strip.text=element_text(size=10,face="bold"))
    guardar_g(g_c04, file.path(ruta_sal,"C04_heatmap_entidad_comparativo.png"), 14, 10)
  }

  cat("  ✓ Gráficos comparativos guardados\n")
}

# ---------------------------------------------------------------------------- #
# 12. EJECUCIÓN PRINCIPAL
# ---------------------------------------------------------------------------- #
cat("\n", strrep("#",65),"\n")
cat("  FASE 4 — PORCENTAJE DEL MÁXIMO POSIBLE (PMP)\n")
cat("  Fórmula: PMP = (media_ítem / cat_max_ítem) × 100\n")
cat(sprintf("  Umbrales: Bajo<%.0f%% | Medio %.0f-%.0f%% | Alto %.0f-%.0f%% | Muy alto>%.0f%%\n",
            UMBRAL_BAJO,UMBRAL_BAJO,UMBRAL_MEDIO,UMBRAL_MEDIO,UMBRAL_ALTO,UMBRAL_ALTO))
cat("  Desagregaciones: GLOBAL | POR NIVEL | POR ENTIDAD | POR CCT\n")
cat("  PRE = azul oscuro #1A3A6C | POST = naranja #F47B20\n")
cat("  Inicio:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n")
cat(strrep("#",65),"\n")

RESULTADOS_F4 <- list()
for (nm in names(INSTRUMENTOS))
  RESULTADOS_F4[[nm]] <- tryCatch(analizar_pmp(nm), error=function(e) {
    cat("\n!! ERROR en",nm,":",conditionMessage(e),"\n"); NULL
  })

graficos_comparativos(RESULTADOS_F4)

# Tabla resumen final
d_res <- do.call(rbind, lapply(names(RESULTADOS_F4), function(nm) {
  r <- RESULTADOS_F4[[nm]]
  if (is.null(r)||is.null(r$d_glob)) return(NULL)
  r$d_glob %>% mutate(instrumento=nm)
}))
if (!is.null(d_res)) {
  write.csv(d_res, file.path(ruta_sal,"00_RESUMEN_PMP_GLOBAL.csv"), row.names=FALSE)
  cat("\n  RESUMEN PMP GLOBAL:\n")
  print(d_res %>% select(instrumento,momento,pmp) %>%
          pivot_wider(names_from=momento,values_from=pmp) %>%
          mutate(delta_pp=round(coalesce(POST,0)-coalesce(PRE,0),1)),
        row.names=FALSE)
}
cat("\n  Fin:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"\n\n")
