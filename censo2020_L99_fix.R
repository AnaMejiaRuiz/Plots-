# ═══════════════════════════════════════════════════════════════════════════════
#  ANÁLISIS CUANTITATIVO — SECCIÓN 3: CENSO DE POBLACIÓN Y VIVIENDA 2020
#  Proyecto: Desafiliación Escolar en EMS | SEP-SEMS / BID
#  Responsable: Ana Laura Mejía | Junio 2026
#  Nivel: Publicación académica L99
# ═══════════════════════════════════════════════════════════════════════════════
#  Fuentes:
#   F1 — Microdatos de ejemplo, personas y viviendas (INEGI, 2021)
#   F2 — ITER Nacional 2020, tabulados por localidad (INEGI, 2021)
#   F3 — IRS 2020, calculado desde ITER (metodología CONEVAL, 2020)
#
#  Preguntas de investigación:
#   PI-1 a PI-4: Microdatos — desigualdades, no asistencia, hogar, perfil
#   PI-5 a PI-7: ITER       — volumen territorial, no asistencia 15-17, carencias
#   PI-8 a PI-9: IRS CONEVAL — rezago y relación con vulnerabilidad educativa
# ═══════════════════════════════════════════════════════════════════════════════


# ── 0. CONFIGURACIÓN ────────────────────────────────────────────────────────
pkgs <- c(
  "tidyverse", "janitor", "readxl", "data.table",
  "sf", "scales", "patchwork",
  "ggridges", "ggtext", "ggrepel",
  "gt",          # gtExtras eliminado por incompatibilidad con xfun >= 0.55
  "reticulate",
  "factoextra",
  "corrr",
  "ggdist"
)

nuevos <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(nuevos)) install.packages(nuevos, quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Python (para gráficos de nivel publicación no nativos en R)
use_python(Sys.which("python"), required = TRUE)
py_available <- py_available()
if (py_available) {
  py_pkgs <- c("matplotlib","seaborn","numpy","pandas")
  for (p in py_pkgs) {
    tryCatch(import(p), error = function(e)
      system(paste("pip install", p, "-q")))
  }
  message("Python disponible — se usará para gráficos avanzados.")
} else {
  message("Python no disponible — todos los gráficos se generarán en R.")
}

# ── Paleta BID / SEMS ────────────────────────────────────────────────────────
pal <- list(
  vino1  = "#6B1A2B", vino2  = "#8B2642", vino3  = "#B05070",
  azul1  = "#1B3A5C", azul2  = "#2E5F8A", azul3  = "#5B8DB8",
  dor1   = "#C4975A", dor2   = "#E0BA87", dor3   = "#F5E0C0",
  gris1  = "#2C3E50", gris2  = "#5D6D7E", gris3  = "#BDC3C7",
  fondo  = "#F8F9FA", fondo2 = "#EEF0F4"
)
pal_disc <- c(pal$vino1, pal$azul1, pal$dor1, pal$vino2,
              pal$azul2, pal$dor2, pal$vino3, pal$azul3)

# ── Tema ggplot2 publicación ────────────────────────────────────────────────
tema_pub <- theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title        = element_markdown(
      color = pal$vino1, face = "bold", size = 13, lineheight = 1.2),
    plot.subtitle     = element_markdown(
      color = pal$gris1, size = 10, lineheight = 1.3),
    plot.caption      = element_markdown(
      color = pal$gris2, size = 7.5, hjust = 0, face = "italic"),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = pal$fondo2, linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    axis.title        = element_text(color = pal$azul1, size = 9.5),
    axis.text         = element_text(color = pal$gris1, size = 8.5),
    legend.title      = element_text(face = "bold", size = 9),
    legend.text       = element_text(size = 8.5),
    legend.position   = "bottom",
    strip.text        = element_text(
      face = "bold", color = pal$azul1, size = 9),
    strip.background  = element_rect(fill = pal$fondo2, color = NA),
    plot.margin       = margin(12, 16, 10, 12)
  )
theme_set(tema_pub)

# ── Etiquetas de entidad ─────────────────────────────────────────────────────
cat_ent <- c(
  "01"="AGS","02"="BC","03"="BCS","04"="CAMP","05"="COAH","06"="COL",
  "07"="CHIS","08"="CHIH","09"="CDMX","10"="DGO","11"="GTO","12"="GRO",
  "13"="HGO","14"="JAL","15"="MEX","16"="MICH","17"="MOR","18"="NAY",
  "19"="NL","20"="OAX","21"="PUE","22"="QRO","23"="QROO","24"="SLP",
  "25"="SIN","26"="SON","27"="TAB","28"="TAMS","29"="TLAX","30"="VER",
  "31"="YUC","32"="ZAC"
)

nom_ent_full <- c(
  "01"="Aguascalientes","02"="Baja California","03"="Baja California Sur",
  "04"="Campeche","05"="Coahuila","06"="Colima","07"="Chiapas",
  "08"="Chihuahua","09"="Ciudad de México","10"="Durango",
  "11"="Guanajuato","12"="Guerrero","13"="Hidalgo","14"="Jalisco",
  "15"="Estado de México","16"="Michoacán","17"="Morelos","18"="Nayarit",
  "19"="Nuevo León","20"="Oaxaca","21"="Puebla","22"="Querétaro",
  "23"="Quintana Roo","24"="San Luis Potosí","25"="Sinaloa","26"="Sonora",
  "27"="Tabasco","28"="Tamaulipas","29"="Tlaxcala","30"="Veracruz",
  "31"="Yucatán","32"="Zacatecas"
)

# ── Directorios ──────────────────────────────────────────────────────────────
dirs <- c("outputs/censo2020/graficos","outputs/censo2020/tablas",
          "outputs/censo2020/python","data/censo2020/coneval",
          "data/censo2020/procesados","data/censo2020/ite")
invisible(lapply(dirs, dir.create, recursive=TRUE, showWarnings=FALSE))

# ── Funciones auxiliares ─────────────────────────────────────────────────────
guardar_graf <- function(g, nombre, w=11, h=7.5, dpi=320) {
  ruta <- paste0("outputs/censo2020/graficos/", nombre, ".png")
  ggsave(ruta, g, width=w, height=h, dpi=dpi, bg="white")
  message("✓ ", nombre)
}

guardar_tabla <- function(df, nombre) {
  write_csv(df, paste0("outputs/censo2020/tablas/", nombre, ".csv"))
  message("✓ tabla: ", nombre)
}

tabla_gt_pub <- function(df, titulo, subtitulo=NULL, fuente=NULL,
                          col_highlight=NULL) {
  gt_obj <- df |>
    gt() |>
    tab_header(title=md(paste0("**",titulo,"**")),
               subtitle=subtitulo) |>
    tab_source_note(source_note=md(paste0("*",fuente,"*"))) |>
    tab_style(
      style=list(cell_fill(color=pal$vino1),
                 cell_text(color="white", weight="bold")),
      locations=cells_column_labels()) |>
    tab_style(
      style=cell_fill(color=pal$fondo2),
      locations=cells_body(rows=seq(2,nrow(df),2))) |>
    opt_table_font(font=list(google_font("Source Sans Pro"),"sans-serif")) |>
    tab_options(table.font.size=px(12),
                heading.title.font.size=px(14),
                data_row.padding=px(5))
  if (!is.null(col_highlight)) {
    gt_obj <- gt_obj |>
      tab_style(
        style=cell_text(color=pal$vino1, weight="bold"),
        locations=cells_body(columns=all_of(col_highlight)))
  }
  gt_obj
}

norm_mm <- function(x, invert=FALSE) {
  r <- range(x, na.rm=TRUE)
  if (r[1]==r[2]) return(rep(0,length(x)))
  v <- (x-r[1])/(r[2]-r[1])*100
  if (invert) 100-v else v
}

safe_col <- function(df, col) {
  if (col %in% names(df)) df[[col]] else rep(NA_character_, nrow(df))
}


# ═══════════════════════════════════════════════════════════════════════════════
# 1. RUTAS — ajusta el usuario
# ═══════════════════════════════════════════════════════════════════════════════

BASE <- "C:/Users/aname/Desktop/DESAFILIACION_ESCOLAR/data/censo2020"
RUTA_PER <- file.path(BASE, "Personas00.CSV")
RUTA_VIV <- file.path(BASE, "Viviendas00.CSV")
RUTA_ITE <- file.path(BASE, "ITER_NALCSV20.csv")
RUTA_IRS <- "data/coneval/IRS_2020_municipal_estatal.xlsx"

for (f in c(RUTA_PER, RUTA_VIV))
  if (!file.exists(f)) stop("No encontrado: ", f)
message("✓ Archivos verificados")


# ═══════════════════════════════════════════════════════════════════════════════
# F1 ─ MICRODATOS DE EJEMPLO
# Variables personas (cuestionario básico, con flags robustos):
#   ENT, MUN, TAMLOC, SEXO, EDAD, PARENT, ASISTEN, HLENGUA,
#   NIVACAD, CONACT, HIJOS_NAC_VIVOS, CAUSA_MIG_V (si existe)
#   ELENGUA, PERTE_INDIGENA, AFRODES (si existen)
#   DIS_VER, DIS_OIR, DIS_CAMINAR, DIS_RECORDAR,
#   DIS_BANARSE, DIS_HABLAR, DIS_MENTAL (si existen)
# Variables viviendas (cuestionario básico):
#   ID_VIV, ENT, MUN, NUMPERS, TOTCUART, CUADORM, PISOS,
#   ELECTRICIDAD, AGUA_ENTUBADA, DRENAJE, INTERNET,
#   COMPUTADORA, CELULAR, TIPOHOG
# ═══════════════════════════════════════════════════════════════════════════════

# ── 1. LIBRERÍAS ─────────────────────────────────────────────────────────────
library(dplyr)
library(vroom)
library(janitor) # Para clean_names()

# ── 2. CARGA INTELIGENTE Y FILTRADO AL VUELO ─────────────────────────────────

message("Cargando personas (padrón completo)...")

# per_raw = padrón completo; el filtro de edad se aplica al crear cohorte (línea ~308)
per_raw <- vroom(
  RUTA_PER,
  locale = locale(encoding = "latin1"),
  col_types = cols(
    EDAD = col_number(),
    .default = col_character()
  ),
  show_col_types = FALSE
) |>
  clean_names()

message("Cargando viviendas...")
# Si el archivo de viviendas también es gigante, vroom lo leerá en un instante
viv_raw <- vroom(
  RUTA_VIV,
  locale = locale(encoding = "latin1"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |> 
  clean_names()

# ── 3. REPORTES Y VERIFICACIÓN ───────────────────────────────────────────────

cat(sprintf(
  "Personas (padrón completo): %s | Viviendas: %s\n",
  format(nrow(per_raw), big.mark = ","),
  format(nrow(viv_raw), big.mark = ",")
))

cat(sprintf(
  "Entidades presentes: %s\n",
  n_distinct(per_raw$ent)
))

print(sort(unique(per_raw$ent)))

# Forzar limpieza de basura en la RAM
gc()


# ── 3. FLAGS DE VARIABLES OPCIONALES ─────────────────────────────────────────
vp <- names(per_raw); vv <- names(viv_raw)

f <- list(
  hlengua   = "hlengua"        %in% vp,
  elengua   = "elengua"        %in% vp,
  pertind   = "perte_indigena"  %in% vp,
  asisten   = "asisten"        %in% vp,
  afrodes   = "afrodes"        %in% vp,
  causa_mig = "causa_mig_v"    %in% vp,
  hnv       = "hijos_nac_vivos" %in% vp,
  dis_ver   = "dis_ver"        %in% vp,
  dis_oir   = "dis_oir"        %in% vp,
  dis_cam   = "dis_caminar"    %in% vp,
  dis_rec   = "dis_recordar"   %in% vp,
  dis_ban   = "dis_banarse"    %in% vp,
  dis_hab   = "dis_hablar"     %in% vp,
  dis_men   = "dis_mental"     %in% vp
)

cat("\n── Variables opcionales disponibles ──\n")
for (nm in names(f)) cat(sprintf("  %-14s %s\n", nm,
  ifelse(f[[nm]], "✓ disponible", "✗ ausente en base ejemplo")))

# ── 4. VIVIENDAS ─────────────────────────────────────────────────────────────
# ELECTRICIDAD: 1=sí, 3=no | AGUA_ENTUBADA: 1=dentro, 2=patio, 3=no tiene
# DRENAJE: 1=red pública ... 5=no tiene | INTERNET: 7=sí, 8=no
# COMPUTADORA: 1=sí, 2=no | CELULAR: 5=sí, 6=no | PISOS: 1=tierra
library(stringr)

viviendas <- viv_raw |>
  mutate(
    numpers      = as.numeric(numpers),
    totcuart     = as.numeric(totcuart),
    cuadorm      = as.numeric(cuadorm),
    hacinamiento = numpers / pmax(totcuart, 1, na.rm=TRUE),
    sin_elect    = as.integer(electricidad  == "3"),
    sin_agua     = as.integer(agua_entubada == "3"),
    sin_drenaje  = as.integer(drenaje       == "5"),
    sin_internet = as.integer(internet      == "8"),
    sin_pc       = as.integer(computadora   == "2"),
    sin_celular  = as.integer(celular       == "6"),
    brecha_dig   = as.integer(sin_internet==1 & sin_pc==1),
    piso_tierra  = as.integer(pisos        == "1"),
    tipo_hogar   = case_when(
      tipohog=="1"~"Nuclear",    tipohog=="2"~"Ampliado",
      tipohog=="3"~"Compuesto",  tipohog=="5"~"Unipersonal",
      TRUE~"Otro"),
    cve_ent      = str_pad(ent, 2, "left", "0"),
    cve_mun      = str_pad(mun, 3, "left", "0"),
    cve_mun_full = paste0(cve_ent, cve_mun)
  ) |>
  select(id_viv, cve_ent, cve_mun, cve_mun_full, numpers,
         totcuart, cuadorm, hacinamiento, sin_elect, sin_agua,
         sin_drenaje, sin_internet, sin_pc, brecha_dig,
         piso_tierra, tipo_hogar, tamloc)

# ── 5. PERSONAS — COHORTE 8-11 AÑOS ─────────────────────────────────────────

cohorte <- per_raw |>
  mutate(edad = as.numeric(edad)) |>
  filter(edad >= 8, edad <= 11) |>
  mutate(
    cve_ent      = str_pad(ent, 2, "left", "0"),
    cve_mun      = str_pad(mun, 3, "left", "0"),
    cve_mun_full = paste0(cve_ent, cve_mun),
    nom_ent_abr  = cat_ent[cve_ent],
    nom_ent_full = nom_ent_full[cve_ent],

    rural = case_when(
      tamloc=="1"~"Rural", tamloc %in% c("2","3","4","5")~"Urbana",
      TRUE~NA_character_),

    sexo = case_when(
      sexo=="1"~"Hombre", sexo=="3"~"Mujer", TRUE~NA_character_),

    asiste = case_when(
      asisten=="1"~"Asiste", asisten=="3"~"No asiste", TRUE~NA_character_),
    no_asiste = as.integer(asisten=="3"),

    # ── Condición indígena — criterios disponibles ──
    hli   = as.integer(hlengua=="1"),
    hli_l = if_else(hlengua=="1","Hablante LI","No hablante"),

    # Criterio ampliado: solo con variables disponibles
    indig = case_when(
      hlengua=="1" ~ "Indígena",
      f$hlengua   & .data[["hlengua"]]=="1"        ~ "Indígena",
      TRUE ~ "No indígena"),

    # ── Afrodescendencia ──
    afro   = as.integer(if(f$afrodes) afrodes=="1" else FALSE),
    afro_l = if(f$afrodes)
               if_else(afrodes=="1","Afrodesc.","No afrodesc.")
             else "No disp. en base ejemplo",

    # ── Migración ──
    migrante = as.integer(if(f$causa_mig)
                 !is.na(causa_mig_v) & causa_mig_v!="" else FALSE),

    # ── Discapacidad — escala 3=mucha dificultad, 4=no puede
    #    dis_mental usa 5=Sí ──
    d_vis  = as.integer(if(f$dis_ver) dis_ver      %in% c("3","4") else FALSE),
    d_aud  = as.integer(if(f$dis_oir) dis_oir      %in% c("3","4") else FALSE),
    d_mot  = as.integer(if(f$dis_cam) dis_caminar  %in% c("3","4") else FALSE),
    d_cog  = as.integer(if(f$dis_rec) dis_recordar %in% c("3","4") else FALSE),
    d_aut  = as.integer(if(f$dis_ban) dis_banarse  %in% c("3","4") else FALSE),
    d_com  = as.integer(if(f$dis_hab) dis_hablar   %in% c("3","4") else FALSE),
    d_men  = as.integer(if(f$dis_men) dis_mental   == "5"          else FALSE),
    discap = as.integer(d_vis|d_aud|d_mot|d_cog|d_aut|d_com|d_men),
    discap_l = if_else(discap==1,"Con discapacidad","Sin discapacidad")
  ) |>
  left_join(viviendas |>
    select(id_viv, hacinamiento, sin_elect, sin_agua, sin_drenaje,
           brecha_dig, piso_tierra, tipo_hogar), by="id_viv")

# Escolaridad y actividad del jefe/a
# Detectar nombre real de la columna de parentesco (varía según archivo)
col_parent <- intersect(c("parent","parentesco","relacion"), names(per_raw))[1]
if (is.na(col_parent))
  stop("No se encontró columna de parentesco en per_raw. Columnas disponibles: ",
       paste(names(per_raw), collapse=", "))

jefes <- per_raw |>
  filter(.data[[col_parent]] == "01") |>
  mutate(
    escol = case_when(
      nivacad=="00"~"Sin escolaridad", nivacad=="01"~"Preescolar",
      nivacad=="02"~"Primaria",        nivacad=="03"~"Secundaria",
      nivacad %in% c("04","05")~"Bachillerato",
      as.numeric(nivacad) >= 6 ~ "Superior", TRUE~NA_character_),
    escol_n = case_when(
      nivacad=="00"~0, nivacad=="01"~1, nivacad=="02"~2,
      nivacad=="03"~3, nivacad %in% c("04","05")~4,
      as.numeric(nivacad)>=6~5, TRUE~NA_real_),
    activ = case_when(
      as.numeric(conact) %in% 10:20 ~ "Trabaja",
      conact=="30" ~ "Busca empleo",
      conact=="50" ~ "Estudia",
      conact=="60" ~ "Quehaceres del hogar",
      conact %in% c("70","80") ~ "No trabaja",
      TRUE~NA_character_)
  ) |>
  select(id_viv, escol, escol_n, activ)

cohorte <- cohorte |> left_join(jefes, by="id_viv")
cat(sprintf("✓ Cohorte 8-11 años: %s personas\n",
    format(nrow(cohorte), big.mark=",")))


# ═══════════════════════════════════════════════════════════════════════════════
# PI-1 ─ DESIGUALDADES ESTRUCTURALES (F1)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── PI-1: Desigualdades estructurales ──")

# TABLA 1 — Resumen estadístico de la cohorte
t1 <- cohorte |>
  summarise(
    N                         = n(),
    `% Hombres`               = mean(sexo=="Hombre",    na.rm=T)*100,
    `% Rural`                 = mean(rural=="Rural",     na.rm=T)*100,
    `% Hablante LI`           = mean(hli==1,             na.rm=T)*100,
    `% Indígena (ampliado)`   = mean(indig=="Indígena",  na.rm=T)*100,
    `% Afrodescendiente`      = mean(afro==1,            na.rm=T)*100,
    `% Con discapacidad`      = mean(discap==1,          na.rm=T)*100,
    `% No asiste escuela`     = mean(no_asiste==1,       na.rm=T)*100,
    `% Brecha digital`        = mean(brecha_dig==1,      na.rm=T)*100,
    `Hacinamiento (media)`    = mean(hacinamiento,       na.rm=T),
    `% Piso tierra`           = mean(piso_tierra==1,     na.rm=T)*100,
    `% Sin agua`              = mean(sin_agua==1,        na.rm=T)*100
  ) |>
  pivot_longer(everything(), names_to="Indicador", values_to="Valor") |>
  mutate(Valor = round(Valor, 2))
guardar_tabla(t1, "t1_resumen_cohorte_F1")

# TABLA 2 — Por entidad: indicadores clave
t2 <- cohorte |>
  group_by(nom_ent_full, nom_ent_abr) |>
  summarise(
    n              = n(),
    pct_hli        = mean(hli==1,        na.rm=T)*100,
    pct_afro       = mean(afro==1,       na.rm=T)*100,
    pct_discap     = mean(discap==1,     na.rm=T)*100,
    pct_rural      = mean(rural=="Rural",na.rm=T)*100,
    pct_no_asiste  = mean(no_asiste==1,  na.rm=T)*100,
    pct_brecha     = mean(brecha_dig==1, na.rm=T)*100,
    hacinam        = mean(hacinamiento,  na.rm=T),
    .groups="drop"
  ) |> arrange(desc(pct_hli))
guardar_tabla(t2, "t2_indicadores_entidad_F1")

# ── GRÁFICO 1: Panel de desigualdades estructurales ─────────────────────────

g1_data <- t2 |>
  select(nom_ent_abr, pct_hli, pct_afro, pct_discap) |>
  pivot_longer(-nom_ent_abr, names_to="ind", values_to="pct") |>
  mutate(ind_lab = case_when(
    ind=="pct_hli"   ~ "Hablantes de\nlengua indígena",
    ind=="pct_afro"  ~ "Afrodescendientes",
    ind=="pct_discap"~ "Con discapacidad"),
    ind_lab = factor(ind_lab, levels=c(
      "Hablantes de\nlengua indígena","Afrodescendientes","Con discapacidad")))

g1 <- ggplot(g1_data, aes(pct, reorder(nom_ent_abr, pct),
                           fill=ind_lab)) +
  geom_col(show.legend=FALSE, alpha=.88, width=.75) +
  facet_wrap(~ind_lab, scales="free_x", nrow=1) +
  scale_fill_manual(values=c(pal$azul1, pal$dor1, pal$vino2)) +
  scale_x_continuous(labels=label_number(suffix="%",accuracy=.1),
                     expand=expansion(mult=c(0,.12))) +
  labs(
    title  = "**Desigualdades estructurales en la cohorte 8-11 años**
              <span style='color:#5D6D7E;font-size:10pt;font-weight:normal'>
              Distribución porcentual por entidad federativa</span>",
    x=NULL, y=NULL,
    caption="**Fuente:** Censo de Población y Vivienda 2020,
             microdatos de ejemplo (INEGI). Elaboración propia.<br>
             *Resultados de carácter ilustrativo basados en muestra de ejemplo.*") +
  theme(panel.grid.major.y=element_blank(),
        axis.text.y=element_text(size=8))
guardar_graf(g1, "g1_PI1_desigualdades_estructurales", w=13, h=8)

# ── GRÁFICO 2: Brechas por sexo en HLI y no asistencia ──────────────────────

g2_data <- cohorte |>
  filter(!is.na(sexo)) |>
  group_by(nom_ent_abr, sexo) |>
  summarise(
    pct_hli      = mean(hli==1,       na.rm=T)*100,
    pct_no_asis  = mean(no_asiste==1, na.rm=T)*100,
    .groups="drop"
  ) |>
  pivot_longer(c(pct_hli, pct_no_asis), names_to="ind", values_to="pct") |>
  mutate(ind_lab = if_else(ind=="pct_hli",
                           "% Hablante LI","% No asiste escuela"))

g2 <- ggplot(g2_data,
       aes(pct, reorder(nom_ent_abr, pct), color=sexo, shape=sexo)) +
  geom_line(aes(group=nom_ent_abr), color=pal$gris3, linewidth=.5) +
  geom_point(size=2.5, alpha=.9) +
  facet_wrap(~ind_lab, scales="free_x") +
  scale_color_manual(
    values=c("Hombre"=pal$azul1,"Mujer"=pal$vino2), name=NULL) +
  scale_shape_manual(
    values=c("Hombre"=16,"Mujer"=17), name=NULL) +
  scale_x_continuous(labels=label_number(suffix="%",accuracy=.1),
                     expand=expansion(mult=c(.02,.12))) +
  labs(
    title  = "**Brecha de género en condición indígena y asistencia escolar**
              <span style='color:#5D6D7E;font-size:10pt;font-weight:normal'>
              Cohorte 8-11 años — por entidad federativa</span>",
    x=NULL, y=NULL,
    caption="**Fuente:** Microdatos de ejemplo, Censo 2020 (INEGI).") +
  theme(panel.grid.major.y=element_blank(),
        legend.position="top", axis.text.y=element_text(size=7.5))
guardar_graf(g2, "g2_PI1_brecha_genero_hli_asistencia", w=12, h=8)


# ═══════════════════════════════════════════════════════════════════════════════
# PI-2 ─ NO ASISTENCIA ESCOLAR (F1)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── PI-2: No asistencia escolar ──")

t3 <- cohorte |>
  filter(!is.na(rural), !is.na(sexo)) |>
  group_by(nom_ent_abr, rural, sexo) |>
  summarise(n=n(), n_noasis=sum(no_asiste,na.rm=T),
            pct=n_noasis/n*100, .groups="drop")
guardar_tabla(t3, "t3_noasistencia_entidad_rural_sexo_F1")

# ── GRÁFICO 3: No asistencia por ruralidad y sexo (Cleveland dot) ────────────

g3_base <- cohorte |>
  filter(!is.na(rural), !is.na(sexo)) |>
  group_by(nom_ent_abr, rural) |>
  summarise(pct=mean(no_asiste==1,na.rm=T)*100,.groups="drop") |>
  pivot_wider(names_from=rural, values_from=pct) |>
  mutate(brecha = Rural - Urbana,
         ord    = Rural)

g3 <- g3_base |>
  pivot_longer(c(Rural,Urbana), names_to="tipo", values_to="pct") |>
  ggplot(aes(pct, reorder(nom_ent_abr, ord))) +
  geom_line(aes(group=nom_ent_abr), color=pal$gris3, linewidth=.7) +
  geom_point(aes(fill=tipo), shape=21, size=3.2, color="white", stroke=.4) +
  geom_text(data=g3_base,
    aes(x=pmax(Rural,Urbana)+.4, y=nom_ent_abr,
        label=sprintf("+%.1f%%",brecha)),
    size=2.6, color=pal$vino1, fontface="bold", hjust=0) +
  scale_fill_manual(
    values=c("Rural"=pal$dor1,"Urbana"=pal$azul2),
    name="Tipo de localidad") +
  scale_x_continuous(labels=label_number(suffix="%",accuracy=.1),
                     expand=expansion(mult=c(.02,.18))) +
  labs(
    title  = "**Brecha urbano-rural en no asistencia escolar**
              <span style='color:#5D6D7E;font-size:10pt;font-weight:normal'>
              Cohorte 8-11 años | etiqueta = brecha rural sobre urbana</span>",
    x="% que no asiste a la escuela", y=NULL,
    caption="**Fuente:** Microdatos de ejemplo, Censo 2020 (INEGI).") +
  theme(panel.grid.major.y=element_blank(),
        axis.text.y=element_text(size=8))
guardar_graf(g3, "g3_PI2_noasistencia_rural_urban_cleveland", w=11, h=8)

# ── GRÁFICO 4: No asistencia por condición étnica (HLI × sexo) ───────────────

g4_data <- cohorte |>
  filter(!is.na(sexo), !is.na(hli_l)) |>
  group_by(hli_l, sexo, rural) |>
  summarise(pct=mean(no_asiste==1,na.rm=T)*100,.groups="drop") |>
  filter(!is.na(rural))

g4 <- ggplot(g4_data,
       aes(x=interaction(hli_l,rural,sep=" · "), y=pct,
           fill=sexo)) +
  geom_col(position=position_dodge(.8), width=.7, alpha=.9) +
  geom_text(aes(label=sprintf("%.1f%%",pct)),
            position=position_dodge(.8), vjust=-.5,
            size=2.8, color=pal$gris1) +
  scale_fill_manual(
    values=c("Hombre"=pal$azul1,"Mujer"=pal$vino2), name=NULL) +
  scale_y_continuous(labels=label_number(suffix="%"),
                     expand=expansion(mult=c(0,.18))) +
  labs(
    title  = "**No asistencia escolar según condición étnica, tipo de localidad y sexo**",
    subtitle="Cohorte 8-11 años — microdatos de ejemplo, Censo 2020",
    x=NULL, y="% que no asiste",
    caption="**Fuente:** Microdatos de ejemplo, Censo 2020 (INEGI).") +
  theme(axis.text.x=element_text(size=8,angle=15,hjust=1))
guardar_graf(g4, "g4_PI2_noasistencia_etnia_sexo_rural", w=11, h=6.5)


# ═══════════════════════════════════════════════════════════════════════════════
# PI-3 ─ CONDICIONES DEL HOGAR (F1)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── PI-3: Condiciones del hogar ──")

t4 <- cohorte |>
  group_by(nom_ent_abr) |>
  summarise(
    pct_sin_agua   = mean(sin_agua==1,  na.rm=T)*100,
    pct_sin_elect  = mean(sin_elect==1, na.rm=T)*100,
    pct_sin_dren   = mean(sin_drenaje==1,na.rm=T)*100,
    pct_brecha_dig = mean(brecha_dig==1,na.rm=T)*100,
    pct_piso_tierra= mean(piso_tierra==1,na.rm=T)*100,
    hacinam_media  = mean(hacinamiento, na.rm=T),
    .groups="drop"
  )
guardar_tabla(t4, "t4_carencias_vivienda_entidad_F1")

# ── GRÁFICO 5: Heatmap de carencias por entidad ──────────────────────────────
# (Python via reticulate si disponible, ggplot2 si no)

if (py_available) {
  # Exportar datos para Python
  t4_py <- t4 |>
    rename(Entidad=nom_ent_abr,
           `Sin agua`=pct_sin_agua,
           `Sin electricidad`=pct_sin_elect,
           `Sin drenaje`=pct_sin_dren,
           `Brecha digital`=pct_brecha_dig,
           `Piso de tierra`=pct_piso_tierra,
           Hacinamiento=hacinam_media)

  write_csv(t4_py, "outputs/censo2020/python/heatmap_data.csv")

  py_run_string('
import pandas as pd, numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import seaborn as sns

df = pd.read_csv("outputs/censo2020/python/heatmap_data.csv", index_col="Entidad")
df_z = (df - df.mean()) / df.std()   # estandarizar para comparabilidad

# Ordenar por índice compuesto
df_z["orden"] = df_z.mean(axis=1)
df_z = df_z.sort_values("orden")
df_z = df_z.drop(columns="orden")
df = df.loc[df_z.index]

fig, ax = plt.subplots(figsize=(12, 10), facecolor="white")
cmap = sns.diverging_palette(210, 10, as_cmap=True)

sns.heatmap(
    df_z,
    ax=ax, cmap=cmap, center=0,
    linewidths=0.4, linecolor="#EEEEEE",
    cbar_kws={"label":"Desviaciones estándar (z-score)",
              "shrink":0.6, "orientation":"horizontal",
              "pad":0.02},
    annot=df.round(1), fmt=".1f",
    annot_kws={"size":7.5, "color":"#2C3E50"}
)

ax.set_title("Carencias materiales en hogares de la cohorte 8-11 años\nMicrodatos de ejemplo — Censo 2020 (INEGI)",
             fontsize=13, fontweight="bold", color="#6B1A2B", pad=14)
ax.set_xlabel("Indicador de carencia", fontsize=10, color="#1B3A5C", labelpad=8)
ax.set_ylabel("Entidad federativa", fontsize=10, color="#1B3A5C")
ax.tick_params(axis="y", labelsize=8.5, rotation=0)
ax.tick_params(axis="x", labelsize=9,  rotation=20)

plt.figtext(0.01, 0.005,
    "Valores en celdas: valor original. Color: z-score relativo a la media nacional.  "
    "Fuente: Microdatos de ejemplo, Censo 2020 (INEGI). Elaboración propia.",
    fontsize=7, color="#888888", style="italic")
plt.tight_layout()
plt.savefig("outputs/censo2020/graficos/g5_PI3_heatmap_carencias_python.png",
            dpi=320, bbox_inches="tight", facecolor="white")
plt.close()
print("g5 heatmap guardado (Python)")
')
  message("✓ g5_PI3_heatmap_carencias_python")
} else {
  # Versión ggplot2 del heatmap
  g5_data <- t4 |>
    pivot_longer(-nom_ent_abr, names_to="carencia", values_to="valor") |>
    group_by(carencia) |>
    mutate(z = (valor - mean(valor,na.rm=T))/sd(valor,na.rm=T)) |>
    ungroup() |>
    mutate(carencia = recode(carencia,
      pct_sin_agua="Sin agua", pct_sin_elect="Sin electricidad",
      pct_sin_dren="Sin drenaje", pct_brecha_dig="Brecha digital",
      pct_piso_tierra="Piso tierra", hacinam_media="Hacinamiento"))

  g5 <- ggplot(g5_data,
         aes(carencia, reorder(nom_ent_abr, z), fill=z)) +
    geom_tile(color="white", linewidth=.3) +
    geom_text(aes(label=round(valor,1)), size=2.5, color=pal$gris1) +
    scale_fill_gradient2(low=pal$azul2, mid="white", high=pal$vino1,
      midpoint=0, name="z-score") +
    scale_x_discrete(labels=function(x) str_wrap(x,10)) +
    labs(
      title  = "**Carencias materiales en hogares de la cohorte 8-11 años**",
      subtitle="Color: posición relativa vs media nacional | Valor: dato original",
      x=NULL, y=NULL,
      caption="**Fuente:** Microdatos de ejemplo, Censo 2020 (INEGI).") +
    theme(axis.text.x=element_text(angle=20,hjust=1,size=8.5),
          axis.text.y=element_text(size=8.5),
          legend.position="right")
  guardar_graf(g5, "g5_PI3_heatmap_carencias_R", w=12, h=9)
}

# ── GRÁFICO 6: Escolaridad del jefe del hogar (barras 100%) ──────────────────

niveles_escol <- c("Sin escolaridad","Preescolar","Primaria",
                   "Secundaria","Bachillerato","Superior")
colores_escol <- setNames(
  c(pal$vino1,"#922B21",pal$dor1,"#E0BA87",pal$azul2,pal$azul1),
  niveles_escol)

g6_data <- cohorte |>
  filter(!is.na(escol)) |>
  mutate(escol = factor(escol, levels = niveles_escol)) |>
  count(nom_ent_abr, escol, .drop = FALSE) |>
  group_by(nom_ent_abr) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

# Orden por proporción de escolaridad baja (externo a aes para evitar conflicto de niveles)
ord_g6 <- g6_data |>
  filter(escol %in% c("Sin escolaridad", "Preescolar", "Primaria")) |>
  group_by(nom_ent_abr) |>
  summarise(pct_baja = sum(pct), .groups = "drop") |>
  arrange(pct_baja)

g6_data <- g6_data |>
  mutate(nom_ent_abr = factor(nom_ent_abr, levels = ord_g6$nom_ent_abr))

g6 <- ggplot(g6_data, aes(n, nom_ent_abr, fill = escol)) +
  geom_col(position = "fill", width = .85) +
  scale_fill_manual(values = colores_escol, name = "Último nivel aprobado",
                    drop = FALSE) +
  scale_x_continuous(labels = label_percent(),
                     expand = expansion(mult = c(0, .01))) +
  geom_vline(xintercept = .5, linetype = "dashed",
             color = pal$gris2, linewidth = .5) +
  labs(
    title    = "**Escolaridad del jefe/a del hogar — cohorte 8-11 años (2020)**",
    subtitle = "Proporción por nivel educativo y entidad federativa",
    x = "Proporción", y = NULL,
    caption  = "**Fuente:** Microdatos de ejemplo, Censo 2020 (INEGI).
             Escolaridad de la persona de referencia del hogar (PARENT=01).") +
  theme(axis.text.y = element_text(size = 8),
        legend.key.size = unit(.4, "cm"))
guardar_graf(g6, "g6_PI3_escolaridad_jefe_hogar", w=13, h=8)


# ═══════════════════════════════════════════════════════════════════════════════
# PI-4 ─ PERFIL MULTIDIMENSIONAL — ÍNDICE DE VULNERABILIDAD (F1)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── PI-4: Índice de vulnerabilidad educativa ──")

vuln_micro <- cohorte |>
  group_by(cve_ent, cve_mun, cve_mun_full, nom_ent_abr) |>
  summarise(
    n_coh      = n(),
    pct_hli    = mean(hli==1,        na.rm=T)*100,
    pct_rural  = mean(rural=="Rural",na.rm=T)*100,
    pct_discap = mean(discap==1,     na.rm=T)*100,
    pct_noasis = mean(no_asiste==1,  na.rm=T)*100,
    hacinam    = mean(hacinamiento,  na.rm=T),
    pct_sinagu = mean(sin_agua==1,   na.rm=T)*100,
    pct_sinele = mean(sin_elect==1,  na.rm=T)*100,
    pct_brecha = mean(brecha_dig==1, na.rm=T)*100,
    pct_piso   = mean(piso_tierra==1,na.rm=T)*100,
    .groups="drop"
  ) |>
  filter(n_coh >= 8) |>
  mutate(
    n_hli    = norm_mm(pct_hli),
    n_rural  = norm_mm(pct_rural),
    n_discap = norm_mm(pct_discap),
    n_noasis = norm_mm(pct_noasis),
    n_agua   = norm_mm(pct_sinagu),
    n_elect  = norm_mm(pct_sinele),
    n_brecha = norm_mm(pct_brecha),
    n_piso   = norm_mm(pct_piso),
    n_hacin  = norm_mm(hacinam),
    IVE      = rowMeans(cbind(n_hli,n_rural,n_discap,n_noasis,
                              n_agua,n_elect,n_brecha,n_piso,
                              n_hacin), na.rm=TRUE),
    quintil  = ntile(IVE, 5),
    grupo    = factor(case_when(
      quintil==5~"Muy alta",quintil==4~"Alta",quintil==3~"Media",
      quintil==2~"Baja",quintil==1~"Muy baja"),
      levels=c("Muy alta","Alta","Media","Baja","Muy baja"))
  )

t5 <- vuln_micro |>
  arrange(desc(IVE)) |> slice_head(n=20) |>
  select(nom_ent_abr, cve_mun_full, n_coh,
         pct_hli, pct_rural, pct_brecha, pct_noasis, IVE, grupo)
guardar_tabla(t5, "t5_top20_municipios_vulnerables_F1")
guardar_tabla(vuln_micro, "t5b_vulnerabilidad_municipal_F1")

# ── GRÁFICO 7: Boxplot del IVE por entidad ──────────────────────────────────

col_quintil <- c("Muy alta"=pal$vino1,"Alta"=pal$vino2,
                 "Media"=pal$dor1,"Baja"=pal$azul2,"Muy baja"=pal$azul1)

g7 <- vuln_micro |>
  ggplot(aes(IVE, reorder(nom_ent_abr, IVE, median), fill=grupo)) +
  geom_boxplot(alpha=.8, outlier.size=.8, outlier.alpha=.4,
               linewidth=.4) +
  scale_fill_manual(values=col_quintil, name="Quintil IVE") +
  scale_x_continuous(labels=label_number(accuracy=1),
                     expand=expansion(mult=c(.02,.05))) +
  labs(
    title  = "**Índice de Vulnerabilidad Educativa (IVE) por entidad federativa**",
    subtitle="Distribución municipal — 10 componentes normalizados min-max",
    x="IVE (0-100)", y=NULL,
    caption="**Fuente:** Microdatos de ejemplo, Censo 2020 (INEGI). Elaboración propia.
             *IVE = promedio de 9 componentes: HLI, ruralidad, discapacidad, no asistencia,
             carencias de agua/electricidad, brecha digital, piso tierra, hacinamiento.*") +
  theme(axis.text.y=element_text(size=8),
        legend.key.size=unit(.4,"cm"))
guardar_graf(g7, "g7_PI4_IVE_boxplot_entidad", w=12, h=8)

# ── GRÁFICO 8: Radar de componentes por quintil (Python) ─────────────────────

comp_quintil <- vuln_micro |>
  group_by(grupo) |>
  summarise(across(c(n_hli,n_rural,n_discap,n_noasis,n_agua,
                     n_elect,n_brecha,n_piso,n_hacin,n_escol),
                   ~mean(.x,na.rm=T)), .groups="drop")

write_csv(comp_quintil, "outputs/censo2020/python/radar_data.csv")

if (py_available) {
  py_run_string('
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

df = pd.read_csv("outputs/censo2020/python/radar_data.csv")
labels = {
    "n_hli":"HLI","n_rural":"Ruralidad",
    "n_discap":"Discapacidad","n_noasis":"No asiste",
    "n_agua":"Sin agua","n_elect":"Sin electricidad",
    "n_brecha":"Brecha digital","n_piso":"Piso tierra",
    "n_hacin":"Hacinamiento","n_escol":"Escolaridad\nbaja jefe"
}
cats   = list(labels.values())
N      = len(cats)
angles = np.linspace(0, 2*np.pi, N, endpoint=False).tolist()
angles += angles[:1]

colores = {"Muy alta":"#6B1A2B","Alta":"#8B2642",
           "Media":"#C4975A","Baja":"#2E5F8A","Muy baja":"#1B3A5C"}

fig, ax = plt.subplots(figsize=(9,9),
    subplot_kw=dict(polar=True), facecolor="white")

for _, row in df.iterrows():
    grupo = row["grupo"]
    vals  = row[list(labels.keys())].values.tolist()
    vals += vals[:1]
    ax.plot(angles, vals, color=colores[grupo],
            linewidth=2.2, label=grupo)
    ax.fill(angles, vals, color=colores[grupo], alpha=0.08)

ax.set_xticks(angles[:-1])
ax.set_xticklabels(cats, size=9.5, color="#2C3E50")
ax.set_ylim(0,100)
ax.set_yticks([20,40,60,80])
ax.set_yticklabels(["20","40","60","80"], size=7.5, color="#888888")
ax.grid(color="#EEEEEE", linewidth=.7)
ax.spines["polar"].set_color("#CCCCCC")

ax.set_title("Perfil de componentes del IVE por quintil de vulnerabilidad\nCohorte 8-11 años — Microdatos de ejemplo, Censo 2020",
    size=12, fontweight="bold", color="#6B1A2B", pad=20)

handles = [mpatches.Patch(color=c, label=g)
           for g,c in colores.items()]
ax.legend(handles=handles, loc="lower right",
          bbox_to_anchor=(1.35,0.0), fontsize=9,
          title="Quintil IVE", title_fontsize=9.5, frameon=False)

plt.figtext(0.5,-0.01,
    "Valores normalizados 0-100 (min-max). Mayor valor = mayor riesgo.\n"
    "Fuente: Microdatos de ejemplo, Censo 2020 (INEGI). Elaboración propia.",
    ha="center", fontsize=7.5, color="#888888", style="italic")
plt.tight_layout()
plt.savefig("outputs/censo2020/graficos/g8_PI4_radar_IVE_quintiles_python.png",
            dpi=320, bbox_inches="tight", facecolor="white")
plt.close()
print("g8 radar guardado (Python)")
')
  message("✓ g8_PI4_radar_IVE_quintiles_python")
} else {
  # Versión ggplot2 del radar (aproximado con barras polares)
  g8 <- comp_quintil |>
    pivot_longer(-grupo, names_to="comp", values_to="val") |>
    mutate(comp=recode(comp,
      n_hli="HLI",n_rural="Ruralidad",n_discap="Discapacidad",
      n_noasis="No asiste",n_agua="Sin agua",n_elect="Sin elect.",
      n_brecha="Brecha dig.",n_piso="Piso tierra",
      n_hacin="Hacinamiento",n_escol="Escol. baja"),
    grupo=factor(grupo,levels=c("Muy alta","Alta","Media","Baja","Muy baja"))) |>
    ggplot(aes(comp, val, fill=grupo, group=grupo)) +
    geom_col(position="dodge", width=.75, alpha=.85) +
    scale_fill_manual(values=col_quintil, name="Quintil IVE") +
    scale_y_continuous(labels=label_number(accuracy=1)) +
    scale_x_discrete(labels=function(x) str_wrap(x,8)) +
    labs(title="**Componentes del IVE por quintil de vulnerabilidad**",
         x=NULL, y="Valor normalizado (0-100)",
         caption="**Fuente:** Microdatos de ejemplo, Censo 2020 (INEGI).") +
    theme(axis.text.x=element_text(size=7.5,angle=20,hjust=1))
  guardar_graf(g8, "g8_PI4_componentes_IVE_quintil_R", w=13, h=6.5)
}


# ═══════════════════════════════════════════════════════════════════════════════
# F2 ─ ITER NACIONAL 2020
# Variables clave (post clean_names — ya confirmadas en diagnóstico):
#   entidad, mun, loc, nom_loc, tamloc
#   pobtot, pobfem, pobmas
#   p_8a14, p_8a14_f, p_8a14_m → cohorte 8-14
#   p_12a14, p_12a14_f, p_12a14_m → restar para 8-11
#   p_15a17, p_15a17_f, p_15a17_m, p15a17a, p15a17a_f, p15a17a_m
#   p3ym_hli, p3ym_hli_f, p3ym_hli_m
#   phog_ind (hogares indígenas)
#   pob_afro, pob_afro_f, pob_afro_m
#   pcon_disc, pcdisc_mot, pcdisc_vis, pcdisc_leng, pcdisc_aud, pcdisc_men
#   p6a11_noa, p6a11_noaf, p6a11_noam
#   p12a14noa, p12a14noaf, p12a14noam
#   vivpar_hab, vivparh_cv
#   vph_s_elec, vph_aguafv, vph_nodren, vph_inter, vph_pc, vph_cel
#   vph_pisoti, vph_refri, vph_lavad
#   p15ym_an, p15ym_se, p15pri_in, p15pri_co, p15sec_in (para IRS)
#   psinder (sin derechohabiencia — para IRS)
#   vph_excsa, vph_letr (para IRS)
#   pnacoe, pnacoe_f, pnacoe_m (nacidos en otro estado — migración)
# ═══════════════════════════════════════════════════════════════════════════════

# Limpiar objetos ITER de sesiones previas
rm(list = ls()[grepl("iter_raw|iter_ent|iter_mun|iter_loc",
                      ls(), ignore.case = TRUE)])
invisible(gc())

message("\n════ F2: ITER NACIONAL 2020 ════")

if (!file.exists(RUTA_ITE)) {
  message("⚠ ITER no disponible — omitiendo F2 y F3.")
  iter_disponible <- FALSE
} else {
  iter_disponible <- TRUE

  message("Cargando ITER...")
  iter_raw <- fread(RUTA_ITE,
    stringsAsFactors = FALSE,
    encoding         = "Latin-1",
    sep              = ",",
    na.strings       = c("", "NA", "N/A")) |>
    as.data.frame() |>
    rename_with(tolower)

  cat(sprintf("✓ ITER: %s localidades, %s variables\n",
      format(nrow(iter_raw), big.mark=","), ncol(iter_raw)))

  # ── Preparación ITER ──────────────────────────────────────────────────────
  vars_num_iter <- c(
    "pobtot","pobfem","pobmas",
    "p_8a14","p_8a14_f","p_8a14_m",
    "p_12a14","p_12a14_f","p_12a14_m",
    "p_15a17","p_15a17_f","p_15a17_m",
    "p15a17a","p15a17a_f","p15a17a_m",
    "p3ym_hli","p3ym_hli_f","p3ym_hli_m",
    "phog_ind","pob_afro","pob_afro_f","pob_afro_m",
    "pcon_disc","pcdisc_mot","pcdisc_vis",
    "pcdisc_leng","pcdisc_aud","pcdisc_men",
    "p6a11_noa","p6a11_noaf","p6a11_noam",
    "p12a14noa","p12a14noaf","p12a14noam",
    "pnacoe","pnacoe_f","pnacoe_m",
    "vivpar_hab","vivparh_cv",
    "vph_s_elec","vph_aguafv","vph_nodren",
    "vph_inter","vph_pc","vph_cel","vph_pisoti",
    "vph_refri","vph_lavad","vph_excsa","vph_letr",
    "p_15ymas","p15ym_an","p15ym_se",
    "p15pri_in","p15pri_co","p15sec_in","psinder",
    "p_6a11","p_12a14"
  )
  vars_num_iter <- intersect(vars_num_iter, names(iter_raw))

  iter <- iter_raw |>
    mutate(
      cve_ent      = formatC(entidad, width=2, flag="0"),
      cve_mun      = formatC(mun,     width=3, flag="0"),
      cve_loc      = formatC(loc,     width=4, flag="0"),
      cve_mun_full = paste0(cve_ent, cve_mun),
      nom_ent_abr  = cat_ent[cve_ent],
      rural_iter   = case_when(
        tamloc=="1"~"Rural",
        tamloc %in% c("2","3","4","5")~"Urbana",
        TRUE~NA_character_),
      across(all_of(vars_num_iter),
             ~as.numeric(str_replace_all(.x,"[^0-9\\.]","")))
    ) |>
    mutate(
      # Cohorte exacta 8-11 = (8-14) − (12-14)
      p_8a11   = p_8a14   - p_12a14,
      p_8a11_f = p_8a14_f - p_12a14_f,
      p_8a11_m = p_8a14_m - p_12a14_m,
      # No asistencia 15-17
      p15a17_noa   = pmax(p_15a17   - p15a17a,   0, na.rm=TRUE),
      p15a17_noa_f = pmax(p_15a17_f - p15a17a_f, 0, na.rm=TRUE),
      p15a17_noa_m = pmax(p_15a17_m - p15a17a_m, 0, na.rm=TRUE),
      # No asistencia 6-11 (para IRS)
      p6a14_noa = coalesce(p6a11_noa,0) + coalesce(p12a14noa,0),
      p6a14_tot = coalesce(p_6a11,0)    + coalesce(p_12a14,0),
      # Brecha digital: sin internet y sin pc
      vph_brecha   = pmax(vivpar_hab - coalesce(vph_inter,0)
                                     - coalesce(vph_pc,0), 0, na.rm=TRUE),
      vph_sin_inter= pmax(vivpar_hab - coalesce(vph_inter,0), 0, na.rm=TRUE)
    )

  # ── Agregación estatal ────────────────────────────────────────────────────
  agg_ent <- function(col) sum(col, na.rm=TRUE)

  iter_ent <- iter |>
    filter(nom_loc == "Total de la Entidad") |>
    mutate(
      pct_hli        = p3ym_hli   / pobtot      * 100,
      pct_hli_f      = p3ym_hli_f / pobfem       * 100,
      pct_hli_m      = p3ym_hli_m / pobmas        * 100,
      pct_afro       = pob_afro   / pobtot        * 100,
      pct_discap     = pcon_disc  / pobtot        * 100,
      pct_migr       = pnacoe     / pobtot        * 100,
      pct_8a11       = p_8a11     / pobtot        * 100,
      pct_8a11_f     = p_8a11_f   / p_8a11        * 100,
      tasa_noa_1517  = p15a17_noa / p_15a17       * 100,
      tasa_noa_f     = p15a17_noa_f / p_15a17_f   * 100,
      tasa_noa_m     = p15a17_noa_m / p_15a17_m   * 100,
      pct_sin_elect  = vph_s_elec / vivparh_cv    * 100,
      pct_sin_agua   = vph_aguafv / vivparh_cv    * 100,
      pct_sin_dren   = vph_nodren / vivparh_cv    * 100,
      pct_sin_inter  = vph_sin_inter / vivparh_cv * 100,
      pct_brecha     = vph_brecha / vivparh_cv    * 100,
      pct_piso       = vph_pisoti / vivparh_cv    * 100
    )

  # ── Agregación municipal ──────────────────────────────────────────────────
  iter_mun <- iter |>
    filter(nom_loc == "Total del Municipio") |>
    mutate(
      pct_hli        = p3ym_hli   / pobtot       * 100,
      pct_afro       = pob_afro   / pobtot        * 100,
      pct_discap     = pcon_disc  / pobtot        * 100,
      pct_8a11       = p_8a11     / pobtot        * 100,
      tasa_noa_1517  = p15a17_noa / p_15a17       * 100,
      pct_sin_agua   = vph_aguafv / vivparh_cv    * 100,
      pct_brecha     = vph_brecha / vivparh_cv    * 100,
      pct_rural_loc  = NA_real_  # se calcula abajo
    )

  # % rural por municipio (suma localidades rurales)
  rural_mun <- iter |>
    filter(!nom_loc %in% c("Total del Municipio","Total de la Entidad",
                            "Total nacional")) |>
    group_by(cve_mun_full) |>
    summarise(
      pob_rural = sum(pobtot[rural_iter=="Rural"], na.rm=TRUE),
      pobtot_m  = sum(pobtot, na.rm=TRUE), .groups="drop") |>
    mutate(pct_rural = pob_rural / pobtot_m * 100)

  iter_mun <- iter_mun |>
    left_join(rural_mun |> select(cve_mun_full, pct_rural),
              by="cve_mun_full")

  cat(sprintf("✓ ITER: %d entidades, %d municipios\n",
      nrow(iter_ent), nrow(iter_mun)))
  guardar_tabla(iter_ent, "t6_iter_estatal_F2")
  guardar_tabla(iter_mun, "t7_iter_municipal_F2")

  # ── PI-5: Volumen y distribución territorial de la cohorte ───────────────
  message("\n── PI-5: Volumen territorial cohorte 8-11 ──")

  t8 <- iter_ent |>
    select(nom_ent_abr, p_8a11, p_8a11_f, p_8a11_m,
           pct_8a11, pct_8a11_f) |>
    arrange(desc(p_8a11)) |>
    mutate(across(where(is.numeric), ~round(.x,1)))
  guardar_tabla(t8, "t8_iter_cohorte_811_entidad_F2")

  tot_nac  <- sum(iter_ent$p_8a11, na.rm=TRUE)
  tot_f    <- sum(iter_ent$p_8a11_f, na.rm=TRUE)
  cat(sprintf("Cohorte 8-11 — universo censal:\n  Total: %s | Mujeres: %.1f%%\n",
      format(tot_nac, big.mark=","), tot_f/tot_nac*100))

  # ── GRÁFICO 9: Cohorte 8-11 por entidad — pirámide ───────────────────────

  g9_data <- iter_ent |>
    select(nom_ent_abr, p_8a11_f, p_8a11_m) |>
    pivot_longer(-nom_ent_abr, names_to="sexo", values_to="n") |>
    mutate(
      sexo  = if_else(sexo=="p_8a11_f","Mujer","Hombre"),
      n_dir = if_else(sexo=="Hombre", -n/1000, n/1000)) |>
    left_join(iter_ent |> select(nom_ent_abr, p_8a11), by="nom_ent_abr")

  max_n <- max(abs(g9_data$n_dir), na.rm=TRUE)

  g9 <- ggplot(g9_data,
         aes(n_dir, reorder(nom_ent_abr, p_8a11), fill=sexo)) +
    geom_col(width=.8, alpha=.88) +
    geom_vline(xintercept=0, color="white", linewidth=.6) +
    scale_fill_manual(
      values=c("Hombre"=pal$azul1,"Mujer"=pal$vino2), name=NULL) +
    scale_x_continuous(
      limits=c(-max_n*1.05, max_n*1.05),
      labels=function(x) paste0(abs(x),"k")) +
    labs(
      title  = "**Distribución territorial de la cohorte 8-11 años — universo censal**",
      subtitle="ITER Nacional 2020 | cohorte = p_8a14 − p_12a14",
      x="Personas (miles) ← Hombres  |  Mujeres →", y=NULL,
      caption="**Fuente:** ITER Nacional 2020 (INEGI). Universo censal completo.") +
    theme(axis.text.y=element_text(size=8),
          legend.position="top")
  guardar_graf(g9, "g9_PI5_iter_cohorte_piramide_entidad", w=10, h=8.5)

  # ── PI-6: No asistencia 15-17 años ───────────────────────────────────────
  message("\n── PI-6: No asistencia 15-17 (indicador desafiliación) ──")

  t9 <- iter_ent |>
    select(nom_ent_abr, p_15a17, p15a17_noa,
           tasa_noa_1517, tasa_noa_f, tasa_noa_m) |>
    arrange(desc(tasa_noa_1517)) |>
    mutate(across(where(is.numeric),~round(.x,1)))
  guardar_tabla(t9, "t9_iter_noasistencia_1517_entidad_F2")

  # ── GRÁFICO 10: No asistencia 15-17 — dumbell por sexo ───────────────────

  g10_data <- iter_ent |>
    select(nom_ent_abr, tasa_noa_f, tasa_noa_m, tasa_noa_1517) |>
    filter(!is.na(tasa_noa_1517))

  g10 <- ggplot(g10_data,
         aes(y=reorder(nom_ent_abr, tasa_noa_1517))) +
    geom_segment(aes(x=tasa_noa_m, xend=tasa_noa_f,
                     yend=nom_ent_abr),
                 color=pal$gris3, linewidth=1.5) +
    geom_point(aes(x=tasa_noa_m), color=pal$azul1,
               size=3.5, alpha=.9) +
    geom_point(aes(x=tasa_noa_f), color=pal$vino2,
               size=3.5, alpha=.9) +
    geom_point(aes(x=tasa_noa_1517), shape="|",
               size=4, color=pal$gris1) +
    scale_x_continuous(
      labels=label_number(suffix="%",accuracy=.1),
      expand=expansion(mult=c(.02,.12))) +
    annotate("text", x=Inf, y=1, hjust=1.05, vjust=-1,
             label="● Hombres = Azul | ● Mujeres = Vino | | = Total",
             size=2.8, color=pal$gris2, fontface="italic") +
    labs(
      title  = "**Tasa de no asistencia escolar en adolescentes 15-17 años (2020)**",
      subtitle="ITER Nacional 2020 — indicador directo de riesgo de desafiliación EMS",
      x="% que no asiste", y=NULL,
      caption="**Fuente:** ITER Nacional 2020 (INEGI). Universo censal.<br>
               Cálculo: (p_15a17 − p15a17a) / p_15a17 × 100.") +
    theme(panel.grid.major.y=element_blank(),
          axis.text.y=element_text(size=8))
  guardar_graf(g10, "g10_PI6_iter_noasistencia_1517_dumbell", w=11, h=8)

  # ── PI-7: Carencias de servicios básicos (universo) ───────────────────────
  message("\n── PI-7: Carencias de servicios básicos ──")

  t10 <- iter_ent |>
    select(nom_ent_abr, pct_sin_elect, pct_sin_agua,
           pct_sin_dren, pct_sin_inter, pct_brecha, pct_piso) |>
    arrange(desc(pct_sin_agua)) |>
    mutate(across(where(is.numeric),~round(.x,1)))
  guardar_tabla(t10, "t10_iter_carencias_vivienda_entidad_F2")

  # ── GRÁFICO 11: Carencias — slope chart (tendencia vs media) ─────────────

  media_nac <- iter_ent |>
    summarise(across(c(pct_sin_elect,pct_sin_agua,pct_sin_dren,
                       pct_sin_inter,pct_brecha),
                     ~mean(.x,na.rm=T)))

  g11_data <- iter_ent |>
    select(nom_ent_abr,pct_sin_elect,pct_sin_agua,
           pct_sin_dren,pct_sin_inter,pct_brecha) |>
    pivot_longer(-nom_ent_abr,names_to="carencia",values_to="pct") |>
    mutate(carencia_lab = recode(carencia,
      pct_sin_elect="Sin electricidad",
      pct_sin_agua ="Sin agua",
      pct_sin_dren ="Sin drenaje",
      pct_sin_inter="Sin internet",
      pct_brecha   ="Brecha digital"),
    carencia_lab=factor(carencia_lab,
      levels=c("Sin agua","Sin drenaje","Sin electricidad",
               "Sin internet","Brecha digital")))

  media_long <- media_nac |>
    pivot_longer(everything(), names_to="carencia", values_to="media") |>
    mutate(carencia_lab=recode(carencia,
      pct_sin_elect="Sin electricidad",pct_sin_agua="Sin agua",
      pct_sin_dren="Sin drenaje",pct_sin_inter="Sin internet",
      pct_brecha="Brecha digital"))

  g11 <- ggplot(g11_data,
         aes(pct, reorder(nom_ent_abr, pct))) +
    geom_col(aes(fill=pct), width=.8, alpha=.85) +
    geom_vline(data=media_long, aes(xintercept=media),
               linetype="dashed", color=pal$vino1, linewidth=.7) +
    facet_wrap(~carencia_lab, scales="free_x", nrow=1) +
    scale_fill_gradient(low=pal$azul3, high=pal$vino1,
                        name="% de\nviviendas",
                        labels=label_number(suffix="%",accuracy=1)) +
    scale_x_continuous(labels=label_number(suffix="%",accuracy=1),
                       expand=expansion(mult=c(0,.15))) +
    labs(
      title  = "**Carencias de servicios básicos en viviendas — universo censal**",
      subtitle="ITER Nacional 2020 | línea punteada = media nacional",
      x=NULL, y=NULL,
      caption="**Fuente:** ITER Nacional 2020 (INEGI). Universo censal completo.") +
    theme(axis.text.y=element_text(size=7),
          axis.text.x=element_text(size=7),
          legend.position="right",
          panel.grid.major.y=element_blank())
  guardar_graf(g11, "g11_PI7_iter_carencias_panel_universo", w=16, h=9)


  # ═════════════════════════════════════════════════════════════════════════════
  # F3 ─ IRS CALCULADO DESDE ITER (metodología CONEVAL)
  # PI-8: rezago social | PI-9: relación rezago-vulnerabilidad
  # ═════════════════════════════════════════════════════════════════════════════

  message("\n════ F3: IRS DESDE ITER (metodología CONEVAL) ════")

  # Variables insumo del IRS (11 componentes oficiales CONEVAL)
  vars_irs <- c("i_analf","i_asistesc","i_edbasinc","i_sdsalud",
                "i_ptierra","i_nosan","i_noagua","i_nodren",
                "i_noelec","i_nolav","i_noref")

  # Calcular variables insumo
  iter_vars <- iter |>
    filter(!is.na(pobmas), !is.na(p_15ymas)) |>
    mutate(
      i_analf    = p15ym_an / p_15ymas,
      i_asistesc = (coalesce(p6a11_noa,0)+coalesce(p12a14noa,0)) /
                    pmax(coalesce(p_6a11,0)+coalesce(p_12a14,0), 1),
      i_edbasinc = (coalesce(p15ym_se,0)+coalesce(p15pri_in,0)+
                    coalesce(p15pri_co,0)+coalesce(p15sec_in,0)) / p_15ymas,
      i_sdsalud  = coalesce(psinder,0) / pobtot,
      i_ptierra  = coalesce(vph_pisoti,0) / pmax(coalesce(vivparh_cv,0),1),
      i_nosan    = 1 - ((coalesce(vph_excsa,0)+coalesce(vph_letr,0)) /
                         pmax(coalesce(vivparh_cv,0),1)),
      i_noagua   = coalesce(vph_aguafv,0) / pmax(coalesce(vivparh_cv,0),1),
      i_nodren   = coalesce(vph_nodren,0) / pmax(coalesce(vivparh_cv,0),1),
      i_noelec   = coalesce(vph_s_elec,0) / pmax(coalesce(vivparh_cv,0),1),
      i_nolav    = 1-(coalesce(vph_lavad,0)/pmax(coalesce(vivparh_cv,0),1)),
      i_noref    = 1-(coalesce(vph_refri,0)/pmax(coalesce(vivparh_cv,0),1))
    )

  # Función IRS: ACP + Dalenius-Hodges
  calc_irs <- function(df, nivel="municipio") {
    df_clean <- df |> drop_na(all_of(vars_irs)) |>
      filter(if_all(all_of(vars_irs), is.finite))
    acp  <- prcomp(df_clean[,vars_irs], scale.=TRUE)
    vexp <- summary(acp)$importance[2,1]*100
    cat(sprintf("  ACP %s — varianza PC1: %.1f%%\n", nivel, vexp))
    pred <- as.data.frame(predict(acp, df_clean))
    df_clean$indice <- pred$PC1
    df_clean$irs    <- (df_clean$indice - mean(df_clean$indice)) /
                        sd(df_clean$indice)
    df_clean$y      <- 100/(max(df_clean$irs)-min(df_clean$irs)) *
                        (df_clean$irs-min(df_clean$irs))
    df_clean$interv <- as.integer(cut(df_clean$y,
      c(-Inf,10,20,30,40,50,60,70,80,90,Inf), labels=1:10))
    df_clean$grado_rs <- NA_character_
    
    if(nivel=="entidad"){
      df_clean$grado_rs[df_clean$interv <= 1] <- "Muy bajo"
      df_clean$grado_rs[df_clean$interv > 1 & df_clean$interv <= 3] <- "Bajo"
      df_clean$grado_rs[df_clean$interv > 3 & df_clean$interv <= 4] <- "Medio"
      df_clean$grado_rs[df_clean$interv > 4 & df_clean$interv <= 6] <- "Alto"
      df_clean$grado_rs[df_clean$interv > 6] <- "Muy alto"
    }
    
    if(nivel=="municipio"){
      df_clean$grado_rs[df_clean$interv <= 1] <- "Muy bajo"
      df_clean$grado_rs[df_clean$interv > 1 & df_clean$interv <= 2] <- "Bajo"
      df_clean$grado_rs[df_clean$interv > 2 & df_clean$interv <= 3] <- "Medio"
      df_clean$grado_rs[df_clean$interv > 3 & df_clean$interv <= 4] <- "Alto"
      df_clean$grado_rs[df_clean$interv > 4] <- "Muy alto"
    }
    
    if(nivel=="localidad"){
      df_clean$grado_rs[df_clean$interv <= 1] <- "Muy bajo"
      df_clean$grado_rs[df_clean$interv > 1 & df_clean$interv <= 2] <- "Bajo"
      df_clean$grado_rs[df_clean$interv > 2 & df_clean$interv <= 3] <- "Medio"
      df_clean$grado_rs[df_clean$interv > 3 & df_clean$interv <= 5] <- "Alto"
      df_clean$grado_rs[df_clean$interv > 5] <- "Muy alto"
    }
    
    df_clean$grado_rs <- factor(
      df_clean$grado_rs,
      levels = c("Muy bajo","Bajo","Medio","Alto","Muy alto")
    )
    df_clean
  }

  base_ent_i  <- iter_vars |> filter(nom_loc=="Total de la Entidad")
  base_mun_i  <- iter_vars |> filter(nom_loc=="Total del Municipio")
  base_loc_i  <- iter_vars |>
    filter(!nom_loc %in% c("Total de la Entidad","Total del Municipio",
      "Total nacional","Localidades de una vivienda",
      "Localidades de dos viviendas"),
      coalesce(vivparh_cv,0)!=0, coalesce(p_15ymas,0)!=0) |>
    mutate(i_asistesc=if_else(
      coalesce(p_6a11,0)==0|coalesce(p_12a14,0)==0, 0, i_asistesc))

  message("Calculando IRS...")
  irs_ent  <- calc_irs(base_ent_i,  "entidad")
  irs_mun  <- calc_irs(base_mun_i,  "municipio")
  irs_loc  <- calc_irs(base_loc_i,  "localidad")

  cat(sprintf("IRS: %d entidades, %d municipios, %d localidades\n",
      nrow(irs_ent), nrow(irs_mun), nrow(irs_loc)))

  saveRDS(irs_mun, "data/coneval/irs_municipal_calc.rds")
  saveRDS(irs_ent, "data/coneval/irs_estatal_calc.rds")
  saveRDS(irs_loc, "data/coneval/irs_localidad_calc.rds")
  write_csv(irs_mun |> select(cve_mun_full,irs,grado_rs),
            "data/coneval/irs_municipal_calc.csv")

  # TABLA 11 — IRS por entidad con componentes
  t11 <- irs_ent |>
    mutate(nom_ent_abr=cat_ent[cve_ent]) |>
    select(nom_ent_abr, all_of(vars_irs), irs, grado_rs) |>
    arrange(desc(irs)) |>
    mutate(across(all_of(vars_irs),~round(.x*100,1)))
  guardar_tabla(t11, "t11_irs_entidad_componentes_F3")

  # ── GRÁFICO 12: Heatmap de componentes IRS (Python) ──────────────────────

  write_csv(t11, "outputs/censo2020/python/irs_componentes.csv")

  if (py_available) {
    py_run_string('
import pandas as pd, numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mc
import seaborn as sns

df = pd.read_csv("outputs/censo2020/python/irs_componentes.csv")
df = df.set_index("nom_ent_abr")
cols = ["i_analf","i_asistesc","i_edbasinc","i_sdsalud",
        "i_ptierra","i_nosan","i_noagua","i_nodren",
        "i_noelec","i_nolav","i_noref"]
col_labs = ["Analfabetismo","No asistencia\n6-14a","Ed. básica\nincompleta",
            "Sin derecho-\nhabiencia","Piso de\ntierra","Sin sanitario",
            "Sin agua","Sin drenaje","Sin electricidad",
            "Sin lavadora","Sin refrigerador"]

data_val = df[cols].copy()
data_z   = (data_val - data_val.mean()) / data_val.std()

orden = data_z.mean(axis=1).sort_values(ascending=False).index
data_val = data_val.loc[orden]
data_z   = data_z.loc[orden]

grado_col = {"Muy alto":"#6B1A2B","Alto":"#8B2642","Medio":"#C4975A",
             "Bajo":"#2E5F8A","Muy bajo":"#1B3A5C"}

fig, (ax_col, ax) = plt.subplots(1,2,
    figsize=(14.5,9), facecolor="white",
    gridspec_kw={"width_ratios":[0.25,10],"wspace":0.02})

# Barra lateral de grado
grados = df.loc[orden,"grado_rs"]
for i, (ent, g) in enumerate(grados.items()):
    ax_col.barh(i, 1, color=grado_col.get(str(g),"#cccccc"),
                align="center")
ax_col.set_yticks(range(len(grados)))
ax_col.set_yticklabels(list(grados.index), fontsize=8.5)
ax_col.set_xlim(0,1); ax_col.set_xticks([])
ax_col.set_title("Grado", fontsize=9, color="#2C3E50", pad=6)
ax_col.invert_yaxis()
for spine in ax_col.spines.values(): spine.set_visible(False)

cmap = sns.diverging_palette(230,20, as_cmap=True)
sns.heatmap(data_z, ax=ax, cmap=cmap, center=0,
    linewidths=0.35, linecolor="white",
    cbar_kws={"label":"z-score","shrink":0.55,
              "orientation":"horizontal","pad":0.02},
    annot=data_val.round(1), fmt=".1f",
    annot_kws={"size":7,"color":"#2c3e50"},
    yticklabels=False)
ax.set_xticklabels(col_labs, fontsize=8.5, rotation=20, ha="right")
ax.set_title("Componentes del Índice de Rezago Social 2020 por entidad federativa",
    fontsize=13, fontweight="bold", color="#6B1A2B", pad=12)
ax.set_xlabel("Indicador de carencia (valor en celda = % de pob/viviendas; "
              "color = z-score relativo a media nacional)",
              fontsize=8, color="#5D6D7E", labelpad=8)

plt.figtext(0.5,-0.01,
    "Fuente: ITER Nacional 2020 (INEGI). Cálculo propio con metodología "
    "CONEVAL (ACP + Dalenius-Hodges). Elaboración propia.",
    ha="center",fontsize=7.5,color="#888888",style="italic")
plt.savefig("outputs/censo2020/graficos/g12_PI8_irs_heatmap_componentes_python.png",
    dpi=320, bbox_inches="tight", facecolor="white")
plt.close()
print("g12 heatmap IRS guardado (Python)")
')
    message("✓ g12_PI8_irs_heatmap_componentes_python")
  }

  # ── GRÁFICO 13: IRS por entidad — lollipop coloreado por grado ───────────

  g13_data <- irs_ent |>
    mutate(
      nom_ent_abr = cat_ent[cve_ent],
      grado_rs    = factor(grado_rs,
        levels=c("Muy alto","Alto","Medio","Bajo","Muy bajo"))) |>
    filter(!is.na(nom_ent_abr))

  col_grado <- c("Muy alto"=pal$vino1,"Alto"=pal$vino2,
                 "Medio"=pal$dor1,"Bajo"=pal$azul2,"Muy bajo"=pal$azul1)

  g13 <- ggplot(g13_data,
         aes(irs, reorder(nom_ent_abr, irs), color=grado_rs)) +
    geom_segment(aes(xend=0, yend=nom_ent_abr),
                 linewidth=1.2, alpha=.6) +
    geom_point(size=4, alpha=.9) +
    geom_text(aes(label=round(irs,2)), hjust=-.3, size=2.8,
              color=pal$gris1, fontface="bold") +
    geom_vline(xintercept=0, color=pal$gris2, linewidth=.5, linetype="dotted") +
    scale_color_manual(values=col_grado, name="Grado de rezago") +
    scale_x_continuous(expand=expansion(mult=c(.05,.2))) +
    labs(
      title  = "**Índice de Rezago Social por entidad federativa (2020)**",
      subtitle="Calculado desde ITER — metodología oficial CONEVAL (ACP + Dalenius-Hodges)",
      x="IRS (estandarizado)", y=NULL,
      caption="**Fuente:** ITER Nacional 2020 (INEGI). Cálculo propio
               con metodología CONEVAL 2020. Elaboración propia.") +
    theme(panel.grid.major.y=element_blank(),
          axis.text.y=element_text(size=8.5),
          legend.position="right",
          legend.key.size=unit(.4,"cm"))
  guardar_graf(g13, "g13_PI8_irs_entidad_lollipop", w=11, h=8)

  # ── PI-9: Relación rezago-vulnerabilidad ─────────────────────────────────
  message("\n── PI-9: Relación IRS × IVE ──")

  irs_mun_join <- irs_mun |>
    select(cve_mun_full, irs, grado_rs)

  vuln_irs <- vuln_micro |>
    left_join(irs_mun_join, by="cve_mun_full") |>
    filter(!is.na(irs), !is.na(IVE))

  t12 <- vuln_irs |>
    filter(!is.na(grado_rs)) |>
    mutate(grado_rs=factor(grado_rs,
      levels=c("Muy alto","Alto","Medio","Bajo","Muy bajo"))) |>
    group_by(grado_rs) |>
    summarise(
      n_municipios   = n(),
      IVE_media      = round(mean(IVE,na.rm=T),1),
      IVE_sd         = round(sd(IVE,na.rm=T),1),
      pct_hli_media  = round(mean(pct_hli,na.rm=T),1),
      pct_rural_media= round(mean(pct_rural,na.rm=T),1),
      pct_brecha_med = round(mean(pct_brecha,na.rm=T),1),
      .groups="drop"
    ) |> arrange(desc(IVE_media))
  guardar_tabla(t12, "t12_IVE_por_grado_rezago_F3")
  cat("\n── IVE por grado de rezago ──\n"); print(t12)

  # ── GRÁFICO 14: Dispersión IRS × IVE con marginal (Python) ──────────────

  scatter_data <- vuln_irs |>
    select(nom_ent_abr, cve_mun_full, irs, IVE, grado_rs,
           pct_hli, pct_rural, n_coh) |>
    filter(!is.na(grado_rs))
  write_csv(scatter_data, "outputs/censo2020/python/scatter_irs_ive.csv")

  if (py_available) {
    py_run_string('
import pandas as pd, numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from scipy import stats

df = pd.read_csv("outputs/censo2020/python/scatter_irs_ive.csv").dropna()

colores = {"Muy alto":"#6B1A2B","Alto":"#8B2642","Medio":"#C4975A",
           "Bajo":"#2E5F8A","Muy bajo":"#1B3A5C"}
grados  = ["Muy alto","Alto","Medio","Bajo","Muy bajo"]

fig = plt.figure(figsize=(13,9), facecolor="white")
gs  = fig.add_gridspec(3,3,hspace=.06,wspace=.06)
ax_main = fig.add_subplot(gs[1:3,0:2])
ax_top  = fig.add_subplot(gs[0,0:2], sharex=ax_main)
ax_rgt  = fig.add_subplot(gs[1:3,2], sharey=ax_main)

for g in grados:
    sub = df[df["grado_rs"]==g]
    if len(sub)==0: continue
    ax_main.scatter(sub["irs"], sub["IVE"],
        c=colores[g], alpha=0.45,
        s=np.clip(sub["n_coh"]/3,10,120),
        edgecolors="white", linewidths=0.3, label=g)
    ax_top.hist(sub["irs"],   bins=25, color=colores[g], alpha=0.5)
    ax_rgt.hist(sub["IVE"], bins=25, color=colores[g], alpha=0.5,
                orientation="horizontal")

# Línea de regresión
slope, intercept, r, p, _ = stats.linregress(df["irs"],df["IVE"])
x_line = np.linspace(df["irs"].min(),df["irs"].max(),200)
ax_main.plot(x_line, slope*x_line+intercept,
    color="#2C3E50", linewidth=2, linestyle="--", zorder=5,
    label=f"Regresión (r={r:.2f}, p<0.001)")

ax_main.set_xlabel("Índice de Rezago Social — IRS (CONEVAL, 2020)",
    fontsize=10, color="#1B3A5C", labelpad=8)
ax_main.set_ylabel("Índice de Vulnerabilidad Educativa — IVE (0-100)",
    fontsize=10, color="#1B3A5C", labelpad=8)
ax_main.tick_params(labelsize=8.5)
ax_main.legend(loc="upper left", fontsize=8, framealpha=.8,
    title="Grado de rezago", title_fontsize=8.5)
ax_main.text(0.02, 0.97, f"r = {r:.3f}  |  p < 0.001",
    transform=ax_main.transAxes, fontsize=9.5,
    fontweight="bold", color="#6B1A2B",
    va="top", bbox=dict(boxstyle="round,pad=0.3",
    facecolor="white",edgecolor="#CCCCCC",alpha=0.9))
ax_main.grid(color="#EEEEEE",linewidth=0.5)
ax_main.spines[["top","right"]].set_visible(False)

ax_top.set_ylabel("n", fontsize=8); ax_top.tick_params(labelsize=7.5)
ax_top.spines[["top","right"]].set_visible(False)
plt.setp(ax_top.get_xticklabels(),visible=False)

ax_rgt.set_xlabel("n",fontsize=8); ax_rgt.tick_params(labelsize=7.5)
ax_rgt.spines[["top","right"]].set_visible(False)
plt.setp(ax_rgt.get_yticklabels(),visible=False)

fig.suptitle(
    "Relación entre Índice de Rezago Social (CONEVAL) e Índice de Vulnerabilidad\n"
    "Educativa (IVE) a nivel municipal — Cohorte 8-11 años, 2020",
    fontsize=13, fontweight="bold", color="#6B1A2B", y=0.98)

plt.figtext(0.5,-0.01,
    "Un punto = un municipio. Tamaño del punto proporcional a n de la cohorte en la muestra.  "
    "Fuentes: Microdatos de ejemplo (INEGI) + IRS calculado desde ITER 2020 (metodología CONEVAL). "
    "Elaboración propia.",
    ha="center",fontsize=7.5,color="#888888",style="italic")
plt.savefig(
    "outputs/censo2020/graficos/g14_PI9_scatter_marginal_IRS_IVE_python.png",
    dpi=320, bbox_inches="tight", facecolor="white")
plt.close()
print("g14 scatter marginal guardado (Python)")
')
    message("✓ g14_PI9_scatter_marginal_IRS_IVE_python")
  }

  # Versión ggplot2 del scatter (siempre se genera como respaldo)
  g14_r <- vuln_irs |>
    filter(!is.na(grado_rs)) |>
    mutate(grado_rs=factor(grado_rs,
      levels=c("Muy alto","Alto","Medio","Bajo","Muy bajo"))) |>
    ggplot(aes(irs, IVE, color=grado_rs, size=n_coh)) +
    geom_point(alpha=.5) +
    geom_smooth(method="lm", se=TRUE, color=pal$gris1,
                fill=pal$fondo2, linewidth=.9,
                show.legend=FALSE) +
    scale_color_manual(values=col_grado, name="Grado de rezago") +
    scale_size_continuous(range=c(1,5), guide="none") +
    scale_x_continuous(labels=label_number(accuracy=.01)) +
    scale_y_continuous(labels=label_number(accuracy=1)) +
    labs(
      title  = "**Relación entre rezago social (IRS) y vulnerabilidad educativa (IVE)**",
      subtitle="Un punto = un municipio | tamaño proporcional a n de la cohorte en la muestra",
      x="IRS — Índice de Rezago Social (CONEVAL 2020)",
      y="IVE — Índice de Vulnerabilidad Educativa (0-100)",
      caption="**Fuentes:** Microdatos de ejemplo (INEGI) +
               IRS calculado desde ITER 2020 (metodología CONEVAL).
               Elaboración propia.") +
    theme(legend.position="right",
          legend.key.size=unit(.4,"cm"))
  guardar_graf(g14_r, "g14_PI9_scatter_IRS_IVE_R", w=11, h=7.5)

  # ── GRÁFICO 15: Boxplot IVE por grado de rezago ──────────────────────────

  g15 <- vuln_irs |>
    filter(!is.na(grado_rs)) |>
    mutate(grado_rs=factor(grado_rs,
      levels=c("Muy alto","Alto","Medio","Bajo","Muy bajo"))) |>
    ggplot(aes(grado_rs, IVE, fill=grado_rs)) +
    ggdist::stat_halfeye(adjust=.5, width=.5, .width=0,
                         point_colour=NA, alpha=.7) +
    geom_boxplot(width=.12, outlier.shape=NA, alpha=.9) +
    ggdist::stat_dots(side="left", dotsize=.4, binwidth=1.5,
                      alpha=.4, color=NA) +
    scale_fill_manual(values=col_grado) +
    coord_flip() +
    labs(
      title  = "**Distribución del IVE según grado de rezago social (CONEVAL)**",
      subtitle="Municipios — Microdatos de ejemplo (INEGI) + IRS 2020 calculado desde ITER",
      x=NULL, y="IVE — Índice de Vulnerabilidad Educativa (0-100)",
      caption="**Fuentes:** Microdatos de ejemplo (INEGI) + metodología CONEVAL (ACP). Elaboración propia.") +
    theme(legend.position="none",
          axis.text.y=element_text(size=10))
  guardar_graf(g15, "g15_PI9_IVE_por_grado_rezago_raincloud", w=11, h=7)
}  # fin if iter_disponible


# ═══════════════════════════════════════════════════════════════════════════════
# MAPA DE VULNERABILIDAD MUNICIPAL
# ═══════════════════════════════════════════════════════════════════════════════

shp <- "data/shapefiles/municipios_2020.shp"
if (file.exists(shp) && exists("vuln_micro")) {
  message("\n── Mapa de vulnerabilidad municipal ──")
  mapa_df <- st_read(shp, quiet=TRUE) |>
    mutate(cve_mun_full=paste0(
      str_pad(CVE_ENT,2,"left","0"),
      str_pad(CVE_MUN,3,"left","0"))) |>
    left_join(vuln_micro |> select(cve_mun_full,IVE,grupo),
              by="cve_mun_full")

  g_mapa <- ggplot(mapa_df) +
    geom_sf(aes(fill=IVE), color="white", linewidth=.04) +
    scale_fill_gradient2(
      low="#AED6F1", mid=pal$dor1, high=pal$vino1,
      midpoint=50, na.value="#F0F0F0",
      labels=label_number(accuracy=1),
      name="IVE\n(0-100)") +
    labs(
      title  = "**Índice de Vulnerabilidad Educativa municipal**",
      subtitle="Cohorte 8-11 años en 2020 — aprox. 14-17 años en 2026",
      caption="**Fuentes:** Microdatos de ejemplo, Censo 2020 (INEGI) e IRS 2020 (CONEVAL). Elaboración propia.") +
    theme_void(base_size=11) +
    theme(
      plot.title    = element_markdown(
        color=pal$vino1, face="bold", size=13),
      plot.subtitle = element_markdown(color=pal$gris1, size=10),
      plot.caption  = element_markdown(
        color=pal$gris2, size=7.5, hjust=0, face="italic"),
      legend.position="right",
      plot.background=element_rect(fill="white",color=NA),
      plot.margin=margin(10,16,10,12))
  guardar_graf(g_mapa, "mapa_IVE_municipal", w=13, h=9)
} else {
  message("Shapefile no disponible: https://www.inegi.org.mx/temas/mg/")
  message("Guardar en: ", shp)
}


# ═══════════════════════════════════════════════════════════════════════════════
# GUARDAR BASES PROCESADAS
# ═══════════════════════════════════════════════════════════════════════════════

saveRDS(cohorte,     "data/censo2020/procesados/cohorte_8_11_F1.rds")
saveRDS(vuln_micro,  "data/censo2020/procesados/vulnerabilidad_municipal_F1.rds")
write_csv(vuln_micro,"data/censo2020/procesados/vulnerabilidad_municipal_F1.csv")

if (iter_disponible) {
  saveRDS(iter_ent,  "data/censo2020/procesados/iter_estatal_F2.rds")
  saveRDS(iter_mun,  "data/censo2020/procesados/iter_municipal_F2.rds")
}

message("\n", strrep("═",60))
message("ANÁLISIS SECCIÓN 3 — CENSO 2020 COMPLETADO")
message("Gráficos: outputs/censo2020/graficos/")
message("Tablas:   outputs/censo2020/tablas/")
message("Bases:    data/censo2020/procesados/")
message(strrep("─",60))
message("F1 Microdatos:  G1-G9   — PI 1-4 (perfiles/desigualdades)")
message("F2 ITER:        G10-G11 — PI 5-7 (distribución territorial)")
message("F3 IRS CONEVAL: G12-G15 — PI 8-9 (rezago/vulnerabilidad)")
if (py_available) {
  message("Python (seaborn/matplotlib): G5,G8,G12,G14")
}
message(strrep("═",60))
