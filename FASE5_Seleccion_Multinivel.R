# ==============================================================================
# FASE 5 — SELECCIÓN DE VARIABLES PARA ANÁLISIS MULTINIVEL
# ==============================================================================
# ESTRUCTURA JERÁRQUICA:
#   Modelo Estudiantes: L1 = Alumno | L2 = Grado/Grupo | L3 = Escuela (CCT)
#   Modelo Docentes:    L1 = Docente | L2 = Escuela (CCT)
#
# FLUJO DE INFLUENCIA CRUZADA (Hallinger & Heck, 1998; Bronfenbrenner, 1979):
#   CTX_DIR  → L2 en modelo de docentes y estudiantes (liderazgo escolar)
#   CTX_DOC  → L1 en modelo de docentes; agregado a L2 en modelo de estudiantes
#   CTX_EST  → L1 en modelo de estudiantes
#   CTX_PMF  → L1 individual o L2 agregado en modelo de estudiantes
#
# CRITERIOS DE SELECCIÓN (7 etapas secuenciales):
#   1. Teoría    — pertinencia conceptual en evaluación educativa con TIC
#   2. ICC       — CCI > 0.05 → varianza entre escuelas suficiente para L2
#   3. Varianza  — SD > 0 y n_categorías ≥ 2 en la muestra disponible
#   4. Datos     — % NA < 25% (umbral operacional)
#   5. Nivel     — asignación definitiva L1/L2/L2agg según CCI y teoría
#   6. Colinealidad — VIF < 5 entre predictores del mismo nivel
#   7. Tamaño muestral — mínimo 30 unidades nivel 2 para efectos L2
#
# CENTRADO (Little et al., 2006; Enders & Tofighi, 2007):
#   Predictores L1 continuos/ordinales → CWC (group-mean centering)
#   Predictores L2 continuos/ordinales → CGM (grand-mean centering)
#   Variables L2 agregadas             → CGM sobre la media de escuela
#   Predictores binarios               → sin centrar (probabilidad de referencia)
#
# OUTPUTS:
#   • Tabla de decisión global (todas las variables de todos los CTX)
#   • Tabla de ICC por variable (para variables con suficiente varianza)
#   • Tabla de correlaciones entre predictores seleccionados (por modelo)
#   • Datasets listos para MLM: estudiantes_MLM.rds | docentes_MLM.rds
#   • Resumen de la estructura jerárquica final
# ==============================================================================

# ---------------------------------------------------------------------------- #
# 0. PAQUETES
# ---------------------------------------------------------------------------- #
pkgs <- c("readxl","writexl","openxlsx","dplyr","tidyr","stringr",
          "ggplot2","scales","RColorBrewer","patchwork",
          "lme4","psych","corrplot","car","mice","naniar")
for (p in pkgs) if (!requireNamespace(p,quietly=TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl);   library(writexl);  library(openxlsx)
  library(dplyr);    library(tidyr);    library(stringr)
  library(ggplot2);  library(scales);   library(RColorBrewer); library(patchwork)
  library(lme4);     library(psych);    library(car)
})

theme_set(theme_minimal(base_size=11)+
  theme(plot.title=element_text(size=12,face="bold"),
        plot.background=element_rect(fill="white",color=NA),
        panel.grid.minor=element_blank()))

seg <- function(e) tryCatch(e, error=function(x) NULL)
guardar_g <- function(g,path,w=10,h=6)
  if (!is.null(g)) tryCatch(ggsave(path,g,width=w,height=h,dpi=150,bg="white"),
                             error=function(x) NULL)

# ---------------------------------------------------------------------------- #
# 1. RUTAS ← ADAPTAR
# ---------------------------------------------------------------------------- #
ruta_pre <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/00_DATOS/Cuestionarios_independientes/"
ruta_sal <- "C:/Users/almejia/Desktop/RESULTADOS 25_26/05_MULTINIVEL/"
dir.create(ruta_sal, showWarnings=FALSE, recursive=TRUE)
for (d in c("TABLAS","GRAFICOS","DATASETS")) dir.create(file.path(ruta_sal,d), showWarnings=FALSE)

# ---------------------------------------------------------------------------- #
# 2. VALORES NA Y RECODIFICACIÓN
# ---------------------------------------------------------------------------- #
NA_VALS <- c("No lo sé/Prefiero no contestar","No lo sé/ Prefiero no contestar",
             "No lo sé / Prefiero no contestar","No lo sé",
             "Prefiero no contestar","No aplica","NA","")

limpiar <- function(x) { x[trimws(as.character(x)) %in% NA_VALS] <- NA; x }

COD <- list(
  # Escalas CTX (cuestionarios de contexto)
  frecuencia_4   = c("Nunca"=0,"Pocas veces"=1,"Muchas veces"=2,"Siempre"=3),
  frecuencia_5   = c("Nunca"=0,"Rara vez"=1,"Algunas veces"=2,"Frecuentemente"=3,"Siempre"=4),
  acuerdo_5      = c("Totalmente en desacuerdo"=0,"Algo en desacuerdo"=1,
                     "Ni de acuerdo ni en desacuerdo"=2,"Algo de acuerdo"=3,
                     "Totalmente de acuerdo"=4),
  receptor_5     = c("Nada receptivos"=0,"Poco receptivos"=1,"Indiferentes"=2,
                     "Medianamente receptivos"=3,"Muy receptivos"=4),
  binaria        = c("No"=0,"Sí"=1),
  # Escalas HD/HI/HSXXI — necesarias para calcular PMP del outcome en Spearman
  conocimiento_4 = c("No sé / Nunca he oído hablar de esto"=0,
                     "Conozco un poco el tema"=1,
                     "Sí, conozco bien este tema"=2,
                     "Totalmente, e incluso podría explicárselo a otras personas"=3),
  habilidad_4    = c("No sé cómo hacerlo"=0,
                     "Puedo hacerlo con ayuda"=1,
                     "Puedo hacerlo por mi cuenta"=2,
                     "Puedo hacerlo con confianza y si es necesario puedo ayudar a otras personas"=3),
  hi_acuerdo_5   = c("No estoy de acuerdo"=0,"Algo de acuerdo"=1,"De acuerdo"=2,
                     "Muy de acuerdo"=3,"Totalmente de acuerdo"=4),
  hi_frecuencia_5= c("Casi nunca"=0,"Algunas veces durante el semestre/año"=1,
                     "1 a 3 veces al mes"=2,"1 a 3 veces por semana"=3,
                     "Casi todos los días"=4)
)

recodificar <- function(vec) {
  v <- limpiar(as.character(vec))
  for (d in COD) {
    n_v <- sum(!is.na(v)); if (n_v==0) next
    if (sum(v[!is.na(v)] %in% names(d),na.rm=TRUE)/n_v >= 0.60) return(as.numeric(d[v]))
  }
  num <- suppressWarnings(as.numeric(v))
  if (mean(!is.na(num),na.rm=TRUE) >= 0.50) return(num)
  rep(NA_real_, length(v))
}

# ---------------------------------------------------------------------------- #
# 3. CATÁLOGO TEÓRICO DE VARIABLES (las 4 figuras)
# ---------------------------------------------------------------------------- #
# Cada entrada: variable | etiqueta | naturaleza | nivel_teorico | justificacion
# nivel_teorico: L1 (individuo) | L2esc (escuela, agrega para esc) | L2prop (L2 propio de ese nivel)
# justificacion: referencia teórica resumida

CATALOGO <- list(

  # ── CTX_DIR ───────────────────────────────────────────────────────────────
  CTX_DIR = list(
    archivo   = "CTX_DIR_pre.xlsx",
    cct_col   = "CCT",
    nivel_col = "Nivel",
    vars = tribble(
      ~col,  ~etiqueta,                                    ~naturaleza,  ~nivel_teorico, ~justificacion,
      "Q13", "Años en el cargo",                           "continua",   "L2esc",        "Experiencia directiva → calidad de liderazgo (Leithwood,2005)",
      "Q29", "Días de falta al mes",                       "continua",   "L2esc",        "Ausentismo directivo → debilita supervisión pedagógica",
      "Q32", "Escuela tiene aula de medios",               "binaria",    "L2esc",        "Infraestructura TIC → oportunidad de aprendizaje (Kozma,2003)",
      "Q33", "Aula de medios con internet",                "binaria",    "L2esc",        "Conectividad → acceso a recursos digitales educativos",
      "Q34", "Computadoras para alumnos",                  "binaria",    "L2esc",        "Ratio dispositivos → uso efectivo de TIC (UNESCO,2020)",
      "Q35", "Proyector disponible",                       "binaria",    "L2esc",        "Infraestructura presentación → práctica docente TIC",
      "Q37", "Frecuencia: revisa planeaciones docentes",   "ordinal",    "L2esc",        "Supervisión pedagógica → práctica docente (Hallinger,2005)",
      "Q38", "Frecuencia: supervisa trabajo en aula",      "ordinal",    "L2esc",        "Observación de clase → retroalimentación docente",
      "Q39", "Frecuencia: brinda retroalimentación",       "ordinal",    "L2esc",        "Feedback directivo → mejora de la práctica (Robinson,2009)",
      "Q40", "Frecuencia: promueve uso de TIC",            "ordinal",    "L2esc",        "Liderazgo instruccional TIC → adopción docente (Schiller,2003)",
      "Q41", "Participa en comunidades de aprendizaje",    "ordinal",    "L2esc",        "Desarrollo profesional colectivo → capacidad escolar",
      "Q42", "Gestiona recursos para TIC",                 "ordinal",    "L2esc",        "Liderazgo transformacional → recursos tecnológicos (Bass,1990)",
      "Q43", "Fomenta trabajo colaborativo",               "ordinal",    "L2esc",        "Clima organizacional colaborativo → aprendizaje docente",
      "Q49", "Receptividad docentes: nuevos métodos",      "ordinal",    "L2esc",        "Percepción directiva → apertura al cambio pedagógico",
      "Q50", "Receptividad docentes: TIC",                 "ordinal",    "L2esc",        "Cultura de innovación escolar (Fullan,2007)",
      "Q59", "Horas semanales internet (docencia)",        "continua",   "L2esc",        "Uso TIC directivo → modelado de práctica digital",
      "Q60", "Horas semanales internet (administración)",  "continua",   "L2esc",        "Gestión digital → eficiencia administrativa",
      "Q65", "Actitud: TIC mejoran aprendizaje",           "ordinal",    "L2esc",        "Creencias directivas → normas de TIC en escuela (Ertmer,2005)",
      "Q66", "Actitud: preparado para acompañar en TIC",  "ordinal",    "L2esc",        "Auto-eficacia directiva TIC → liderazgo tecnológico",
      "Q67", "Actitud: apoya uso TIC en aula",             "ordinal",    "L2esc",        "Apoyo directivo → motivación docente en TIC (Becker,2000)"
    )
  ),

  # ── CTX_DOC ───────────────────────────────────────────────────────────────
  CTX_DOC = list(
    archivo   = "CTX_DOC_pre.xlsx",
    cct_col   = "CCT",
    nivel_col = NULL,
    vars = tribble(
      ~col,  ~etiqueta,                                    ~naturaleza, ~nivel_teorico, ~justificacion,
      "Q9",  "Edad del docente",                           "continua",  "L1doc",        "Trayectoria profesional → adoptación de innovaciones (Rogers,2003)",
      "Q12", "Años de experiencia docente",                "continua",  "L1doc",        "Expertise pedagógico → calidad de la enseñanza (Berliner,2004)",
      "Q13", "Grupos que atiende",                         "continua",  "L1doc",        "Carga docente → tiempo disponible para TIC",
      "Q15", "Total de alumnos",                           "continua",  "L1doc",        "Tamaño de grupo → ratio atención individual",
      "Q17", "Tiene computadora personal",                 "binaria",   "L1doc",        "Acceso TIC personal → práctica cotidiana digital (ISTE,2017)",
      "Q18", "Tiene tablet",                               "binaria",   "L1doc",        "Variedad dispositivos → versatilidad pedagógica",
      "Q20", "Tiene internet en casa",                     "binaria",   "L1doc",        "Conectividad doméstica → preparación de clase con TIC",
      "Q24", "Formación en tecnología educativa",          "binaria",   "L1doc",        "Capacitación TIC → adopción efectiva (Ertmer,2005)",
      "Q25", "% del currículo cubierto",                   "ordinal",   "L1doc",        "Cobertura curricular → contexto de implementación PFE",
      "Q26", "Días de falta al mes",                       "continua",  "L2agg",        "Ausentismo docente → exposición del alumno (Ehrenberg,1991)",
      "Q43", "Horas semanales internet (general)",         "continua",  "L1doc",        "Uso TIC personal → disposición tecnológica docente",
      "Q44", "Horas semanales internet (preparar clases)", "continua",  "L1doc",        "Integración TIC en planeación → práctica digital (Mishra,2006)",
      "Q45", "Frecuencia: promueve exploración en clase",  "ordinal",   "L1doc",        "Práctica de enseñanza → autonomía del alumno (Vygotsky,1978)",
      "Q46", "Frecuencia: anima a los alumnos",            "ordinal",   "L1doc",        "Motivación extrínseca → autoeficacia del alumno (Bandura,1997)",
      "Q47", "Frecuencia: valora esfuerzo del alumno",     "ordinal",   "L1doc",        "Retroalimentación formativa → aprendizaje (Hattie,2009)",
      "Q48", "Frecuencia: atiende dudas individuales",     "ordinal",   "L1doc",        "Atención diferenciada → equidad en el aprendizaje",
      "Q49", "Actitud: TIC mejoran el aprendizaje",        "ordinal",   "L1doc",        "Creencias docentes TIC → integración efectiva (Ertmer,2005)",
      "Q50", "Actitud: preparado para usar TIC",           "ordinal",   "L1doc",        "Auto-eficacia TIC → uso en aula (Bandura,1997)",
      "Q51", "Actitud: integro TIC en mi planeación",      "ordinal",   "L1doc",        "TPACK — integración tecnopedagógica (Mishra & Koehler,2006)",
      "Q53", "Actitud: falta infraestructura TIC",         "ordinal",   "L1doc",        "Barrera percibida → freno a la adopción (Ertmer,1999)",
      "Q54", "Actitud: recibo apoyo del director en TIC",  "ordinal",   "L2agg",        "Percepción de apoyo directivo → adopción TIC (Hew,2007)"
    )
  ),

  # ── CTX_EST ───────────────────────────────────────────────────────────────
  CTX_EST = list(
    archivo   = "CTX_EST_pre.xlsx",
    cct_col   = "CCT",
    nivel_col = "Nivel",
    vars = tribble(
      ~col,  ~etiqueta,                                    ~naturaleza, ~nivel_teorico, ~justificacion,
      "Q17", "Edad del estudiante",                        "continua",  "L1est",        "Desarrollo cognitivo → madurez para uso TIC (Piaget,1972)",
      "Q18", "Nivel educativo de la madre",                "ordinal",   "L1est",        "Capital cultural familiar → logro académico (Bourdieu,1986)",
      "Q19", "Número de libros en casa",                   "continua",  "L1est",        "Proxy capital cultural → habilidades lectoras (PISA,2022)",
      "Q20", "¿El alumno trabaja?",                        "ordinal",   "L1est",        "Doble rol → tiempo disponible para estudio (Coleman,1966)",
      "Q21", "Personas que viven en casa",                 "ordinal",   "L1est",        "Hacinamiento → condiciones de estudio en casa",
      "Q38", "Días de falta al ciclo escolar",             "continua",  "L1est",        "Ausentismo → exposición al programa PFE (Alexander,2001)",
      "Q40", "Computadora en casa",                        "binaria",   "L1est",        "Brecha digital → acceso equitativo a TIC (OECD,2015)",
      "Q41", "Tablet en casa",                             "binaria",   "L1est",        "Dispositivos múltiples → práctica digital cotidiana",
      "Q42", "Smartphone propio",                          "binaria",   "L1est",        "Conectividad móvil → uso informal de TIC",
      "Q43", "Internet en casa",                           "binaria",   "L1est",        "Acceso doméstico → aprendizaje en línea (Warschauer,2004)",
      "Q45", "Tiene correo electrónico",                   "binaria",   "L1est",        "Literacidad digital → comunicación académica",
      "Q46", "Horas semanales de uso de computadora",      "continua",  "L1est",        "Exposición TIC → habilidades digitales (Hargittai,2010)",
      "Q47", "Escuela tiene aula de medios",               "binaria",   "L2esc",        "Infraestructura escolar → acceso equitativo (Kozma,2003)",
      "Q49", "Actitud: aula de medios ayuda a aprender",   "ordinal",   "L1est",        "Actitud hacia TIC → uso voluntario (TAM: Davis,1989)",
      "Q52", "Sabe usar procesador de texto",              "binaria",   "L1est",        "Habilidad digital básica → capital tecnológico",
      "Q53", "Sabe usar hoja de cálculo",                  "binaria",   "L1est",        "Literacidad digital avanzada → STEM skills",
      "Q55", "Sabe usar buscadores",                       "binaria",   "L1est",        "Alfab. informacional → búsqueda crítica (ACRL,2016)"
    )
  ),

  # ── CTX_PMF ───────────────────────────────────────────────────────────────
  CTX_PMF = list(
    archivo   = "CTX_PMF_pre.xlsx",
    cct_col   = "CCT",
    nivel_col = "Nivel",
    vars = tribble(
      ~col,  ~etiqueta,                                    ~naturaleza, ~nivel_teorico, ~justificacion,
      "Q16", "Número de integrantes del hogar",            "continua",  "L1pmf",        "Hacinamiento → privación relativa (CONEVAL,2020)",
      "Q18", "Ingreso mensual aproximado",                 "continua",  "L1pmf",        "SES → acceso a recursos educativos (Coleman,1966)",
      "Q19", "Tenencia de vivienda",                       "ordinal",   "L1pmf",        "Estabilidad residencial → continuidad escolar",
      "Q23", "Computadora en casa",                        "binaria",   "L1pmf",        "Recursos TIC domésticos → prácticas digitales del hijo",
      "Q25", "Smartphone en casa",                         "binaria",   "L1pmf",        "Conectividad familiar → comunicación hogar-escuela",
      "Q26", "Internet en casa",                           "binaria",   "L1pmf",        "Acceso doméstico → soporte homework digital (Warschauer,2004)",
      "Q32", "Nivel educativo del padre/madre",            "ordinal",   "L1pmf",        "Capital cultural → aspiraciones educativas (Bourdieu,1986)",
      "Q35", "Aspiración educativa para el hijo/a",        "ordinal",   "L1pmf",        "Expectativas parentales → motivación del alumno (Heckman,2006)",
      "Q39", "Reuniones escolares asistidas",              "continua",  "L2agg",        "Involucramiento parental → apoyo institucional (Epstein,2001)",
      "Q40", "Satisfacción con la escuela",                "ordinal",   "L1pmf",        "Clima escolar percibido → continuidad y adherencia",
      "Q41", "Ha buscado al docente para temas escolares", "binaria",   "L1pmf",        "Comunicación proactiva → capital social (Putnam,1995)",
      "Q44", "Importancia de aprender tecnología",         "ordinal",   "L1pmf",        "Valoración TIC → refuerzo en casa (Hoover-Dempsey,1995)",
      "Q47", "Horas semanales uso computadora (tutor)",    "continua",  "L1pmf",        "Modelado digital parental → disposición del hijo",
      "Q48", "Horas semanales internet (tutor)",           "continua",  "L1pmf",        "Uso TIC parental → ambiente digital doméstico"
    )
  )
)

# ---------------------------------------------------------------------------- #
# 4. FUNCIONES DE ANÁLISIS
# ---------------------------------------------------------------------------- #

# ── 4.1 ICC con modelo nulo (lmer) ────────────────────────────────────────────
calcular_icc <- function(vec, grupos, min_grupos=5, min_por_grupo=3) {
  df_m <- data.frame(y=suppressWarnings(as.numeric(vec)),
                      g=as.character(grupos)) %>%
    filter(!is.na(y), !is.na(g))
  n_grupos <- n_distinct(df_m$g)
  if (nrow(df_m) < 20 || n_grupos < min_grupos) return(NA_real_)
  tam_grupos <- table(df_m$g)
  if (mean(tam_grupos) < min_por_grupo) return(NA_real_)
  m0 <- tryCatch(
    suppressMessages(lmer(y ~ 1 + (1|g), data=df_m, REML=TRUE)),
    error=function(e) NULL, warning=function(w) NULL)
  if (is.null(m0)) return(NA_real_)
  vc <- as.data.frame(VarCorr(m0))
  var_entre <- vc$vcov[vc$grp=="g"]
  var_error <- vc$vcov[vc$grp=="Residual"]
  if (is.null(var_entre)||is.null(var_error)||is.na(var_entre+var_error)) return(NA_real_)
  if ((var_entre+var_error)==0) return(NA_real_)
  round(var_entre/(var_entre+var_error), 4)
}

# ── 4.2 Estadísticos descriptivos de una variable ────────────────────────────
desc_var <- function(vec) {
  v   <- suppressWarnings(as.numeric(vec)); v_ok <- v[!is.na(v)]
  n   <- length(v_ok); n_total <- length(vec)
  pct_na <- round(mean(is.na(v))*100, 1)
  list(n=n, n_total=n_total, pct_na=pct_na,
       media=if(n>0)round(mean(v_ok),3) else NA_real_,
       sd   =if(n>1)round(sd(v_ok),3)   else NA_real_,
       min  =if(n>0)min(v_ok)            else NA_real_,
       max  =if(n>0)max(v_ok)            else NA_real_,
       n_cats=if(n>0)length(unique(v_ok)) else NA_integer_)
}


# ── 4.2b Correlación con el outcome PRE (diferenciada por tipo de variable) ──
# Variables ORDINALES    → Spearman ρ (rho): no paramétrico, respeta rangos
# Variables CONTINUAS    → Pearson r: asume escala de intervalo, más potente
# Variables BINARIAS     → Pearson punto-biserial r (= Pearson con 0/1)
#
# El tipo de coeficiente se elige según la naturaleza declarada en el catálogo
# o detectada automáticamente.
# Si no se puede cargar el outcome → NA (no penaliza la decisión de inclusión).
calcular_correlacion <- function(vec_pred, vec_outcome, naturaleza="ordinal") {
  v1 <- suppressWarnings(as.numeric(vec_pred))
  v2 <- suppressWarnings(as.numeric(vec_outcome))
  idx_ok <- !is.na(v1) & !is.na(v2)
  if (sum(idx_ok) < 10) return(list(r=NA_real_, p=NA_real_,
                                     metodo=NA_character_, coef_nombre=NA_character_))

  # Seleccionar método según naturaleza
  metodo <- if (naturaleza %in% c("continua")) "pearson" else "spearman"
  coef_nombre <- case_when(
    naturaleza == "continua"          ~ "r Pearson",
    naturaleza == "binaria"           ~ "r Punto-biserial",
    naturaleza %in% c("ordinal","categorica") ~ "rho Spearman",
    TRUE                              ~ "rho Spearman"
  )

  # Verificar varianza antes de correlacionar (SD=0 → r no definida)
  if (sd(v1[idx_ok], na.rm=TRUE) == 0 || sd(v2[idx_ok], na.rm=TRUE) == 0)
    return(list(r=NA_real_, p=NA_real_, metodo=metodo,
                coef_nombre=coef_nombre,
                nota="SD=0: variable sin varianza, correlacion no definida"))

  res <- tryCatch(
    cor.test(v1[idx_ok], v2[idx_ok], method=metodo, exact=FALSE),
    error=function(e) NULL,
    warning=function(w) tryCatch(
      cor.test(v1[idx_ok], v2[idx_ok], method=metodo, exact=FALSE),
      error=function(e) NULL))
  if (is.null(res)) return(list(r=NA_real_, p=NA_real_,
                                 metodo=metodo, coef_nombre=coef_nombre))

  list(r          = round(as.numeric(res$estimate), 3),
       p          = round(res$p.value, 4),
       metodo     = metodo,
       coef_nombre= coef_nombre)
}

# ── 4.3 Árbol de decisión por criterios (duros y blandos) ───────────────────
#
# CRITERIOS DUROS → EXCLUIR (fallar uno es suficiente para excluir):
#   C1: %NA > 25%          → datos insuficientes, sesgo de selección
#   C2: n_cats < 2         → sin varianza, imposible incluir en modelo
#   C3: |r| < 0.05 Y p≥.20 → correlación nula con el outcome (doble evidencia)
#
# CRITERIOS BLANDOS → REVISION (fallar requiere juicio teórico):
#   C4: ICC < 0.05 para L2  → poca varianza entre escuelas (podría ser L1 agg)
#   C5: |r| 0.05-0.10 con p 0.10-0.20 → correlación marginal con el outcome
#
# Si no falla ningún criterio → INCLUIR
# ---------------------------------------------------------------------------
decidir_variable <- function(pct_na, icc, n_cats, nivel_teorico, nat,
                              r_cor=NA, p_cor=NA) {
  # r_cor: coeficiente de correlación (rho o r según tipo)
  # p_cor: p-valor asociado
  razones_excluir  <- character(0)
  razones_revision <- character(0)

  # ── CRITERIOS DUROS (EXCLUIR) ──────────────────────────────────────────
  # C1: Datos faltantes excesivos
  if (!is.na(pct_na) && pct_na > 25)
    razones_excluir <- c(razones_excluir,
                          sprintf("C1-DURO: NA=%.0f%% (>25%% — sesgo de seleccion)", pct_na))

  # C2: Sin varianza observada
  if (!is.na(n_cats) && n_cats < 2)
    razones_excluir <- c(razones_excluir,
                          "C2-DURO: sin varianza (1 categoria — imposible en modelo)")

  # C3: Correlacion nula con el outcome (doble evidencia: r Y p)
  if (!is.na(r_cor) && !is.na(p_cor)) {
    if (abs(r_cor) < 0.05 && p_cor >= 0.20)
      razones_excluir <- c(razones_excluir,
                            sprintf("C3-DURO: r=%.3f p=%.3f (|r|<0.05 Y p>=.20 — correlacion nula)",
                                    r_cor, p_cor))
  }

  # ── CRITERIOS BLANDOS (REVISION) ───────────────────────────────────────
  # C4: ICC bajo para variables L2 (puede ser L1 agg o exclusion)
  if (nivel_teorico %in% c("L2esc","L2agg") && !is.na(icc) && icc < 0.05)
    razones_revision <- c(razones_revision,
                           sprintf("C4-BLANDO: ICC=%.3f (<0.05 — poca var. entre escuelas; considerar L1 agregado)",
                                   icc))

  # C5: Correlacion marginal con el outcome
  if (!is.na(r_cor) && !is.na(p_cor)) {
    if (abs(r_cor) >= 0.05 && abs(r_cor) < 0.10 &&
        p_cor >= 0.10 && p_cor < 0.20)
      razones_revision <- c(razones_revision,
                             sprintf("C5-BLANDO: r=%.3f p=%.3f (correlacion marginal — decision teorica)",
                                     r_cor, p_cor))
  }

  # ── ÁRBOL DE DECISIÓN ──────────────────────────────────────────────────
  if (length(razones_excluir) > 0)
    return(list(decision="EXCLUIR",
                razon=paste(razones_excluir, collapse=" | "),
                nivel_final=NA_character_,
                criterio_principal=razones_excluir[1]))

  if (length(razones_revision) > 0)
    return(list(decision="REVISION",
                razon=paste(razones_revision, collapse=" | "),
                nivel_final=nivel_teorico,
                criterio_principal=razones_revision[1]))

  return(list(decision="INCLUIR",
              razon=sprintf("Cumple todos los criterios (r=%.3f, p=%.3f, ICC=%.3f)",
                             ifelse(is.na(r_cor),0,r_cor),
                             ifelse(is.na(p_cor),1,p_cor),
                             ifelse(is.na(icc),0,icc)),
              nivel_final=nivel_teorico,
              criterio_principal="Todos los criterios cumplidos"))
}

# ── 4.4 Centrado de variable ──────────────────────────────────────────────────
tipo_centrado <- function(nat, nivel_final) {
  # Guardia: nivel_final puede ser NA cuando la variable se excluye
  if (is.null(nivel_final) || is.na(nivel_final)) return(NA_character_)
  if (!is.na(nat) && nat == "binaria") return("Sin centrar (0/1 referencia)")
  if (nivel_final %in% c("L1est","L1doc","L1pmf"))
    return("CWC: centrado en la media del grupo")
  if (nivel_final %in% c("L2esc","L2agg","L2prop"))
    return("CGM: centrado en la gran media")
  return("CGM")
}

# ---------------------------------------------------------------------------- #
# 5. ANÁLISIS POR CUESTIONARIO
# ---------------------------------------------------------------------------- #
analizar_cuestionario <- function(nm_ctx) {
  cfg <- CATALOGO[[nm_ctx]]
  cat("\n", strrep("─",60), "\n  CTX:", nm_ctx, "\n")

  ruta_arch <- paste0(ruta_pre, cfg$archivo)
  if (!file.exists(ruta_arch)) {
    cat("  Archivo no encontrado:", ruta_arch, "\n"); return(NULL)
  }
  df_raw <- read_excel(ruta_arch)
  N <- nrow(df_raw)
  cat("  N =", N, "\n")

  # Columnas de metadatos a ignorar en el análisis
  ID_COLS <- c("Fecha","Q1","Q2","con.Q1","con.Q2","CCT","Escuela","Folio","ID",
               "Nombre","Nombre PF","Apellido Paterno PF","Apellido Materno PF",
               "ID PF","ID Est","Nombre Est","Apellido Paterno Est",
               "Apellido Materno Est","Duracion","Momento","Cuestionario",
               "Consentimiento","Estatus Consentimiento","Lista",
               "Coordinador","Facilitador","Donante","Nivel","Turno","Grado","Grupo")

  # Todas las columnas a analizar (no metadata)
  cols_analizar <- setdiff(names(df_raw), ID_COLS)
  cat("  Columnas en cuestionario:", length(cols_analizar), "\n")

  # Índice teórico: variables pre-seleccionadas del CATÁLOGO
  catalogo_idx <- setNames(
    lapply(seq_len(nrow(cfg$vars)), function(i) cfg$vars[i,]),
    cfg$vars$col
  )

  cct_vec <- if (!is.null(cfg$cct_col) && cfg$cct_col %in% names(df_raw))
    df_raw[[cfg$cct_col]] else rep("SIN_CCT", N)

  # ── Cargar y emparejar outcome PRE para Spearman ──────────────────────────
  # Columnas exactas verificadas en los archivos del proyecto:
  #   CTX_DIR: CCT (join por escuela → media de HD_DOC por CCT)
  #   CTX_DOC: Folio (join individual con HD_DOC)
  #   CTX_EST: Folio (join individual con HD_EST)
  #   CTX_PMF: CCT  (join por escuela → media de HD_EST por CCT)
  OUTCOME_CFG <- list(
    CTX_DIR = list(archivo="HD_DOC_pre.xlsx", items=paste0("Q",9:90),
                    por_cct=TRUE,  cct_ctx="CCT", cct_out="CCT",
                    folio_ctx=NULL, folio_out=NULL),
    CTX_DOC = list(archivo="HD_DOC_pre.xlsx", items=paste0("Q",9:90),
                    por_cct=FALSE, cct_ctx="CCT", cct_out="CCT",
                    folio_ctx="Folio", folio_out="Folio"),
    CTX_EST = list(archivo="HD_EST_pre.xlsx", items=paste0("Q",16:97),
                    por_cct=FALSE, cct_ctx="CCT", cct_out="CCT",
                    folio_ctx="Folio", folio_out="Folio"),
    CTX_PMF = list(archivo="HD_EST_pre.xlsx", items=paste0("Q",16:97),
                    por_cct=TRUE,  cct_ctx="CCT", cct_out="CCT",
                    folio_ctx=NULL, folio_out=NULL)
  )
  oc <- OUTCOME_CFG[[nm_ctx]]
  vec_outcome_aligned <- rep(NA_real_, N)

  ruta_out <- paste0(ruta_pre, oc$archivo)
  if (file.exists(ruta_out)) {
    df_out <- tryCatch(read_excel(ruta_out), error=function(e) NULL)
    if (!is.null(df_out)) {
      # Calcular PMP del outcome
      # IMPORTANTE: los ítems de HD/HI/HSXXI son texto (etiquetas de escala)
      # Se debe recodificar PRIMERO con COD antes de normalizar
      items_out <- intersect(oc$items, names(df_out))
      if (length(items_out) > 0) {
        # Recodificar cada ítem a numérico usando COD (incluye conocimiento_4, habilidad_4, etc.)
        items_num_out <- as.data.frame(lapply(
          df_out[, items_out, drop=FALSE],
          recodificar  # usa el COD completo definido al inicio del script
        ))
        # Detectar cat_max por ítem (ya en escala numérica 0-3 o 0-4)
        cm_out <- sapply(items_out, function(c) {
          v <- items_num_out[[c]]; v <- v[!is.na(v)]
          if (length(v)==0) return(NA_integer_)
          mv <- max(v)
          if (mv <= 1) return(1L)
          if (mv <= 3) return(3L)
          return(4L)
        })
        # Normalizar: PMP_ítem = valor_num / cat_max × 100
        pmp_mat <- mapply(function(c, cm) {
          v <- items_num_out[[c]]
          if (is.na(cm) || cm == 0) return(rep(NA_real_, nrow(df_out)))
          v / cm * 100
        }, items_out, cm_out, SIMPLIFY=TRUE)
        df_out$PMP_out <- if (!is.null(dim(pmp_mat)))
          round(rowMeans(pmp_mat, na.rm=TRUE), 2)
        else
          rep(NA_real_, nrow(df_out))
        # Diagnóstico
        n_pmp_ok <- sum(!is.na(df_out$PMP_out))
        pmp_med  <- mean(df_out$PMP_out, na.rm=TRUE)
        cat(sprintf("  PMP outcome calculado: n=%d, media=%.1f%%\n", n_pmp_ok, pmp_med))
      } else {
        df_out$PMP_out <- NA_real_
        cat("  AVISO: No se encontraron ítems del outcome en el archivo\n")
      }

      if (isTRUE(oc$por_cct) &&
          oc$cct_ctx %in% names(df_raw) &&
          oc$cct_out %in% names(df_out)) {
        # Emparejamiento por CCT: media escolar del outcome
        pmp_por_cct <- tapply(df_out$PMP_out,
                               trimws(as.character(df_out[[oc$cct_out]])),
                               mean, na.rm=TRUE)
        cct_ctx_vec <- trimws(as.character(df_raw[[oc$cct_ctx]]))
        vec_outcome_aligned <- as.numeric(pmp_por_cct[cct_ctx_vec])

      } else if (!isTRUE(oc$por_cct) &&
                 !is.null(oc$folio_ctx) && !is.null(oc$folio_out) &&
                 oc$folio_ctx %in% names(df_raw) &&
                 oc$folio_out %in% names(df_out)) {
        # Emparejamiento por Folio: 1 a 1 individual
        folio_ctx_vec <- trimws(as.character(df_raw[[oc$folio_ctx]]))
        folio_out_vec <- trimws(as.character(df_out[[oc$folio_out]]))
        idx_match <- match(folio_ctx_vec, folio_out_vec)
        vec_outcome_aligned <- df_out$PMP_out[idx_match]
      }

      n_match <- sum(!is.na(vec_outcome_aligned))
      cat(sprintf("  Outcome: %d/%d emparejados (%.0f%%) | PMP medio: %.1f%%
",
                  n_match, N, n_match/N*100,
                  mean(vec_outcome_aligned, na.rm=TRUE)))
    }
  } else {
    cat("  Archivo outcome no encontrado — Spearman sera NA
")
  }

  # ── Analizar TODAS las columnas del cuestionario ──────────────────────────
  resultados <- lapply(cols_analizar, function(col) {
    # Buscar en catálogo teórico
    en_catalogo  <- col %in% names(catalogo_idx)
    etiqueta     <- if(en_catalogo) catalogo_idx[[col]]$etiqueta     else paste("Reactivo",col)
    naturaleza   <- if(en_catalogo) catalogo_idx[[col]]$naturaleza   else "por_detectar"
    niv_teo      <- if(en_catalogo) catalogo_idx[[col]]$nivel_teorico else "no_evaluado"
    justificacion<- if(en_catalogo) catalogo_idx[[col]]$justificacion else
      "Sin evaluacion teorica previa — incluida por exhaustividad"

    # Detectar naturaleza si no está en catálogo
    if (naturaleza == "por_detectar") {
      vec_c <- limpiar(as.character(df_raw[[col]]))
      vec_c_ok <- vec_c[!is.na(vec_c)]
      n_uniq <- length(unique(vec_c_ok))
      num_prop <- mean(!is.na(suppressWarnings(as.numeric(vec_c_ok))))
      if (n_uniq <= 2)      naturaleza <- "binaria"
      else if (num_prop >= 0.7 && n_uniq > 5) naturaleza <- "continua"
      else if (n_uniq <= 10) naturaleza <- "ordinal"
      else if (mean(nchar(vec_c_ok))>35) naturaleza <- "texto_abierto"
      else naturaleza <- "categorica"
    }

    # Omitir texto abierto
    if (naturaleza == "texto_abierto")
      return(data.frame(
        fuente=nm_ctx, col=col, etiqueta=etiqueta, naturaleza=naturaleza,
        nivel_teorico=niv_teo, en_catalogo_teorico=en_catalogo,
        justificacion="Pregunta abierta — excluida del analisis cuantitativo",
        n=NA, n_total=N, pct_na=NA, media=NA, sd=NA,
        min_val=NA, max_val=NA, n_cats=NA, icc_cct=NA,
        coef_tipo="No aplica", r_correlacion=NA_real_, p_correlacion=NA_real_,
        interpretacion_r="No aplica (texto abierto)",
        decision="EXCLUIR",
        razon="Texto abierto — no cuantificable",
        criterio_principal="C0: texto abierto",
        nivel_final=NA, centrado=NA, stringsAsFactors=FALSE))

    vec_num  <- recodificar(df_raw[[col]])
    desc     <- desc_var(vec_num)
    icc_cct  <- calcular_icc(vec_num, cct_vec)

    # Correlación con outcome alineado (método según naturaleza de la variable)
    corr <- calcular_correlacion(vec_num, vec_outcome_aligned, naturaleza)

    # Interpretación con nombre del coeficiente explícito
    interp_r <- if (is.na(corr$r)) {
      "No calculable (sin emparejamiento o n<10)"
    } else {
      caso <- case_when(
        abs(corr$r) >= 0.30 ~ "Alta",
        abs(corr$r) >= 0.10 ~ "Moderada",
        abs(corr$r) >= 0.05 ~ "Debil",
        TRUE                 ~ "Nula"
      )
      sprintf("%s | %s: %.3f (p%s)",
              caso,
              coalesce(corr$coef_nombre, "r"),
              corr$r,
              ifelse(corr$p < .001, "<.001", sprintf("=%.3f", corr$p)))
    }

    decision <- decidir_variable(desc$pct_na, icc_cct, desc$n_cats,
                                  niv_teo, naturaleza,
                                  corr$r, corr$p)
    centrado <- tipo_centrado(naturaleza, decision$nivel_final)

    data.frame(
      fuente              = nm_ctx,
      col                 = col,
      etiqueta            = etiqueta,
      naturaleza          = naturaleza,
      nivel_teorico       = niv_teo,
      en_catalogo_teorico = en_catalogo,
      justificacion       = justificacion,
      n                   = desc$n,
      n_total             = N,
      pct_na              = desc$pct_na,
      media               = desc$media,
      sd                  = desc$sd,
      min_val             = desc$min,
      max_val             = desc$max,
      n_cats              = desc$n_cats,
      icc_cct             = icc_cct,
      coef_tipo           = coalesce(corr$coef_nombre, "No calculable"),
      r_correlacion       = corr$r,
      p_correlacion       = corr$p,
      interpretacion_r    = interp_r,
      decision            = decision$decision,
      razon               = decision$razon,
      criterio_principal  = decision$criterio_principal,
      nivel_final         = decision$nivel_final,
      centrado            = centrado,
      stringsAsFactors    = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), resultados))
}

# ---------------------------------------------------------------------------- #
# 6. EJECUTAR ANÁLISIS PARA TODOS LOS CTX
# ---------------------------------------------------------------------------- #
cat("\n", strrep("#",65), "\n")
cat("  FASE 5 — SELECCIÓN DE VARIABLES PARA ANÁLISIS MULTINIVEL\n")
cat("  Inicio:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("#",65), "\n")

lista_resultados <- lapply(names(CATALOGO), analizar_cuestionario)
names(lista_resultados) <- names(CATALOGO)
df_global <- do.call(rbind, lista_resultados)
rownames(df_global) <- NULL

# ---------------------------------------------------------------------------- #
# 7. TABLA DE ASIGNACIÓN A MODELOS
# ---------------------------------------------------------------------------- #
# Mapa de qué variables entran en qué modelo (según flujo de influencia cruzada)
#
# MODELO DOCENTES:
#   L1doc: variables propias CTX_DOC del individuo docente
#   L2esc: variables CTX_DIR agregadas a escuela (liderazgo como contexto)
#
# MODELO ESTUDIANTES:
#   L1est: variables propias CTX_EST + CTX_PMF del individuo estudiante
#   L2grado: variables CTX_EST o CTX_DOC agregadas por grado (cuando aplica)
#   L2esc: variables CTX_DIR + CTX_DOC_agg + CTX_PMF_agg agregadas a escuela

asignar_modelo <- function(df) {
  df %>%
    mutate(
      modelo_docentes = case_when(
        fuente=="CTX_DOC" & nivel_final=="L1doc"  ~ "L1 (docente)",
        fuente=="CTX_DIR" & nivel_final=="L2esc"  ~ "L2 (escuela)",
        fuente=="CTX_DOC" & nivel_final=="L2agg"  ~ "L2 (escuela, agregado)",
        TRUE ~ NA_character_
      ),
      modelo_estudiantes = case_when(
        fuente=="CTX_EST" & nivel_final=="L1est"  ~ "L1 (alumno)",
        fuente=="CTX_PMF" & nivel_final=="L1pmf"  ~ "L1 (alumno/familia)",
        fuente=="CTX_EST" & nivel_final=="L2esc"  ~ "L2 (escuela)",
        fuente=="CTX_DOC" & nivel_final=="L2agg"  ~ "L2 (escuela, agg. docentes)",
        fuente=="CTX_DIR" & nivel_final=="L2esc"  ~ "L2 (escuela, director)",
        fuente=="CTX_PMF" & nivel_final=="L2agg"  ~ "L2 (escuela, agg. padres)",
        TRUE ~ NA_character_
      )
    )
}

df_global <- asignar_modelo(df_global)

# ---------------------------------------------------------------------------- #
# 8. RESUMEN DE SELECCIÓN
# ---------------------------------------------------------------------------- #
cat("\n\n  RESUMEN DE DECISIONES:\n")
cat("  ", strrep("─",55), "\n", sep="")
cat(sprintf("  %-16s  %7s  %9s  %8s\n", "Cuestionario","INCLUIR","REVISION","EXCLUIR"))
cat("  ", strrep("─",55), "\n", sep="")
for (nm in names(CATALOGO)) {
  df_n <- df_global[df_global$fuente==nm,]
  cat(sprintf("  %-16s  %7d  %9d  %8d\n", nm,
              sum(df_n$decision=="INCLUIR"),
              sum(df_n$decision=="REVISION"),
              sum(df_n$decision=="EXCLUIR")))
}
cat("  ", strrep("─",55), "\n", sep="")
cat(sprintf("  %-16s  %7d  %9d  %8d\n", "TOTAL",
            sum(df_global$decision=="INCLUIR"),
            sum(df_global$decision=="REVISION"),
            sum(df_global$decision=="EXCLUIR")))

# ---------------------------------------------------------------------------- #
# 9. COLINEALIDAD ENTRE PREDICTORES SELECCIONADOS
# ---------------------------------------------------------------------------- #
cat("\n  Calculando colinealidad entre predictores...\n")

calcular_cor_predictores <- function(df_decision, df_datos, modelo_col, nivel_fil) {
  vars_sel <- df_decision %>%
    filter(.data[[modelo_col]] == nivel_fil, decision=="INCLUIR") %>%
    pull(col)
  if (length(vars_sel) < 2) return(NULL)
  cols_ok <- intersect(vars_sel, names(df_datos))
  if (length(cols_ok) < 2) return(NULL)
  df_num <- as.data.frame(lapply(df_datos[, cols_ok, drop=FALSE], recodificar))
  df_num <- df_num[, sapply(df_num, function(c) sd(c,na.rm=TRUE)>0 &&
                               sum(!is.na(c))>10), drop=FALSE]
  if (ncol(df_num) < 2) return(NULL)
  mat <- cor(df_num, use="pairwise.complete.obs")
  list(mat=mat, cols=colnames(mat))
}

# ---------------------------------------------------------------------------- #
# 10. GUARDAR TABLAS EXCEL — UNA PESTAÑA POR FIGURA + HOJAS RESUMEN
# ---------------------------------------------------------------------------- #
cat("  Guardando tablas Excel...\n")

# Estilos de formato condicional
verde    <- createStyle(bgFill="#C8E6C9", fontColour="#1A5C1A", textDecoration="bold")
amarillo <- createStyle(bgFill="#FFF9C4", fontColour="#7A6800")
rojo     <- createStyle(bgFill="#FFCDD2", fontColour="#7A0000")
gris     <- createStyle(bgFill="#F5F5F5", fontColour="#777777")
azul_enc <- createStyle(bgFill="#1A3A6C", fontColour="white",
                         textDecoration="bold", halign="center")

aplicar_formato <- function(wb, hoja, df) {
  # Encabezado azul siempre
  addStyle(wb, hoja, style=azul_enc, rows=1, cols=1:ncol(df), gridExpand=FALSE)
  # Filas: solo si existe columna decision
  if (!"decision" %in% names(df)) return(invisible(NULL))
  for (i in seq_len(nrow(df))) {
    dec <- df$decision[i]
    if (length(dec)==0 || is.na(dec)) next
    est <- if (dec=="INCLUIR") verde
           else if (dec=="REVISION") amarillo
           else if (dec=="EXCLUIR") rojo
           else gris
    addStyle(wb, hoja, style=est, rows=i+1, cols=1:ncol(df), gridExpand=FALSE)
  }
}

wb <- createWorkbook()

# ── PESTAÑAS POR FIGURA (una por cada cuestionario de contexto) ───────────────
FIGURAS_INFO <- list(
  CTX_DIR = list(nombre_hoja="DIR_Directores",    titulo="Directores"),
  CTX_DOC = list(nombre_hoja="DOC_Docentes",      titulo="Docentes"),
  CTX_EST = list(nombre_hoja="EST_Estudiantes",   titulo="Estudiantes"),
  CTX_PMF = list(nombre_hoja="PMF_Padres_Tutores",titulo="Padres/Madres/Tutores")
)

# Columnas a mostrar en cada pestaña por figura (orden de lectura para el investigador)
cols_figura <- c(
  "col","etiqueta","naturaleza","en_catalogo_teorico",
  "nivel_teorico","n","n_total","pct_na",
  "media","sd","min_val","max_val","n_cats",
  "icc_cct","r_correlacion","p_correlacion","interpretacion_r",
  "decision","razon","criterio_principal",
  "nivel_final","centrado","justificacion"
)

for (nm_ctx in names(FIGURAS_INFO)) {
  info  <- FIGURAS_INFO[[nm_ctx]]
  df_f  <- df_global %>%
    filter(fuente == nm_ctx) %>%
    select(any_of(cols_figura)) %>%
    arrange(decision, col)  # INCLUIR primero, luego REVISION, luego EXCLUIR

  if (nrow(df_f) == 0) next
  addWorksheet(wb, info$nombre_hoja)
  writeData(wb, info$nombre_hoja, df_f)
  aplicar_formato(wb, info$nombre_hoja, df_f)
  cat(sprintf("  Hoja %s: %d reactivos (%d INCLUIR | %d REVISION | %d EXCLUIR)\n",
              info$nombre_hoja, nrow(df_f),
              sum(df_f$decision=="INCLUIR", na.rm=TRUE),
              sum(df_f$decision=="REVISION", na.rm=TRUE),
              sum(df_f$decision=="EXCLUIR",  na.rm=TRUE)))
}

# ── HOJA RESUMEN: Solo variables INCLUIDAS de todos los CTX ───────────────────
df_incluidas <- df_global %>%
  filter(decision=="INCLUIR") %>%
  select(any_of(c("fuente","col","etiqueta","naturaleza","nivel_final",
                   "modelo_docentes","modelo_estudiantes",
                   "icc_cct","r_correlacion","p_correlacion","interpretacion_r",
                   "pct_na","media","sd","centrado","justificacion",
                   "decision"))) %>%   # decision necesaria para aplicar_formato
  arrange(fuente, col)

addWorksheet(wb, "RESUMEN_Incluidas")
writeData(wb, "RESUMEN_Incluidas", df_incluidas)
aplicar_formato(wb, "RESUMEN_Incluidas", df_incluidas)

# ── HOJA REVISIÓN MANUAL: solo las que necesitan decisión ─────────────────────
df_rev <- df_global %>%
  filter(decision=="REVISION") %>%
  mutate(
    criterio_revision = case_when(
      grepl("C4-BLANDO",criterio_principal) ~
        "ICC bajo (<0.05) — poca varianza entre escuelas",
      grepl("C5-BLANDO",criterio_principal) ~
        "Correlacion marginal con outcome (r entre .05-.10, p entre .10-.20)",
      TRUE ~ razon
    ),
    accion_sugerida = case_when(
      grepl("C4-BLANDO",criterio_principal) & !is.na(icc_cct) & icc_cct < 0.03 ~
        "PROBABLEMENTE EXCLUIR: ICC muy bajo (<.03). Solo retener si teoria muy solida",
      grepl("C4-BLANDO",criterio_principal) ~
        "REVISAR: ICC<.05. Considerar como L1 agregada o retener en bloque teorico",
      grepl("C5-BLANDO",criterio_principal) & !is.na(r_correlacion) & abs(r_correlacion)<0.06 ~
        "PROBABLEMENTE EXCLUIR: r practicamente nulo. Retener solo si moderador clave",
      grepl("C5-BLANDO",criterio_principal) ~
        "REVISAR: r marginal. Incluir en bloque teorico y dejar que AIC/BIC decida en F6",
      TRUE ~ "Revisar con criterio teorico del investigador"
    ),
    referencia = "Conover(1999); Field(2013) r>=.10; Kraemer & Blasey(2015)"
  ) %>%
  select(fuente,col,etiqueta,icc_cct,r_correlacion,p_correlacion,pct_na,
         criterio_revision,accion_sugerida,referencia) %>%
  arrange(fuente,col)

addWorksheet(wb, "REVISION_Manual")
writeData(wb, "REVISION_Manual", df_rev)
aplicar_formato(wb, "REVISION_Manual", df_rev)

# ── HOJA ICC: todas las variables con ICC calculado ───────────────────────────
df_icc <- df_global %>%
  filter(!is.na(icc_cct)) %>%
  mutate(interpretacion_icc = case_when(
    icc_cct >= 0.25 ~ "Alta variabilidad entre escuelas",
    icc_cct >= 0.10 ~ "Variabilidad moderada entre escuelas",
    icc_cct >= 0.05 ~ "Variabilidad baja pero suficiente",
    TRUE            ~ "Variabilidad insuficiente para L2"
  )) %>%
  select(fuente,col,etiqueta,naturaleza,nivel_teorico,icc_cct,
         interpretacion_icc,decision) %>%
  arrange(fuente, desc(icc_cct))

addWorksheet(wb, "ICC_por_variable")
writeData(wb, "ICC_por_variable", df_icc)
aplicar_formato(wb, "ICC_por_variable", df_icc)

# ── HOJA MAPA: asignación de variables a modelos MLM ─────────────────────────
df_mapa <- bind_rows(
  df_global %>% filter(!is.na(modelo_docentes), decision=="INCLUIR") %>%
    select(fuente,col,etiqueta,nivel_final,centrado,
           modelo=modelo_docentes) %>% mutate(para_modelo="Docentes"),
  df_global %>% filter(!is.na(modelo_estudiantes), decision=="INCLUIR") %>%
    select(fuente,col,etiqueta,nivel_final,centrado,
           modelo=modelo_estudiantes) %>% mutate(para_modelo="Estudiantes")
) %>% arrange(para_modelo, modelo, fuente)

addWorksheet(wb, "MAPA_Modelos_MLM")
writeData(wb, "MAPA_Modelos_MLM", df_mapa)

# ── HOJA SPEARMAN: tabla completa de correlaciones ───────────────────────────
df_spear_tbl <- df_global %>%
  filter(!is.na(r_correlacion)) %>%
  mutate(pasa_criterio_r = case_when(
    abs(r_correlacion)>=0.10 | p_correlacion<0.10 ~ "PASA (|r|>=.10 o p<.10)",
    abs(r_correlacion)>=0.05                   ~ "REVISION (r marginal)",
    TRUE                                     ~ "NO PASA (correlacion nula)"
  )) %>%
  select(fuente,col,etiqueta,naturaleza,nivel_teorico,
         r_correlacion,p_correlacion,interpretacion_r,pasa_criterio_r,decision) %>%
  arrange(fuente, desc(abs(r_correlacion)))

addWorksheet(wb, "Correlacion_con_Outcome")
writeData(wb, "Correlacion_con_Outcome", df_spear_tbl)
aplicar_formato(wb, "Correlacion_con_Outcome", df_spear_tbl)

tryCatch(saveWorkbook(wb, file.path(ruta_sal,"TABLAS","F5_Seleccion_Variables.xlsx"),
                      overwrite=TRUE), error=function(e)
  cat("  [Excel save error]", conditionMessage(e), "\n"))

# CSVs adicionales
write.csv(df_global,  file.path(ruta_sal,"TABLAS","F5_decision_global.csv"),  row.names=FALSE)
write.csv(df_icc,     file.path(ruta_sal,"TABLAS","F5_icc_variables.csv"),    row.names=FALSE)
write.csv(df_mapa,    file.path(ruta_sal,"TABLAS","F5_mapa_modelos.csv"),     row.names=FALSE)
write.csv(df_rev,     file.path(ruta_sal,"TABLAS","F5_revision_manual.csv"),  row.names=FALSE)
write.csv(df_spear_tbl,file.path(ruta_sal,"TABLAS","F5_correlacion.csv"),       row.names=FALSE)

# ---------------------------------------------------------------------------- #
# 11. VISUALIZACIONES
# ---------------------------------------------------------------------------- #
cat("  Generando gráficos...\n")

# G1: Distribución de ICC por cuestionario
# Etiquetas de referencia para líneas de umbral ICC
df_umb_icc <- data.frame(
  y_val = c(0.052, 0.102, 0.252),
  label = c("5%: umbral minimo L2", "10%: moderado", "25%: alta varianza entre esc.")
)
g_icc <- df_icc %>%
  mutate(fuente   = factor(fuente),
         decision = factor(decision, levels=c("INCLUIR","REVISION","EXCLUIR"))) %>%
  ggplot(aes(x=reorder(col, icc_cct), y=icc_cct, fill=decision)) +
  geom_col(width=0.72, alpha=0.88, color="white", linewidth=0.2) +
  geom_hline(yintercept=0.05,  linetype="dashed", color="#F4A261", linewidth=0.7) +
  geom_hline(yintercept=0.10,  linetype="dashed", color="#2A9D8F", linewidth=0.5) +
  geom_hline(yintercept=0.25,  linetype="dashed", color="#1A3A6C", linewidth=0.5) +
  geom_text(data=df_umb_icc,
            aes(x=Inf, y=y_val, label=label),
            hjust=1.05, vjust=-0.3, size=2.4, color="grey30",
            inherit.aes=FALSE) +
  scale_fill_manual(values=c(INCLUIR="#1A9641", REVISION="#FDAE61", EXCLUIR="#D7191C"),
                    name="Decision") +
  scale_y_continuous(labels=label_number(suffix="%", scale=100),
                     breaks=c(0, .05, .10, .20, .30, .50)) +
  coord_flip() +
  facet_wrap(~fuente, scales="free_y", ncol=2) +
  labs(title="CCI por variable - cuestionarios de contexto",
       subtitle="CCI = varianza entre escuelas / varianza total | Modelo nulo (lmer, REML)",
       x=NULL, y="CCI (Coeficiente de Correlacion Intraclase)",
       caption="Lineas punteadas: 5%=umbral minimo L2 | 10%=moderado | 25%=alta varianza") +
  theme(axis.text.y=element_text(size=7), strip.text=element_text(face="bold"))
guardar_g(g_icc, file.path(ruta_sal,"GRAFICOS","G1_ICC_variables.png"), 14, 14)

# G2: Resumen de decisiones por cuestionario (donut / barras apiladas)
df_res_dec <- df_global %>%
  count(fuente, decision) %>%
  group_by(fuente) %>% mutate(pct=round(n/sum(n)*100,1)) %>% ungroup() %>%
  mutate(decision=factor(decision,levels=c("INCLUIR","REVISION","EXCLUIR")))

g_dec <- ggplot(df_res_dec, aes(x=fuente, y=n, fill=decision)) +
  geom_col(position="stack", width=0.65, color="white", linewidth=0.4) +
  geom_text(aes(label=sprintf("%d\n(%.0f%%)",n,pct)),
            position=position_stack(vjust=0.5), size=3.2, fontface="bold",
            color="white") +
  scale_fill_manual(values=c(INCLUIR="#1A9641", REVISION="#FDAE61", EXCLUIR="#D7191C"),
                    name="Decisión") +
  scale_y_continuous(breaks=seq(0,30,5)) +
  labs(title="Decisiones de selección por cuestionario de contexto",
       subtitle="Verde=incluida | Naranja=revisión manual | Rojo=excluida",
       x="Cuestionario de contexto", y="N° de variables") +
  theme(axis.text.x=element_text(size=10,face="bold"))
guardar_g(g_dec, file.path(ruta_sal,"GRAFICOS","G2_decisiones.png"), 9, 5)

# G3: Mapa de flujo de influencia cruzada entre figuras
# Implementado como heatmap: cuestionario × modelo, coloreado por N vars
df_mapa_heat <- df_mapa %>%
  count(fuente, para_modelo, modelo) %>%
  mutate(modelo_lbl=str_replace_all(modelo,"\\(|\\)","") %>% str_trim())
g_mapa <- ggplot(df_mapa_heat,
                  aes(x=para_modelo, y=fuente, fill=n, size=n)) +
  geom_point(shape=21, color="white", stroke=0.5) +
  geom_text(aes(label=n), color="white", fontface="bold", size=3.5) +
  scale_fill_gradient(low="#ABD9E9", high="#1A3A6C", name="N vars") +
  scale_size_continuous(range=c(6,18), guide="none") +
  labs(title="Mapa de influencia cruzada entre figuras educativas",
       subtitle="Tamaño y color del punto = N° de variables del CTX que entran al modelo",
       x="Modelo objetivo", y="Cuestionario de origen",
       caption="Un cuestionario puede aportar variables a múltiples modelos\n(ej: CTX_DIR aporta L2 tanto al modelo de docentes como al de estudiantes)") +
  theme(panel.grid.major=element_line(color="#EEEEEE"),
        axis.text=element_text(size=10,face="bold"))
guardar_g(g_mapa, file.path(ruta_sal,"GRAFICOS","G3_mapa_influencia.png"), 9, 6)

# G4: Distribución de % NA por cuestionario (filtro de datos)
g_na <- df_global %>%
  mutate(decision=factor(decision, levels=c("INCLUIR","REVISION","EXCLUIR"))) %>%
  ggplot(aes(x=reorder(paste0(fuente," ",col), pct_na),
             y=pct_na, fill=decision)) +
  geom_col(width=0.75, alpha=0.85) +
  geom_hline(yintercept=25, linetype="dashed", color="#D7191C", linewidth=0.7) +
  geom_text(data=data.frame(y=27, label="25%: umbral de exclusion"),
            aes(x=1, y=y, label=label),
            hjust=0, size=2.8, color="#D7191C", inherit.aes=FALSE) +
  scale_fill_manual(values=c(INCLUIR="#1A9641", REVISION="#FDAE61", EXCLUIR="#D7191C"),
                    name="Decision") +
  scale_y_continuous(labels=label_number(suffix="%")) +
  coord_flip() +
  facet_wrap(~fuente, scales="free_y", ncol=2) +
  labs(title="% de datos faltantes por variable",
       subtitle="Linea roja = umbral de exclusion (25%)",
       x=NULL, y="% NA") +
  theme(axis.text.y=element_text(size=6.5))
guardar_g(g_na, file.path(ruta_sal,"GRAFICOS","G4_missing_data.png"), 14, 12)

# G5: Correlaciones de Spearman con el outcome PRE (por cuestionario)
if ("r_correlacion" %in% names(df_global) && sum(!is.na(df_global$r_correlacion))>0) {
  df_spear_plot <- df_global %>%
    filter(!is.na(r_correlacion)) %>%
    mutate(decision=factor(decision,levels=c("INCLUIR","REVISION","EXCLUIR")),
           abs_r=abs(r_correlacion),
           etiq_r=sprintf("r=%.2f%s", r_correlacion,
                           case_when(p_correlacion<.001~"***",
                                     p_correlacion<.01 ~"**",
                                     p_correlacion<.05 ~"*",
                                     p_correlacion<.10 ~".",
                                     TRUE~"")))
  g_spear <- ggplot(df_spear_plot,
                     aes(x=reorder(paste0(fuente,"\n",col), r_correlacion),
                         y=r_correlacion, fill=decision)) +
    geom_col(width=0.75, alpha=0.88) +
    geom_hline(yintercept=0,  color="#444", linewidth=0.5) +
    geom_hline(yintercept=c(0.10, 0.30),
               linetype="dashed", color="#1A9641", linewidth=0.5, alpha=0.7) +
    geom_hline(yintercept=c(-0.10, -0.30),
               linetype="dashed", color="#1A9641", linewidth=0.5, alpha=0.7) +
    geom_text(aes(label=etiq_r,
                   hjust=ifelse(r_correlacion>=0,-0.1,1.1)),
              size=2.4, color="grey20") +
    geom_text(data=data.frame(y=c(0.105, 0.305), label=c("|r|=.10","|r|=.30")),
              aes(x=Inf, y=y, label=label),
              hjust=1.1, size=2.5, color="#1A9641", inherit.aes=FALSE) +
    scale_fill_manual(values=c(INCLUIR="#1A9641",REVISION="#FDAE61",EXCLUIR="#D7191C"),
                      name="Decision") +
    scale_y_continuous(breaks=seq(-0.5,0.5,0.1),
                        labels=label_number(accuracy=0.1)) +
    coord_flip() +
    facet_wrap(~fuente, scales="free_y", ncol=2) +
    labs(title="Correlacion con el outcome PRE (PMP global) — rho o r segun tipo de variable",
         subtitle="Verde punteado: |coef|=.10 (umbral inclusion) | rho Spearman (ordinal) | r Pearson (continua)",
         x=NULL, y="r de Spearman",
         caption="* p<.05 | ** p<.01 | *** p<.001 | . p<.10") +
    theme(axis.text.y=element_text(size=6.5), strip.text=element_text(face="bold"))
  guardar_g(g_spear, file.path(ruta_sal,"GRAFICOS","G5_Correlacion_outcome.png"), 14, 14)
}

# ---------------------------------------------------------------------------- #
# 12. PREPARAR DATASETS PARA MLM
# ---------------------------------------------------------------------------- #
cat("  Preparando datasets para MLM...\n")

# Función: construir dataset limpio para un modelo dado
construir_dataset_mlm <- function(modelo_nombre, modelo_col, archivos_ctx) {

  vars_modelo <- df_global %>%
    filter(!is.na(.data[[modelo_col]]), decision=="INCLUIR")

  dfs <- list()
  for (nm_ctx in unique(vars_modelo$fuente)) {
    cfg <- CATALOGO[[nm_ctx]]
    ruta_a <- paste0(ruta_pre, cfg$archivo)
    if (!file.exists(ruta_a)) next
    df_raw <- read_excel(ruta_a)

    vars_ctx <- vars_modelo %>% filter(fuente==nm_ctx)
    cols_sel <- intersect(vars_ctx$col, names(df_raw))
    if (length(cols_sel)==0) next

    # Columnas de identificación disponibles
    id_cols <- intersect(c("CCT","Folio","ID","Nivel","Grado","Grupo","Turno",
                            "Parentesco","Nivel.1"), names(df_raw))
    df_sub <- df_raw[, c(id_cols, cols_sel), drop=FALSE]

    # Recodificar ítems
    for (col in cols_sel) df_sub[[col]] <- recodificar(df_sub[[col]])

    # Agregar Entidad
    if ("CCT" %in% names(df_sub))
      df_sub$Entidad <- substr(as.character(df_sub$CCT), 1, 2)

    dfs[[nm_ctx]] <- df_sub
  }
  dfs
}

# Datasets base
cat("    Construyendo dataset docentes...\n")
ds_doc <- construir_dataset_mlm("Docentes", "modelo_docentes",
                                 c("CTX_DOC","CTX_DIR"))
cat("    Construyendo dataset estudiantes...\n")
ds_est <- construir_dataset_mlm("Estudiantes", "modelo_estudiantes",
                                 c("CTX_EST","CTX_PMF","CTX_DOC","CTX_DIR"))

# Guardar como RDS (preserva tipos de datos)
saveRDS(ds_doc, file.path(ruta_sal,"DATASETS","datos_docentes_MLM.rds"))
saveRDS(ds_est, file.path(ruta_sal,"DATASETS","datos_estudiantes_MLM.rds"))

# Guardar como CSV (para compatibilidad)
for (nm in names(ds_doc))
  write.csv(ds_doc[[nm]], file.path(ruta_sal,"DATASETS",
    paste0("doc_",nm,".csv")), row.names=FALSE)
for (nm in names(ds_est))
  write.csv(ds_est[[nm]], file.path(ruta_sal,"DATASETS",
    paste0("est_",nm,".csv")), row.names=FALSE)

# ---------------------------------------------------------------------------- #
# 13. RESUMEN FINAL
# ---------------------------------------------------------------------------- #
cat("\n\n", strrep("═",65), "\n")
cat("  RESUMEN FASE 5 — SELECCIÓN COMPLETADA\n")
cat(strrep("═",65), "\n\n")

vars_inc  <- sum(df_global$decision=="INCLUIR")
vars_rev  <- sum(df_global$decision=="REVISION")
vars_exc  <- sum(df_global$decision=="EXCLUIR")
cat(sprintf("  Variables analizadas:  %d\n", nrow(df_global)))
cat(sprintf("  INCLUIDAS:             %d (%.1f%%)\n", vars_inc, vars_inc/nrow(df_global)*100))
cat(sprintf("  EN REVISION:           %d (%.1f%%) — ver hoja 05 del Excel\n", vars_rev, vars_rev/nrow(df_global)*100))
cat(sprintf("  EXCLUIDAS:             %d (%.1f%%)\n\n", vars_exc, vars_exc/nrow(df_global)*100))

cat("  MODELO DOCENTES (variables incluidas):\n")
df_mapa %>% filter(para_modelo=="Docentes") %>%
  count(modelo) %>% arrange(modelo) %>%
  {for(i in seq_len(nrow(.))) cat(sprintf("    %-40s %d vars\n", .$modelo[i], .$n[i]))}

cat("\n  MODELO ESTUDIANTES (variables incluidas):\n")
df_mapa %>% filter(para_modelo=="Estudiantes") %>%
  count(modelo) %>% arrange(modelo) %>%
  {for(i in seq_len(nrow(.))) cat(sprintf("    %-40s %d vars\n", .$modelo[i], .$n[i]))}

cat("\n  Archivos guardados en:", ruta_sal, "\n")
cat("  Fin:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n\n")

cat(strrep("─",65), "\n")
cat("  NOTAS METODOLÓGICAS\n")
cat(strrep("─",65), "\n")
cat("  CCI: Calculado con modelo nulo en lmer (REML)\n")
cat("  Umbral CCI ≥ 0.05: Raudenbush & Bryk (2002); Snijders & Bosker (2012)\n")
cat("  Centrado CWC/CGM: Enders & Tofighi (2007); Kreft & de Leeuw (1998)\n")
cat("  Influencia cruzada: Hallinger & Heck (1998) — modelo de efectos indirectos\n")
cat("  Brecha digital: Warschauer (2004); OECD (2015) 'Students, Computers and Learning'\n")
cat("  Capital cultural: Bourdieu (1986); Coleman (1966) 'Equality of Educational Opportunity'\n")
cat("  TPACK docente: Mishra & Koehler (2006); Ertmer (2005) — barreras primera y segunda orden\n")
cat("  Liderazgo pedagógico: Leithwood et al. (2005); Robinson et al. (2009)\n")
cat(strrep("─",65), "\n")
