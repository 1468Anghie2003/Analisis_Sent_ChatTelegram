# =====================================================================
#  Análisis de Sentimientos (carpeta GrupoXDMSent) | Minería de Datos - Carrera de Informática, UMSA
#  Análisis de sentimientos del chat de Telegram "Curso de Temporada"
#  (grupo de estudiantes de Informática, junio 2023 - octubre 2026)
# ---------------------------------------------------------------------
#  00_config.R : rutas, parámetros, paquetes, tema visual, bitácora,
#                índice de figuras, checkpoints y utilidades de texto.
#  Todas las fases (F01 ... F11) empiezan con source("00_config.R").
#  REPRODUCIBILIDAD: para usar otro chat basta con dejar su export JSON
#  de Telegram en 01_Dataset/ (el primer .json que encuentre se usa).
# =====================================================================

options(warn = 1, scipen = 9, stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------
# 0.1 RUTAS -----------------------------------------------------------
#> La carpeta raíz es la que contiene 01_Dataset. Funciona si se abre el
#> proyecto .Rproj, si se ejecuta desde 02_Codigo o desde la raíz.
# ---------------------------------------------------------------------
.buscar_raiz <- function() {
  cand <- c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."),
            "C:/Users/HP/Desktop/GrupoXDMSent")
  for (c in cand) if (dir.exists(file.path(c, "01_Dataset"))) return(normalizePath(c, winslash = "/"))
  stop("No se encontró la carpeta 01_Dataset. Abrir GrupoXDMSent.Rproj o fijar RAIZ en 00_config.R")
}
RAIZ <- .buscar_raiz()
DIR <- list(
  dataset = file.path(RAIZ, "01_Dataset"),
  codigo  = file.path(RAIZ, "02_Codigo"),
  fuentes = file.path(RAIZ, "02_Codigo", "fuentes"),
  dic     = file.path(RAIZ, "03_Diccionarios"),
  emoji   = file.path(RAIZ, "03_Diccionarios", "emoji_fluent3d"),
  res     = file.path(RAIZ, "04_Resultados"),
  fig     = file.path(RAIZ, "04_Resultados", "figuras"),
  tab     = file.path(RAIZ, "04_Resultados", "tablas"),
  mod     = file.path(RAIZ, "04_Resultados", "modelos"),
  chk     = file.path(RAIZ, "04_Resultados", "checkpoints"),
  bit     = file.path(RAIZ, "05_Bitacora"),
  web     = file.path(RAIZ, "docs"))
invisible(lapply(DIR, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---------------------------------------------------------------------
# 0.2 PARÁMETROS (único lugar a editar) ------------------------------
# ---------------------------------------------------------------------
.json <- list.files(DIR$dataset, pattern = "\\.json$", full.names = TRUE)
if (!length(.json)) stop("No hay ningún archivo .json en 01_Dataset/")
CFG <- list(
  version       = "1.0",
  proyecto      = "Análisis de Sentimientos",
  carpeta       = "GrupoXDMSent",
  titulo        = "Análisis de Sentimientos",
  subtitulo     = "Análisis de sentimientos del grupo de Telegram del Curso de Temporada de Informática (UMSA)",
  materia       = "Minería de Datos - Carrera de Informática, UMSA",
  archivo_json  = .json[1],
  zona_horaria  = "America/La_Paz",
  semilla       = 2026,
  anonimizar    = TRUE,    # TRUE: los autores se publican como "Miembro 0001" (ética de datos)
  publicar_textos = TRUE,  # TRUE: el dashboard muestra mensajes de ejemplo (con teléfonos, correos y @ ocultos)
  min_largo_texto = 2,     # caracteres mínimos para considerar un mensaje con texto analizable
  min_rep_publicidad = 3,  # un texto largo repetido >= 3 veces se considera difusión masiva
  min_freq_vocab  = 3,     # frecuencia mínima de un término para entrar al vocabulario del dashboard
  k_topicos       = 4:12,  # candidatos de k para LDA
  min_msgs_dia_lda = 5,    # un día entra como documento LDA si tiene al menos 5 mensajes
  min_menciones_docente = 20,
  anonimizar_docentes = FALSE, # TRUE: los docentes se publican como "Docente 01" (recomendado si el dashboard será público)
  dpi           = 170,
  mostrar       = FALSE)   # TRUE para ver cada gráfico en RStudio mientras se guarda
set.seed(CFG$semilla)

# Periodo académico (calendario UMSA aproximado; editable). Mes-día de inicio.
PERIODOS <- data.frame(
  periodo = c("Verano", "Semestre I", "Invierno", "Semestre II"),
  inicio  = c("12-01", "03-01", "06-10", "08-16"))

# Los 15 sentimientos acordados en clase (orden fijo en todo el proyecto).
#> Cambio acordado: "frustración" reemplaza a "amor" y "satisfacción" a
#> "vergüenza". "Seguridad" se integra en tranquilidad e "ironía" se usa
#> como señal que invierte la polaridad (ver F06 y F07).
SENT_ORDEN <- c("alegria", "gratitud", "esperanza", "admiracion", "tranquilidad", "satisfaccion",
                "sorpresa", "curiosidad",
                "confusion", "preocupacion", "tristeza", "enojo", "decepcion", "cansancio", "frustracion")
SENT_LAB <- c(alegria = "Alegría", gratitud = "Gratitud", esperanza = "Esperanza", admiracion = "Admiración",
              tranquilidad = "Tranquilidad", satisfaccion = "Satisfacción", sorpresa = "Sorpresa",
              curiosidad = "Curiosidad", confusion = "Confusión", preocupacion = "Preocupación",
              tristeza = "Tristeza", enojo = "Enojo", decepcion = "Decepción", cansancio = "Cansancio",
              frustracion = "Frustración")
SENT_COL <- c(alegria = "#FFB400", gratitud = "#FF7A1A", esperanza = "#00BFA6", admiracion = "#FF2E7E",
              tranquilidad = "#76C442", satisfaccion = "#18975A", sorpresa = "#A66CFF", curiosidad = "#2D8CFF",
              confusion = "#6E7FA8", preocupacion = "#0E7490", tristeza = "#3A56D4", enojo = "#E3170A",
              decepcion = "#8E5572", cansancio = "#8D6E63", frustracion = "#B5179E")
SENT_EMOJI <- c(alegria = "😂", gratitud = "🙏", esperanza = "🤞", admiracion = "👏", tranquilidad = "😌",
                satisfaccion = "🥳", sorpresa = "😮", curiosidad = "👀", confusion = "🤔", preocupacion = "😰",
                tristeza = "😢", enojo = "😡", decepcion = "😒", cansancio = "🫠", frustracion = "😤")
POL_COL <- c(positivo = "#16A34A", neutral = "#94A3B8", negativo = "#E11D48")

# Fases del proyecto: código, carpeta, título, tema de clase, color y fondo
FASES <- data.frame(
  cod    = sprintf("F%02d", 1:11),
  nombre = c("extraccion", "exploracion", "limpieza", "tokenizacion", "stopwords_tfidf", "diccionarios",
             "sentimientos", "topicos", "entidades_relaciones", "patrones_prediccion", "cierre"),
  titulo = c("Extracción: del JSON a registros", "Exploración del dataset original",
             "Preprocesamiento y limpieza", "Tokenización y n-gramas",
             "Palabras vacías, shallow linguistics y TF-IDF", "Diccionarios de sentimientos, emojis y jerga",
             "Análisis de sentimientos", "Modelado de tópicos (LDA)",
             "Entidades, lugares y relaciones", "Patrones ocultos y predicción", "Cierre, hallazgos y bitácora"),
  color  = c("#F26B1D", "#E0A100", "#17A34A", "#0D9488", "#0284C7", "#4F46E5", "#9333EA", "#DB2777",
             "#E11D48", "#5E9E0E", "#475569"),
  fondo  = c("#FFF3EA", "#FFF8E2", "#ECFAF1", "#E8F8F6", "#EAF5FC", "#EFEFFE", "#F6EDFF", "#FFEDF5",
             "#FFEEF1", "#F3FAE8", "#F1F4F8"))

# ---------------------------------------------------------------------
# 0.3 PAQUETES ---------------------------------------------------------
# ---------------------------------------------------------------------
PKG_REQ <- c("jsonlite", "data.table", "stringi", "ggplot2", "scales", "patchwork", "ggrepel",
             "tidytext", "tm", "topicmodels", "stopwords", "SnowballC", "igraph", "arules",
             "forecast", "glmnet", "openxlsx", "ragg", "systemfonts", "png", "ggridges",
             "zoo", "slam", "dplyr", "Matrix")
# Rscript no trae un espejo de CRAN configurado: se fija uno para poder instalar paquetes
if (is.null(getOption("repos")) || getOption("repos")[["CRAN"]] %in% c("@CRAN@", "", NA))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
.falt <- PKG_REQ[!vapply(PKG_REQ, requireNamespace, logical(1), quietly = TRUE)]
if (length(.falt) && file.access(.libPaths()[1], 2) != 0) {
  # En Windows la carpeta de R en Archivos de programa no es escribible: se usa la biblioteca personal
  .lib_usr <- Sys.getenv("R_LIBS_USER"); dir.create(.lib_usr, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(.lib_usr, .libPaths()))
}
if (length(.falt)) {
  message("Instalando paquetes que faltan (solo la primera vez): ", paste(.falt, collapse = ", "))
  install.packages(.falt, dependencies = TRUE, type = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType"))
  .falt2 <- .falt[!vapply(.falt, requireNamespace, logical(1), quietly = TRUE)]
  if (length(.falt2)) stop("No se pudieron instalar: ", paste(.falt2, collapse = ", "), ". Revisar conexion a internet o instalarlos desde RStudio.")
}
suppressPackageStartupMessages({
  library(data.table); library(stringi); library(ggplot2); library(scales); library(patchwork)
})

# ---------------------------------------------------------------------
# 0.4 FUENTES Y TEMA VISUAL -------------------------------------------
#> Se registran fuentes propias (incluidas en 02_Codigo/fuentes) para que
#> los gráficos se vean igual en cualquier computadora: Fraunces (títulos
#> en cursiva), Manrope (texto) y JetBrains Mono (cifras y código).
# ---------------------------------------------------------------------
.ff <- function(f) file.path(DIR$fuentes, f)
FT_TIT <- "Fraunces XD"; FT_TXT <- "Manrope XD"; FT_MONO <- "JetBrains XD"
tryCatch({
  systemfonts::register_font(FT_TIT, plain = .ff("Fraunces-SemiBold.ttf"), bold = .ff("Fraunces-SemiBold.ttf"),
                             italic = .ff("Fraunces-Italic.ttf"), bolditalic = .ff("Fraunces-SemiBoldItalic.ttf"))
  systemfonts::register_font(FT_TXT, plain = .ff("Manrope-Regular.ttf"), bold = .ff("Manrope-Bold.ttf"),
                             italic = .ff("Manrope-Regular.ttf"), bolditalic = .ff("Manrope-ExtraBold.ttf"))
  systemfonts::register_font(FT_MONO, plain = .ff("JetBrainsMono-Medium.ttf"), bold = .ff("JetBrainsMono-Medium.ttf"),
                             italic = .ff("JetBrainsMono-Medium.ttf"), bolditalic = .ff("JetBrainsMono-Medium.ttf"))
}, error = function(e) { message("Fuentes propias no disponibles; se usa la fuente del sistema");
  FT_TIT <<- ""; FT_TXT <<- ""; FT_MONO <<- "" })

TINTA <- "#1C1A2B"; TINTA2 <- "#4A4860"; TINTA3 <- "#77758C"
#> La fuente de cada figura se arma con el nombre del chat y las fechas
#> del propio JSON, para que siga siendo correcta si se cambia el dataset.
FUENTE_DATOS <- local({
  t <- readChar(CFG$archivo_json, file.size(CFG$archivo_json), useBytes = TRUE)
  nom <- stri_match_first_regex(t, '"name"\\s*:\\s*"([^"]*)"')[, 2]
  fe <- stri_match_all_regex(t, '"date"\\s*:\\s*"(\\d{4})-(\\d{2})')[[1]]
  m <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  per <- if (nrow(fe)) sprintf("%s-%s a %s-%s", m[as.integer(fe[1, 3])], fe[1, 2], m[as.integer(fe[nrow(fe), 3])], fe[nrow(fe), 2]) else ""
  Encoding(nom) <- "UTF-8"
  sprintf('Fuente: chat de Telegram "%s" (Informática UMSA), %s', nom, per)
})

GRAF <- new.env()
GRAF$fase <- "F00"; GRAF$nombre <- "config"; GRAF$titulo <- ""; GRAF$n <- 0L
GRAF$color <- "#475569"; GRAF$fondo <- "#F1F4F8"; GRAF$dir <- DIR$fig

theme_xd <- function(base = 12.5) {
  ac <- GRAF$color; bg <- GRAF$fondo
  theme_minimal(base_size = base, base_family = FT_TXT) +
    theme(plot.background  = element_rect(fill = bg, color = ac, linewidth = 1.6),
          panel.background = element_rect(fill = bg, color = NA),
          panel.grid.major = element_line(color = alpha(ac, 0.14), linewidth = 0.35),
          panel.grid.minor = element_blank(),
          text             = element_text(color = TINTA),
          axis.text        = element_text(color = TINTA2, size = base - 2),
          axis.title       = element_text(color = TINTA2, size = base - 1.5),
          plot.title       = element_text(family = FT_TIT, face = "bold.italic", size = base + 9, color = TINTA,
                                          margin = margin(b = 4)),
          plot.title.position = "plot",
          plot.subtitle    = element_text(color = TINTA2, size = base, margin = margin(b = 12), lineheight = 1.15),
          plot.caption     = element_text(color = TINTA3, size = base - 3.5, hjust = 0, margin = margin(t = 10)),
          plot.caption.position = "plot",
          legend.position  = "top", legend.justification = "left",
          legend.title     = element_text(color = TINTA2, size = base - 2),
          legend.text      = element_text(color = TINTA2, size = base - 2.5),
          legend.background = element_rect(fill = NA, color = NA),
          legend.key       = element_rect(fill = NA, color = NA),
          strip.text       = element_text(family = FT_TIT, face = "bold.italic", color = ac, size = base + 1, hjust = 0),
          plot.margin      = margin(22, 24, 16, 22))
}
theme_set(theme_xd())

# ---------------------------------------------------------------------
# 0.5 BITÁCORA ----------------------------------------------------------
#> Cada decisión queda registrada con fecha, fase, paso, filas antes y
#> después, archivo generado y justificación. Se guarda en
#> 05_Bitacora/bitacora.csv y F11 la convierte en un Excel con diseño.
# ---------------------------------------------------------------------
RUTA_BIT <- file.path(DIR$bit, "bitacora.csv")
BIT_COLS <- c("id", "fecha_hora", "fase", "paso", "accion", "detalle", "filas_antes", "filas_despues",
              "archivo", "justificacion")
BITACORA <- if (file.exists(RUTA_BIT)) as.data.frame(fread(RUTA_BIT, encoding = "UTF-8", colClasses = "character")) else
  setNames(data.frame(matrix(character(), 0, length(BIT_COLS))), BIT_COLS)

bit <- function(paso, accion, detalle = "", antes = NA, despues = NA, archivo = "", justificacion = "", eco = TRUE) {
  fila <- data.frame(id = NA, fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                     fase = paste(GRAF$fase, GRAF$titulo, sep = " | "), paso = paso, accion = accion,
                     detalle = detalle, filas_antes = as.character(antes), filas_despues = as.character(despues),
                     archivo = archivo, justificacion = justificacion)
  b <- rbind(get("BITACORA", envir = globalenv()), fila)
  b$id <- seq_len(nrow(b))
  assign("BITACORA", b, envir = globalenv())
  fwrite(b, RUTA_BIT, bom = TRUE)
  if (eco) cat(sprintf("  [bitácora %s] %s: %s\n", GRAF$fase, paso, accion))
  invisible(NULL)
}

# ---------------------------------------------------------------------
# 0.6 FASES, FIGURAS Y TABLAS -------------------------------------------
#> Cada figura se guarda como Fxx_nn_nombre.png en la carpeta de su fase y
#> se registra en indice_figuras.csv con título y descripción: ese índice
#> es el que usa el dashboard y el informe (la numeración coincide).
# ---------------------------------------------------------------------
RUTA_IDX <- file.path(DIR$fig, "indice_figuras.csv")
IDX_FIG <- if (file.exists(RUTA_IDX)) as.data.frame(fread(RUTA_IDX, encoding = "UTF-8")) else
  data.frame(fase = character(), nro = integer(), archivo = character(), ruta = character(),
             titulo = character(), descripcion = character())

fase_inicio <- function(cod) {
  f <- FASES[FASES$cod == cod, ]
  GRAF$fase <- cod; GRAF$nombre <- f$nombre; GRAF$titulo <- f$titulo; GRAF$n <- 0L
  GRAF$color <- f$color; GRAF$fondo <- f$fondo
  GRAF$dir <- file.path(DIR$fig, paste0(cod, "_", f$nombre))
  dir.create(GRAF$dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(GRAF$dir, pattern = "\\.png$", full.names = TRUE))       # una corrida nueva reemplaza
  unlink(list.files(DIR$tab, pattern = paste0("^", cod, "_"), full.names = TRUE))
  IDX_FIG <<- IDX_FIG[IDX_FIG$fase != cod, , drop = FALSE]
  b <- get("BITACORA", envir = globalenv()); b <- b[!startsWith(b$fase, cod), , drop = FALSE]
  if (nrow(b)) b$id <- seq_len(nrow(b)); assign("BITACORA", b, envir = globalenv())
  theme_set(theme_xd())
  cat("\n", strrep("=", 72), "\n ", cod, " | ", f$titulo, "\n", strrep("=", 72), "\n", sep = "")
  GRAF$t0 <- Sys.time()
}

guardar_fig <- function(p, nombre, titulo, descripcion, w = 11, h = 7) {
  GRAF$n <- GRAF$n + 1L
  arch <- sprintf("%s_%02d_%s.png", GRAF$fase, GRAF$n, nombre)
  ruta <- file.path(GRAF$dir, arch)
  pie  <- sprintf("Análisis de Sentimientos · Figura %s-%02d · %s", GRAF$fase, GRAF$n, FUENTE_DATOS)
  ancho <- max(60, round(w * 10.8))
  envolver <- function(t) if (is.null(t) || !nzchar(t)) t else paste(stri_wrap(t, ancho, whitespace_only = TRUE), collapse = "\n")
  envolver_lineas <- function(t) if (is.null(t)) t else paste(vapply(strsplit(t, "\n", fixed = TRUE)[[1]], envolver, ""), collapse = "\n")
  #> El título dentro de la imagen es siempre el mismo título formal que
  #> figura en el índice de figuras y en el informe (numeración Fxx-nn).
  if (inherits(p, "patchwork")) {
    p$patches$annotation$subtitle <- envolver_lineas(p$patches$annotation$subtitle)
    p <- p & theme(plot.background = element_rect(fill = GRAF$fondo, color = NA),
                   plot.title = element_text(family = FT_TIT, face = "bold.italic", size = 15, color = TINTA))
    p <- p + plot_annotation(title = titulo, caption = pie, theme = theme_xd())
  } else {
    p <- p + labs(title = titulo, subtitle = envolver_lineas(p$labels$subtitle), caption = pie)
    #> En ggplot2 4.x un gráfico de aspecto fijo (coord_equal, coord_polar)
    #> ya no ocupa todo el lienzo; se envuelve para que el marco de color
    #> abarque la imagen completa, igual que en el resto de figuras.
    fijo <- inherits(p$coordinates, c("CoordFixed", "CoordPolar", "CoordRadial")) || tryCatch(!p$coordinates$is_free(), error = function(e) FALSE)
    if (GG4 && fijo) {
      p <- patchwork::wrap_elements(full = p + theme(plot.background = element_rect(fill = NA, color = NA))) +
        theme(plot.background = element_rect(fill = GRAF$fondo, color = GRAF$color, linewidth = 1.6))
    }
  }
  ok <- tryCatch({
    ggsave(ruta, p, width = w, height = h, dpi = CFG$dpi, device = ragg::agg_png, bg = GRAF$fondo)
    TRUE }, error = function(e) { cat("  ! No se pudo guardar", arch, ":", conditionMessage(e), "\n"); FALSE })
  if (CFG$mostrar) print(p)
  if (ok) {
    IDX_FIG <<- rbind(IDX_FIG, data.frame(fase = GRAF$fase, nro = GRAF$n, archivo = arch,
                                          ruta = file.path("04_Resultados", "figuras", basename(GRAF$dir), arch),
                                          titulo = titulo, descripcion = descripcion))
    IDX_FIG <<- IDX_FIG[order(IDX_FIG$fase, IDX_FIG$nro), ]
    fwrite(IDX_FIG, RUTA_IDX, bom = TRUE)
    bit(sprintf("Figura %s-%02d", GRAF$fase, GRAF$n), titulo, descripcion,
        archivo = file.path("04_Resultados", "figuras", basename(GRAF$dir), arch),
        justificacion = "Cada figura queda registrada con su número, que se repite en el informe y en el dashboard", eco = FALSE)
    cat(sprintf("  [figura] %s\n", arch))
  }
  invisible(arch)
}

guardar_tabla <- function(df, nombre) {
  arch <- sprintf("%s_%s.csv", GRAF$fase, nombre)
  fwrite(as.data.table(df), file.path(DIR$tab, arch), bom = TRUE)
  invisible(arch)
}

fase_fin <- function() {
  seg <- round(as.numeric(difftime(Sys.time(), GRAF$t0, units = "secs")), 1)
  bit("Cierre de fase", sprintf("Fase completada en %.1f s", seg),
      sprintf("%d figuras generadas", GRAF$n))
}

# ---------------------------------------------------------------------
# 0.7 CHECKPOINTS -------------------------------------------------------
#> Cada fase guarda su resultado en 04_Resultados/checkpoints (.rds); así
#> cualquier fase se puede correr sola si las anteriores ya se corrieron.
# ---------------------------------------------------------------------
guardar_chk <- function(obj, nombre) saveRDS(obj, file.path(DIR$chk, paste0(nombre, ".rds")), compress = TRUE)
cargar_chk <- function(nombre) {
  f <- file.path(DIR$chk, paste0(nombre, ".rds"))
  if (!file.exists(f)) stop("Falta el checkpoint '", nombre, "'. Ejecutar antes la fase que lo genera (ver EJECUTAR_TODO.R)")
  readRDS(f)
}

# ---------------------------------------------------------------------
# 0.8 UTILIDADES DE TEXTO ---------------------------------------------
# ---------------------------------------------------------------------
#> Expresión regular de emojis (Unicode): banderas, pictogramas con tono de
#> piel y secuencias unidas con ZWJ (por ejemplo 🤷‍♂️ o ❤️‍🔥).
RX_EMOJI <- paste0("(?:\\p{Regional_Indicator}{2})|",
                   "(?:\\p{Extended_Pictographic}\\x{FE0F}?[\\x{1F3FB}-\\x{1F3FF}]?",
                   "(?:\\x{200D}\\p{Extended_Pictographic}\\x{FE0F}?[\\x{1F3FB}-\\x{1F3FF}]?)*)")
norm_emoji <- function(x) stri_replace_all_regex(x, "[\\x{FE0F}\\x{1F3FB}-\\x{1F3FF}]", "")
emoji_codigo <- function(e) vapply(e, function(z) paste(sprintf("%x", utf8ToInt(z)), collapse = "-"), "", USE.NAMES = FALSE)
emoji_png <- function(e) {
  f <- file.path(DIR$emoji, paste0(emoji_codigo(norm_emoji(e)), ".png"))
  ifelse(file.exists(f), f, NA_character_)
}

RX_URL   <- "(?:https?://|www\\.)\\S+|\\b\\S+\\.(?:com|org|net|bo|gle|me|ly|io|edu)(?:/\\S*)?"
RX_MAIL  <- "[\\w.+-]+@[\\w-]+\\.[\\w.]+"
RX_FONO  <- "(?:\\+?591[\\s-]?)?\\b[67]\\d{7}\\b"
RX_MENC  <- "@\\w+"

#> Oculta datos personales para mostrar textos en el dashboard.
enmascarar <- function(x) {
  x <- stri_replace_all_regex(x, RX_MAIL, "[correo]")
  x <- stri_replace_all_regex(x, RX_FONO, "[teléfono]")
  x <- stri_replace_all_regex(x, RX_MENC, "@usuario")
  x
}

#> Minúsculas, sin tildes (ñ -> n, igual que el diccionario de clase) y
#> alargamientos reducidos ("graciaaas" -> "gracias"). Conserva números y
#> signos para que las reglas de entidades y frases sigan funcionando.
normalizar_base <- function(x) {
  x <- stri_trans_nfkc(x)   # NFKC: convierte letras "decoradas" (𝗱𝗲, ｄｅ) en letras normales
  x <- stri_replace_all_regex(x, "[\\x{00A0}\\x{202F}\\x{2007}\\x{200B}\\x{2060}]", " ")
  x <- stri_trans_tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  # letras repetidas 3+ veces dentro de una palabra ("siii", "nooo"); un token
  # completo como "iii" (número romano) se respeta
  x <- stri_replace_all_regex(x, "(?<=\\p{L})(\\p{L})\\1{2,}|(\\p{L})\\2{2,}(?=\\p{L})", "$1$2")
  stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
}

# ---------------------------------------------------------------------
# 0.9 PERIODOS ----------------------------------------------------------
# ---------------------------------------------------------------------
periodo_academico <- function(fecha) {
  md <- format(fecha, "%m-%d"); y <- as.integer(format(fecha, "%Y"))
  ini <- PERIODOS$inicio
  per <- ifelse(md >= ini[1] | md < ini[2], "Verano",
         ifelse(md < ini[3], "Semestre I", ifelse(md < ini[4], "Invierno", "Semestre II")))
  anio_ref <- ifelse(per == "Verano" & md >= ini[1], y + 1L, y)   # el verano de dic-2024 es "Verano 2025"
  paste(per, anio_ref)
}
semestre <- function(fecha) {
  m <- as.integer(format(fecha, "%m")); paste0(ifelse(m <= 6, "I-", "II-"), format(fecha, "%Y"))
}

# ---------------------------------------------------------------------
# 0.10 AYUDAS GRÁFICAS --------------------------------------------------
# ---------------------------------------------------------------------
num <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")

#> Dibuja emojis 3D (Fluent Emoji, licencia MIT) como imágenes dentro de
#> un ggplot: x e y en coordenadas de datos; w y h en unidades de cada eje.
# Compatibilidad entre versiones de ggplot2 (3.4 a 4.x) e igraph (1.x y 2.x):
#> ggplot2 4.0 cambió label.size por linewidth en las etiquetas y
#> reemplazó geom_errorbarh por geom_errorbar(orientation = "y");
#> estas funciones hacen que el mismo código corra en ambas versiones.
GG4 <- utils::packageVersion("ggplot2") >= "3.5.99"
.borde <- function(a) { if (GG4 && !is.null(a$label.size)) { a$linewidth <- a$label.size; a$label.size <- NULL }; a }
geom_label_xd <- function(...) do.call(ggplot2::geom_label, .borde(list(...)))
annotate_label <- function(...) do.call(ggplot2::annotate, c(list("label"), .borde(list(...))))
barra_error_h <- function(mapping, alto = 0, ...) ggplot2::geom_errorbar(mapping, width = alto, orientation = "y", ...)
a_no_dirigido <- function(g, ...) {
  f <- if (exists("as_undirected", envir = asNamespace("igraph"))) get("as_undirected", envir = asNamespace("igraph")) else igraph::as.undirected
  f(g, ...)
}

agregar_emojis <- function(p, emojis, x, y, w, h) {
  for (i in seq_along(emojis)) {
    f <- emoji_png(emojis[i]); if (is.na(f)) next
    img <- png::readPNG(f)
    p <- p + annotation_custom(grid::rasterGrob(img, interpolate = TRUE),
                               xmin = x[i] - w / 2, xmax = x[i] + w / 2, ymin = y[i] - h / 2, ymax = y[i] + h / 2)
  }
  p
}

MESES_ES <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
etiq_mes <- function(x) ifelse(is.na(x), "", paste0(MESES_ES[as.integer(format(x, "%m"))], "\n", format(x, "%Y")))
etiq_mes1 <- function(x) ifelse(is.na(x), "", paste(MESES_ES[as.integer(format(x, "%m"))], format(x, "%Y")))
DIAS_ES <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

sent_lab <- function(s) unname(SENT_LAB[s])
escala_sent <- function(...) scale_fill_manual(values = SENT_COL, labels = SENT_LAB, ...)
escala_sent_col <- function(...) scale_color_manual(values = SENT_COL, labels = SENT_LAB, ...)

cat("Configuración cargada | raíz:", RAIZ, "| dataset:", basename(CFG$archivo_json), "\n")
