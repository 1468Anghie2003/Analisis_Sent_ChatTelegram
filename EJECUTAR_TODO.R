# =====================================================================
#  GrupoXDMSent | Mineria de Datos - Carrera de Informatica, UMSA
#  EJECUTAR_TODO.R : corre las 11 fases en orden con un solo Source
#  (botón Source en RStudio o, en la terminal: Rscript EJECUTAR_TODO.R)
#  Requisito: el export JSON de Telegram dentro de 01_Dataset/.
#  Después: source("GENERAR_DASHBOARD.R") actualiza la página en docs/.
#  Para correr una sola fase: abrir 02_Codigo/Fxx_*.R y Source; carga sola
#  el resultado de las fases anteriores desde 04_Resultados/checkpoints.
#  Cada corrida queda registrada en 05_Bitacora/corridas.csv y la consola
#  completa en 05_Bitacora/log_consola.txt.
# =====================================================================
rm(list = ls())
if (!dir.exists("02_Codigo")) stop("Ejecutar desde la carpeta GrupoXDMSent (abrir GrupoXDMSent.Rproj)")
FASES_R <- c("F01_extraccion.R", "F02_exploracion.R", "F03_limpieza.R", "F04_tokenizacion.R", "F05_stopwords_tfidf.R",
             "F06_diccionarios.R", "F07_sentimientos.R", "F08_topicos.R", "F09_entidades_relaciones.R",
             "F10_patrones_prediccion.R", "F11_cierre.R")
dir.create("05_Bitacora", showWarnings = FALSE)
.log <- file(file.path("05_Bitacora", "log_consola.txt"), open = "wt", encoding = "UTF-8")
sink(.log, split = TRUE); sink(.log, type = "message")
.inicio <- Sys.time(); .tiempos <- c(); .estado <- "completa"
tryCatch({
  for (.f in FASES_R) {
    .t0 <- Sys.time()
    rm(list = setdiff(ls(all.names = FALSE), c("FASES_R", ".f", ".t0", ".inicio", ".tiempos", ".estado", ".log")))
    source(file.path("02_Codigo", .f), encoding = "UTF-8")
    .tiempos[sub("_.*", "", .f)] <- round(as.numeric(difftime(Sys.time(), .t0, units = "secs")), 1)
  }
}, error = function(e) {
  .estado <<- paste("error en", .f, ":", conditionMessage(e))
  cat("\n*** ERROR en", .f, ":", conditionMessage(e), "***\nCorregir y volver a ejecutar esa fase (02_Codigo/", .f, ")\n")
}, finally = {
  .fila <- data.frame(corrida = format(.inicio, "%Y%m%d-%H%M%S"), inicio = format(.inicio, "%Y-%m-%d %H:%M:%S"),
                      fin = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), minutos = round(as.numeric(difftime(Sys.time(), .inicio, units = "mins")), 2),
                      estado = .estado, R = R.version.string, sistema = Sys.info()[["sysname"]], t(.tiempos), check.names = FALSE)
  .f_c <- file.path("05_Bitacora", "corridas.csv")
  .prev <- if (file.exists(.f_c)) tryCatch(read.csv(.f_c, check.names = FALSE, encoding = "UTF-8"), error = function(e) NULL) else NULL
  .tot <- if (is.null(.prev)) .fila else merge(.prev, .fila, all = TRUE, sort = FALSE)
  write.csv(.tot, .f_c, row.names = FALSE, fileEncoding = "UTF-8")
  print(.tiempos)
  cat("Tiempo total:", .fila$minutos, "minutos | estado:", .estado, "\n")
  sink(type = "message"); sink(); close(.log)
})
