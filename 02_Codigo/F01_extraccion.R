# =====================================================================
#  F01 | EXTRACCIÓN: DEL JSON A REGISTROS (TUPLAS)
#  Tema de clase: 1. Introducción a la extracción de texto
#  - Técnica: extracción estructurada (parseo JSON) + Regex sobre las
#    entidades de texto que Telegram ya separa (links, hashtags, menciones).
#  - Resultado: 01_dataset_original.csv = el "dataset sucio": una fila por
#    registro, TODAS las claves del JSON como columnas, sin perder un dato.
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
fase_inicio("F01")

# ---- 1. Lectura del export de Telegram -------------------------------
#> jsonlite lee el archivo tal cual (simplifyVector = FALSE) para conservar
#> la estructura anidada de cada mensaje (listas de entidades, reacciones).
chat <- jsonlite::fromJSON(CFG$archivo_json, simplifyVector = FALSE)
msgs <- chat$messages
bit("1. Lectura", "Se leyó el export JSON de Telegram",
    sprintf("Archivo %s (%.1f MB) | chat '%s' (%s) | %s registros", basename(CFG$archivo_json),
            file.size(CFG$archivo_json) / 1e6, chat$name, chat$type, num(length(msgs))),
    despues = length(msgs), archivo = basename(CFG$archivo_json),
    justificacion = "El formato JSON 'legible por máquina' de Telegram Desktop conserva todos los metadatos")

# ---- 2. Inventario de claves (anatomía del JSON) ----------------------
#> Se cuentan todas las claves que aparecen en algún registro; las anidadas
#> (listas u objetos) se marcan para guardarlas como texto JSON.
claves <- unique(unlist(lapply(msgs, names)))
es_anidada <- function(v) !is.null(v) && (is.list(v) || length(v) > 1)
inv <- rbindlist(lapply(claves, function(k) {
  vals <- lapply(msgs, `[[`, k)
  data.table(clave = k, presentes = sum(!vapply(vals, is.null, TRUE)),
             anidada = any(vapply(vals, es_anidada, TRUE)))
}))
inv[, pct := presentes / length(msgs)]
setorder(inv, -presentes)
guardar_tabla(inv, "inventario_claves")
bit("2. Inventario", sprintf("%d claves distintas en el JSON", nrow(inv)),
    sprintf("Anidadas: %s", paste(inv[anidada == TRUE, clave], collapse = ", ")),
    justificacion = "Saber qué trae cada registro antes de aplanarlo evita perder campos poco frecuentes")

# ---- 3. Aplanado sin pérdida -----------------------------------------
#> Cada clave se vuelve una columna. Los valores simples se copian; los
#> anidados (text_entities, reactions, poll, members...) se guardan como
#> texto JSON, que se puede volver a leer con jsonlite::fromJSON.
a_texto <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (es_anidada(v)) return(as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null")))
  as.character(v)
}
orig <- as.data.table(setNames(lapply(claves, function(k) vapply(msgs, function(r) a_texto(r[[k]]), "")), claves))
# texto plano: la unión de los fragmentos de text_entities (Telegram parte el texto en trozos)
orig[, texto_plano := vapply(msgs, function(r) paste0(vapply(r$text_entities, function(e) e$text %||% "", ""), collapse = ""), "")]
orig[, `:=`(chat_nombre = chat$name, chat_tipo = chat$type, chat_id = as.character(chat$id))]
orig[, fecha_hora_local := format(as.POSIXct(as.numeric(date_unixtime), origin = "1970-01-01", tz = CFG$zona_horaria),
                                  "%Y-%m-%d %H:%M:%S")]
setcolorder(orig, c("id", "type", "date", "date_unixtime", "fecha_hora_local"))
bit("3. Aplanado", "JSON convertido a tabla: 1 fila = 1 registro",
    sprintf("%s filas x %d columnas (%d claves del JSON + texto_plano + 3 de metadatos del chat + fecha local)",
            num(nrow(orig)), ncol(orig), length(claves)),
    antes = length(msgs), despues = nrow(orig),
    justificacion = "Trabajar con tuplas (registros) permite filtrar por fecha, autor o tipo como en una hoja de cálculo")

# ---- 4. Auditoría de no pérdida ----------------------------------------
#> Se comprueba: (a) mismo número de registros, (b) cada clave tiene la
#> misma cantidad de valores no nulos en el JSON y en la tabla y (c) los
#> campos anidados vuelven a ser idénticos al leer su texto JSON.
aud <- inv[, .(clave, json = presentes)]
aud[, tabla := vapply(clave, function(k) sum(!is.na(orig[[k]])), 1L)]
reconst_ok <- function(k) {
  idx <- which(!is.na(orig[[k]]))
  all(vapply(idx, function(i) {
    o <- msgs[[i]][[k]]
    if (!es_anidada(o)) return(identical(as.character(o), orig[[k]][i]))
    r <- jsonlite::fromJSON(orig[[k]][i], simplifyVector = FALSE)
    identical(jsonlite::toJSON(o, auto_unbox = TRUE, null = "null"), jsonlite::toJSON(r, auto_unbox = TRUE, null = "null"))
  }, TRUE))
}
aud[, identico := vapply(clave, reconst_ok, TRUE)]
aud[, ok := json == tabla & identico]
guardar_tabla(aud, "auditoria_no_perdida")
stopifnot(nrow(orig) == length(msgs), all(aud$ok))
bit("4. Auditoría", "Verificación de no pérdida superada",
    sprintf("%d/%d claves con el mismo conteo y valores idénticos tras reconstruir", sum(aud$ok), nrow(aud)),
    antes = length(msgs), despues = nrow(orig),
    justificacion = "Garantiza que el CSV es una copia fiel del JSON (requisito: no perder un solo dato)")

# ---- 5. Escritura del dataset original ----------------------------------
#> El resultado se escribe como 01_dataset_original.csv: es el "dataset
#> sucio" del proyecto, sin ningún filtro. Se usa UTF-8 con BOM para que
#> Excel muestre bien tildes y emojis, y se guarda además un checkpoint
#> .rds para que la fase siguiente no tenga que volver a leer el JSON.
ruta_orig <- file.path(DIR$dataset, "01_dataset_original.csv")
fwrite(orig, ruta_orig, bom = TRUE)
guardar_chk(orig, "f01_original")
bit("5. Escritura", "Se guardó el dataset original (sucio) en CSV", sprintf("%.1f MB, UTF-8 con BOM para Excel", file.size(ruta_orig) / 1e6),
    despues = nrow(orig), archivo = "01_Dataset/01_dataset_original.csv",
    justificacion = "El CSV se abre en Excel y en R; el BOM conserva tildes y emojis")

# ---- 6. Figuras ---------------------------------------------------------
#> Cinco figuras documentan la extracción: qué claves trae el JSON, qué
#> tipos de registro hay, cómo se distribuyen en el tiempo, qué entidades
#> separa Telegram y la prueba visual de que no se perdió ningún valor.
# F01-01 Anatomía del JSON
d1 <- copy(inv)[, clave := factor(clave, rev(clave))]
d1[, tipo := ifelse(anidada, "Anidada (se guarda como texto JSON)", "Simple (valor directo)")]
p <- ggplot(d1, aes(pct, clave, fill = tipo)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = num(presentes)), hjust = -0.15, family = FT_MONO, size = 3.1, color = TINTA2) +
  scale_x_continuous(labels = percent, expand = expansion(c(0, 0.12))) +
  scale_fill_manual(values = c("#F26B1D", "#FFB27A"), name = NULL) +
  labs(title = "Anatomía del JSON exportado de Telegram",
       subtitle = sprintf("%d claves distintas en %s registros; el porcentaje indica en cuántos registros aparece cada una",
                          nrow(inv), num(length(msgs))), x = "Registros que contienen la clave", y = NULL)
guardar_fig(p, "anatomia_json", "Anatomía del JSON exportado de Telegram",
            "Presencia de cada clave del JSON en los registros. Solo id, type, date, text y text_entities están en todos; el resto aparece según el tipo de mensaje (respuestas, reacciones, fotos, stickers, avisos del sistema). Las claves anidadas se guardan como texto JSON para no perder información.", h = 9)

# F01-02 Composición de los registros (waffle de 100 cuadros)
tipo_cont <- with(orig, fifelse(type == "service", "Aviso del sistema",
             fifelse(!is.na(poll), "Encuesta",
             fifelse(!is.na(photo), "Foto",
             fifelse(!is.na(media_type) & media_type == "sticker", "Sticker",
             fifelse(!is.na(media_type) & media_type %in% c("video_file", "animation", "video_message"), "Video / GIF",
             fifelse(!is.na(media_type) & media_type %in% c("voice_message", "audio_file"), "Audio",
             fifelse(!is.na(file), "Archivo", "Texto"))))))))
comp <- data.table(tipo = tipo_cont)[, .N, by = tipo][order(-N)]
comp[, cuadros := round(100 * N / sum(N))]
comp[cuadros == 0, cuadros := 1L]
comp[1, cuadros := cuadros + (100 - sum(comp$cuadros))]
waf <- data.table(tipo = rep(comp$tipo, comp$cuadros))[, `:=`(i = 0:99)][, `:=`(x = i %% 20, y = i %/% 20)]
waf[, tipo := factor(tipo, comp$tipo)]
col_tipo <- setNames(c("#F26B1D", "#FFB000", "#2D8CFF", "#9333EA", "#17A34A", "#E11D48", "#0D9488", "#8D6E63")[seq_len(nrow(comp))], comp$tipo)
p <- ggplot(waf, aes(x, -y, fill = tipo)) +
  geom_tile(color = GRAF$fondo, linewidth = 2.2, width = 0.92, height = 0.92) +
  coord_equal() + scale_fill_manual(values = col_tipo, name = NULL,
                                    labels = sprintf("%s  %s (%s)", comp$tipo, num(comp$N), pct(comp$N / sum(comp$N)))) +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(), panel.grid = element_blank(), legend.position = "right") +
  labs(title = "Composición de los registros del chat", x = NULL, y = NULL,
       subtitle = "Cada cuadro representa 1% de los registros exportados")
guardar_fig(p, "composicion_registros", "Composición de los registros del chat",
            "Gráfico de cuadros (waffle) con la proporción de cada tipo de registro. La mayoría son mensajes de texto; los avisos del sistema (personas que entran al grupo, mensajes fijados) no son conversación y se excluyen en la limpieza.", w = 11, h = 3.9)

# F01-03 Registros por mes y tipo
orig_f <- copy(orig)[, mes := as.Date(paste0(substr(fecha_hora_local, 1, 7), "-01"))]
d3 <- orig_f[, .N, by = .(mes, tipo = fifelse(type == "service", "Aviso del sistema", "Mensaje"))]
p <- ggplot(d3, aes(mes, N, fill = tipo)) + geom_col(width = 25) +
  scale_fill_manual(values = c("Aviso del sistema" = "#FFB27A", "Mensaje" = "#F26B1D"), name = NULL) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = expansion(0.01)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Registros por mes según su tipo", x = NULL, y = "Registros por mes",
       subtitle = sprintf("Del %s al %s; los picos coinciden con las inscripciones de verano (dic-ene) e invierno (jun-jul)",
                          format(min(as.Date(orig$fecha_hora_local)), "%d/%m/%Y"), format(max(as.Date(orig$fecha_hora_local)), "%d/%m/%Y")))
guardar_fig(p, "registros_por_mes", "Registros por mes según su tipo",
            "Cantidad mensual de registros extraídos. Los avisos del sistema se concentran en los meses en que entran nuevos estudiantes al grupo; la conversación se dispara en las inscripciones de cada temporada.", h = 6)

# F01-04 Entidades de texto que Telegram separa
ents <- rbindlist(lapply(msgs, function(r) if (length(r$text_entities)) data.table(tipo = vapply(r$text_entities, function(e) e$type %||% "?", "")) else NULL))
d4 <- ents[, .N, by = tipo][order(N)]
d4[, tipo := factor(tipo, tipo)]
p <- ggplot(d4, aes(N, tipo)) +
  geom_segment(aes(x = 1, xend = N, yend = tipo), color = alpha(GRAF$color, .5), linewidth = 1.2) +
  geom_point(size = 4.2, color = GRAF$color) +
  geom_text(aes(label = num(N)), hjust = -0.35, family = FT_MONO, size = 3.3) +
  scale_x_log10(labels = comma, expand = expansion(c(0.02, 0.15))) +
  labs(title = "Entidades que Telegram ya separa dentro del texto",
       subtitle = "Cada mensaje llega partido en fragmentos tipados: texto plano, enlaces, hashtags, teléfonos, menciones...\nEscala logarítmica",
       x = "Fragmentos (escala log)", y = NULL)
guardar_fig(p, "entidades_texto", "Entidades de texto separadas por Telegram",
            "Tipos de fragmento en text_entities. Es una extracción de información ya hecha por Telegram (similar a un NER básico): los enlaces y hashtags luego ayudan a detectar publicidad, y los teléfonos se ocultan por privacidad.", h = 6.4)

# F01-05 Certificado de no pérdida
d5 <- melt(aud[, .(clave, JSON = json, CSV = tabla)], id.vars = "clave", variable.name = "fuente", value.name = "n")
d5[, clave := factor(clave, rev(inv$clave))]
p <- ggplot(d5, aes(n, clave)) +
  geom_point(aes(color = fuente, size = fuente, shape = fuente), stroke = 1.3) +
  scale_color_manual(values = c(JSON = "#F26B1D", CSV = "#1C1A2B"), name = NULL) +
  scale_size_manual(values = c(JSON = 5, CSV = 2), name = NULL) +
  scale_shape_manual(values = c(JSON = 1, CSV = 16), name = NULL) +
  scale_x_log10(labels = comma) +
  labs(title = "Auditoría de no pérdida: JSON frente a CSV",
       subtitle = sprintf("Valores no nulos por clave en el JSON (círculo) y en el CSV (punto). %d de %d claves coinciden y sus valores se reconstruyen idénticos",
                          sum(aud$ok), nrow(aud)), x = "Valores presentes (escala log)", y = NULL)
guardar_fig(p, "auditoria_no_perdida", "Auditoría de no pérdida JSON → CSV",
            "Cada punto negro (CSV) cae dentro de su círculo (JSON): la conversión conserva todos los valores. Además, los campos anidados se volvieron a leer y resultaron idénticos al original.", h = 9)

fase_fin()
rm(msgs); invisible(gc())
