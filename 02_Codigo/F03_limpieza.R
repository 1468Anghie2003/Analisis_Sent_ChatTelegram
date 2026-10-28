# =====================================================================
#  F03 | PREPROCESAMIENTO Y LIMPIEZA
#  Tema de clase: 1. Pasos generales de extracción de texto -> Preprocesamiento
#  (limpieza de texto y normalización).
#  Secuencia aplicada:
#   1. Excluir avisos del sistema (no son conversación).
#   2. Detectar y excluir publicidad / difusión con 5 reglas (Regex).
#   3. Marcar duplicados y contenido no analizable (no se borran).
#   4. Normalizar el texto en 8 pasos (ruido, emojis, jerga, tildes...).
#   5. Derivar variables (tiempo, periodo académico, autor anónimo...).
#  Resultado: 02_dataset_limpio.csv y 04_registros_excluidos.csv
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
fase_inicio("F03")

orig <- cargar_chk("f01_original")
N0 <- nrow(orig)
jerga <- fread(file.path(DIR$dic, "dic_jerga_expresiones.csv"), encoding = "UTF-8")

# ---- 1. Avisos del sistema --------------------------------------------
#> type == "service" son eventos automáticos (alguien entró, se fijó un
#> mensaje, se cambió el nombre del grupo). No expresan sentimientos.
serv <- orig[type == "service"]
dt <- orig[type == "message"]
bit("1. Avisos del sistema", "Se excluyen los registros de tipo service",
    sprintf("Acciones: %s", paste(serv[, .N, by = action][order(-N), sprintf("%s (%s)", action, num(N))], collapse = ", ")),
    antes = N0, despues = nrow(dt), archivo = "01_Dataset/04_registros_excluidos.csv",
    justificacion = "Son eventos automáticos de Telegram, no conversación entre miembros")

# ---- 2. Publicidad y difusión ---------------------------------------------
#> Cinco reglas independientes; basta una para marcar el mensaje:
#>  R1 enlace de invitación (WhatsApp, t.me, formularios, acortadores)
#>  R2 reenviado desde un canal (difusión masiva, no escrito por el miembro)
#>  R3 texto largo idéntico publicado 3 o más veces (copia-pega)
#>  R4 lenguaje de anuncio + formato de anuncio (enlace, hashtag, texto largo o muchos emojis)
#>  R5 formato promocional (2+ hashtags, muchos emojis en texto largo, botones de bot)
dt[, texto := fifelse(is.na(texto_plano), "", texto_plano)]
dt[, tmin := normalizar_base(texto)]
ents <- lapply(dt$text_entities, function(j) if (is.na(j) || j == "[]") character() else vapply(jsonlite::fromJSON(j, simplifyVector = FALSE), function(e) e$type %||% "", ""))
dt[, `:=`(n_links = vapply(ents, function(e) sum(e %in% c("link", "text_link")), 1L),
          n_hashtags = vapply(ents, function(e) sum(e == "hashtag"), 1L))]
em_lista <- stri_extract_all_regex(dt$texto, RX_EMOJI, omit_no_match = TRUE)
dt[, n_emojis := lengths(em_lista)]

RX_INVITA  <- "chat\\.whatsapp\\.com|wa\\.me/|t\\.me/|telegram\\.me/|forms\\.gle|docs\\.google\\.com/forms|bit\\.ly|linktr\\.ee|tinyurl|forms\\.office"
RX_ANUNCIO <- paste0("\\b(unete|unanse|inscribete|inscribanse|inscripciones abiertas|aprovecha|te invitamos|los invitamos|les invitamos|",
                     "invita a|participa|registrate|registro abierto|cupos limitados|promocion|oferta|descuento|clases particulares|",
                     "webinar|gana premios|premios|no te lo pierdas|no te pierdas|gratis|gratuito|certificate|postula|hackathon|",
                     "concurso|torneo|diplomado|seminario|charla)\\b")
dt[, R1 := stri_detect_regex(tmin, RX_INVITA)]
dt[, R2 := !is.na(forwarded_from_id) & stri_startswith_fixed(fifelse(is.na(forwarded_from_id), "", forwarded_from_id), "channel")]
dt[, clave_rep := stri_replace_all_regex(stri_replace_all_regex(tmin, RX_URL, ""), "[^\\p{L}\\p{N}]+", " ")]
dt[, n_rep := .N, by = clave_rep]
dt[, R3 := nchar(clave_rep) > 40 & n_rep >= CFG$min_rep_publicidad]
dt[, R4 := stri_detect_regex(tmin, RX_ANUNCIO) & (n_links > 0 | n_hashtags > 0 | nchar(texto) > 150 | n_emojis >= 4)]
dt[, R5 := n_hashtags >= 2 | (n_emojis >= 6 & nchar(texto) > 200) | !is.na(inline_bot_buttons) | !is.na(via_bot)]
dt[, es_publicidad := R1 | R2 | R3 | R4 | R5]
dt[, motivo := apply(.SD, 1, function(r) paste(c("R1_enlace_invitacion", "R2_reenvio_canal", "R3_texto_repetido",
                                                 "R4_lenguaje_anuncio", "R5_formato_promocional")[as.logical(r)], collapse = "+")),
   .SDcols = c("R1", "R2", "R3", "R4", "R5")]
reglas <- dt[, .(R1 = sum(R1), R2 = sum(R2), R3 = sum(R3), R4 = sum(R4), R5 = sum(R5), total = sum(es_publicidad))]
guardar_tabla(dt[es_publicidad == TRUE, .N, by = motivo][order(-N)], "publicidad_combinaciones")
bit("2. Publicidad", sprintf("%s mensajes marcados como publicidad o difusión", num(reglas$total)),
    sprintf("R1 enlace=%s, R2 canal=%s, R3 repetido=%s, R4 anuncio=%s, R5 formato=%s (un mensaje puede cumplir varias)",
            num(reglas$R1), num(reglas$R2), num(reglas$R3), num(reglas$R4), num(reglas$R5)),
    antes = nrow(dt), despues = nrow(dt) - reglas$total, archivo = "04_Resultados/tablas/F03_publicidad_combinaciones.csv",
    justificacion = "Indicación de clase: excluir publicidad de grupos, cursos y actividades; no expresa el sentir de los miembros")

excl <- rbind(serv[, .(id, fecha_hora_local, tipo = type, motivo = paste0("aviso_sistema:", action), autor = actor, texto = texto_plano)],
              dt[es_publicidad == TRUE, .(id, fecha_hora_local, tipo = type, motivo, autor = from, texto = texto_plano)])
setorder(excl, id)
fwrite(excl, file.path(DIR$dataset, "04_registros_excluidos.csv"), bom = TRUE)
publi <- dt[es_publicidad == TRUE][, fh := as.POSIXct(as.numeric(date_unixtime), origin = "1970-01-01", tz = CFG$zona_horaria)]
dt <- dt[es_publicidad == FALSE]

# ---- 3. Duplicados y contenido no analizable ----------------------------------
#> Duplicado: mismo autor y mismo texto en menos de 5 minutos (envío doble).
#> No analizable: foto, archivo o video sin texto ni emoji. Ambos se
#> CONSERVAN (son actividad real del chat) pero se marcan para no inflar
#> el análisis de texto.
setorder(dt, date_unixtime)
dt[, ux := as.numeric(date_unixtime)]
dt[, duplicado := nchar(clave_rep) > 3 & (ux - shift(ux)) < 300 & clave_rep == shift(clave_rep), by = from_id]
dt[is.na(duplicado), duplicado := FALSE]
bit("3. Duplicados", sprintf("%s envíos dobles marcados (se conservan)", num(sum(dt$duplicado))),
    "Mismo autor y mismo texto en menos de 5 minutos", antes = nrow(dt), despues = nrow(dt),
    justificacion = "Se marcan en lugar de borrar: la indicación es eliminar solo publicidad y lo que no es conversación")

# ---- 4. Normalización del texto (8 pasos) ---------------------------------------
#> 4.1 texto_mostrar: se ocultan correos, teléfonos y @usuarios (privacidad)
#> 4.2 se cuentan y quitan URLs, correos, teléfonos y menciones
#> 4.3 se extraen los emojis a su propia columna (luego tienen diccionario)
#> 4.4 se detectan expresiones y jerga (xd, :v, x2, chuta, nomás...)
#> 4.5 minúsculas, sin tildes y sin alargamientos (normalizar_base)
#> 4.6 la jerga se lleva a su forma estándar (q -> que, xq -> porque...)
#> 4.7 las risas se unifican (jajaja, jsjs, jeje -> jaja; xddd -> xd)
#> 4.8 texto_limpio: solo letras y espacios, listo para tokenizar
dt[, texto_mostrar := enmascarar(texto)]
ruido <- data.table(
  elemento = c("Enlaces (URL)", "Correos", "Teléfonos", "Menciones @"),
  n = c(sum(stri_count_regex(dt$texto, RX_URL)), sum(stri_count_regex(dt$texto, RX_MAIL)),
        sum(stri_count_regex(dt$texto, RX_FONO)), sum(stri_count_regex(dt$texto, RX_MENC))))
t1 <- stri_replace_all_regex(dt$texto, paste(RX_URL, RX_MAIL, RX_FONO, RX_MENC, sep = "|"), " ")
em <- stri_extract_all_regex(t1, RX_EMOJI, omit_no_match = TRUE)
dt[, emojis := vapply(em, function(e) paste(norm_emoji(e), collapse = " "), "")]
dt[, n_emojis := lengths(em)]
dt[!is.na(sticker_emoji), emoji_sticker := norm_emoji(sticker_emoji)]
t2 <- stri_replace_all_regex(t1, RX_EMOJI, " ")
t2 <- stri_replace_all_regex(t2, "[\\x{FE0F}\\x{200D}\\x{1F3FB}-\\x{1F3FF}\\x{20E3}]", "")
ruido <- rbind(ruido, data.table(elemento = "Emojis", n = sum(dt$n_emojis)))

# 4.4 expresiones (sobre el texto original, distinguiendo XD de xd)
expr_hits <- lapply(seq_len(nrow(jerga)), function(i) stri_count_regex(t2, jerga$patron[i]))
expr_mat <- do.call(cbind, expr_hits); colnames(expr_mat) <- jerga$expresion
dt[, expresiones := apply(expr_mat, 1, function(r) paste(jerga$expresion[r > 0], collapse = "|"))]
uso_jerga <- data.table(expresion = jerga$expresion, tipo = jerga$tipo, mensajes = colSums(expr_mat > 0), apariciones = colSums(expr_mat))
guardar_tabla(uso_jerga[order(-mensajes)], "uso_jerga_expresiones")

# 4.5 normalización base
t3 <- normalizar_base(t2)
n_alarg <- sum(stri_count_regex(stri_trans_tolower(t2), "(\\p{L})\\1{2,}"))
ruido <- rbind(ruido, data.table(elemento = "Alargamientos (holaaa)", n = n_alarg))

# 4.6 y 4.7 jerga a forma estándar y risas
norm_tab <- jerga[normaliza_a != ""]
n_norm <- integer(nrow(norm_tab))
for (i in seq_len(nrow(norm_tab))) {
  n_norm[i] <- sum(stri_count_regex(t3, norm_tab$patron[i]))
  t3 <- stri_replace_all_regex(t3, norm_tab$patron[i], norm_tab$normaliza_a[i])
}
t3 <- stri_replace_all_regex(t3, "(?<![\\p{L}\\d])x+d+(?![\\p{L}\\d])", "xd")
normalizaciones <- data.table(expresion = norm_tab$expresion, normaliza_a = norm_tab$normaliza_a, veces = n_norm)[veces > 0][order(-veces)]
guardar_tabla(normalizaciones, "normalizaciones_jerga")
ruido <- rbind(ruido, data.table(elemento = "Jerga y abreviaturas normalizadas", n = sum(n_norm)))
dt[, texto_norm := stri_trim_both(stri_replace_all_regex(t3, "\\s+", " "))]

# 4.8 texto limpio (solo letras)
dt[, texto_limpio := stri_trim_both(stri_replace_all_regex(stri_replace_all_regex(texto_norm, "[^\\p{L}\\s]", " "), "\\s+", " "))]
ruido <- rbind(ruido, data.table(elemento = "Números y signos de puntuación", n = sum(stri_count_regex(dt$texto_norm, "[^\\p{L}\\s]"))))
guardar_tabla(ruido, "ruido_removido")
bit("4. Normalización", "Texto normalizado en 8 pasos",
    sprintf("URLs %s, teléfonos %s, emojis extraídos %s, alargamientos %s, jerga normalizada %s",
            num(ruido[elemento == "Enlaces (URL)", n]), num(ruido[elemento == "Teléfonos", n]), num(sum(dt$n_emojis)),
            num(n_alarg), num(sum(n_norm))),
    antes = nrow(dt), despues = nrow(dt), archivo = "04_Resultados/tablas/F03_ruido_removido.csv",
    justificacion = "Sin normalizar, 'Graciaaas', 'gracias' y 'GRACIAS' serían palabras distintas y el diccionario no las reconocería")

# ---- 5. Variables derivadas ----------------------------------------------------------
#> Se agregan las variables que usan todas las fases siguientes: tiempo
#> (fecha, hora, mes, semana, día, semestre y periodo académico), tipo de
#> contenido, longitud, si el mensaje es analizable (tiene palabras,
#> emojis o sticker), si es pregunta o respuesta y sus reacciones.
#> El autor se reemplaza por un alias ("Miembro 0001" = el más activo).
#> La correspondencia alias -> nombre real se guarda aparte, en
#> 05_Bitacora/privado/, solo para uso interno del equipo y del docente:
#> no entra al dashboard ni se sube a GitHub (.gitignore).
dt[, fh := as.POSIXct(ux, origin = "1970-01-01", tz = CFG$zona_horaria)]
dt[, `:=`(fecha = as.Date(fh, tz = CFG$zona_horaria), hora = as.integer(format(fh, "%H")), minuto = as.integer(format(fh, "%M")))]
dt[, `:=`(anio = as.integer(format(fecha, "%Y")), mes = format(fecha, "%Y-%m"), semana = format(fecha, "%G-S%V"),
          dia_semana = DIAS_ES[as.integer(format(fecha, "%u"))], semestre = semestre(fecha), periodo = periodo_academico(fecha))]
dt[, tipo_contenido := fifelse(!is.na(poll), "encuesta", fifelse(!is.na(photo), "foto",
                       fifelse(!is.na(media_type) & media_type == "sticker", "sticker",
                       fifelse(!is.na(media_type) & media_type %in% c("video_file", "animation", "video_message"), "video",
                       fifelse(!is.na(media_type) & media_type %in% c("voice_message", "audio_file"), "audio",
                       fifelse(!is.na(file), "archivo", "texto"))))))]
dt[, `:=`(n_caracteres = nchar(texto), n_palabras = fifelse(texto_limpio == "", 0L, stri_count_words(texto_limpio)))]
dt[, analizable := n_palabras > 0 & nchar(texto_limpio) >= CFG$min_largo_texto | n_emojis > 0 | !is.na(emoji_sticker)]
dt[, es_pregunta := stri_detect_regex(texto, "[?¿]") | stri_detect_regex(texto_norm, "^(alguien|saben|sabes|como|cuando|donde|que|quien|cual|cuanto|hay|habra|se puede|disculpen)\\b")]
dt[, es_respuesta := !is.na(reply_to_message_id)]
reac <- lapply(dt$reactions, function(j) if (is.na(j)) NULL else jsonlite::fromJSON(j, simplifyVector = FALSE))
dt[, reacciones := vapply(reac, function(r) if (is.null(r)) "" else paste(sprintf("%s:%d", norm_emoji(vapply(r, function(z) z$emoji %||% "?", "")),
                                                                                  vapply(r, function(z) as.integer(z$count), 1L)), collapse = "|"), "")]
dt[, n_reacciones := vapply(reac, function(r) if (is.null(r)) 0L else sum(vapply(r, function(z) as.integer(z$count), 1L)), 1L)]
# autor anónimo: Miembro 0001 = quien más mensajes escribió
rk <- dt[, .N, by = from_id][order(-N)][, alias := sprintf("Miembro %04d", seq_len(.N))]
dt <- merge(dt, rk[, .(from_id, alias)], by = "from_id", all.x = TRUE)
dt[, autor := if (CFG$anonimizar) alias else from]
dir.create(file.path(DIR$bit, "privado"), showWarnings = FALSE)
priv <- dt[, .(nombre_telegram = from[!is.na(from)][.N] %||% NA_character_, mensajes = .N,
               primer_mensaje = min(fecha_hora_local), ultimo_mensaje = max(fecha_hora_local)), by = .(alias, from_id)][order(alias)]
fwrite(priv, file.path(DIR$bit, "privado", "tabla_anonimizacion.csv"), bom = TRUE)
setorder(dt, ux)
bit("5. Variables derivadas", "Se agregan 20 variables de tiempo, contenido e interacción",
    sprintf("%s mensajes analizables de %s; %s preguntas; %s respuestas; %s autores (anonimizados: %s)",
            num(sum(dt$analizable)), num(nrow(dt)), num(sum(dt$es_pregunta)), num(sum(dt$es_respuesta)),
            num(uniqueN(dt$from_id)), CFG$anonimizar),
    antes = nrow(dt), despues = nrow(dt),
    justificacion = "Permiten segmentar por día, semana, mes, semestre o periodo académico y analizar interacción")
bit("5b. Tabla de anonimización", sprintf("Correspondencia de %s alias con su nombre de Telegram", num(nrow(priv))),
    "Uso interno: permite identificar a un miembro si el docente lo solicita; no se publica",
    archivo = "05_Bitacora/privado/tabla_anonimizacion.csv",
    justificacion = "Ética de datos: el análisis y el dashboard solo muestran alias")

# ---- 6. Dataset limpio ---------------------------------------------------------------
#> Se eligen 31 columnas ordenadas (tiempo, autor anonimizado, texto en sus
#> tres versiones: para mostrar, normalizado y limpio, emojis, expresiones,
#> interacción y banderas de control) y se guarda 02_dataset_limpio.csv.
#> Junto a él quedan en un checkpoint los registros excluidos y las tablas
#> de reglas para las figuras y la bitácora.
cols <- c("id", "fecha_hora_local", "fecha", "hora", "minuto", "anio", "mes", "semana", "dia_semana", "semestre", "periodo",
          "autor", "tipo_contenido", "texto_mostrar", "texto_norm", "texto_limpio", "n_caracteres", "n_palabras",
          "emojis", "n_emojis", "emoji_sticker", "expresiones", "es_pregunta", "es_respuesta", "reply_to_message_id",
          "reacciones", "n_reacciones", "editado", "reenviado", "duplicado", "analizable")
dt[, `:=`(editado = !is.na(edited), reenviado = !is.na(forwarded_from) | !is.na(forwarded_from_id))]
limpio <- dt[, ..cols]
setnames(limpio, "reply_to_message_id", "responde_a_id")
fwrite(limpio, file.path(DIR$dataset, "02_dataset_limpio.csv"), bom = TRUE)
guardar_chk(list(limpio = limpio, publi = publi[, .(id, fh, motivo, R1, R2, R3, R4, R5)],
                 serv = serv[, .(id, action)], ruido = ruido, normalizaciones = normalizaciones, uso_jerga = uso_jerga, reglas = reglas,
                 alias = rk), "f03_limpio")
bit("6. Dataset limpio", "Se guardó el dataset limpio", sprintf("%s filas x %d columnas", num(nrow(limpio)), ncol(limpio)),
    antes = N0, despues = nrow(limpio), archivo = "01_Dataset/02_dataset_limpio.csv",
    justificacion = "Es la base de todas las fases siguientes y del dashboard")

# ---- 7. Figuras -------------------------------------------------------------------------
#> Ocho figuras documentan la limpieza: la cascada de registros, cómo se
#> detectó la publicidad, su evolución en el tiempo, ejemplos de
#> normalización, el ruido quitado, la longitud de los mensajes, la jerga
#> normalizada y cuánto contenido queda analizable por año.
# F03-01 Cascada de limpieza
nana <- sum(!limpio$analizable)
cas <- data.table(paso = c("Registros exportados", "Avisos del sistema", "Publicidad y difusión", "Dataset limpio", "  sin texto analizable", "  con texto o emojis"),
                  valor = c(N0, -nrow(serv), -nrow(publi), nrow(limpio), -nana, sum(limpio$analizable)))
cas[, `:=`(fin = c(N0, N0 - nrow(serv), nrow(limpio), nrow(limpio), nrow(limpio) - nana, sum(limpio$analizable)))]
cas[, ini := c(0, N0, N0 - nrow(serv), 0, nrow(limpio), 0)]
cas[, tipo := c("total", "baja", "baja", "total", "baja", "final")]
cas[, x := seq_len(.N)]
p <- ggplot(cas) +
  geom_rect(aes(xmin = x - .38, xmax = x + .38, ymin = pmin(ini, fin), ymax = pmax(ini, fin), fill = tipo), color = NA) +
  geom_text(aes(x, pmax(ini, fin), label = ifelse(valor < 0, paste0("−", num(-valor)), num(valor))), vjust = -0.6, family = FT_MONO, size = 4) +
  scale_fill_manual(values = c(total = "#17A34A", baja = "#F97362", final = "#0B6E36"), guide = "none") +
  scale_x_continuous(breaks = cas$x, labels = stri_trim_both(cas$paso)) + scale_y_continuous(labels = comma, expand = expansion(c(0, .1))) +
  labs(title = "Cascada de limpieza del dataset", x = NULL, y = "Registros",
       subtitle = "Solo se eliminan avisos automáticos y publicidad; los mensajes sin texto se conservan como actividad pero no entran al análisis de texto")
guardar_fig(p, "cascada_limpieza", "Cascada de limpieza del dataset",
            "Gráfico de cascada con cada exclusión. Del total exportado se quitan los avisos del sistema y la publicidad o difusión; dentro del dataset limpio se distingue lo que tiene texto o emojis (analizable) de lo que es solo multimedia.", h = 6.6)

# F03-02 Reglas de publicidad (estilo UpSet)
comb <- publi[, .N, by = .(R1, R2, R3, R4, R5)][order(-N)][1:min(.N, 10)]
comb[, k := seq_len(.N)]
mat <- melt(comb, id.vars = c("k", "N"), variable.name = "regla", value.name = "activa")
lab_r <- c(R1 = "R1 enlace de invitación", R2 = "R2 reenvío de canal", R3 = "R3 texto repetido", R4 = "R4 lenguaje de anuncio", R5 = "R5 formato promocional")
pa <- ggplot(comb, aes(k, N)) + geom_col(fill = GRAF$color, width = .7) + geom_text(aes(label = N), vjust = -.4, family = FT_MONO, size = 3.4) +
  scale_x_continuous(breaks = NULL) + labs(x = NULL, y = "Mensajes", title = NULL) + theme(plot.margin = margin(4, 8, 0, 8))
pb <- ggplot(mat, aes(k, regla)) + geom_point(aes(color = activa), size = 5) +
  geom_line(data = mat[activa == TRUE], aes(group = k), color = GRAF$color, linewidth = 1.2) +
  scale_color_manual(values = c(`TRUE` = GRAF$color, `FALSE` = alpha(TINTA3, .25)), guide = "none") +
  scale_y_discrete(labels = lab_r, limits = rev) + scale_x_continuous(breaks = NULL) + labs(x = "Combinación de reglas", y = NULL) +
  theme(plot.margin = margin(0, 8, 4, 8))
p <- (pa / pb) + plot_layout(heights = c(1.4, 1)) +
  plot_annotation(title = "Reglas de detección de publicidad",
                  subtitle = sprintf("%s mensajes excluidos. Cada columna es una combinación de reglas; la mayoría cae por enlaces de invitación a grupos de WhatsApp", num(nrow(publi))))
guardar_fig(p, "reglas_publicidad", "Reglas de detección de publicidad (combinaciones)",
            "Diagrama estilo UpSet: arriba, cuántos mensajes cumplen cada combinación de reglas; abajo, qué reglas forman la combinación. Los enlaces de invitación a grupos son la señal principal; el texto repetido y el lenguaje de anuncio capturan las campañas de eventos.", h = 7.6)

# F03-03 Publicidad en el tiempo
pm <- rbind(publi[, .(mes = as.Date(format(fh, "%Y-%m-01")), tipo = "Publicidad")],
            limpio[, .(mes = as.Date(paste0(mes, "-01")), tipo = "Conversación")])[, .N, by = .(mes, tipo)]
pm2 <- dcast(pm, mes ~ tipo, value.var = "N", fill = 0)[, prop := Publicidad / (Publicidad + `Conversación`)]
p <- ggplot(pm2, aes(mes)) +
  geom_col(aes(y = Publicidad), fill = "#F97362", width = 25) +
  geom_line(aes(y = prop * max(Publicidad) / max(prop)), color = "#0B6E36", linewidth = 1.2) +
  geom_point(aes(y = prop * max(Publicidad) / max(prop)), color = "#0B6E36", size = 2) +
  scale_y_continuous(name = "Mensajes de publicidad", sec.axis = sec_axis(~ . * max(pm2$prop) / max(pm2$Publicidad), name = "Proporción del mes", labels = percent)) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes) +
  labs(title = "Publicidad por mes y su proporción", x = NULL,
       subtitle = "Barras: mensajes de publicidad por mes. Línea: qué parte de todo lo publicado ese mes era publicidad")
guardar_fig(p, "publicidad_tiempo", "Publicidad por mes y su proporción",
            "La publicidad (grupos de WhatsApp por materia, eventos, cursos) aparece en las mismas semanas de inscripción; en algunos meses llega a ser una parte importante de lo publicado, por eso es imprescindible quitarla antes de medir sentimientos.", h = 6.2)

# F03-04 Antes y después: ejemplos
set.seed(CFG$semilla)
ej <- dt[n_emojis > 0 & n_caracteres > 25 & n_caracteres < 85 & stri_detect_regex(expresiones, "\\w")][sample(.N, min(.N, 3))]
ej <- rbind(ej, dt[n_caracteres > 25 & n_caracteres < 85 & stri_detect_regex(texto, "(\\p{L})\\1{2,}")][sample(.N, min(.N, 2))],
            dt[n_caracteres > 20 & n_caracteres < 80 & stri_detect_regex(texto, "\\b[qQ]\\b|\\bxq\\b")][sample(.N, min(.N, 2))])
tab <- ej[, .(Original = texto_mostrar, Normalizado = texto_norm, `Limpio (tokens)` = texto_limpio, Emojis = emojis)]
tab <- melt(tab[, fila := .I], id.vars = "fila", variable.name = "col", value.name = "txt")
tab[, txt := stri_replace_all_regex(txt, RX_EMOJI, "")]
tab[, txt := vapply(txt, function(z) paste(stri_wrap(z, 34), collapse = "\n"), "")]
tab[col == "Emojis", txt := vapply(ej$emojis[fila], function(e) if (e == "") "-" else paste(unique(strsplit(e, " ")[[1]]), collapse = " "), "")]
tab[, x := as.integer(col)]
p <- ggplot(tab, aes(x, -fila)) +
  geom_tile(aes(fill = col), width = .97, height = .92, color = NA) +
  geom_text(aes(label = ifelse(col == "Emojis", "", txt)), family = FT_TXT, size = 3.05, lineheight = .95, color = TINTA) +
  scale_fill_manual(values = c("#FFE1D6", "#E3F6E9", "#C9EFD6", "#FFF4C2"), guide = "none") +
  scale_x_continuous(breaks = 1:4, labels = c("Original (con datos ocultos)", "Normalizado", "Limpio (solo letras)", "Emojis extraídos"), position = "top") +
  theme(panel.grid = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(family = FT_TIT, face = "bold.italic", size = 13, color = TINTA)) +
  labs(title = "Antes y después de la normalización", x = NULL, y = NULL,
       subtitle = "Ejemplos reales: minúsculas, sin tildes ni alargamientos, jerga estándar (q → que) y emojis en su propia columna")
p <- agregar_emojis(p, unlist(lapply(seq_len(nrow(ej)), function(i) head(unique(strsplit(ej$emojis[i], " ")[[1]]), 3))),
                    x = unlist(lapply(seq_len(nrow(ej)), function(i) { k <- length(head(unique(strsplit(ej$emojis[i], " ")[[1]]), 3)); if (k == 0 || ej$emojis[i] == "") numeric() else 4 + (seq_len(k) - (k + 1) / 2) * .22 })),
                    y = unlist(lapply(seq_len(nrow(ej)), function(i) { k <- length(head(unique(strsplit(ej$emojis[i], " ")[[1]]), 3)); if (ej$emojis[i] == "") numeric() else rep(-i, k) })),
                    w = .17, h = .55)
guardar_fig(p, "antes_despues_normalizacion", "Ejemplos de normalización del texto",
            "Mensajes reales en cada etapa: el original con teléfonos y correos ocultos, el texto normalizado (minúsculas, sin tildes ni letras repetidas, jerga estándar), el texto limpio que se tokeniza y los emojis extraídos, que se analizan con su propio diccionario.", w = 13, h = 8.4)

# F03-05 Ruido removido
r5 <- copy(ruido)[order(n)][, elemento := factor(elemento, elemento)]
p <- ggplot(r5, aes(n, elemento)) +
  geom_segment(aes(x = 0, xend = n, yend = elemento), color = alpha(GRAF$color, .45), linewidth = 2) +
  geom_point(color = GRAF$color, size = 5) + geom_text(aes(label = num(n)), hjust = -.45, family = FT_MONO, size = 3.6) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, .15))) +
  labs(title = "Elementos de ruido removidos o transformados", x = "Apariciones en el dataset limpio", y = NULL,
       subtitle = "Elementos que no aportan al análisis de palabras (o que se analizan aparte, como los emojis)")
guardar_fig(p, "ruido_removido", "Elementos de ruido removidos o transformados",
            "Cantidad de enlaces, correos, teléfonos, menciones, emojis, alargamientos, jerga y signos tratados en la normalización. Los emojis no se pierden: pasan a una columna propia con diccionario.", h = 5.8)

# F03-06 Longitud antes y después
lg <- rbind(data.table(etapa = "Antes (palabras del texto original)", n = stri_count_words(dt$texto)),
            data.table(etapa = "Después (palabras del texto limpio)", n = dt$n_palabras))[n > 0]
p <- ggplot(lg, aes(n, fill = etapa)) +
  geom_density(alpha = .55, color = NA, adjust = 1.4) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 300)) +
  scale_fill_manual(values = c("#F97362", "#17A34A"), name = NULL) +
  labs(title = "Longitud de los mensajes antes y después de limpiar", x = "Palabras por mensaje (escala log)", y = "Densidad",
       subtitle = sprintf("Mediana: %d palabras antes y %d después de limpiar; la mayoría de mensajes son preguntas o respuestas breves",
                          median(stri_count_words(dt$texto)[stri_count_words(dt$texto) > 0]), median(dt$n_palabras[dt$n_palabras > 0])))
guardar_fig(p, "longitud_antes_despues", "Longitud de los mensajes antes y después de limpiar",
            "Distribución de palabras por mensaje antes y después de la limpieza. Los mensajes son muy cortos, lo que justifica usar diccionarios (que funcionan con pocas palabras) y agrupar mensajes por día para modelar tópicos.", h = 5.8)

# F03-07 Jerga normalizada
nz <- normalizaciones[1:min(.N, 14)][order(veces)][, etiqueta := factor(sprintf("%s  →  %s", expresion, normaliza_a), sprintf("%s  →  %s", expresion, normaliza_a))]
p <- ggplot(nz, aes(veces, etiqueta)) + geom_col(fill = "#7BD39A", width = .65) +
  geom_text(aes(label = num(veces)), hjust = -.2, family = FT_MONO, size = 3.5) +
  scale_x_continuous(expand = expansion(c(0, .12))) +
  theme(axis.text.y = element_text(family = FT_MONO, size = 11, color = TINTA)) +
  labs(title = "Jerga y abreviaturas llevadas a su forma estándar", x = "Reemplazos realizados", y = NULL,
       subtitle = "Diccionario de jerga del proyecto (03_Diccionarios/dic_jerga_expresiones.csv)")
guardar_fig(p, "jerga_normalizada", "Normalización de jerga y abreviaturas",
            "Reemplazos de jerga juvenil y abreviaturas por su forma estándar (q → que, xq → porque, porfa → por favor, risas → jaja). Así un mismo significado se cuenta como una sola palabra.", h = 6.4)

# F03-08 Contenido analizable por año
ca <- limpio[, .(Analizable = sum(analizable & n_palabras > 0), `Solo emoji o sticker` = sum(analizable & n_palabras == 0),
                 `Solo multimedia (sin texto)` = sum(!analizable)), by = anio]
ca <- melt(ca, id.vars = "anio")[, p := value / sum(value), by = anio]
p <- ggplot(ca, aes(factor(anio), p, fill = variable)) + geom_col(width = .62) +
  geom_text(aes(label = ifelse(p > .03, pct(p, 0), "")), position = position_stack(vjust = .5), color = "white", family = FT_TXT, fontface = "bold") +
  scale_fill_manual(values = c("#17A34A", "#7BD39A", "#B9C7BE"), name = NULL) + scale_y_continuous(labels = percent) +
  labs(title = "Contenido analizable por año", x = NULL, y = "Mensajes del año",
       subtitle = "Proporción de mensajes con texto analizable, solo emoji/sticker o solo multimedia (fotos sin descripción, archivos)")
guardar_fig(p, "contenido_analizable_anio", "Contenido analizable por año",
            "Proporción de mensajes del dataset limpio con texto analizable, con solo emojis o stickers, o solo multimedia. Casi todo el chat es texto, lo que favorece el análisis léxico.", h = 6)

fase_fin()
