# =====================================================================
#  F11 | CIERRE: HALLAZGOS, ÍNDICES Y BITÁCORA EN EXCEL
#  - Redacta automáticamente los hallazgos con las cifras de cada fase
#    (si cambia el dataset, cambian los textos).
#  - Genera el Excel de bitácora con diseño: portada, resumen por fase,
#    bitácora detallada, corridas, datasets, diccionario de columnas,
#    índice de figuras y archivos generados.
#  - Dibuja el diagrama del pipeline y el tablero de hallazgos.
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
fase_inicio("F11")

L <- cargar_chk("f03_limpio"); F6 <- cargar_chk("f06_dic"); F7 <- cargar_chk("f07_sent"); F8 <- cargar_chk("f08_lda")
F9 <- cargar_chk("f09_ent"); F10 <- cargar_chk("f10_pat"); F5 <- cargar_chk("f05_stop")
ds <- F7$ds; cs <- ds[sentimiento %in% SENT_ORDEN]
dist <- cs[, .N, by = sentimiento][, p := N / sum(N)][order(-N)]
pos <- mean(cs$polaridad == "positivo"); neg <- mean(cs$polaridad == "negativo")
emo <- F6$DE[sentimiento %in% SENT_ORDEN & usos_texto > 0][order(-usos_texto)][1]
rea <- F6$DE[order(-usos_reaccion)][1]
ris <- ds[analizable == TRUE & puntaje_neg > puntaje_pos, mean(stri_detect_regex(texto_norm, "\\b(jaja\\w*|xd)\\b") | stri_detect_regex(emojis, "😂|🤣|💀|😅|🥲"))]
top_m <- F7$ds[, .N, by = autor][order(-N)][1]
x2 <- L$uso_jerga[expresion == "x2", mensajes]
pico <- F10$picos[order(-N)][1]
tsent <- merge(F8$msg_top[sentimiento %in% c("frustracion", "enojo")], F8$etq[, .(topico = topic, etiqueta)], by = "topico")[, .N, by = etiqueta][order(-N)][1]
doc_top <- F9$doc[menciones >= CFG$min_menciones_docente][order(-indice)][1]
lug <- F9$men[tipo == "lugar", .(n = .N, neg = mean(polaridad == "negativo")), by = entidad][n >= 20][order(-neg)][1]

# ---- 1. Hallazgos ------------------------------------------------------
#> Cada hallazgo es una frase con huecos que se completa con las cifras
#> calculadas en las fases anteriores (checkpoints). Si se cambia el
#> dataset, los valores y los textos se actualizan solos.
H <- data.table(categoria = character(), titulo = character(), texto = character(), valor = character())
add <- function(cat, tit, txt, val) H <<- rbind(H, data.table(categoria = cat, titulo = tit, texto = txt, valor = val))
na <- ds[, .N, by = autor][order(N)][, pc := cumsum(N) / sum(N)]
gini <- 1 - 2 * sum((na$pc - na$N / sum(na$N) / 2) / nrow(na))
top10 <- na[order(-N)][seq_len(ceiling(.1 * .N)), sum(N)] / sum(na$N)
add("Datos", "Volumen del dataset analizado", sprintf("Se analizaron %s registros exportados del %s al %s; tras excluir %s avisos del sistema y %s mensajes de publicidad quedaron %s mensajes de %s miembros.",
    num(nrow(cargar_chk("f01_original"))), format(min(ds$fecha), "%d/%m/%Y"), format(max(ds$fecha), "%d/%m/%Y"), num(nrow(L$serv)), num(nrow(L$publi)), num(nrow(ds)), num(uniqueN(ds$autor))), num(nrow(ds)))
add("Sentimientos", "Sentimiento predominante", sprintf("El sentimiento predominante es %s (%s de los mensajes con sentimiento), seguido de %s (%s) y %s (%s).",
    tolower(SENT_LAB[dist$sentimiento[1]]), pct(dist$p[1]), tolower(SENT_LAB[dist$sentimiento[2]]), pct(dist$p[2]), tolower(SENT_LAB[dist$sentimiento[3]]), pct(dist$p[3])), pct(dist$p[1]))
add("Sentimientos", "Balance de polaridad", sprintf("Entre los mensajes con sentimiento, %s tienen polaridad positiva y %s negativa (la confusión se clasifica como negativa); la polaridad negativa se concentra en confusión y frustración.", pct(pos), pct(neg)),
    sprintf("%s / %s", pct(pos, 0), pct(neg, 0)))
add("Sentimientos", "Presencia de risa en mensajes negativos", sprintf("El %s de los mensajes con polaridad negativa incluye marcadores de risa (jaja, xd o emojis de risa), lo que indica que parte de las quejas se expresa en tono humorístico.", pct(ris)), pct(ris))
add("Emojis", "Emoji más frecuente", sprintf("En el texto, el emoji con sentimiento más frecuente es %s (%s; %s usos). Como reacción, el más usado es %s, con %s usos.",
    emo$emoji, emo$significado_chat, num(emo$usos_texto), rea$emoji, num(rea$usos_reaccion)), emo$emoji)
add("Lenguaje", "Expresión de adhesión 'x2'", sprintf("La expresión 'x2' (equivalente a 'yo también') aparece en %s mensajes y se usa para sumarse a una consulta o a un pedido.", num(x2)), num(x2))
add("Comunidad", "Concentración de la participación", sprintf("El 10%% de miembros más activos produce el %s de los mensajes (índice de Gini %.2f). El miembro más activo (%s) escribió %s mensajes.",
    pct(top10), gini, top_m$autor, num(top_m$N)), sprintf("Gini %.2f", gini))
add("Comunidad", "Tasa de respuesta a preguntas", sprintf("El %s de las preguntas recibe al menos una respuesta explícita; la mediana del tiempo hasta la primera respuesta es de %s minutos.",
    pct(mean(F10$pq$respondida)), num(median(F10$pq$minutos, na.rm = TRUE))), pct(mean(F10$pq$respondida)))
add("Comunidad", "Hora con mayor tasa de respuesta", sprintf("Las preguntas publicadas a las %02d h tienen la mayor tasa de respuesta (%s).", F10$mejor$hora, pct(F10$mejor$tasa)), sprintf("%02d h", F10$mejor$hora))
add("Temas", "Tópicos identificados (LDA)", sprintf("El modelo LDA identificó %d tópicos: %s. El tópico con mayor proporción de frustración y enojo es %s.", F8$K, paste(F8$etq$nombre, collapse = ", "), tsent$etiqueta), as.character(F8$K))
add("Temas", "Día de mayor actividad", sprintf("El %s se registraron %s mensajes, el máximo del periodo; las palabras distintivas de ese día fueron: %s.", format(pico$fecha, "%d/%m/%Y"), num(pico$N), pico$palabras), num(pico$N))
add("Entidades", "Docente con mejor percepción", sprintf("Entre los docentes con %d o más menciones, %s presenta el índice de percepción más alto (%s; %d menciones). Se trata de la percepción expresada en el chat, no de una evaluación docente.",
    CFG$min_menciones_docente, doc_top$entidad, pct(doc_top$indice), doc_top$menciones), doc_top$entidad)
if (nrow(lug)) add("Entidades", "Lugar con mayor carga negativa", sprintf("%s es el lugar con mayor proporción de menciones negativas: %s de sus %d menciones.", lug$entidad, pct(lug$neg), lug$n), lug$entidad)
add("Causas", "Factores asociados a la incertidumbre", sprintf("Las palabras más asociadas a los mensajes de confusión y preocupación son: %s.", paste(F10$causas$palabra[1:6], collapse = ", ")), F10$causas$palabra[1])
add("Predicción", "Pronóstico del próximo pico de actividad", sprintf("El modelo %s pronostica el próximo pico de actividad para la semana del %s (aproximadamente %s mensajes).",
    F10$mejor_m, format(F10$pron[which.max(media), s], "%d/%m/%Y"), num(max(F10$pron$media))), format(F10$pron[which.max(media), s], "%d/%m/%Y"))
add("Validación", "Validación del diccionario", sprintf("Las palabras y los emojis coinciden en polaridad en el %s de los mensajes (kappa = %.2f); el diccionario de emojis coincide en %s con el Emoji Sentiment Ranking, y un clasificador entrenado sin palabras del diccionario alcanza F1 = %.2f.",
    pct(F7$po), F7$kappa, pct(mean(F6$val$polaridad == F6$val$esr_pol | F6$val$esr_pol == "neutral")), max(F10$res_cv$f1)), sprintf("κ %.2f", F7$kappa))
H[, id := seq_len(.N)]
fwrite(H, file.path(DIR$res, "hallazgos.csv"), bom = TRUE)
bit("1. Hallazgos", sprintf("%d hallazgos redactados automáticamente", nrow(H)), paste(H$titulo, collapse = " | "), archivo = "04_Resultados/hallazgos.csv",
    justificacion = "Los textos se generan con las cifras reales: si se cambia el dataset, los hallazgos se actualizan")

# ---- 2. Figuras ----------------------------------------------------------
#> Tres figuras de cierre: el diagrama del pipeline, los registros de
#> bitácora y figuras por fase, y el tablero de hallazgos.
# F11-01 Diagrama del pipeline
fz <- as.data.table(FASES)[, `:=`(i = seq_len(.N))][, `:=`(x = ((i - 1) %% 4) * 3.2, y = -((i - 1) %/% 4) * 2.3)]
fz[, etiqueta := vapply(titulo, function(t) paste(stri_wrap(t, 24), collapse = "\n"), "")]
datos <- data.table(x = c(-2.8, 3.2 * 3 + 2.8, 3.2 * 3 + 2.8), y = c(0, -2.3, -4.6),
                    t = c("ChatTelegramJSON.json\n(dataset original)", "02_dataset_limpio.csv\n03_dataset_sentimientos.csv", "docs/ (dashboard)\nbitácora + figuras"))
flechas <- fz[, .(x0 = x, y0 = y, x1 = shift(x, -1), y1 = shift(y, -1))][!is.na(x1)]
p <- ggplot() +
  geom_curve(data = flechas[y0 == y1], aes(x = x0 + 1.25, y = y0, xend = x1 - 1.25, yend = y1), curvature = 0, color = TINTA3, linewidth = .8, arrow = arrow(length = unit(.18, "cm"))) +
  geom_path(data = rbindlist(lapply(which(flechas$y0 != flechas$y1), function(i) { f <- flechas[i]; m <- (f$y0 + f$y1) / 2
    data.table(g = i, x = c(f$x0, f$x0, f$x1, f$x1), y = c(f$y0 - .62, m, m, f$y1 + .62)) })), aes(x, y, group = g), color = TINTA3, linewidth = .8,
    arrow = arrow(length = unit(.18, "cm"))) +
  geom_tile(data = fz, aes(x, y, fill = I(fondo), color = I(color)), width = 2.5, height = 1.24, linewidth = 1.4) +
  geom_text(data = fz, aes(x, y + .36, label = cod, color = I(color)), family = FT_MONO, size = 4.2, fontface = "bold") +
  geom_text(data = fz, aes(x, y - .12, label = etiqueta), family = FT_TIT, fontface = "bold.italic", size = 3.6, color = TINTA, lineheight = .9) +
  geom_label_xd(data = datos, mapping = aes(x, y, label = t), family = FT_MONO, size = 3, fill = "white", label.size = .4, color = TINTA) +
  coord_cartesian(xlim = c(-4.5, 14.5)) + theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Diagrama del pipeline del proyecto", x = NULL, y = NULL,
       subtitle = "Cada fase es un script de R con su propio color; las salidas de una fase son la entrada de la siguiente (checkpoints)")
guardar_fig(p, "pipeline", "Diagrama del pipeline del proyecto",
            "Las 11 fases del proyecto en orden, del JSON original al dashboard. Cada caja es un script de 02_Codigo con su color; los datasets intermedios y los checkpoints permiten volver a correr cualquier fase por separado.", w = 14, h = 7.6)

# F11-02 Bitácora por fase
b <- as.data.table(BITACORA)[!startsWith(paso, "Figura "), ][, cod := substr(fase, 1, 3)]
bf <- b[, .N, by = cod]
figs <- as.data.table(IDX_FIG)[, .(figuras = .N), by = .(cod = fase)]
bf <- merge(merge(FASES[, c("cod", "titulo", "color")], bf, by = "cod", all.x = TRUE), figs, by = "cod", all.x = TRUE)
setDT(bf); bf[is.na(N), N := 0L]; bf[is.na(figuras), figuras := 0L]
bf[cod == "F11", figuras := figuras + 1L]
bfl <- melt(bf, id.vars = c("cod", "titulo", "color"), measure.vars = c("N", "figuras"))
bfl[, variable := factor(variable, c("N", "figuras"), c("Registros en la bitácora", "Figuras"))]
p <- ggplot(bfl, aes(value, factor(cod, rev(FASES$cod)), fill = I(color))) + geom_col(width = .7) +
  geom_text(aes(label = value), hjust = -.3, family = FT_MONO, size = 3.4) + facet_wrap(~variable, scales = "free_x") +
  scale_y_discrete(labels = setNames(paste(FASES$cod, stri_sub(FASES$titulo, 1, 34)), FASES$cod)) + scale_x_continuous(expand = expansion(c(0, .2))) +
  labs(title = "Registros de bitácora y figuras por fase", x = NULL, y = NULL, subtitle = "Pasos documentados y figuras generadas por cada fase del pipeline")
guardar_fig(p, "bitacora_por_fase", "Registros de bitácora y figuras por fase",
            "Cantidad de pasos registrados en la bitácora y de figuras generadas en cada fase. Toda decisión del proyecto quedó documentada con su justificación en 05_Bitacora.", w = 12.5, h = 6.4)

# F11-03 Tablero de hallazgos
hh <- H[1:min(.N, 16)][, `:=`(k = seq_len(.N) - 1)][, `:=`(x = k %% 4, y = -(k %/% 4))]
colh <- c(Datos = "#475569", Sentimientos = "#9333EA", Emojis = "#DB2777", Lenguaje = "#0D9488", Comunidad = "#65A30D", Temas = "#E11D48",
          Entidades = "#F26B1D", Causas = "#0284C7", Predicción = "#4F46E5", Validación = "#E0A100")
hh[, col := colh[categoria]]
hh[, txt := vapply(stri_replace_all_regex(texto, RX_EMOJI, ""), function(t) paste(stri_wrap(t, 42), collapse = "\n"), "")]
hh[, val := stri_replace_all_regex(valor, RX_EMOJI, "")]
p <- ggplot(hh) + geom_tile(aes(x, y, fill = I(alpha(col, .1)), color = I(col)), width = .95, height = .92, linewidth = 1.1) +
  geom_text(aes(x - .43, y + .37, label = toupper(categoria), color = I(col)), hjust = 0, family = FT_MONO, size = 2.8) +
  geom_text(aes(x - .43, y + .23, label = val), hjust = 0, family = FT_TIT, fontface = "bold.italic", size = 6.2, color = TINTA) +
  geom_text(aes(x - .43, y + .09, label = titulo), hjust = 0, family = FT_TXT, fontface = "bold", size = 3.4, color = TINTA) +
  geom_text(aes(x - .43, y + .01, label = txt), hjust = 0, vjust = 1, family = FT_TXT, size = 2.65, color = TINTA2, lineheight = .9) +
  theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(x = NULL, y = NULL, subtitle = sprintf("%d hallazgos redactados automáticamente con las cifras de cada fase", nrow(hh)))
emo_h <- hh[valor != val & nchar(stri_trim_both(val)) == 0]
if (nrow(emo_h)) p <- agregar_emojis(p, emo_h$valor, x = emo_h$x - .37, y = emo_h$y + .23, w = .11, h = .17)
guardar_fig(p, "tablero_hallazgos", "Tablero de hallazgos principales",
            "Resumen visual de los hallazgos más relevantes, uno por tarjeta, con la cifra clave de cada uno. Todos los textos se generan desde los resultados de las fases anteriores.", w = 16, h = 2.7 * ceiling(nrow(hh) / 4) + 1.4)

# ---- 3. Excel de bitácora --------------------------------------------------
#> Bitacora_GrupoXDMSent.xlsx reúne en hojas con formato: portada, resumen
#> por fase (con el tema de clase aplicado), bitácora detallada coloreada
#> por fase, corridas, datasets, diccionario de columnas, índice de figuras,
#> hallazgos y lista de archivos generados.
desc_cols <- c(id = "Identificador del mensaje en Telegram", fecha_hora_local = "Fecha y hora en Bolivia (UTC-4)", fecha = "Fecha (AAAA-MM-DD)",
  hora = "Hora del día (0-23)", minuto = "Minuto", anio = "Año", mes = "Año-mes", semana = "Semana ISO (AAAA-Snn)", dia_semana = "Día de la semana",
  semestre = "Semestre calendario (I: ene-jun, II: jul-dic)", periodo = "Periodo académico (Verano, Semestre I, Invierno, Semestre II + año)",
  autor = "Autor anonimizado (Miembro 0001 = el más activo)", tipo_contenido = "texto, foto, sticker, video, audio, archivo o encuesta",
  texto_mostrar = "Texto original con correos, teléfonos y @menciones ocultos", texto_norm = "Texto normalizado (minúsculas, sin tildes, jerga estándar)",
  texto_limpio = "Solo letras: listo para tokenizar", n_caracteres = "Caracteres del texto original", n_palabras = "Palabras del texto limpio",
  emojis = "Emojis del mensaje (separados por espacio)", n_emojis = "Cantidad de emojis", emoji_sticker = "Emoji asociado al sticker",
  expresiones = "Expresiones de jerga detectadas (xd, :v, x2...)", es_pregunta = "TRUE si es una pregunta", es_respuesta = "TRUE si responde a otro mensaje",
  responde_a_id = "id del mensaje al que responde", reacciones = "Reacciones recibidas (emoji:cantidad|...)", n_reacciones = "Total de reacciones",
  editado = "TRUE si se editó", reenviado = "TRUE si es reenviado", duplicado = "TRUE si es un envío doble", analizable = "TRUE si tiene texto o emojis analizables",
  sentimiento = "Sentimiento dominante (uno de los 15, sin_sentimiento o no_analizable)", puntaje_pos = "Suma de puntajes positivos",
  puntaje_neg = "Suma de puntajes negativos", puntaje = "puntaje_pos - puntaje_neg", polaridad = "positivo, negativo o neutral",
  intensidad = "Suma de todos los puntajes", ironia = "TRUE si se detectó ironía", n_sentimientos = "Cuántos sentimientos distintos tiene el mensaje")
for (s in SENT_ORDEN) desc_cols[paste0("s_", s)] <- paste("Puntaje de", tolower(SENT_LAB[s]))
dicc <- data.table(columna = names(ds), descripcion = ifelse(names(ds) %in% names(desc_cols), desc_cols[names(ds)], ""),
                   en_dataset_limpio = names(ds) %in% names(L$limpio), ejemplo = vapply(names(ds), function(k) { v <- ds[[k]][!is.na(ds[[k]]) & as.character(ds[[k]]) != ""][1]; stri_sub(as.character(v), 1, 60) }, ""))
datasets <- data.table(
  dataset = c("ChatTelegramJSON.json", "01_dataset_original.csv", "02_dataset_limpio.csv", "03_dataset_sentimientos.csv", "04_registros_excluidos.csv"),
  estado = c("Original (sucio)", "Original (sucio) en tabla", "Limpio", "Limpio + sentimientos", "Excluidos"),
  filas = c(nrow(cargar_chk("f01_original")), nrow(cargar_chk("f01_original")), nrow(L$limpio), nrow(ds), nrow(L$serv) + nrow(L$publi)),
  columnas = c(NA, ncol(cargar_chk("f01_original")), ncol(L$limpio), ncol(ds), 6),
  descripcion = c("Export de Telegram Desktop en formato JSON legible por máquina; no se modifica.",
                  "El JSON aplanado sin pérdida: 1 fila por registro, todas las claves como columnas, los campos anidados como texto JSON (F01).",
                  "Sin avisos del sistema ni publicidad; texto normalizado, emojis y jerga extraídos, variables de tiempo y autor anonimizado (F03).",
                  "El dataset limpio con el sentimiento dominante, la polaridad, la ironía y los 15 puntajes (F07).",
                  "Registros quitados en la limpieza con el motivo (aviso del sistema o regla de publicidad que cumplió) (F03)."))
corr <- { f <- file.path(DIR$bit, "corridas.csv"); if (file.exists(f)) fread(f, encoding = "UTF-8") else data.table(nota = "Ejecutar EJECUTAR_TODO.R para registrar corridas") }
arch <- data.table(archivo = list.files(RAIZ, recursive = TRUE))[!stri_detect_regex(archivo, "^(04_Resultados/checkpoints|03_Diccionarios/emoji_fluent3d|docs/emoji|docs/img|\\.git)")]
arch[, `:=`(carpeta = dirname(archivo), kb = round(file.size(file.path(RAIZ, archivo)) / 1024, 1))]
TEMA_CLASE <- c(F01 = "1. Introducción a la extracción de texto", F02 = "Exploración (EDA, base de la minería de datos)",
                F03 = "1. Pasos generales: preprocesamiento (limpieza y normalización)", F04 = "2. Tokenización; 3. Bigramas, caracteres, Ej5, Ej6",
                F05 = "5. Palabras vacías, 5.1 Shallow linguistics, 6, 7, 9. TF-IDF", F06 = "4. Ej9 sentimientos con léxico; 15 sentimientos de clase",
                F07 = "1. Análisis de sentimientos; 4. Ej9 gráfica de sentimientos", F08 = "5.2 Topic modeling, 8. LDA, 10. Tópicos con tm",
                F09 = "1. NER y extracción de relaciones; 4. Ej8 lugares", F10 = "7. Chatbot (insumos) + métodos externos", F11 = "Cierre y documentación")
resumen <- merge(FASES[, c("cod", "titulo")], bf[, .(cod, registros = N, figuras)], by = "cod")
setDT(resumen); resumen[, tema_clase := TEMA_CLASE[cod]][, script := paste0("02_Codigo/", cod, "_", FASES$nombre[match(cod, FASES$cod)], ".R")]

wb <- openxlsx::createWorkbook(creator = "GrupoXDMSent")
openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 10)
st_t <- openxlsx::createStyle(fontSize = 22, textDecoration = "bold", fontColour = "#1C1A2B")
st_s <- openxlsx::createStyle(fontSize = 11, fontColour = "#4A4860", textDecoration = "italic")
tabla <- function(nombre, df, titulo, nota, color, anchos) {
  openxlsx::addWorksheet(wb, nombre, tabColour = color, gridLines = FALSE)
  openxlsx::writeData(wb, nombre, titulo, 1, 1); openxlsx::addStyle(wb, nombre, st_t, 1, 1)
  openxlsx::writeData(wb, nombre, nota, 1, 2); openxlsx::addStyle(wb, nombre, st_s, 2, 1)
  openxlsx::writeData(wb, nombre, as.data.frame(df), startRow = 4, headerStyle = openxlsx::createStyle(fgFill = color, fontColour = "#FFFFFF", textDecoration = "bold",
                      halign = "center", valign = "center", wrapText = TRUE, border = "Bottom", borderColour = "#FFFFFF"))
  openxlsx::addStyle(wb, nombre, openxlsx::createStyle(wrapText = TRUE, valign = "top", border = "Bottom", borderColour = "#E5E7EB"),
                     rows = 5:(nrow(df) + 4), cols = seq_len(ncol(df)), gridExpand = TRUE)
  openxlsx::addFilter(wb, nombre, rows = 4, cols = seq_len(ncol(df)))
  openxlsx::setColWidths(wb, nombre, seq_len(ncol(df)), anchos); openxlsx::freezePane(wb, nombre, firstActiveRow = 5)
  openxlsx::setRowHeights(wb, nombre, 4, 30)
}
# Portada
openxlsx::addWorksheet(wb, "Portada", tabColour = "#1C1A2B", gridLines = FALSE)
eq <- fread(file.path(DIR$codigo, "equipo.csv"), encoding = "UTF-8")
port <- c(CFG$proyecto, CFG$titulo, CFG$subtitulo, CFG$materia, "", "Integrantes:", paste0("   ", eq$nombre), "",
          sprintf("Bitácora generada el %s", format(Sys.time(), "%d/%m/%Y %H:%M")),
          "Cómo leer este libro: 'Resumen por fase' da la vista general; 'Bitácora detallada' tiene cada paso con su justificación;",
          "'Corridas' registra cada ejecución completa; 'Datasets' explica qué es el dataset sucio y qué el limpio;",
          "'Diccionario de columnas' describe cada variable; 'Índice de figuras' coincide con el informe y el dashboard.")
openxlsx::writeData(wb, "Portada", port, 2, 2)
openxlsx::addStyle(wb, "Portada", openxlsx::createStyle(fontSize = 30, textDecoration = "bold", fontColour = "#9333EA"), 2, 2)
openxlsx::addStyle(wb, "Portada", openxlsx::createStyle(fontSize = 20, textDecoration = c("bold", "italic"), fontColour = "#1C1A2B"), 3, 2)
openxlsx::addStyle(wb, "Portada", openxlsx::createStyle(fontSize = 12, fontColour = "#4A4860"), 4:5, 2, gridExpand = TRUE)
openxlsx::setColWidths(wb, "Portada", 1:2, c(3, 140))
tabla("Resumen por fase", resumen[, .(Fase = cod, Titulo = titulo, Tema_de_clase = tema_clase, Script_principal = script, Pasos_en_bitacora = registros, Figuras = figuras)],
      "Resumen por fase", "Cada fase es un script reproducible; su color se mantiene en figuras y dashboard.", "#4F46E5", c(8, 40, 52, 40, 12, 10))
for (i in seq_len(nrow(FASES))) openxlsx::addStyle(wb, "Resumen por fase", openxlsx::createStyle(fgFill = FASES$fondo[i], fontColour = FASES$color[i], textDecoration = "bold"), 4 + i, 1, stack = TRUE)
bd <- as.data.table(BITACORA)
tabla("Bitacora detallada", bd[, .(ID = id, Fecha_hora = fecha_hora, Fase = fase, Paso = paso, Accion = accion, Detalle = detalle, Filas_antes = filas_antes,
                                   Filas_despues = filas_despues, Archivo = archivo, Justificacion = justificacion)],
      "Bitácora detallada", "Un registro por decisión o paso; se reescribe en cada corrida (las corridas quedan en su hoja).", "#9333EA", c(6, 18, 30, 24, 40, 60, 11, 11, 32, 60))
for (i in seq_len(nrow(bd))) { k <- match(substr(bd$fase[i], 1, 3), FASES$cod); if (!is.na(k))
  openxlsx::addStyle(wb, "Bitacora detallada", openxlsx::createStyle(fgFill = FASES$fondo[k]), 4 + i, 1:10, gridExpand = TRUE, stack = TRUE) }
tabla("Corridas", corr, "Corridas del pipeline", "Cada ejecución de EJECUTAR_TODO.R: inicio, fin, duración por fase y estado.", "#65A30D", rep(18, ncol(corr)))
tabla("Datasets", datasets, "Dataset sucio y dataset limpio", "Qué contiene cada archivo de 01_Dataset.", "#F26B1D", c(30, 22, 10, 10, 90))
tabla("Diccionario de columnas", dicc, "Diccionario de columnas", "Variables de 03_dataset_sentimientos.csv (las del limpio están marcadas).", "#0D9488", c(22, 60, 14, 50))
tabla("Indice de figuras", as.data.table(IDX_FIG)[, .(Fase = fase, Nro = nro, Archivo = archivo, Titulo = titulo, Descripcion = descripcion)],
      "Índice de figuras", "La numeración Fxx-nn es la misma en el informe, el dashboard y las carpetas.", "#DB2777", c(7, 6, 42, 46, 90))
tabla("Hallazgos", H[, .(ID = id, Categoria = categoria, Titulo = titulo, Hallazgo = texto, Cifra = valor)], "Hallazgos", "Redactados automáticamente con los resultados.", "#E0A100", c(5, 14, 32, 100, 16))
tabla("Archivos generados", arch[, .(Carpeta = carpeta, Archivo = archivo, KB = kb)], "Archivos del proyecto", "Inventario de archivos (sin imágenes de emojis ni checkpoints).", "#475569", c(36, 70, 10))
openxlsx::saveWorkbook(wb, file.path(DIR$bit, "Bitacora_GrupoXDMSent.xlsx"), overwrite = TRUE)
bit("3. Excel de bitácora", "Bitácora con diseño en Excel", "Hojas: portada, resumen por fase, detallada, corridas, datasets, columnas, índice de figuras, hallazgos, archivos",
    archivo = "05_Bitacora/Bitacora_GrupoXDMSent.xlsx", justificacion = "Formato pedido para documentar todas las fases y su relación con el informe")
fase_fin()
cat("\nListo. Siguiente paso: source('GENERAR_DASHBOARD.R')\n")
