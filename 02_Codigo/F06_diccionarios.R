# =====================================================================
#  F06 | DICCIONARIOS DE SENTIMIENTOS, EMOJIS Y JERGA
#  Tema de clase: 4 (Ej9, análisis de sentimientos con léxico) y los 15
#  sentimientos acordados en clase con la estructura S(polaridad,
#  definición, términos, raíces). Se amplía con lo hallado en el dataset.
#  Diccionarios que salen de esta fase:
#   - dic_sentimientos.csv  (términos, frases y raíces por sentimiento)
#   - dic_emojis.csv        (significado en el chat, familia, sentimiento)
#   - dic_jerga_expresiones.csv (ya creado; aquí se audita su uso)
#   - Diccionario_GrupoXDMSent.xlsx (todo junto, con imágenes de emojis)
#  Fuentes externas: Emoji Sentiment Ranking (Kralj Novak et al., 2015),
#  nombres CLDR en español (Unicode) e imágenes Fluent Emoji 3D (MIT).
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
source(file.path(DIR$codigo, "motor_sentimientos.R"), encoding = "UTF-8")
fase_inicio("F06")

L <- cargar_chk("f03_limpio"); lim <- L$limpio

# ---- 1. Diccionario de los 15 sentimientos ---------------------------
#> construir_diccionario() (motor_sentimientos.R) une tres fuentes: las
#> listas de la clase tal como se dieron, los términos de amor y vergüenza
#> reasignados (esas dos emociones se reemplazaron por frustración y
#> satisfacción) y la ampliación obtenida al explorar el dataset. Todo se
#> normaliza igual que el texto (minúsculas, sin tildes) y cada término se
#> clasifica como frase, palabra exacta o raíz. Si un término aparece en dos
#> sentimientos, prevalece el de la lista de clase.
D <- construir_diccionario()
dic <- D$dic
fwrite(dic, file.path(DIR$dic, "dic_sentimientos.csv"), bom = TRUE)
res_dic <- dic[, .(terminos = .N, clase = sum(startsWith(origen, "Clase")), ampliacion = sum(origen == "Ampliación (dataset)"),
                   frases = sum(tipo == "frase"), raices = sum(tipo == "raiz")), by = sentimiento]
bit("1. Diccionario de sentimientos", sprintf("%s términos para 15 sentimientos", num(nrow(dic))),
    sprintf("%s de clase (incluye %d reasignados de amor y vergüenza) y %s agregados al explorar el dataset; %d conflictos resueltos a favor de la clase",
            num(sum(res_dic$clase)), sum(stri_detect_fixed(dic$origen, "reasignado")), num(sum(res_dic$ampliacion)), nrow(D$conflictos)),
    archivo = "03_Diccionarios/dic_sentimientos.csv",
    justificacion = "El diccionario de clase cubre emociones generales; el chat necesita frases propias (sin cupo, alguien sabe, me jaló, ni modo)")

# ---- 2. Diccionario de emojis ----------------------------------------------
#> Se unen: la interpretación curada para este chat (familia, sentimiento,
#> intensidad), el nombre oficial en español (CLDR), el puntaje externo del
#> Emoji Sentiment Ranking y la cantidad de usos en texto, reacciones y stickers.
cur  <- fread(file.path(DIR$dic, "dic_emojis_curado.csv"), encoding = "UTF-8")
cldr <- fread(file.path(DIR$dic, "fuentes_externas", "emoji_nombres_cldr_es.csv"), encoding = "UTF-8")
esr  <- fread(file.path(DIR$dic, "fuentes_externas", "Emoji_Sentiment_Data_v1.0.csv"), encoding = "UTF-8")
esr[, `:=`(emoji = norm_emoji(Emoji), esr_puntaje = round((Positive - Negative) / Occurrences, 3), esr_ocurrencias = Occurrences)]
esr <- esr[!duplicated(emoji), .(emoji, esr_puntaje, esr_ocurrencias)]
u_txt <- data.table(emoji = unlist(strsplit(lim$emojis[lim$emojis != ""], " ")))[, .(usos_texto = .N), by = emoji]
u_stk <- lim[!is.na(emoji_sticker), .(usos_sticker = .N), by = .(emoji = emoji_sticker)]
rq <- lim[reacciones != "", unlist(strsplit(reacciones, "|", fixed = TRUE))]
u_rea <- data.table(emoji = sub(":\\d+$", "", rq), n = as.integer(sub("^.*:", "", rq)))[, .(usos_reaccion = sum(n)), by = emoji]
todos <- unique(c(u_txt$emoji, u_stk$emoji, u_rea$emoji, cur$emoji))
DE <- data.table(emoji = todos)
DE <- merge(DE, cur, by = "emoji", all.x = TRUE)
DE <- merge(DE, cldr[, .(emoji = norm_emoji(emoji), nombre_es, grupo_unicode)][!duplicated(emoji)], by = "emoji", all.x = TRUE)
DE <- merge(DE, esr, by = "emoji", all.x = TRUE)
DE <- Reduce(function(a, b) merge(a, b, by = "emoji", all.x = TRUE), list(DE, u_txt, u_stk, u_rea))
for (k in c("usos_texto", "usos_sticker", "usos_reaccion")) set(DE, which(is.na(DE[[k]])), k, 0L)
DE[, usos_total := usos_texto + usos_sticker + usos_reaccion]
DE[, curado := !is.na(familia)]
DE[is.na(familia), `:=`(familia = "informativo / objeto", sentimiento = "neutral", intensidad = 0L,
                        significado_chat = paste0("uso informativo o decorativo (", fifelse(is.na(nombre_es), "emoji", nombre_es), ")"))]
DE[, codigo := emoji_codigo(emoji)]
DE[, imagen_3d := file.exists(file.path(DIR$emoji, paste0(codigo, ".png")))]
DE[, polaridad := fifelse(sentimiento %in% SENT_ORDEN[1:6], "positivo", fifelse(sentimiento %in% SENT_ORDEN[9:15], "negativo", "neutral"))]
setorder(DE, -usos_total)
fwrite(DE, file.path(DIR$dic, "dic_emojis.csv"), bom = TRUE)
caras_sin <- DE[curado == FALSE & usos_total > 0 & grupo_unicode %in% c("Smileys & Emotion", "People & Body")]
bit("2. Diccionario de emojis", sprintf("%d emojis con significado (%d curados para este chat)", nrow(DE), sum(DE$curado)),
    sprintf("%s usos en texto, %s en reacciones, %s en stickers; %d familias de significado; caras/manos usadas sin curar: %d",
            num(sum(DE$usos_texto)), num(sum(DE$usos_reaccion)), num(sum(DE$usos_sticker)), uniqueN(DE[curado == TRUE, familia]), nrow(caras_sin)),
    archivo = "03_Diccionarios/dic_emojis.csv",
    justificacion = "Un emoji no significa lo mismo en todos los contextos: 💀 es risa y 🙏 es 'por favor' en el chat; por eso se interpreta para este grupo y se contrasta con un léxico externo")

# ---- 3. Jerga con sentimiento (solo señales que no son palabras) ---------------
#> Del diccionario de jerga solo se usan como señal los emoticones y signos
#> (:( , :v, ??, T_T, F, uff). Las expresiones con palabras (chuta, ni
#> modo, me jaló) ya están en el diccionario de sentimientos; contarlas
#> otra vez duplicaría su peso.
jerga <- fread(file.path(DIR$dic, "dic_jerga_expresiones.csv"), encoding = "UTF-8")
JS <- jerga[sentimiento != "" & (stri_detect_regex(tipo, "emotic|puntuaci") | expresion %in% c("F", "uff")), .(expresion, sentimiento)]
jerga_uso <- merge(jerga, L$uso_jerga[, .(expresion, mensajes, apariciones)], by = "expresion", all.x = TRUE)[order(-mensajes)]
fwrite(jerga_uso, file.path(DIR$dic, "dic_jerga_expresiones_uso.csv"), bom = TRUE)
bit("3. Jerga y expresiones", sprintf("%d expresiones en el diccionario de jerga; %d aportan sentimiento como señal no verbal", nrow(jerga), nrow(JS)),
    paste("Con sentimiento:", paste(JS$expresion, collapse = " ")),
    archivo = "03_Diccionarios/dic_jerga_expresiones_uso.csv",
    justificacion = "Las palabras de jerga (chuta, ni modo, me jaló) ya están en el diccionario de sentimientos; aquí solo se suman emoticones y signos para no contarlos dos veces")

# ---- 4. Aplicación del motor y cobertura ---------------------------------------
#> puntuar_mensajes() recorre cada mensaje analizable: busca primero frases
#> (peso 1.5), luego palabras (1.0) y raíces (0.8); descarta un término si
#> va precedido de una negación, multiplica por 1.5 si va después de un
#> intensificador, suma emojis (0.5 + 0.25 x intensidad) y emoticones (0.8)
#> y aplica la regla de ironía. La cobertura indica qué parte de los
#> mensajes tiene al menos una señal y de qué fuente proviene.
an <- lim[analizable == TRUE]
R <- puntuar_mensajes(an$id, an$texto_limpio, an$texto_norm, an$emojis, an$emoji_sticker, an$expresiones, D, DE, JS)
H <- R$H
fuentes_msg <- H[, .(txt = any(fuente %like% "frase|palabra|raiz"), emo = any(fuente == "emoji"), sig = any(fuente == "emoticon")), by = id]
cob <- merge(an[, .(id, anio)], fuentes_msg, by = "id", all.x = TRUE)
for (k in c("txt", "emo", "sig")) set(cob, which(is.na(cob[[k]])), k, FALSE)
cob[, combinacion := fifelse(txt & (emo | sig), "Texto + emoji/emoticón", fifelse(txt, "Solo texto", fifelse(emo | sig, "Solo emoji/emoticón", "Sin señal (neutral)")))]
uso_term <- H[, .(mensajes = uniqueN(id), peso = sum(peso)), by = .(termino, sentimiento, fuente)][order(-mensajes)]
clase_usada <- merge(dic, uso_term[fuente %in% c("frase", "palabra", "raiz") | fuente %like% "ironia", .(m = sum(mensajes)), by = termino], by = "termino", all.x = TRUE)
clase_usada[is.na(m), m := 0]
aud <- clase_usada[, .(encontrados = sum(m > 0), no_encontrados = sum(m == 0), aciertos = sum(m)), by = .(sentimiento, origen = fifelse(startsWith(origen, "Clase"), "Clase", "Ampliación"))]
guardar_tabla(uso_term, "uso_terminos"); guardar_tabla(aud, "auditoria_diccionario")
guardar_tabla(R$negados[, .N, by = .(termino, sentimiento)][order(-N)], "terminos_negados")
bit("4. Cobertura", sprintf("%s de los mensajes analizables tienen al menos una señal de sentimiento", pct(mean(cob$combinacion != "Sin señal (neutral)"))),
    sprintf("Solo texto %s, texto+emoji %s, solo emoji %s, sin señal %s. Negaciones descartadas: %s; intensificados: %s",
            pct(mean(cob$combinacion == "Solo texto")), pct(mean(cob$combinacion == "Texto + emoji/emoticón")),
            pct(mean(cob$combinacion == "Solo emoji/emoticón")), pct(mean(cob$combinacion == "Sin señal (neutral)")),
            num(nrow(R$negados)), num(sum(H$peso %in% c(1.5 * 1, 0.8 * 1.5) & H$fuente %in% c("palabra", "raiz")))),
    antes = nrow(an), despues = sum(cob$combinacion != "Sin señal (neutral)"),
    justificacion = "Un mensaje sin señal se considera neutral o informativo (por ejemplo, horarios o avisos)")

# ---- 5. Validación del diccionario de emojis con un léxico externo ---------------
#> Se compara la polaridad asignada a cada emoji con el Emoji Sentiment
#> Ranking (Kralj Novak et al., 2015), un léxico construido con 1.6 millones
#> de tuits etiquetados. Solo se comparan emojis con 30 o más ocurrencias en
#> ese léxico; los desacuerdos se revisan porque suelen ser usos propios
#> del chat.
val <- DE[curado == TRUE & !is.na(esr_puntaje) & esr_ocurrencias >= 30 & polaridad != "neutral"]
val[, esr_pol := fifelse(esr_puntaje > 0.05, "positivo", fifelse(esr_puntaje < -0.05, "negativo", "neutral"))]
acuerdo <- mean(val$polaridad == val$esr_pol | val$esr_pol == "neutral")
acuerdo_estricto <- mean(val$polaridad == val$esr_pol)
bit("5. Validación de emojis", sprintf("Coincidencia con el Emoji Sentiment Ranking: %s (estricta %s)", pct(acuerdo), pct(acuerdo_estricto)),
    sprintf("%d emojis comparables; desacuerdos: %s", nrow(val), paste(val[polaridad != esr_pol & esr_pol != "neutral", emoji], collapse = " ")),
    justificacion = "El léxico externo (tuits europeos) sirve de control; los desacuerdos son usos propios del chat (😭 por estrés, 💀 como risa)")

guardar_chk(list(D = D, DE = DE, JS = JS, R = R, cob = cob, uso_term = uso_term, aud = aud, val = val), "f06_dic")

# ---- 6. Excel de diccionarios -----------------------------------------------------
#> Se arma Diccionario_GrupoXDMSent.xlsx con openxlsx: una hoja por
#> diccionario (sentimientos, términos, emojis con su imagen, familias,
#> jerga, reasignados, reglas del motor y palabras vacías), con estilos y
#> filtros, para revisarlo sin abrir R.
wb <- openxlsx::createWorkbook()
estilo_tit <- openxlsx::createStyle(fontName = "Calibri", fontSize = 20, textDecoration = "bold", fontColour = "#4F46E5")
estilo_cab <- openxlsx::createStyle(fontName = "Calibri", fontSize = 11, textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#4F46E5",
                                    halign = "center", valign = "center", wrapText = TRUE, border = "Bottom")
estilo_txt <- openxlsx::createStyle(fontName = "Calibri", fontSize = 10, valign = "center", wrapText = TRUE)
hoja <- function(nombre, df, titulo, nota, anchos = NULL, color = "#4F46E5") {
  openxlsx::addWorksheet(wb, nombre, tabColour = color, gridLines = FALSE)
  openxlsx::writeData(wb, nombre, titulo, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, nombre, estilo_tit, rows = 1, cols = 1)
  openxlsx::writeData(wb, nombre, nota, startRow = 2, startCol = 1)
  openxlsx::addStyle(wb, nombre, openxlsx::createStyle(fontColour = "#555366", textDecoration = "italic"), rows = 2, cols = 1)
  openxlsx::writeDataTable(wb, nombre, as.data.frame(df), startRow = 4, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  openxlsx::addStyle(wb, nombre, openxlsx::createStyle(fgFill = color, fontColour = "#FFFFFF", textDecoration = "bold", halign = "center", wrapText = TRUE),
                     rows = 4, cols = seq_len(ncol(df)), gridExpand = TRUE)
  openxlsx::addStyle(wb, nombre, estilo_txt, rows = 5:(nrow(df) + 4), cols = seq_len(ncol(df)), gridExpand = TRUE, stack = TRUE)
  openxlsx::setColWidths(wb, nombre, cols = seq_len(ncol(df)), widths = if (is.null(anchos)) "auto" else anchos)
  openxlsx::freezePane(wb, nombre, firstActiveRow = 5)
}
res15 <- merge(D$defs, res_dic, by = c("sentimiento"))[, etiqueta := SENT_LAB[sentimiento]][match(SENT_ORDEN, sentimiento)]
res15 <- merge(res15, R$W[!is.na(sentimiento), .(mensajes_dominante = .N), by = sentimiento], by = "sentimiento", all.x = TRUE)[match(SENT_ORDEN, sentimiento)]
hoja("15 sentimientos", res15[, .(Sentimiento = etiqueta, Polaridad = polaridad, Definicion = definicion, Terminos = terminos, De_clase = clase,
                                  Ampliacion = ampliacion, Frases = frases, Raices = raices, Mensajes_dominante = mensajes_dominante)],
     "Los 15 sentimientos del proyecto", "Acordados en clase (frustración reemplaza a amor y satisfacción a vergüenza). Polaridad: 1 positivo, 0 neutro, -1 negativo.",
     c(16, 10, 60, 10, 10, 11, 9, 9, 14))
hoja("Terminos", merge(dic, uso_term[, .(mensajes = sum(mensajes)), by = termino], by = "termino", all.x = TRUE)[order(sentimiento, -mensajes)][
       , .(Sentimiento = SENT_LAB[sentimiento], Termino = termino, Tipo = tipo, Origen = origen, Polaridad = polaridad, Mensajes_donde_aparece = fifelse(is.na(mensajes), 0L, as.integer(mensajes)))],
     "Diccionario de términos", "Frases (2+ palabras), palabras exactas y raíces. Comparación sin tildes ni mayúsculas.", c(15, 30, 10, 28, 10, 14))
em_x <- DE[, .(Emoji = emoji, Imagen = "", Nombre_oficial = nombre_es, Significado_en_el_chat = significado_chat, Familia = familia,
               Sentimiento = ifelse(sentimiento %in% SENT_ORDEN, SENT_LAB[sentimiento], "Neutral / informativo"), Intensidad = intensidad,
               Usos_texto = usos_texto, Usos_reaccion = usos_reaccion, Usos_sticker = usos_sticker, Puntaje_ESR = esr_puntaje, Curado = curado)]
hoja("Emojis", em_x, "Diccionario de emojis", "Imágenes: Fluent Emoji 3D (Microsoft, licencia MIT). Puntaje ESR: Emoji Sentiment Ranking (Kralj Novak et al., 2015).",
     c(8, 8, 26, 50, 16, 16, 9, 9, 10, 9, 10, 8), color = "#DB2777")
n_img <- min(nrow(DE), 180)
for (i in seq_len(n_img)) {
  f <- emoji_png(DE$emoji[i]); if (is.na(f)) next
  openxlsx::insertImage(wb, "Emojis", f, startRow = 4 + i, startCol = 2, width = .26, height = .26, units = "in")
}
openxlsx::setRowHeights(wb, "Emojis", rows = 5:(4 + n_img), heights = 22)
hoja("Familias de emojis", DE[curado == TRUE, .(Emojis = paste(emoji, collapse = " "), Cantidad = .N, Usos = sum(usos_total)), by = .(Familia = familia, Sentimiento = SENT_LAB[sentimiento])][order(-Usos)],
     "Emojis distintos con el mismo significado", "Cada familia agrupa emojis que en el chat se usan para expresar lo mismo.", c(18, 16, 60, 10, 10), color = "#DB2777")
hoja("Jerga y expresiones", jerga_uso[, .(Expresion = expresion, Tipo = tipo, Significado = significado, Sentimiento = ifelse(sentimiento == "", "-", SENT_LAB[sentimiento]),
                                          Normaliza_a = normaliza_a, Mensajes = mensajes, Patron_regex = patron)],
     "Jerga, caracteres y expresiones", "Risas, emoticones, abreviaturas, bolivianismos y glosario universitario con su significado.", c(16, 22, 60, 13, 14, 10, 50), color = "#0D9488")
reas <- dic[stri_detect_fixed(origen, "reasignado"), .(Termino = termino, Nuevo_sentimiento = SENT_LAB[sentimiento], Origen = origen)]
hoja("Reasignados y conflictos", rbind(reas, D$conflictos[, .(Termino = termino, Nuevo_sentimiento = paste("descartado de", SENT_LAB[sentimiento]), Origen = "Conflicto: el término ya estaba en otro sentimiento")]),
     "Términos reasignados y conflictos", "Amor y vergüenza salieron de la lista; sus términos se reasignaron para no perderlos.", c(26, 30, 50), color = "#64748B")
hoja("Ironia y reglas", data.table(Regla = c(rep("Señal de ironía (frase)", length(D$ironia)), rep("Señal de ironía (emoji)", length(D$ironia_emoji)),
                                          rep("Negador", length(D$negadores)), rep("Intensificador (x1.5)", length(D$intensificadores))),
                                Valor = c(D$ironia, D$ironia_emoji, D$negadores, D$intensificadores)),
     "Reglas del motor de sentimientos", "Negación: el término no se cuenta. Intensificador: peso x1.5. Ironía: lo positivo pasa a decepción.", c(26, 30), color = "#64748B")
stp <- readLines(file.path(DIR$dic, "stopwords_proyecto.txt"))
hoja("Palabras vacias", data.table(Palabra = stp), "Palabras vacías del proyecto", "Snowball (normalizada) + personalizada del chat (ver F05).", 20, color = "#0284C7")
openxlsx::saveWorkbook(wb, file.path(DIR$dic, "Diccionario_GrupoXDMSent.xlsx"), overwrite = TRUE)
bit("6. Excel", "Diccionario completo en Excel con imágenes de emojis", "Hojas: 15 sentimientos, términos, emojis, familias, jerga, reasignados, reglas, palabras vacías",
    archivo = "03_Diccionarios/Diccionario_GrupoXDMSent.xlsx", justificacion = "Entrega legible para el informe y la revisión docente")

# ---- 7. Figuras -------------------------------------------------------------------------
#> Ocho figuras: tamaño del diccionario por sentimiento, cobertura por
#> fuente de señal, términos más encontrados, aporte de la ampliación,
#> mosaico de emojis, familias de emojis, validación con el léxico externo
#> y términos descartados por negación.
# F06-01 Tamaño del diccionario por sentimiento
d1 <- dic[, .N, by = .(sentimiento, origen = fifelse(startsWith(origen, "Clase"), "Clase", "Ampliación (dataset)"))]
d1[, sentimiento := factor(sentimiento, rev(SENT_ORDEN))]
tot1 <- d1[, .(N = sum(N)), by = sentimiento]
p <- ggplot(d1, aes(N, sentimiento, fill = origen)) + geom_col(width = .7) +
  geom_text(data = tot1, aes(N, sentimiento, label = N), inherit.aes = FALSE, hjust = -.3, family = FT_MONO, size = 3.4) +
  scale_fill_manual(values = c(Clase = "#4F46E5", `Ampliación (dataset)` = "#A5B4FC"), name = NULL) +
  scale_y_discrete(labels = SENT_LAB) + scale_x_continuous(expand = expansion(c(0, .12))) +
  coord_cartesian(clip = "off") + theme(plot.margin = margin(22, 24, 16, 64)) +
  labs(title = "Tamaño del diccionario por sentimiento", x = "Términos (palabras, frases y raíces)", y = NULL,
       subtitle = sprintf("%s términos: %s acordados en clase y %s agregados explorando el chat (jerga, frases típicas, bolivianismos)",
                          num(nrow(dic)), num(sum(d1[origen == "Clase", N])), num(sum(d1[origen != "Clase", N]))))
p <- agregar_emojis(p, SENT_EMOJI[rev(SENT_ORDEN)], x = rep(-max(tot1$N) * .2, 15), y = 1:15, w = max(tot1$N) * .055, h = .8)
guardar_fig(p, "tamano_diccionario", "Tamaño del diccionario por sentimiento",
            sprintf("Número de términos por sentimiento según su origen: la base acordada en clase y la ampliación hecha al explorar el dataset. La ampliación es mayor en %s, emociones que el chat expresa con frases propias.",
                    paste(tolower(SENT_LAB[d1[origen != "Clase"][order(-N)][1:3, as.character(sentimiento)]]), collapse = ", ")), h = 7.6)

# F06-02 Cobertura de señales
c2 <- cob[, .N, by = .(anio, combinacion)][, p := N / sum(N), by = anio]
c2[, combinacion := factor(combinacion, c("Solo texto", "Texto + emoji/emoticón", "Solo emoji/emoticón", "Sin señal (neutral)"))]
p <- ggplot(c2, aes(factor(anio), p, fill = combinacion)) + geom_col(width = .62) +
  geom_text(aes(label = ifelse(p > .035, pct(p, 0), "")), position = position_stack(vjust = .5), color = "white", family = FT_TXT, fontface = "bold") +
  scale_fill_manual(values = c("#4F46E5", "#8B5CF6", "#EC4899", "#CBD5E1"), name = NULL) + scale_y_continuous(labels = percent) +
  labs(title = "Cobertura del diccionario por fuente de señal", x = NULL, y = "Mensajes analizables",
       subtitle = sprintf("En total, %s de los mensajes tiene al menos una señal; el resto son mensajes informativos sin carga emocional",
                          pct(mean(cob$combinacion != "Sin señal (neutral)"))))
guardar_fig(p, "cobertura_senales", "Cobertura del diccionario por fuente de señal",
            "Proporción de mensajes cuyo sentimiento se detecta solo por texto, por texto y emoji, solo por emoji o emoticón, o que no tienen señal. Los emojis aportan una parte pequeña pero valiosa: mensajes como '😭' no tienen palabras.", h = 6)

# F06-03 Términos más encontrados
t3 <- uso_term[fuente %in% c("frase", "palabra", "raiz")][, .(mensajes = sum(mensajes)), by = .(termino, sentimiento)][order(-mensajes)][1:32]
t3[, termino := factor(termino, rev(termino))]
p <- ggplot(t3, aes(mensajes, termino, fill = sentimiento)) + geom_col(width = .74) +
  geom_text(aes(label = num(mensajes)), hjust = -.15, family = FT_MONO, size = 3) +
  escala_sent(name = NULL) + scale_x_continuous(labels = comma, expand = expansion(c(0, .12))) +
  theme(legend.position = "right") +
  labs(title = "Términos del diccionario más encontrados", x = "Mensajes donde aparece", y = NULL,
       subtitle = "Términos del diccionario más encontrados en el chat, coloreados por el sentimiento al que pertenecen")
guardar_fig(p, "terminos_mas_encontrados", "Términos del diccionario más encontrados",
            "Los 32 términos con más apariciones. Dominan las fórmulas de consulta (alguien sabe, recomiendan) y de agradecimiento (gracias), seguidas de quejas por cupos y del humor (jaja, xd).", h = 8.8)

# F06-04 Aporte de la ampliación
a4 <- aud[, .(aciertos = sum(aciertos)), by = .(sentimiento, origen)][, p := aciertos / sum(aciertos), by = sentimiento]
a4[, sentimiento := factor(sentimiento, rev(SENT_ORDEN))]
p <- ggplot(a4, aes(p, sentimiento, fill = origen)) + geom_col(width = .68) +
  geom_text(aes(label = ifelse(p > .06, pct(p, 0), "")), position = position_stack(vjust = .5), color = "white", family = FT_TXT, fontface = "bold", size = 3.4) +
  scale_fill_manual(values = c(Clase = "#4F46E5", `Ampliación` = "#A5B4FC"), name = NULL) +
  scale_y_discrete(labels = SENT_LAB) + scale_x_continuous(labels = percent) +
  labs(title = "Aporte de la ampliación del diccionario", x = "Proporción de detecciones", y = NULL,
       subtitle = "Detecciones de cada sentimiento hechas con términos de clase frente a términos agregados desde el dataset")
guardar_fig(p, "aporte_ampliacion", "Aporte de la ampliación del diccionario",
            sprintf("Para cada sentimiento, qué parte de las detecciones viene de la base de clase y qué parte de la ampliación. Sin la ampliación, %s quedarían casi invisibles porque el chat las expresa con frases propias (sin cupo, alguien sabe, ya me inscribí).",
                    paste(tolower(SENT_LAB[a4[origen == "Ampliación"][order(-p)][1:3, as.character(sentimiento)]]), collapse = ", ")), h = 7)

# F06-05 Mosaico de emojis más usados
top <- DE[usos_total > 0 & imagen_3d == TRUE][1:min(.N, 40)]
top[, `:=`(i = seq_len(.N) - 1)][, `:=`(x = i %% 8, y = -(i %/% 8))]
top[, col := ifelse(sentimiento %in% SENT_ORDEN, SENT_COL[sentimiento], "#CBD5E1")]
p <- ggplot(top, aes(x, y)) +
  geom_tile(aes(fill = I(alpha(col, .22)), color = I(col)), width = .92, height = .9, linewidth = .8) +
  geom_text(aes(y = y - .33, label = num(usos_total)), family = FT_MONO, size = 3.1, color = TINTA) +
  geom_text(aes(y = y + .36, label = stri_sub(ifelse(sentimiento %in% SENT_ORDEN, SENT_LAB[sentimiento], "informativo"), 1, 14)), family = FT_TXT, size = 2.6, color = TINTA2) +
  coord_equal() + theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Mosaico de los 40 emojis más usados", x = NULL, y = NULL,
       subtitle = "Usos en texto, reacciones y stickers. El color del marco es el sentimiento asignado por el diccionario de emojis")
p <- agregar_emojis(p, top$emoji, x = top$x, y = top$y + .02, w = .42, h = .42)
guardar_fig(p, "mosaico_emojis", "Mosaico de los 40 emojis más usados",
            "Cada celda muestra un emoji (imagen 3D Fluent), sus usos y el sentimiento que le asigna el diccionario. Los pulgares dominan por las reacciones; entre las caras destacan la risa, el llanto exagerado y la súplica.", w = 10, h = 7.4)

# F06-06 Familias de emojis
fam <- DE[curado == TRUE, .(emojis = list(emoji[order(-usos_total)]), usos = sum(usos_total), sent = sentimiento[1]), by = familia][order(-usos)][1:min(.N, 12)]
fam[, y := rev(seq_len(.N))]
p <- ggplot(fam) +
  geom_tile(aes(x = 5.7, y = y, fill = I(alpha(SENT_COL[sent], .16))), width = 9.6, height = .84) +
  geom_text(aes(x = .55, y = y, label = familia), hjust = 0, family = FT_TIT, fontface = "bold.italic", size = 4.4, color = TINTA) +
  geom_text(aes(x = .55, y = y - .27, label = sprintf("%s · %s usos", SENT_LAB[sent], num(usos))), hjust = 0, family = FT_TXT, size = 2.9, color = TINTA2) +
  scale_x_continuous(limits = c(.5, 10.6)) + theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Familias de emojis con el mismo significado", x = NULL, y = NULL,
       subtitle = "Familias de emojis que en el chat expresan lo mismo (ordenadas por uso); dentro de cada familia, de más a menos usado")
ex <- fam[, .(e = head(unlist(emojis), 7)), by = .(familia, y)][, x := 3.6 + (seq_len(.N) - 1) * .95, by = familia]
p <- agregar_emojis(p, ex$e, x = ex$x, y = ex$y, w = .62, h = .62)
guardar_fig(p, "familias_emojis", "Familias de emojis con el mismo significado",
            "Agrupación semántica de emojis curada para este chat. Por ejemplo, 😂 🤣 💀 son la misma risa; 😢 😭 🥲 expresan tristeza o estrés; 👍 👌 ✅ son aprobación. Así se comparan emojis distintos que dicen lo mismo.", w = 11, h = 8.4)

# F06-07 Validación con el Emoji Sentiment Ranking
val[, etiqueta := ifelse(polaridad == esr_pol | esr_pol == "neutral", "Coincide", "Uso propio del chat")]
p <- ggplot(val, aes(esr_puntaje, factor(polaridad, c("negativo", "positivo")))) +
  geom_vline(xintercept = 0, linetype = "22", color = TINTA3) +
  geom_jitter(aes(color = etiqueta), height = .22, width = 0, size = 3, alpha = .8) +
  scale_color_manual(values = c(Coincide = "#4F46E5", `Uso propio del chat` = "#EC4899"), name = NULL) +
  scale_y_discrete(labels = c(negativo = "Negativo en\nnuestro diccionario", positivo = "Positivo en\nnuestro diccionario")) +
  labs(title = "Validación externa del diccionario de emojis", x = "Puntaje en el Emoji Sentiment Ranking (-1 negativo, +1 positivo)", y = NULL,
       subtitle = sprintf("%d emojis comparables: coinciden en %s; las diferencias son usos propios del chat (por ejemplo 😭 y 😢 como estrés)", nrow(val), pct(acuerdo)))
vx <- val[etiqueta != "Coincide"]
if (nrow(vx)) p <- agregar_emojis(p, vx$emoji, x = vx$esr_puntaje, y = as.numeric(factor(vx$polaridad, c("negativo", "positivo"))) + .38, w = .06, h = .32)
guardar_fig(p, "validacion_emojis_esr", "Validación del diccionario de emojis frente al Emoji Sentiment Ranking",
            "Cada punto es un emoji del diccionario con su puntaje en el léxico externo Emoji Sentiment Ranking. La mayoría coincide en polaridad; los casos rosados son emojis que en este chat se usan distinto (llanto por estrés académico, risa ante problemas).", h = 6)

# F06-08 Negación e intensificación
ng <- R$negados[, .N, by = .(termino, sentimiento)][order(-N)][1:min(.N, 14)]
ng[, termino := factor(termino, rev(termino))]
p <- ggplot(ng, aes(N, termino, fill = sentimiento)) + geom_col(width = .7) + escala_sent(name = NULL) +
  geom_text(aes(label = N), hjust = -.2, family = FT_MONO, size = 3.3) + scale_x_continuous(expand = expansion(c(0, .12))) +
  theme(legend.position = "right") +
  labs(title = "Términos descartados por negación", x = "Veces que se descartó por negación", y = NULL,
       subtitle = sprintf("%s apariciones descartadas porque iban precedidas de 'no', 'nunca', 'ni', 'sin' o 'jamás' (por ejemplo 'no está mal' no es enojo)", num(nrow(R$negados))))
guardar_fig(p, "negaciones", "Términos descartados por negación",
            "Términos del diccionario que aparecieron negados y por eso no se contaron. Sin esta regla, 'no está mal' sumaría enojo y 'no es fácil' sumaría tranquilidad.", h = 6.4)

fase_fin()
