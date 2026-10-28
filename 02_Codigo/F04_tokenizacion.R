# =====================================================================
#  F04 | TOKENIZACIÓN Y N-GRAMAS
#  Temas de clase: 2. Ejemplo de tokenización (oraciones y palabras en una
#  columna de un data frame) y 3. Bigramas, tokenización de caracteres,
#  traducción carácter por carácter (Ej5) y frecuencia por palabra (Ej6).
#  Se agregan dos leyes clásicas del lenguaje (externas a la clase):
#  Zipf (frecuencia vs rango) y Heaps (crecimiento del vocabulario).
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
suppressPackageStartupMessages(library(tidytext))
fase_inicio("F04")

L <- cargar_chk("f03_limpio")
lim <- L$limpio[analizable == TRUE]
sw_es <- stopwords::stopwords("es", source = "snowball")

# ---- 1. Tokenización de oraciones y palabras (tidytext) ---------------------
#> unnest_tokens() parte la columna de texto: con token = "sentences" en
#> oraciones y con el valor por defecto en palabras (una fila por token).
oraciones <- as.data.table(unnest_tokens(lim[n_palabras > 0, .(id, texto_norm)], oracion, texto_norm, token = "sentences"))
tok <- as.data.table(unnest_tokens(lim[n_palabras > 0, .(id, texto_limpio)], palabra, texto_limpio, token = "words"))
tok[, pos := seq_len(.N), by = id]
bit("1. Oraciones y palabras", "unnest_tokens() sobre texto_norm (oraciones) y texto_limpio (palabras)",
    sprintf("%s oraciones y %s tokens de palabra; vocabulario de %s palabras distintas",
            num(nrow(oraciones)), num(nrow(tok)), num(uniqueN(tok$palabra))),
    antes = nrow(lim), despues = nrow(tok),
    justificacion = "La tokenización convierte el texto en unidades contables: es la base de frecuencias, n-gramas y diccionarios")

# ---- 2. Frecuencia por palabra (Ej6) ------------------------------------------------
#> Ej6 de clase: contar cuántas veces aparece cada palabra. Se ordena de
#> mayor a menor, se asigna el rango (1 = la más frecuente) y se marca si
#> la palabra es vacía (de, que, la...). Esta tabla cruda muestra por qué
#> hace falta quitar palabras vacías en F05: dominan el ranking sin
#> aportar significado.
freq <- tok[, .N, by = palabra][order(-N)]
freq[, `:=`(rango = seq_len(.N), vacia = palabra %in% sw_es)]
guardar_tabla(freq, "frecuencia_palabras_cruda")
bit("2. Frecuencia por palabra", sprintf("%s palabras distintas contadas", num(nrow(freq))),
    sprintf("De las 30 más frecuentes, %d son palabras vacías; la primera con contenido es '%s'",
            sum(freq$vacia[1:30]), freq[vacia == FALSE][1, palabra]),
    archivo = "04_Resultados/tablas/F04_frecuencia_palabras_cruda.csv",
    justificacion = "Ej6 de clase: la frecuencia cruda justifica el filtrado de palabras vacías")

# ---- 3. Caracteres: traducción carácter a carácter (Ej5) -------------------------------
#> Cada carácter del texto original se "traduce" a su clase Unicode:
#> letra, dígito, espacio, puntuación, símbolo o emoji.
chars <- stri_split_boundaries(lim$texto_mostrar, type = "character")
clase_car <- function(ch) fifelse(stri_detect_regex(ch, paste0("^(", RX_EMOJI, ")$")), "Emoji",
                         fifelse(stri_detect_regex(ch, "^\\p{L}+$"), "Letra",
                         fifelse(stri_detect_regex(ch, "^\\p{N}+$"), "Dígito",
                         fifelse(stri_detect_regex(ch, "^\\s+$"), "Espacio",
                         fifelse(stri_detect_regex(ch, "^\\p{P}+$"), "Puntuación", "Otro símbolo")))))
car <- data.table(id = rep(lim$id, lengths(chars)), anio = rep(lim$anio, lengths(chars)), ch = unlist(chars))
car[, clase := clase_car(ch)]
bit("3. Caracteres", "Tokenización de caracteres y traducción de cada carácter a su clase",
    sprintf("%s caracteres: %s", num(nrow(car)), paste(car[, .N, by = clase][order(-N), sprintf("%s %s", clase, pct(N / nrow(car)))], collapse = ", ")),
    justificacion = "Ej5 de clase: traducir carácter por carácter a un nuevo formato; aquí el formato es la clase del carácter")

# ---- 4. Subpalabras: n-gramas de caracteres ---------------------------------------
#> Con token = "character_shingles" cada palabra se parte en secuencias de
#> 3 caracteres (por ejemplo "materia" -> mat, ate, ter, eri, ria). Las
#> subpalabras toleran errores de escritura y variantes ("inscripcion",
#> "inscripsion") que como palabras completas serían distintas.
shing <- as.data.table(unnest_tokens(lim[n_palabras > 0, .(id, texto_limpio)], sub, texto_limpio, token = "character_shingles", n = 3))
shing_f <- shing[, .N, by = sub][order(-N)]
bit("4. Subpalabras", sprintf("%s subpalabras de 3 caracteres (%s distintas)", num(nrow(shing)), num(nrow(shing_f))),
    sprintf("Más frecuentes: %s", paste(shing_f$sub[1:8], collapse = ", ")),
    justificacion = "Tokenización en subpalabras vista en clase; útil frente a la escritura informal del chat")

# ---- 5. Bigramas y trigramas sin filtrar ---------------------------------------------
#> Con token = "ngrams" se forman pares (n = 2) y tríos (n = 3) de palabras
#> consecutivas dentro de cada mensaje. Aquí se cuentan sin filtrar, para
#> ver el efecto de las palabras vacías; en F05 se repite con filtro.
big <- as.data.table(unnest_tokens(lim[n_palabras > 1, .(id, texto_limpio)], bigrama, texto_limpio, token = "ngrams", n = 2))[!is.na(bigrama)]
tri <- as.data.table(unnest_tokens(lim[n_palabras > 2, .(id, texto_limpio)], trigrama, texto_limpio, token = "ngrams", n = 3))[!is.na(trigrama)]
bit("5. N-gramas", "Bigramas y trigramas de palabras consecutivas",
    sprintf("%s bigramas (%s distintos), %s trigramas, %s shingles", num(nrow(big)), num(uniqueN(big$bigrama)), num(nrow(tri)), num(nrow(shing))),
    justificacion = "Los n-gramas capturan frases ('alguien sabe', 'por favor') que una sola palabra no expresa")

guardar_chk(list(tok = tok, freq = freq, big = big, tri = tri), "f04_tokens")

# ---- 6. Leyes de Zipf y Heaps -------------------------------------------------------------
#> Zipf: en lenguaje natural la frecuencia de una palabra es inversamente
#> proporcional a su rango; en escala log-log se ve una recta de pendiente
#> cercana a -1. Heaps: el vocabulario crece como una potencia del número
#> de palabras leídas (V = K·N^b, con b < 1). Se ajustan ambas con
#> regresión lineal sobre logaritmos para comprobar que el chat, pese a su
#> escritura informal, se comporta como lenguaje natural.
zipf <- lm(log10(N) ~ log10(rango), data = freq[rango <= 2000])
setorder(tok, id, pos)
heaps <- data.table(tokens = seq_len(nrow(tok)), vocab = cumsum(!duplicated(tok$palabra)))[seq(1, .N, length.out = 400)]
hfit <- lm(log(vocab) ~ log(tokens), data = heaps[tokens > 1000])
bit("6. Zipf y Heaps", "Ajuste de las leyes de Zipf y Heaps",
    sprintf("Zipf: pendiente %.2f (R² %.3f). Heaps: V = %.1f·N^%.2f", coef(zipf)[2], summary(zipf)$r.squared, exp(coef(hfit)[1]), coef(hfit)[2]),
    justificacion = "Confirman que el chat se comporta como lenguaje natural: pocas palabras muy repetidas y vocabulario que sigue creciendo")

# ---- 7. Figuras ------------------------------------------------------------------------------
#> Nueve figuras: oraciones y palabras por mensaje, palabras más frecuentes
#> sin filtrar, clases de caracteres, signos más usados, subpalabras,
#> bigramas crudos, leyes de Zipf y Heaps, y expresiones con significado.
# F04-01 Oraciones y palabras por mensaje
om <- oraciones[, .N, by = id]
pa <- ggplot(om[N <= 8], aes(factor(N))) + geom_bar(fill = GRAF$color, width = .7) +
  scale_y_continuous(labels = comma) + labs(title = "Oraciones por mensaje", x = "Oraciones", y = "Mensajes")
pb <- ggplot(lim[n_palabras > 0 & n_palabras <= 60], aes(n_palabras)) + geom_histogram(binwidth = 1, fill = "#5EC4B6", color = GRAF$fondo) +
  geom_vline(xintercept = median(lim[n_palabras > 0, n_palabras]), linetype = "22", color = TINTA) +
  annotate_label(x = median(lim[n_palabras > 0, n_palabras]) + 1, y = Inf, vjust = 1.4, hjust = 0, family = FT_TXT, size = 3.4,
           label = sprintf("mediana = %d palabras", median(lim[n_palabras > 0, n_palabras])), fill = "white", label.size = 0) +
  scale_y_continuous(labels = comma) + labs(title = "Palabras por mensaje", x = "Palabras", y = "Mensajes")
p <- (pa | pb) + plot_annotation(title = "Tokenización: oraciones y palabras",
  subtitle = sprintf("%s oraciones y %s palabras en %s mensajes con texto", num(nrow(oraciones)), num(nrow(tok)), num(sum(lim$n_palabras > 0))))
guardar_fig(p, "oraciones_palabras", "Oraciones y palabras por mensaje",
            "Resultado de tokenizar con unnest_tokens(): la gran mayoría de mensajes tiene una sola oración y menos de 15 palabras. Son mensajes cortos, propios de un chat.", h = 5.8)

# F04-02 Palabras más frecuentes sin filtrar
f2 <- freq[1:30][, palabra := factor(palabra, rev(palabra))]
p <- ggplot(f2, aes(N, palabra, fill = vacia)) + geom_col(width = .72) +
  geom_text(aes(label = num(N)), hjust = -.15, family = FT_MONO, size = 3.1) +
  scale_fill_manual(values = c(`TRUE` = "#B7D9D5", `FALSE` = GRAF$color), labels = c(`TRUE` = "Palabra vacía (stopword)", `FALSE` = "Palabra con contenido"), name = NULL) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, .12))) +
  labs(title = "Las 30 palabras más frecuentes, sin filtrar", x = "Frecuencia", y = NULL,
       subtitle = sprintf("Ej6: frecuencia por palabra. %d de las 30 son palabras vacías ('de', 'que', 'el'...): sin quitarlas, ocultan el contenido real",
                          sum(f2$vacia)))
guardar_fig(p, "palabras_frecuentes_crudas", "Palabras más frecuentes sin filtrar",
            "Frecuencia de las 30 palabras más usadas antes de quitar palabras vacías. Aun así ya aparecen términos del dominio como 'alguien', 'lic', 'materia', 'grupo' e 'inf', que anticipan los temas del chat.", h = 8.4)

# F04-03 Clases de caracteres por año
c3 <- car[, .N, by = .(anio, clase)][, p := N / sum(N), by = anio]
c3[, clase := factor(clase, c("Letra", "Espacio", "Puntuación", "Dígito", "Emoji", "Otro símbolo"))]
p <- ggplot(c3, aes(p, factor(anio), fill = clase)) + geom_col(width = .66) +
  geom_text(aes(label = ifelse(p > .025, pct(p, 1), "")), position = position_stack(vjust = .5), color = "white", family = FT_TXT, fontface = "bold", size = 3.4) +
  scale_fill_manual(values = c("#0D9488", "#99D8D0", "#F59E0B", "#6366F1", "#EC4899", "#94A3B8"), name = NULL) +
  scale_x_continuous(labels = percent) +
  labs(title = "Traducción carácter por carácter", x = "Proporción de caracteres", y = NULL,
       subtitle = sprintf("Ej5: cada uno de los %s caracteres se tradujo a su clase Unicode (letra, espacio, puntuación, dígito, emoji)", num(nrow(car))))
guardar_fig(p, "clases_caracteres", "Clases de caracteres por año",
            "Composición de los caracteres del chat según su clase Unicode, por año. Las letras dominan; la puntuación (sobre todo '?') y los emojis son señales expresivas que se aprovechan en el análisis.", h = 5.4)

# F04-04 Signos más usados
sig <- car[clase %in% c("Puntuación", "Otro símbolo"), .N, by = ch][order(-N)][1:16]
signif <- c("?" = "pregunta", "." = "punto", "," = "coma", "!" = "exclamación", ":" = "dos puntos / emoticón", "(" = "paréntesis / emoticón",
            ")" = "paréntesis / emoticón", "¿" = "apertura de pregunta", "-" = "guion (códigos INF-111)", "/" = "barra (fechas, links)",
            "\"" = "comillas", "*" = "negrita de Telegram", "¡" = "apertura de exclamación", "'" = "apóstrofo / emoticón :'v", "_" = "guion bajo",
            "+" = "más (+1)", "@" = "mención", "#" = "hashtag", "%" = "porcentaje", "&" = "y", "=" = "igual", "…" = "puntos suspensivos", "<" = "corazón <3", ";" = "punto y coma")
sig[, etiqueta := sprintf("%s   %s", ch, ifelse(is.na(signif[ch]), "", signif[ch]))]
sig[, etiqueta := factor(etiqueta, rev(etiqueta))]
p <- ggplot(sig, aes(N, etiqueta)) + geom_col(fill = "#F59E0B", width = .7) +
  geom_text(aes(label = num(N)), hjust = -.15, family = FT_MONO, size = 3.2) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, .14))) +
  theme(axis.text.y = element_text(family = FT_MONO, size = 11, color = TINTA)) +
  labs(title = "Signos de puntuación y símbolos más usados", x = "Apariciones", y = NULL,
       subtitle = "Signos de puntuación y símbolos más usados con su función en el chat; el signo de pregunta confirma que es un grupo para resolver dudas")
guardar_fig(p, "signos_mas_usados", "Signos de puntuación y símbolos más usados",
            sprintf("Frecuencia de signos y símbolos con su función. Los tres más usados son %s; el signo de pregunta confirma el carácter de consulta del grupo y varios signos forman emoticones (:v, :( ) que el diccionario de jerga interpreta.", paste0("'", sig$ch[1:3], "'", collapse = ", ")), h = 7)

# F04-05 Subpalabras (shingles)
s5 <- shing_f[1:24][, sub := factor(sub, rev(sub))]
p <- ggplot(s5, aes(N, sub)) + geom_point(size = 5, color = GRAF$color) +
  geom_segment(aes(x = 0, xend = N, yend = sub), color = alpha(GRAF$color, .4), linewidth = 1.6) +
  geom_text(aes(label = num(N)), hjust = -.45, family = FT_MONO, size = 3.2) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, .14))) +
  theme(axis.text.y = element_text(family = FT_MONO, size = 12, color = TINTA)) +
  labs(title = "Tokenización en subpalabras", x = "Frecuencia", y = NULL,
       subtitle = "Los n-gramas de 3 caracteres más comunes ('character shingles'): raíces y terminaciones como 'que', 'ent', 'ion', 'ria' (de materia)")
guardar_fig(p, "subpalabras_shingles", "Subpalabras: n-gramas de caracteres",
            "Tokenización por subpalabras con n-gramas de 3 caracteres. Este nivel es útil para palabras mal escritas o con variantes: 'inscripcion', 'inscribirme' e 'inscritos' comparten fragmentos aunque sean palabras distintas.", h = 7.4)

# F04-06 Bigramas sin filtrar
b6 <- big[, .N, by = bigrama][order(-N)][1:22][, bigrama := factor(bigrama, rev(bigrama))]
p <- ggplot(b6, aes(N, bigrama)) + geom_col(fill = "#5EC4B6", width = .72) +
  geom_text(aes(label = num(N)), hjust = -.15, family = FT_MONO, size = 3.2) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, .12))) +
  labs(title = "Bigramas más frecuentes (sin filtrar)", x = "Frecuencia", y = NULL,
       subtitle = "Pares de palabras consecutivas; ya asoman frases típicas ('alguien sabe', 'por favor', 'de temporada') junto a pares vacíos ('de la')")
guardar_fig(p, "bigramas_crudos", "Bigramas más frecuentes sin filtrar",
            "Bigramas antes de quitar palabras vacías. Frases como 'alguien sabe' o 'por favor' muestran el carácter de consulta del grupo; los pares de palabras vacías se depuran en F05.", h = 7.4)

# F04-07 Ley de Zipf
p <- ggplot(freq, aes(rango, N)) + geom_point(color = alpha(GRAF$color, .35), size = 1.4) +
  geom_abline(intercept = coef(zipf)[1], slope = coef(zipf)[2], color = "#F59E0B", linewidth = 1.2) +
  ggrepel::geom_text_repel(data = freq[rango %in% c(1, 2, 3, 5, 10, 20, 50, 100, 500)], aes(label = palabra), family = FT_TXT, size = 3.4,
                           nudge_x = .25, nudge_y = .15, color = TINTA, segment.color = TINTA3) +
  scale_x_log10(labels = comma) + scale_y_log10(labels = comma) +
  labs(title = "Ley de Zipf: frecuencia frente a rango", x = "Rango de la palabra (escala log)", y = "Frecuencia (escala log)",
       subtitle = sprintf("La frecuencia cae como una potencia del rango: pendiente %.2f (cercana a -1, valor típico del lenguaje natural), R² = %.3f",
                          coef(zipf)[2], summary(zipf)$r.squared))
guardar_fig(p, "ley_zipf", "Ley de Zipf: frecuencia vs rango",
            "Gráfico log-log de frecuencia contra rango. La línea recta confirma la ley de Zipf: unas pocas palabras concentran la mayoría de apariciones, por lo que filtrar palabras vacías cambia mucho los resultados.", h = 6.6)

# F04-08 Ley de Heaps
heaps[, ajuste := exp(predict(hfit, newdata = heaps))]
p <- ggplot(heaps, aes(tokens, vocab)) + geom_line(color = GRAF$color, linewidth = 1.6) +
  geom_line(aes(y = ajuste), color = "#F59E0B", linetype = "22", linewidth = 1) +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Ley de Heaps: crecimiento del vocabulario", x = "Tokens leídos", y = "Palabras distintas vistas",
       subtitle = sprintf("Vocabulario = %.1f · tokens^%.2f. Cada temporada trae nombres de docentes, materias y jerga nuevos", exp(coef(hfit)[1]), coef(hfit)[2]))
guardar_fig(p, "ley_heaps", "Ley de Heaps: crecimiento del vocabulario",
            "Crecimiento del vocabulario a medida que se leen mensajes en orden cronológico, con el ajuste de Heaps (línea punteada). El exponente menor a 1 indica crecimiento cada vez más lento pero sin estancarse.", h = 6)

# F04-09 Expresiones con significado
uj <- L$uso_jerga[mensajes > 0][order(-mensajes)][1:20]
sig_j <- fread(file.path(DIR$dic, "dic_jerga_expresiones.csv"), encoding = "UTF-8")[, .(expresion, significado)]
uj <- merge(uj, sig_j, by = "expresion")[order(mensajes)]
uj[, significado := ifelse(nchar(significado) > 62, paste0(stri_sub(significado, 1, 60), "..."), significado)]
uj[, expresion := factor(expresion, expresion)]
p <- ggplot(uj, aes(mensajes, expresion)) + geom_col(aes(fill = tipo), width = .7) +
  geom_text(aes(label = sprintf("%s   %s", num(mensajes), significado)), hjust = -.04, family = FT_TXT, size = 3, color = TINTA2) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 1.25))) +
  scale_fill_manual(values = colorRampPalette(c("#0D9488", "#2D8CFF", "#9333EA", "#DB2777", "#F26B1D", "#E0A100"))(uniqueN(uj$tipo)), name = NULL) +
  theme(axis.text.y = element_text(family = FT_MONO, size = 12, color = TINTA), legend.position = "bottom") +
  labs(title = "Expresiones y caracteres con significado", x = "Mensajes que la contienen", y = NULL,
       subtitle = "Del diccionario de jerga: emoticones, risas, abreviaturas y bolivianismos con su significado en el chat")
guardar_fig(p, "expresiones_significado", "Expresiones y caracteres con significado",
            sprintf("Expresiones más frecuentes del diccionario de jerga con su significado. Encabezan %s. La insistencia '??', la adhesión 'X2', 'xd/XD' y ':v' son marcas de estilo juvenil; las que tienen sentimiento propio se suman al análisis.", paste0("'", rev(as.character(uj$expresion))[1:4], "'", collapse = ", ")), w = 12, h = 8.2)

fase_fin()
