# =====================================================================
#  F05 | PALABRAS VACÍAS, SHALLOW LINGUISTICS Y TF-IDF
#  Temas de clase: 5. Limpieza de palabras vacías (listas predefinidas,
#  personalización y automatización por frecuencia), 5.1 Shallow
#  linguistics, 6. tidytext + stop words, 7. anti_join y frecuencia,
#  9. TF-IDF. Externo: log-odds ponderado (Monroe et al., 2008) para
#  hallar el "tema latente" de cada mes y diversidad léxica.
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
suppressPackageStartupMessages(library(tidytext))
fase_inicio("F05")

L <- cargar_chk("f03_limpio"); T4 <- cargar_chk("f04_tokens")
lim <- L$limpio; tok <- T4$tok
tok <- merge(tok, lim[, .(id, fecha, anio, mes, periodo, semestre)], by = "id")

# ---- 1. Tres formas de definir palabras vacías ---------------------------
#> (a) Listas predefinidas: Snowball (la de NLTK es casi idéntica) y
#>     stopwords-iso (más amplia). Se normalizan igual que el texto.
#> (b) Personalización: muletillas, saludos y jerga de relleno del chat.
#> (c) Automatización por frecuencia: términos presentes en más del 4% de
#>     los mensajes (no discriminan temas); solo se usan para LDA (F08).
sw_snow <- unique(normalizar_base(stopwords::stopwords("es", source = "snowball")))
sw_iso  <- unique(normalizar_base(stopwords::stopwords("es", source = "stopwords-iso")))
sw_pers <- c("q", "xq", "pq", "porque", "pues", "ps", "nomas", "ya", "ok", "oki", "okey", "hola", "buenas", "buenos", "tardes",
             "noches", "dias", "disculpen", "disculpa", "disculpe", "favor", "porfa", "jaja", "xd", "v", "x", "d", "si", "tambien",
             "solo", "asi", "ahi", "aqui", "alla", "entonces", "osea", "sea", "tal", "vez", "igual", "creo", "hay", "haber", "tener",
             "tiene", "tienen", "tienes", "hacer", "hace", "puede", "pueden", "puedo", "podria", "ser", "estar", "va", "van", "ir",
             "voy", "mas", "menos", "cada", "todo", "todos", "toda", "todas", "otro", "otra", "otros", "otras", "mismo", "misma",
             "aun", "todavia", "recien", "ahora", "cosa", "cosas", "etc", "bro", "chicos", "chicas", "companeros", "companero",
             "gente", "amigo", "amigos", "alguien", "algun", "alguna", "alguno", "algo", "decir", "dice", "dicen", "dijo", "dijeron",
             "ver", "vi", "sale", "salio", "bien", "pasa", "pasar", "dar", "da", "uno", "dos", "e", "o", "u", "y", "a", "c", "m", "s", "l", "p", "g", "b")
STOP <- unique(c(sw_snow, sw_pers))
df_msg <- tok[, .(n = uniqueN(id)), by = palabra][, prop := n / uniqueN(tok$id)]
sw_auto <- setdiff(df_msg[prop > .04, palabra], STOP)
STOP_TOPICOS <- unique(c(STOP, sw_auto, "materia", "materias", "grupo", "lic", "licenciado", "licenciada", "docente", "docentes",
                         "clase", "clases", "temporada", "curso", "cursos", "carrera", "sabe", "saben", "sabes", "pregunta", "consulta",
                         "gracias", "muchas", "hoy", "dia", "manana", "semana", "bueno", "buen", "buena", "siempre", "ano", "anos", "cuenta",
                         "inf", "mat", "ayer", "nadie", "nada", "verdad", "favor", "mejor", "semestre", "parte", "caso", "forma", "ningun", "ninguna"))
writeLines(STOP, file.path(DIR$dic, "stopwords_proyecto.txt"))
writeLines(STOP_TOPICOS, file.path(DIR$dic, "stopwords_topicos.txt"))
sent_palabras <- c("bien", "mal", "nunca", "nada", "mejor", "peor", "bueno", "buena", "nadie", "siempre", "claro", "verdad", "tampoco", "sin", "no", "muy", "mucho", "poco")
comp_sw <- data.table(lista = c("Snowball (NLTK)", "stopwords-iso", "Personalizada del chat", "Lista final (Snowball + personalizada)"),
                      palabras = c(length(sw_snow), length(sw_iso), length(sw_pers), length(STOP)),
                      tokens_quitados = c(mean(tok$palabra %in% sw_snow), mean(tok$palabra %in% sw_iso), mean(tok$palabra %in% sw_pers), mean(tok$palabra %in% STOP)),
                      con_sentimiento = c(paste(intersect(sw_snow, sent_palabras), collapse = ", "), paste(intersect(sw_iso, sent_palabras), collapse = ", "),
                                          paste(intersect(sw_pers, sent_palabras), collapse = ", "), paste(intersect(STOP, sent_palabras), collapse = ", ")))
guardar_tabla(comp_sw, "comparacion_listas_stopwords")
bit("1. Palabras vacías", "Lista final = Snowball + personalizada del chat",
    sprintf("Snowball %d, iso %d, personalizada %d -> final %d palabras (quita %s de los tokens). Automáticas por frecuencia para LDA: %s",
            length(sw_snow), length(sw_iso), length(sw_pers), length(STOP), pct(comp_sw$tokens_quitados[4]), paste(sw_auto, collapse = ", ")),
    archivo = "03_Diccionarios/stopwords_proyecto.txt",
    justificacion = "stopwords-iso elimina palabras con carga emocional (bien, mal, mejor, peor, nunca, siempre); Snowball es más prudente y se completa con la jerga del chat")

# ---- 2. Shallow linguistics: anti_join y frecuencia ------------------------------
#> Limpieza -> dividir en palabras -> anti_join con las palabras vacías ->
#> conteo. Se descartan tokens de una letra y números sueltos.
tok_c <- as.data.table(dplyr::anti_join(tok, data.table(palabra = STOP), by = "palabra"))[nchar(palabra) > 1]
freq_c <- tok_c[, .N, by = palabra][order(-N)]
guardar_tabla(freq_c, "frecuencia_palabras_limpia")
bit("2. Shallow linguistics", "anti_join() de tokens con la lista de palabras vacías",
    sprintf("%s tokens -> %s tokens con contenido (%s palabras distintas). Top 5: %s", num(nrow(tok)), num(nrow(tok_c)),
            num(nrow(freq_c)), paste(freq_c$palabra[1:5], collapse = ", ")),
    antes = nrow(tok), despues = nrow(tok_c),
    justificacion = "Tema 7 de clase: limpiar, dividir palabras, anti_join y frecuencia")

# ---- 3. Frases: bigramas y trigramas con contenido ---------------------------------
#> Se conserva un n-grama si al menos la mitad de sus palabras tiene
#> contenido y no empieza ni termina en artículo o preposición.
art <- c("de", "la", "el", "los", "las", "y", "a", "en", "que", "con", "del", "al", "un", "una", "se", "lo", "o", "e", "su", "sus", "le", "les", "me", "te", "mi", "es")
filtra_ngr <- function(v) {
  w <- strsplit(v, " ", fixed = TRUE)
  vapply(w, function(z) mean(!(z %in% STOP)) >= .5 && !(z[1] %in% art) && !(z[length(z)] %in% art) && all(nchar(z) > 1), TRUE)
}
big_f <- T4$big[, .N, by = bigrama][N >= 3][filtra_ngr(bigrama)][order(-N)]
tri_f <- T4$tri[, .N, by = trigrama][N >= 3][filtra_ngr(trigrama)][order(-N)]
guardar_tabla(big_f, "bigramas_limpios"); guardar_tabla(tri_f, "trigramas_limpios")
bit("3. Frases", "Bigramas y trigramas con contenido",
    sprintf("%s bigramas y %s trigramas distintos con 3+ apariciones. Top: '%s', '%s'", num(nrow(big_f)), num(nrow(tri_f)), big_f$bigrama[1], tri_f$trigrama[1]),
    justificacion = "Las frases más usadas son parte de lo pedido para el dashboard")

# ---- 4. TF-IDF (tema 9) ---------------------------------------------------------------
#> TF-IDF = frecuencia del término en el documento x log(documentos /
#> documentos que lo contienen). Documento = año, o tipo de periodo.
#> Una palabra con TF-IDF alto es frecuente en un año (o periodo) y rara en
#> los demás: describe lo propio de ese año y no lo que se repite siempre.
tfidf_anio <- as.data.table(bind_tf_idf(tok_c[, .N, by = .(anio, palabra)], palabra, anio, N))
tok_c[, tipo_periodo := sub(" \\d{4}$", "", periodo)]
tfidf_per <- as.data.table(bind_tf_idf(tok_c[, .N, by = .(tipo_periodo, palabra)][N >= 3], palabra, tipo_periodo, N))
guardar_tabla(tfidf_anio[order(anio, -tf_idf)][, head(.SD, 25), by = anio], "tfidf_anio")
guardar_tabla(tfidf_per[order(tipo_periodo, -tf_idf)][, head(.SD, 25), by = tipo_periodo], "tfidf_periodo")
bit("4. TF-IDF", "bind_tf_idf() con documento = año y documento = tipo de periodo",
    sprintf("Palabra con mayor TF-IDF por año: %s", paste(tfidf_anio[order(-tf_idf), .SD[1], by = anio][order(anio), sprintf("%s='%s'", anio, palabra)], collapse = "; ")),
    archivo = "04_Resultados/tablas/F05_tfidf_anio.csv",
    justificacion = "Tema 9 de clase: identifica lo distintivo de cada año y periodo académico")

# ---- 5. Log-odds ponderado por mes: "tema latente" -----------------------------------
#> Para cada mes se compara la frecuencia de cada palabra con el resto de
#> meses usando un prior de Dirichlet informativo (Monroe et al., 2008);
#> el z-score alto marca palabras propias de ese mes y no solo frecuentes.
lo <- tok_c[, .N, by = .(mes, palabra)]
alpha <- lo[, .(a = sum(N)), by = palabra]; a0 <- sum(alpha$a)
lo <- merge(lo, alpha, by = "palabra")
lo[, `:=`(n_mes = sum(N)), by = mes]
lo[, n_resto := a - N]; tot <- sum(lo$N)
lo[, n_mes_resto := tot - n_mes]
lo[, delta := log((N + a) / (n_mes + a0 - N - a)) - log((n_resto + a) / (n_mes_resto + a0 - n_resto - a))]
lo[, var := 1 / (N + a) + 1 / (n_resto + a)]
lo[, z := delta / sqrt(var)]
latente <- lo[N >= 4][order(mes, -z)][, head(.SD, 5), by = mes]
guardar_tabla(latente, "tema_latente_mes")
bit("5. Log-odds por mes", "Palabras distintivas de cada mes frente al resto (tema latente)",
    sprintf("Ejemplos de tema latente: %s", paste(latente[, .SD[1], by = mes][order(-z)][1:4, sprintf("%s='%s'", mes, palabra)], collapse = "; ")),
    archivo = "04_Resultados/tablas/F05_tema_latente_mes.csv",
    justificacion = "El log-odds con z-score evita que palabras raras o muy frecuentes dominen, a diferencia del TF-IDF con pocos documentos")

# ---- 6. Diversidad léxica por mes ----------------------------------------------------------
#> TTR estandarizado: proporción de palabras distintas en muestras de 1,000
#> tokens (promedio de 20 muestras) para que los meses sean comparables.
set.seed(CFG$semilla)
div <- tok_c[, .(ttr = if (.N >= 1000) mean(replicate(20, uniqueN(sample(palabra, 1000)) / 1000)) else NA_real_, tokens = .N), by = mes][!is.na(ttr)]
bit("6. Diversidad léxica", sprintf("TTR estandarizado en %d meses con 1,000 o más palabras", nrow(div)),
    sprintf("Rango: %.2f a %.2f; promedio %.2f", min(div$ttr), max(div$ttr), mean(div$ttr)),
    justificacion = "Muestra si en temporada se habla de pocos temas repetidos (TTR bajo) o de temas variados (TTR alto)")

guardar_chk(list(tok_c = tok_c[, .(id, pos, palabra)], STOP = STOP, STOP_TOPICOS = STOP_TOPICOS, big_f = big_f, tri_f = tri_f,
                 tfidf_anio = tfidf_anio, tfidf_per = tfidf_per, latente = latente), "f05_stop")

# ---- 7. Figuras ----------------------------------------------------------------------------
#> Nueve figuras: comparación de listas de palabras vacías, palabras con
#> contenido más usadas, ranking antes y después del filtro, frases más
#> usadas, red de bigramas, TF-IDF por año y por periodo, tema latente de
#> cada mes y diversidad léxica.
# F05-01 Comparación de listas
c1 <- copy(comp_sw)[, lista := factor(lista, rev(lista))]
pa <- ggplot(c1, aes(palabras, lista)) + geom_col(fill = GRAF$color, width = .6) +
  geom_text(aes(label = palabras), hjust = -.2, family = FT_MONO, size = 3.6) + scale_x_continuous(expand = expansion(c(0, .15))) +
  labs(title = "Tamaño de la lista", x = "Palabras", y = NULL)
pb <- ggplot(c1, aes(tokens_quitados, lista)) + geom_col(fill = "#7CC4EE", width = .6) +
  geom_text(aes(label = pct(tokens_quitados)), hjust = -.2, family = FT_MONO, size = 3.6) + scale_x_continuous(labels = percent, expand = expansion(c(0, .2))) +
  labs(title = "Tokens que elimina", x = "Proporción del texto", y = NULL) + theme(axis.text.y = element_blank())
pc <- ggplot(c1, aes(1, lista)) + geom_tile(fill = "white", width = 1, height = .78) +
  geom_text(aes(label = vapply(con_sentimiento, function(z) paste(stri_wrap(z, 46), collapse = "\n"), "")), family = FT_TXT, size = 3, color = "#B42318", lineheight = .9) +
  labs(title = "Palabras con carga emocional que borraría", x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.grid.major = element_blank())
p <- (pa | pb | pc) + plot_layout(widths = c(1, 1, 1.5)) +
  plot_annotation(title = "Comparación de listas de palabras vacías",
                  subtitle = "stopwords-iso borra 'bien', 'mal', 'mejor', 'peor', 'nunca' o 'siempre', que cargan sentimiento. Se eligió Snowball + una lista personalizada del chat")
guardar_fig(p, "comparacion_stopwords", "Comparación de listas de palabras vacías",
            "Tamaño de cada lista, proporción de tokens que elimina y palabras con carga emocional que borraría. La lista final combina Snowball (prudente) con muletillas y jerga del chat; la negación se trata antes, sobre el texto completo, así que quitar 'no' aquí no afecta al análisis de sentimientos.", w = 13, h = 5.8)

# F05-02 Top palabras con contenido
f2 <- freq_c[1:30][, palabra := factor(palabra, rev(palabra))]
p <- ggplot(f2, aes(N, palabra)) +
  geom_segment(aes(x = 0, xend = N, yend = palabra), color = alpha(GRAF$color, .4), linewidth = 2) +
  geom_point(size = 4.6, color = GRAF$color) + geom_text(aes(label = num(N)), hjust = -.45, family = FT_MONO, size = 3.2) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, .14))) +
  labs(title = "Las 30 palabras con contenido más usadas", x = "Frecuencia", y = NULL,
       subtitle = "Después del anti_join con las palabras vacías: el vocabulario del chat gira en torno a materias, docentes, grupos, horarios e inscripciones")
guardar_fig(p, "palabras_contenido_top", "Palabras con contenido más usadas",
            "Frecuencia de las 30 palabras más usadas tras quitar palabras vacías (shallow linguistics). El léxico es académico-administrativo: materia, lic, grupo, paralelo, horario, inscripción, kardex.", h = 8.4)

# F05-03 Antes vs después (slopegraph)
r_antes <- T4$freq[1:15, .(palabra, rango = rango)]
r_desp  <- freq_c[1:15][, .(palabra, rango = seq_len(.N))]
p <- ggplot() +
  geom_text(data = r_antes, aes(1, -rango, label = palabra), hjust = 1, family = FT_TXT, size = 4, color = TINTA3) +
  geom_text(data = r_desp, aes(2, -rango, label = palabra), hjust = 0, family = FT_TXT, fontface = "bold", size = 4, color = GRAF$color) +
  geom_segment(data = merge(r_antes, r_desp, by = "palabra"), aes(x = 1.04, xend = 1.96, y = -rango.x, yend = -rango.y), color = GRAF$color) +
  annotate("text", x = c(1, 2), y = 0.4, label = c("Antes (texto crudo)", "Después (sin palabras vacías)"), hjust = c(1, 0), family = FT_TIT, fontface = "bold.italic", size = 5) +
  scale_x_continuous(limits = c(.45, 2.6)) + theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Ranking de palabras antes y después de quitar palabras vacías", x = NULL, y = NULL,
       subtitle = sprintf("Ranking de las 15 palabras más frecuentes antes y después; solo %s estaban ya en el top crudo", paste0("'", intersect(r_desp$palabra, r_antes$palabra), "'", collapse = ", ")))
guardar_fig(p, "ranking_antes_despues", "Ranking de palabras antes y después de quitar palabras vacías",
            "Comparación del top 15 de palabras antes y después del filtrado. En el texto crudo dominan artículos y preposiciones; tras el filtrado aparecen las palabras que describen los temas reales.", w = 10, h = 7)

# F05-04 Frases más usadas
b4 <- big_f[1:16][, frase := factor(bigrama, rev(bigrama))]
t4 <- tri_f[1:16][, frase := factor(trigrama, rev(trigrama))]
pa <- ggplot(b4, aes(N, frase)) + geom_col(fill = GRAF$color, width = .7) + geom_text(aes(label = N), hjust = -.2, family = FT_MONO, size = 3) +
  scale_x_continuous(expand = expansion(c(0, .15))) + labs(title = "Bigramas", x = "Frecuencia", y = NULL)
pb <- ggplot(t4, aes(N, frase)) + geom_col(fill = "#7CC4EE", width = .7) + geom_text(aes(label = N), hjust = -.2, family = FT_MONO, size = 3) +
  scale_x_continuous(expand = expansion(c(0, .15))) + labs(title = "Trigramas", x = "Frecuencia", y = NULL)
p <- (pa | pb) + plot_annotation(title = "Frases más usadas: bigramas y trigramas",
  subtitle = sprintf("N-gramas con contenido. Las frases más repetidas son %s (bigramas) y %s (trigramas)",
                     paste0("'", big_f$bigrama[1:3], "'", collapse = ", "), paste0("'", tri_f$trigrama[1:3], "'", collapse = ", ")))
guardar_fig(p, "frases_mas_usadas", "Frases más usadas: bigramas y trigramas",
            "Bigramas y trigramas más frecuentes con contenido. Las frases revelan la función del grupo: preguntar por materias, docentes y trámites, y pedir ayuda.", w = 13, h = 7.4)

# F05-05 Red de bigramas
red <- T4$big[, .N, by = bigrama][N >= 6]
red[, c("w1", "w2") := tstrsplit(bigrama, " ", fixed = TRUE)]
red <- red[!(w1 %in% STOP) & !(w2 %in% STOP) & nchar(w1) > 2 & nchar(w2) > 2][order(-N)][1:min(.N, 70)]
g <- igraph::graph_from_data_frame(red[, .(w1, w2, N)], directed = TRUE)
set.seed(CFG$semilla); lay <- igraph::layout_with_fr(g, niter = 2000)
nod <- data.table(nombre = igraph::V(g)$name, x = lay[, 1], y = lay[, 2], grado = igraph::degree(g))
ed <- cbind(red, nod[match(red$w1, nombre), .(x1 = x, y1 = y)], nod[match(red$w2, nombre), .(x2 = x, y2 = y)])
p <- ggplot() +
  geom_segment(data = ed, aes(x1, y1, xend = x2, yend = y2, linewidth = N, alpha = N), color = GRAF$color,
               arrow = arrow(length = unit(.12, "cm"), type = "closed")) +
  geom_point(data = nod, aes(x, y, size = grado), color = "#0B4A6F", fill = "#E0F2FE", shape = 21, stroke = 1.1) +
  ggrepel::geom_text_repel(data = nod, aes(x, y, label = nombre), family = FT_TXT, size = 3.3, color = TINTA, max.overlaps = 40, box.padding = .25) +
  scale_linewidth(range = c(.3, 2.4), guide = "none") + scale_alpha(range = c(.35, .9), guide = "none") + scale_size(range = c(1.5, 6), guide = "none") +
  theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Red de bigramas", x = NULL, y = NULL,
       subtitle = "Las flechas unen palabras consecutivas frecuentes (sin palabras vacías); el grosor indica cuántas veces aparece el par")
guardar_fig(p, "red_bigramas", "Red de bigramas",
            "Grafo dirigido de los bigramas con contenido más frecuentes (algoritmo de Fruchterman-Reingold). Se forman racimos temáticos: nombres de materias (base de datos, sistemas operativos), trámites (adición, retiro, kardex) y la comunicación (grupo whatsapp, link).", w = 12, h = 9)

# F05-06 TF-IDF por año
t6 <- tfidf_anio[order(anio, -tf_idf)][, head(.SD, 10), by = anio]
t6[, palabra2 := reorder_within(palabra, tf_idf, anio)]
p <- ggplot(t6, aes(tf_idf, palabra2, fill = factor(anio))) + geom_col(width = .72, show.legend = FALSE) +
  facet_wrap(~anio, scales = "free", ncol = 4) + scale_y_reordered() +
  scale_fill_manual(values = c("#7CC4EE", "#0EA5E9", "#0369A1", "#0B3A5E")) +
  scale_x_continuous(labels = function(x) formatC(x * 1e4, format = "f", digits = 1)) +
  labs(title = "TF-IDF por año", x = "TF-IDF (x 10 000)", y = NULL,
       subtitle = "Palabras frecuentes en un año y raras en los demás: nombres de docentes, materias nuevas, cambios de pensum y eventos")
guardar_fig(p, "tfidf_por_anio", "TF-IDF por año",
            "Las 10 palabras con mayor TF-IDF en cada año. Revelan qué fue propio de cada año (docentes asignados, materias habilitadas, cambios de plan de estudios), algo que la frecuencia simple no muestra.", w = 13, h = 6.4)

# F05-07 TF-IDF por tipo de periodo
t7 <- tfidf_per[order(tipo_periodo, -tf_idf)][, head(.SD, 10), by = tipo_periodo]
t7[, palabra2 := reorder_within(palabra, tf_idf, tipo_periodo)]
t7[, tipo_periodo := factor(tipo_periodo, c("Verano", "Semestre I", "Invierno", "Semestre II"))]
p <- ggplot(t7, aes(tf_idf, palabra2, fill = tipo_periodo)) + geom_col(width = .72, show.legend = FALSE) +
  facet_wrap(~tipo_periodo, scales = "free", ncol = 4) + scale_y_reordered() +
  scale_fill_manual(values = c(Verano = "#F59E0B", `Semestre I` = "#0EA5E9", Invierno = "#6366F1", `Semestre II` = "#14B8A6")) +
  scale_x_continuous(labels = function(x) formatC(x * 1e4, format = "f", digits = 1)) +
  labs(title = "TF-IDF por periodo académico", x = "TF-IDF (x 10 000)", y = NULL,
       subtitle = "Verano e invierno (cursos de temporada) frente a los semestres regulares, juntando todos los años")
guardar_fig(p, "tfidf_por_periodo", "TF-IDF por periodo académico",
            sprintf("Palabras más distintivas de cada tipo de periodo. En verano destacan %s; en invierno, %s; en el semestre I, %s; en el semestre II, %s. Las temporadas giran en torno a pagos e inscripción y los semestres, en torno a la vida institucional.",
                    paste(t7[tipo_periodo == "Verano", head(palabra, 4)], collapse = ", "), paste(t7[tipo_periodo == "Invierno", head(palabra, 4)], collapse = ", "),
                    paste(t7[tipo_periodo == "Semestre I", head(palabra, 4)], collapse = ", "), paste(t7[tipo_periodo == "Semestre II", head(palabra, 4)], collapse = ", ")), w = 13, h = 6.4)

# F05-08 Tema latente del mes
lt <- latente[, .SD[1], by = mes][, fecha := as.Date(paste0(mes, "-01"))]
lt <- merge(lt, tok_c[, .N, by = mes][, .(mes, msgs = N)], by = "mes")
lt[, tipo := sub(" \\d{4}$", "", periodo_academico(fecha + 14))]
p <- ggplot(lt, aes(fecha, z)) +
  geom_segment(aes(xend = fecha, y = 0, yend = z, color = tipo), linewidth = 1.1) +
  geom_point(aes(size = msgs, color = tipo)) +
  ggrepel::geom_text_repel(aes(label = palabra), family = FT_TIT, fontface = "bold.italic", size = 3.7, color = TINTA,
                           direction = "y", nudge_y = 1.2, segment.color = alpha(TINTA3, .5), max.overlaps = 50) +
  scale_color_manual(values = c(Verano = "#F59E0B", `Semestre I` = "#0EA5E9", Invierno = "#6366F1", `Semestre II` = "#14B8A6"), name = "Periodo") +
  scale_size(range = c(2, 7), name = "Tokens del mes", labels = comma) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes) +
  labs(title = "Tema latente de cada mes (log-odds ponderado)", x = NULL, y = "Fuerza (z del log-odds)",
       subtitle = "Palabra más característica de cada mes frente a todos los demás meses (log-odds ponderado con prior de Dirichlet)")
guardar_fig(p, "tema_latente_mes", "Tema latente de cada mes (log-odds ponderado)",
            "Para cada mes se muestra la palabra que más lo distingue del resto. Así aparecen temas que se pierden al mirar un año completo: un docente nuevo, una materia que se intentó habilitar, un problema del sistema o un evento puntual.", w = 13, h = 7.4)

# F05-09 Diversidad léxica
p <- ggplot(div, aes(as.Date(paste0(mes, "-01")), ttr)) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#0369A1", fill = alpha("#7CC4EE", .3), span = .5) +
  geom_point(aes(size = tokens), color = GRAF$color, alpha = .85) +
  scale_size(range = c(2, 7), name = "Tokens del mes", labels = comma) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes) + scale_y_continuous(labels = percent) +
  labs(title = "Diversidad léxica por mes", x = NULL, y = "Palabras distintas por cada 1,000",
       subtitle = sprintf("TTR estandarizado (promedio de 20 muestras de 1,000 tokens). Correlación entre volumen del mes y diversidad: r = %.2f", cor(div$tokens, div$ttr)))
guardar_fig(p, "diversidad_lexica", "Diversidad léxica por mes",
            sprintf("Proporción de palabras distintas por cada 1,000 tokens (TTR estandarizado). La correlación entre el volumen del mes y su diversidad es %.2f: %s", cor(div$tokens, div$ttr),
                    ifelse(cor(div$tokens, div$ttr) < -0.2, "los meses con más mensajes repiten más vocabulario (muchas preguntas sobre los mismos trámites).", "el volumen de mensajes casi no cambia la variedad del vocabulario.")), h = 6)

fase_fin()
