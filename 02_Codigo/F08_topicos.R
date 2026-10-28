# =====================================================================
#  F08 | MODELADO DE TÓPICOS (LDA)
#  Temas de clase: 5.2 Topic modeling (LDA, LSA), 8. Ejemplo de LDA
#  (limpiar, tokenizar, matriz de términos, cast, LDA, beta por tópico,
#  gamma por documento) y 10. Tópicos más frecuentes con tm
#  (Corpus + DocumentTermMatrix + LDA).
#  Decisión clave: los mensajes son muy cortos (mediana 7 palabras), así que
#  cada DOCUMENTO es un DÍA de conversación; luego el modelo se aplica a
#  cada mensaje para conocer su tópico. k se elige con perplejidad en datos
#  de prueba y coherencia UMass (métodos externos a la clase).
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
suppressPackageStartupMessages({ library(tidytext); library(topicmodels) })
fase_inicio("F08")

L <- cargar_chk("f03_limpio"); F5 <- cargar_chk("f05_stop"); F7 <- cargar_chk("f07_sent")
lim <- L$limpio
tk <- merge(F5$tok_c, lim[, .(id, fecha)], by = "id")[!(palabra %in% F5$STOP_TOPICOS) & nchar(palabra) > 2]

# ---- 1. Documentos = días y matriz documento-término ---------------------
#> LDA necesita documentos con suficientes palabras. Un mensaje del chat
#> tiene en promedio pocas palabras, así que cada documento es un día de
#> conversación (días con 5 o más mensajes analizables). Se conservan los
#> términos que aparecen 8 o más veces en al menos 3 días y se arma la
#> matriz documento-término con cast_dtm() (filas = días, columnas =
#> términos, celdas = frecuencia).
dias_ok <- lim[analizable == TRUE, .N, by = fecha][N >= CFG$min_msgs_dia_lda, fecha]
vocab <- tk[fecha %in% dias_ok, .(n = .N, d = uniqueN(fecha)), by = palabra][n >= 8 & d >= 3, palabra]
cnt <- tk[fecha %in% dias_ok & palabra %in% vocab, .N, by = .(doc = as.character(fecha), palabra)]
dtm <- cast_dtm(cnt, doc, palabra, N)
bit("1. Matriz documento-término", "cast_dtm(): un documento por día con 5 o más mensajes",
    sprintf("%d días x %s términos (términos con 8+ apariciones en 3+ días; sin palabras vacías de tópicos)", nrow(dtm), num(ncol(dtm))),
    antes = nrow(lim), despues = nrow(dtm),
    justificacion = "LDA necesita documentos con varias palabras; un mensaje de 7 palabras no alcanza para estimar una mezcla de temas")

# ---- 2. Elección de k --------------------------------------------------------
#> Perplejidad: qué tan bien predice el modelo días que no vio (20% de
#> prueba); menor es mejor. Coherencia UMass: si las palabras top de cada
#> tópico aparecen juntas en los mismos días; mayor es mejor.
set.seed(CFG$semilla)
prueba <- sample(nrow(dtm), round(.2 * nrow(dtm)))
bin <- as.matrix(dtm) > 0
umass <- function(m, n_top = 10) {
  tp <- terms(m, n_top); mean(apply(tp, 2, function(w) {
    w <- w[w %in% colnames(bin)]; s <- 0; c <- 0
    for (i in 2:length(w)) for (j in 1:(i - 1)) { s <- s + log((sum(bin[, w[i]] & bin[, w[j]]) + 1) / sum(bin[, w[j]])); c <- c + 1 }
    s / c }))
}
sel <- rbindlist(lapply(CFG$k_topicos, function(k) {
  m_tr <- LDA(dtm[-prueba, ], k = k, control = list(seed = CFG$semilla))
  m_all <- LDA(dtm, k = k, control = list(seed = CFG$semilla))
  data.table(k = k, perplejidad = perplexity(m_tr, newdata = dtm[prueba, ]), coherencia = umass(m_all))
}))
sel[, `:=`(perp_n = (perplejidad - min(perplejidad)) / diff(range(perplejidad)), coh_n = (coherencia - min(coherencia)) / diff(range(coherencia)))]
sel[, puntaje := coh_n - perp_n]
K <- sel[which.max(puntaje), k]
guardar_tabla(sel, "seleccion_k")
bit("2. Elección de k", sprintf("k = %d tópicos", K),
    sprintf("Se probaron k = %s; se elige el mejor equilibrio entre baja perplejidad (prueba) y alta coherencia UMass",
            paste(range(CFG$k_topicos), collapse = " a ")),
    justificacion = "Elegir k a ojo es subjetivo; dos métricas complementarias lo hacen reproducible")

# ---- 3. Modelo final, beta y gamma ----------------------------------------
lda <- LDA(dtm, k = K, control = list(seed = CFG$semilla))
beta <- as.data.table(tidy(lda, matrix = "beta"))
gamma <- as.data.table(tidy(lda, matrix = "gamma"))[, fecha := as.Date(document)]
top_t <- beta[order(topic, -beta)][, head(.SD, 15), by = topic]
#> Nombre automático: se compara el top 15 de cada tópico con un pequeño
#> léxico de temas del ámbito universitario; si no hay coincidencia, el
#> nombre son sus 3 palabras principales. Así funciona con otro dataset.
TEMAS <- list(
  "Inscripciones y sistema" = c("inscripcion", "inscribirme", "inscribir", "inscribirse", "ssa", "sistema", "sorteo", "matricula", "inscritos", "inscripciones", "aparece", "cupo", "cupos", "adicion", "retiro"),
  "Pagos y trámites" = c("cpt", "pago", "pagar", "deposito", "boleta", "banco", "qr", "devolucion", "kardex", "carta", "codigo", "verificar", "pague", "documentos", "oficina"),
  "Docentes y recomendaciones" = c("recomiendan", "ensena", "examenes", "examen", "aprobar", "dificil", "facil", "buen", "exigente", "notas", "pasar", "tal", "paralelo", "mejor"),
  "Horarios, aulas y clases" = c("horario", "horarios", "aula", "hora", "manana", "tarde", "noche", "lunes", "virtual", "presencial", "piso", "clases", "pasando", "laboratorio", "choque"),
  "Grupos y coordinación" = c("whatsapp", "link", "enlace", "grupo", "grupos", "unirse", "telegram", "classroom", "pasen", "paso"),
  "Habilitación de materias" = c("habilitar", "habilita", "habilito", "habilitado", "habilitada", "abrir", "abre", "inscritos", "minimo", "formulario", "habiliten", "habilitaron"),
  "Plan de estudios y menciones" = c("pensum", "plan", "antiguo", "nuevo", "mencion", "menciones", "convalidar", "requisito", "nivel", "optativas", "tecnico", "superior"),
  "Vida universitaria y eventos" = c("evento", "carrera", "estudiantes", "elecciones", "votar", "propuestas", "director", "cyber", "comunidad", "auditorio", "universidad", "facultad"))
top20 <- beta[order(topic, -beta)][, head(.SD, 20), by = topic]
sc <- sapply(TEMAS, function(t) top20[, .(s = sum(beta[term %in% t])), by = topic][order(topic), s])
sc <- matrix(sc, nrow = K, dimnames = list(NULL, names(TEMAS)))
asig <- rep(NA_character_, K); m <- sc
while (any(m > 0, na.rm = TRUE) && any(is.na(asig))) {
  ij <- which(m == max(m, na.rm = TRUE), arr.ind = TRUE)[1, ]
  asig[ij[1]] <- colnames(m)[ij[2]]; m[ij[1], ] <- NA; m[, ij[2]] <- NA
}
etq <- top_t[, .(top = paste(head(term, 6), collapse = ", ")), by = topic]
etq[, nombre := ifelse(is.na(asig[topic]), vapply(top, function(t) paste(head(strsplit(t, ", ")[[1]], 3), collapse = " · "), ""), asig[topic])]
etq[, etiqueta := sprintf("T%d · %s", topic, nombre)]
guardar_tabla(etq, "topicos_etiquetas")
for (i in seq_len(nrow(etq))) bit("3. Tópicos", etq$etiqueta[i], paste("Palabras clave:", etq$top[i]), justificacion = "Beta: probabilidad de cada palabra dentro del tópico")

# ---- 4. Tópico de cada mensaje -------------------------------------------------
#> El modelo se entrenó con días, pero se puede aplicar a mensajes sueltos:
#> posterior() calcula para cada mensaje (con 2 o más términos del
#> vocabulario) la probabilidad de cada tópico y se asigna el de mayor
#> probabilidad. Esto permite contar tópicos en cualquier rango de fechas y
#> cruzarlos con los sentimientos.
tkm <- tk[palabra %in% colnames(dtm), .N, by = .(id, palabra)]
ids_ok <- tkm[, .(n = sum(N)), by = id][n >= 2, id]
dtm_m <- cast_dtm(tkm[id %in% ids_ok], id, palabra, N)
post <- posterior(lda, newdata = dtm_m)$topics
msg_top <- data.table(id = rownames(post), topico = max.col(post), prob = apply(post, 1, max))
msg_top <- merge(msg_top, F7$ds[, .(id, fecha, mes, periodo, sentimiento, polaridad)], by = "id")
bit("4. Tópico por mensaje", "posterior() del modelo aplicado a cada mensaje",
    sprintf("%s mensajes con 2+ términos del vocabulario reciben un tópico dominante (probabilidad media %.2f)", num(nrow(msg_top)), mean(msg_top$prob)),
    justificacion = "Permite filtrar tópicos por cualquier rango de fechas en el dashboard y cruzarlos con sentimientos")

# ---- 5. Tema 10: el mismo análisis con tm ----------------------------------------
#> Corpus -> tm_map (minúsculas, sin puntuación ni números, sin palabras
#> vacías, sin espacios extra) -> DocumentTermMatrix -> LDA -> terms().
txt_dia <- lim[analizable == TRUE & fecha %in% dias_ok, .(texto = paste(texto_limpio, collapse = " ")), by = fecha][order(fecha)]
corp <- tm::VCorpus(tm::VectorSource(txt_dia$texto))
corp <- tm::tm_map(corp, tm::content_transformer(tolower))
corp <- tm::tm_map(corp, tm::removePunctuation); corp <- tm::tm_map(corp, tm::removeNumbers)
corp <- tm::tm_map(corp, tm::removeWords, F5$STOP_TOPICOS); corp <- tm::tm_map(corp, tm::stripWhitespace)
dtm_tm <- tm::DocumentTermMatrix(corp, control = list(wordLengths = c(3, Inf), bounds = list(global = c(3, Inf))))
dtm_tm <- dtm_tm[slam::row_sums(dtm_tm) > 0, ]
lda_tm <- LDA(dtm_tm, k = K, control = list(seed = CFG$semilla))
t_tm <- terms(lda_tm, 10); t_td <- terms(lda, 10)
jac <- outer(seq_len(K), seq_len(K), Vectorize(function(i, j) length(intersect(t_td[, i], t_tm[, j])) / length(union(t_td[, i], t_tm[, j]))))
emp <- apply(jac, 1, max)
bit("5. Tema 10 con tm", "Corpus + DocumentTermMatrix + LDA con el paquete tm",
    sprintf("%d x %s; similitud de Jaccard media del mejor emparejamiento con tidytext: %.2f", nrow(dtm_tm), num(ncol(dtm_tm)), mean(emp)),
    justificacion = "Dos caminos de preprocesamiento distintos llegan a tópicos parecidos: los temas son estables")

guardar_chk(list(lda = lda, K = K, beta = beta, gamma = gamma, etq = etq, msg_top = msg_top, sel = sel, jac = jac, vocab = colnames(dtm)), "f08_lda")
saveRDS(lda, file.path(DIR$mod, "modelo_lda.rds"))

# ---- 6. Figuras -------------------------------------------------------------------------
#> Siete figuras: elección de k, palabras de cada tópico, evolución mensual,
#> tópicos por periodo académico, sentimientos dentro de cada tópico,
#> comparación tidytext frente a tm y mensajes de ejemplo de cada tópico.
COLT <- setNames(colorRampPalette(c("#DB2777", "#F97316", "#EAB308", "#16A34A", "#0EA5E9", "#6366F1", "#A855F7", "#E11D48"))(K), etq$etiqueta)
# F08-01 Elección de k
pa <- ggplot(sel, aes(k, perplejidad)) + geom_line(color = GRAF$color, linewidth = 1.3) + geom_point(size = 3, color = GRAF$color) +
  geom_vline(xintercept = K, linetype = "22") + labs(title = "Perplejidad en días de prueba (menor es mejor)", x = "k", y = NULL)
pb <- ggplot(sel, aes(k, coherencia)) + geom_line(color = "#6366F1", linewidth = 1.3) + geom_point(size = 3, color = "#6366F1") +
  geom_vline(xintercept = K, linetype = "22") + labs(title = "Coherencia UMass (mayor es mejor)", x = "k", y = NULL)
p <- (pa | pb) + plot_annotation(title = sprintf("¿Cuántos tópicos? Se eligió k = %d", K),
                                 subtitle = "Se probaron varios valores de k: la línea punteada marca el mejor equilibrio entre ambas métricas normalizadas")
guardar_fig(p, "eleccion_k", "Elección del número de tópicos (k)",
            "Perplejidad del modelo en el 20% de días reservados y coherencia UMass de las palabras principales, para cada k. El valor elegido equilibra capacidad de predicción e interpretabilidad.", h = 5.4)

# F08-02 Palabras por tópico (beta)
b2 <- beta[order(topic, -beta)][, head(.SD, 10), by = topic]
b2 <- merge(b2, etq[, .(topic, etiqueta)], by = "topic")
b2[, term2 := reorder_within(term, beta, etiqueta)]
p <- ggplot(b2, aes(beta, term2, fill = etiqueta)) + geom_col(width = .72, show.legend = FALSE) +
  facet_wrap(~etiqueta, scales = "free", ncol = 4, labeller = label_wrap_gen(28)) + scale_y_reordered() + scale_fill_manual(values = COLT) +
  scale_x_continuous(labels = function(x) formatC(x, format = "f", digits = 3)) +
  theme(strip.text = element_text(size = 11)) +
  labs(title = "Palabras principales de cada tópico", x = "β (probabilidad de la palabra en el tópico)", y = NULL,
       subtitle = sprintf("Modelo LDA con k = %d sobre %d días de conversación; el nombre de cada tópico se asigna automáticamente", K, nrow(dtm)))
guardar_fig(p, "palabras_por_topico", "Palabras principales de cada tópico (β)",
            sprintf("Las 10 palabras con mayor probabilidad β en cada tópico, como en el ejemplo de clase. Los %d tópicos encontrados son: %s.", K, paste(etq$nombre, collapse = "; ")), w = 13.5, h = 4 + 2.6 * ceiling(K / 4))

# F08-03 Tópicos en el tiempo
gm <- merge(gamma, etq[, .(topic, etiqueta)], by = "topic")[, .(g = mean(gamma)), by = .(mes = as.Date(format(fecha, "%Y-%m-01")), etiqueta)]
p <- ggplot(gm, aes(mes, g, fill = etiqueta)) + geom_area(position = "fill", color = "white", linewidth = .2) +
  scale_fill_manual(values = COLT, name = NULL) + scale_y_continuous(labels = percent, expand = c(0, 0)) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes, expand = c(0, 0)) + theme(legend.position = "right") +
  labs(title = "Evolución de los tópicos por mes", x = NULL, y = "Proporción media (γ) de cada tópico",
       subtitle = "Mezcla de tópicos de los días de cada mes (proporción media γ)")
guardar_fig(p, "topicos_tiempo", "Evolución de los tópicos por mes",
            "Proporción media de cada tópico (γ) en los días de cada mes. Muestra cómo rota la conversación a lo largo del año académico: unos meses dominan los trámites y otros las consultas sobre docentes, horarios o el plan de estudios.", w = 13, h = 7)

# F08-04 Tópicos por periodo académico
gp <- merge(gamma[, .(topic, gamma, tipo = sub(" \\d{4}$", "", periodo_academico(fecha)))], etq[, .(topic, etiqueta)], by = "topic")[, .(g = mean(gamma)), by = .(tipo, etiqueta)]
gp[, tipo := factor(tipo, c("Verano", "Semestre I", "Invierno", "Semestre II"))]
p <- ggplot(gp, aes(tipo, etiqueta, fill = g)) + geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = pct(g, 0)), family = FT_MONO, size = 3.8, color = ifelse(gp$g > quantile(gp$g, .7), "white", TINTA)) +
  scale_fill_gradient(low = "#FFF1F7", high = "#9D174D", labels = percent, name = "γ media") +
  scale_y_discrete(limits = rev(etq$etiqueta), labels = function(x) stri_wrap(x, 34, simplify = FALSE) |> vapply(paste, "", collapse = "\n")) +
  theme(panel.grid.major = element_blank()) + labs(title = "Tópicos según el periodo académico", x = NULL, y = NULL,
                                                   subtitle = "Peso promedio de cada tópico en los días de cada tipo de periodo (todos los años juntos)")
guardar_fig(p, "topicos_por_periodo", "Tópicos según el periodo académico",
            sprintf("Mapa de calor del peso medio de cada tópico por periodo. El tópico de mayor peso en cada periodo es: %s.",
                    paste(gp[order(-g)][, .SD[1], by = tipo][order(tipo), sprintf("%s → %s", tipo, etiqueta)], collapse = "; ")), h = 6.8)

# F08-05 Tópico x sentimiento
ts <- merge(msg_top[sentimiento %in% SENT_ORDEN], etq[, .(topico = topic, etiqueta)], by = "topico")[, .N, by = .(etiqueta, sentimiento)][, p := N / sum(N), by = etiqueta]
ts[, sentimiento := factor(sentimiento, SENT_ORDEN)]
p <- ggplot(ts, aes(sentimiento, etiqueta, fill = p)) + geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = ifelse(p >= .03, pct(p, 0), "")), family = FT_MONO, size = 3, color = ifelse(ts$p > .25, "white", TINTA)) +
  scale_fill_gradient(low = "#FFF7FB", high = "#831843", labels = percent, name = "% del tópico") +
  scale_x_discrete(labels = SENT_LAB) + scale_y_discrete(limits = rev(etq$etiqueta), labels = function(x) vapply(stri_wrap(x, 34, simplify = FALSE), paste, "", collapse = "\n")) +
  theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(title = "Sentimientos dentro de cada tópico", x = NULL, y = NULL,
       subtitle = "Distribución de los sentimientos dentro de los mensajes de cada tópico (mensajes con sentimiento)")
guardar_fig(p, "topico_sentimiento", "Sentimientos dentro de cada tópico",
            sprintf("Cruce de tópicos y sentimientos a nivel de mensaje. La frustración pesa más en %s (%s) y la admiración en %s (%s); la curiosidad domina en casi todos los tópicos.",
                    ts[sentimiento == "frustracion"][order(-p)][1, etiqueta], pct(ts[sentimiento == "frustracion"][order(-p)][1, p], 0),
                    ts[sentimiento == "admiracion"][order(-p)][1, etiqueta], pct(ts[sentimiento == "admiracion"][order(-p)][1, p], 0)), w = 13, h = 7)

# F08-06 tidytext vs tm
jd <- as.data.table(as.table(jac)); setnames(jd, c("td", "tm", "j"))
jd[, `:=`(td = factor(td, labels = paste0("T", seq_len(K))), tm = factor(tm, labels = paste0("tm", seq_len(K))))]
p <- ggplot(jd, aes(tm, td, fill = j)) + geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = ifelse(j > 0, formatC(j, format = "f", digits = 2), "")), family = FT_MONO, size = 3.4, color = ifelse(jd$j > .4, "white", TINTA)) +
  scale_fill_gradient(low = "#FFF7FB", high = "#BE185D", name = "Jaccard") + scale_y_discrete(limits = rev) +
  theme(panel.grid.major = element_blank()) +
  labs(title = "Comparación de tópicos: tidytext frente a tm", x = "Tópicos con tm (tema 10)", y = "Tópicos con tidytext (tema 8)",
       subtitle = sprintf("Similitud de Jaccard entre las 10 palabras principales de cada par de tópicos; mejor emparejamiento medio = %.2f", mean(emp)))
guardar_fig(p, "tidytext_vs_tm", "Comparación de tópicos: tidytext frente a tm",
            sprintf("Matriz de similitud de Jaccard entre los tópicos obtenidos con el flujo de tidytext (tema 8) y con el de tm (tema 10). La similitud media del mejor emparejamiento es %.2f: %s", mean(emp),
                    ifelse(mean(emp) >= .3, "los temas se mantienen con ambas herramientas.", "los dos preprocesamientos producen temas parcialmente distintos, lo que muestra la sensibilidad de LDA a la limpieza.")), w = 9.5, h = 7.6)

# F08-07 Ejemplos por tópico
set.seed(CFG$semilla)
ex <- merge(msg_top[prob > .6], lim[, .(id, texto_mostrar, n_caracteres)], by = "id")[n_caracteres > 30 & n_caracteres < 120][, .SD[sample(.N, min(.N, 2))], by = topico]
ex <- merge(ex, etq[, .(topico = topic, etiqueta)], by = "topico")[order(topico)]
ex[, txt := vapply(stri_replace_all_regex(texto_mostrar, RX_EMOJI, ""), function(z) paste(stri_wrap(z, 70), collapse = "\n"), "")]
ex[, y := -seq_len(.N)]
p <- ggplot(ex) + geom_tile(aes(x = 1.5, y = y, fill = etiqueta), width = 3, height = .9, alpha = .18) +
  geom_text(aes(x = .03, y = y, label = etiqueta, color = etiqueta), hjust = 0, family = FT_TIT, fontface = "bold.italic", size = 3.6) +
  geom_text(aes(x = 1.02, y = y, label = txt), hjust = 0, family = FT_TXT, size = 3.1, color = TINTA, lineheight = .92) +
  scale_fill_manual(values = COLT, guide = "none") + scale_color_manual(values = COLT, guide = "none") +
  scale_x_continuous(limits = c(0, 3)) + theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Así suena cada tópico", x = NULL, y = NULL, subtitle = "Dos mensajes reales por tópico con probabilidad mayor a 0.6 (datos personales ocultos)")
guardar_fig(p, "ejemplos_por_topico", "Mensajes de ejemplo de cada tópico",
            "Mensajes reales asignados con alta probabilidad a cada tópico. Ayudan a interpretar los tópicos con el lenguaje del propio chat.", w = 13, h = 1.6 + .62 * nrow(ex))

fase_fin()
