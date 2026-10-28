# =====================================================================
#  F10 | PATRONES OCULTOS Y PREDICCIÓN
#  Más allá de lo visto en clase (métodos externos, justificados):
#   1. Hilos de respuesta: ¿qué preguntas se responden y en cuánto tiempo?
#      -> la mejor hora para preguntar.
#   2. Red de respuestas (grafos, comunidades de Louvain): quién ayuda a quién.
#   3. Días pico (detección de anomalías con mediana móvil y MAD) y qué pasó.
#   4. Causa y efecto de la incertidumbre: términos que la ocasionan,
#      correlación cruzada y prueba de causalidad de Granger.
#   5. Pronóstico de actividad semanal (comparación de 4 modelos).
#   6. Clasificador supervisado (Naive Bayes y regresión logística LASSO)
#      que aprende palabras nuevas de sentimiento.
#   7. ¿Qué mensajes reciben respuesta? (regresión logística, odds ratios).
#  Tema de clase 7 (chatbot): este análisis alimenta al asistente del dashboard.
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
suppressPackageStartupMessages(library(forecast))
fase_inicio("F10")

F7 <- cargar_chk("f07_sent"); F5 <- cargar_chk("f05_stop"); F6 <- cargar_chk("f06_dic"); F8 <- cargar_chk("f08_lda")
ds <- copy(F7$ds)
ds[, fh := as.POSIXct(fecha_hora_local, tz = CFG$zona_horaria)]

# ---- 1. Hilos de respuesta ---------------------------------------------
#> Una respuesta es un mensaje con responde_a_id. Para cada pregunta se mide
#> si recibió respuesta de OTRA persona y cuánto tardó la primera.
resp <- merge(ds[!is.na(responde_a_id), .(id_r = id, orig = as.character(responde_a_id), autor_r = autor, fh_r = fh, sent_r = sentimiento)],
             ds[, .(orig = id, autor_o = autor, fh_o = fh, sent_o = sentimiento, preg = es_pregunta, hora_o = hora, dsem_o = dia_semana)], by = "orig")
resp <- resp[autor_r != autor_o]
resp[, minutos := as.numeric(difftime(fh_r, fh_o, units = "mins"))]
resp <- resp[minutos >= 0]
prim <- resp[, .SD[which.min(minutos)], by = orig]
pq <- ds[es_pregunta == TRUE & analizable == TRUE, .(orig = id, hora, dia_semana, sentimiento, fecha)]
pq <- merge(pq, prim[, .(orig, minutos)], by = "orig", all.x = TRUE)
pq[, respondida := !is.na(minutos)]
ph <- pq[, .(tasa = mean(respondida), mediana = median(minutos, na.rm = TRUE), n = .N), by = hora][order(hora)]
mejor <- ph[n >= 150][order(-tasa)][1]
guardar_tabla(ph, "respuesta_por_hora")
bit("1. Hilos de respuesta", sprintf("%s de las preguntas recibe respuesta explícita; mediana %s minutos", pct(mean(pq$respondida)), num(median(pq$minutos, na.rm = TRUE), 0)),
    sprintf("%s respuestas entre personas distintas. Mejor hora para preguntar: %02dh (%s respondidas)", num(nrow(resp)), mejor$hora, pct(mejor$tasa)),
    justificacion = "Solo cuentan respuestas explícitas (función responder de Telegram); es una cota inferior de la ayuda real")

# ---- 2. Red de respuestas -----------------------------------------------------
#> Cada respuesta (A responde a un mensaje de B) es una arista dirigida de
#> A hacia B con peso igual al número de respuestas. Con igraph se mide
#> cuánto responde y cuánto recibe cada miembro y se detectan comunidades
#> con el algoritmo de Louvain (grupos que se responden más entre sí).
ed <- resp[, .(peso = .N), by = .(de = autor_r, a = autor_o)]
g <- igraph::graph_from_data_frame(ed, directed = TRUE)
igraph::E(g)$weight <- ed$peso
gu <- a_no_dirigido(g, mode = "collapse", edge.attr.comb = list(weight = "sum"))
set.seed(CFG$semilla); com <- igraph::cluster_louvain(gu)
nodos <- data.table(autor = igraph::V(g)$name, respondio = igraph::strength(g, mode = "out"), recibio = igraph::strength(g, mode = "in"),
                    comunidad = igraph::membership(com)[igraph::V(g)$name])
nodos <- merge(nodos, ds[, .(mensajes = .N, preguntas = sum(es_pregunta)), by = autor], by = "autor", all.x = TRUE)
guardar_tabla(nodos[order(-respondio)], "red_respuestas_nodos")
bit("2. Red de respuestas", sprintf("Grafo de %s miembros y %s vínculos de respuesta; %d comunidades (Louvain), modularidad %.2f",
                                    num(igraph::vcount(g)), num(igraph::ecount(g)), max(nodos$comunidad), igraph::modularity(com)),
    sprintf("Miembros que más responden: %s", paste(nodos[order(-respondio)][1:3, sprintf("%s (%d respuestas)", autor, respondio)], collapse = ", ")),
    justificacion = "La red revela líderes informales que sostienen el grupo respondiendo dudas")

# ---- 3. Días pico ------------------------------------------------------------
#> z robusto = (log(n) - mediana móvil de 31 días) / MAD. Un día con z > 2.5
#> es anómalo; para cada uno se buscan las palabras más propias de ese día.
dd <- ds[analizable == TRUE, .N, by = fecha]
dd <- merge(data.table(fecha = seq(min(dd$fecha), max(dd$fecha), "day")), dd, all.x = TRUE)[is.na(N), N := 0L]
dd[, ln := log1p(N)][, med := zoo::rollapply(ln, 31, median, partial = TRUE, align = "center")]
mad_r <- mad(dd$ln - dd$med)
dd[, z := (ln - med) / mad_r]
picos <- dd[z > 2.5 & N >= 40][order(-z)]
picos <- picos[vapply(seq_len(nrow(picos)), function(i) i == 1 || all(abs(as.numeric(picos$fecha[i] - picos$fecha[seq_len(i - 1)])) > 6), TRUE)][1:min(.N, 12)]
picos <- picos[!is.na(fecha)]
tk <- merge(F5$tok_c, ds[, .(id, fecha)], by = "id")
tot_w <- tk[, .N, by = palabra]
pal_pico <- rbindlist(lapply(picos$fecha, function(f) {
  x <- tk[fecha == f, .N, by = palabra][N >= 3]
  x <- merge(x, tot_w[, .(palabra, tot = N)], by = "palabra")
  x[, lo := log((N + .5) / (sum(N) - N + .5)) - log((tot - N + .5) / (sum(tot_w$N) - sum(N) - tot + N + .5))]
  data.table(fecha = f, palabras = paste(x[order(-lo)][1:min(.N, 4), palabra], collapse = ", "))
}))
picos <- merge(picos, pal_pico, by = "fecha")
picos <- merge(picos, ds[sentimiento %in% SENT_ORDEN, .N, by = .(fecha, sentimiento)][order(-N)][, .SD[1], by = fecha][, .(fecha, sent_dom = sentimiento)], by = "fecha", all.x = TRUE)
guardar_tabla(picos, "dias_pico")
bit("3. Días pico", sprintf("%d días anómalos (z robusto > 2.5)", nrow(picos)),
    paste(picos[order(fecha), sprintf("%s: %d msgs (%s)", format(fecha, "%d/%m/%Y"), N, palabras)], collapse = " | "),
    justificacion = "Una mediana móvil con MAD es robusta a los propios picos y separa lo excepcional del ciclo de temporada")

# ---- 4. Causa y efecto de la incertidumbre -------------------------------------
#> Incertidumbre = confusión + preocupación. Causas: palabras sobre-
#> representadas en esos mensajes (log-odds). Efectos: (a) qué sentimiento
#> tienen las respuestas, (b) si la incertidumbre de hoy anticipa el
#> malestar (frustración + enojo) de los días siguientes (Granger).
inc_ids <- ds[sentimiento %in% c("confusion", "preocupacion"), id]
dicw <- unique(unlist(strsplit(F6$D$dic$termino, " ")))
tki <- tk[!(palabra %in% c(dicw, F5$STOP_TOPICOS, "tan", "digo", "pueda", "quieres", "caso", "puedes", "pasa", "seria")) & nchar(palabra) > 2, .(inc = id %in% inc_ids, palabra)]   # sin palabras del diccionario: causas de contexto
ca <- tki[, .(a = sum(inc), b = sum(!inc)), by = palabra][a >= 10]
A <- sum(ca$a); B <- sum(ca$b)
ca[, lo := log((a + .5) / (A - a + .5)) - log((b + .5) / (B - b + .5))][, se := sqrt(1 / (a + .5) + 1 / (A - a + .5) + 1 / (b + .5) + 1 / (B - b + .5))][, z := lo / se]
causas <- ca[order(-z)][1:18]
guardar_tabla(causas, "causas_incertidumbre")
efe <- resp[, .(inc = sent_o %in% c("confusion", "preocupacion"), sent_r)][sent_r %in% SENT_ORDEN, .N, by = .(inc, sent_r)][, p := N / sum(N), by = inc]
serie <- ds[analizable == TRUE, .(n = .N, inc = sum(sentimiento %in% c("confusion", "preocupacion")), mal = sum(sentimiento %in% c("frustracion", "enojo"))), by = fecha][n >= 10][order(fecha)]
serie[, `:=`(p_inc = inc / n, p_mal = mal / n)]
cc <- ccf(serie$p_inc, serie$p_mal, lag.max = 7, plot = FALSE)
ccd <- data.table(lag = as.integer(cc$lag), r = as.numeric(cc$acf))
serie[, `:=`(mal_1 = shift(p_mal), inc_1 = shift(p_inc), mal_2 = shift(p_mal, 2), inc_2 = shift(p_inc, 2))]
s2 <- serie[complete.cases(serie)]
m0 <- lm(p_mal ~ mal_1 + mal_2, data = s2); m1 <- lm(p_mal ~ mal_1 + mal_2 + inc_1 + inc_2, data = s2)
gr <- anova(m0, m1); p_granger <- gr$`Pr(>F)`[2]
bit("4. Causa y efecto", sprintf("Causas de la incertidumbre: %s", paste(causas$palabra[1:8], collapse = ", ")),
    sprintf("Correlación simultánea incertidumbre-malestar r = %.2f; prueba de Granger (2 rezagos): p = %.3f", ccd[lag == 0, r], p_granger),
    justificacion = "Granger no prueba causalidad real, pero indica si la incertidumbre pasada mejora la predicción del malestar futuro")

# ---- 5. Pronóstico de actividad semanal ---------------------------------------
#> Serie semanal de mensajes (frecuencia 52). Se reservan las últimas 26
#> semanas, se comparan 4 modelos por error absoluto medio (MAE) y el
#> mejor se reentrena para pronosticar las próximas 26 semanas.
sem <- ds[, .N, by = .(s = as.Date(cut(fecha, "week")))]
sem <- merge(data.table(s = seq(min(sem$s), max(sem$s), "week")), sem, all.x = TRUE)[is.na(N), N := 0L]
y <- ts(sem$N, frequency = 52)
h <- 26; ntr <- length(y) - h
ytr <- ts(y[1:ntr], frequency = 52); yte <- y[(ntr + 1):length(y)]
fx_tr <- fourier(ytr, K = 4); fx_te <- fourier(ytr, K = 4, h = h)
mods <- list(
  "Ingenuo estacional" = function(z, hh) snaive(z, h = hh),
  "STL + ETS" = function(z, hh) stlf(z, h = hh, lambda = 0.3),
  "TBATS" = function(z, hh) forecast(tbats(z), h = hh),
  "ARIMA + Fourier" = function(z, hh) { fx <- fourier(z, K = 4); forecast(auto.arima(z, xreg = fx, seasonal = FALSE, lambda = 0.3), xreg = fourier(z, K = 4, h = hh)) })
ev <- rbindlist(lapply(names(mods), function(nm) { f <- tryCatch(mods[[nm]](ytr, h), error = function(e) NULL)
  if (is.null(f)) return(NULL); pr <- pmax(as.numeric(f$mean), 0)
  data.table(modelo = nm, MAE = mean(abs(pr - yte)), RMSE = sqrt(mean((pr - yte)^2))) }))
mejor_m <- ev[which.min(MAE), modelo]
fc <- mods[[mejor_m]](y, h)
pron <- data.table(s = max(sem$s) + 7 * seq_len(h), media = pmax(as.numeric(fc$mean), 0),
                   li80 = pmax(as.numeric(fc$lower[, 1]), 0), ls80 = as.numeric(fc$upper[, 1]),
                   li95 = pmax(as.numeric(fc$lower[, 2]), 0), ls95 = as.numeric(fc$upper[, 2]))
guardar_tabla(ev, "pronostico_modelos"); guardar_tabla(pron, "pronostico_26_semanas")
bit("5. Pronóstico", sprintf("Mejor modelo: %s (MAE %.1f mensajes/semana en 26 semanas de prueba)", mejor_m, min(ev$MAE)),
    sprintf("Comparados: %s. Pico pronosticado: semana del %s (~%s mensajes)", paste(sprintf("%s=%.1f", ev$modelo, ev$MAE), collapse = ", "),
            format(pron[which.max(media), s], "%d/%m/%Y"), num(max(pron$media))),
    justificacion = "Comparar modelos en datos no vistos evita elegir uno que solo memoriza el pasado")

# ---- 6. Clasificador supervisado de polaridad ---------------------------------------
#> Supervisión débil: las etiquetas (positivo/negativo) vienen del
#> diccionario. Para que el modelo NO copie al diccionario, se quitan de los
#> predictores todos los términos del diccionario: así aprende palabras
#> nuevas que acompañan a cada polaridad. Validación cruzada de 5 pliegues.
et <- ds[polaridad %in% c("positivo", "negativo") & n_palabras >= 3, .(id, y = factor(polaridad, c("negativo", "positivo")))]
tkc <- F5$tok_c[id %in% et$id & !(palabra %in% dicw) & nchar(palabra) > 2]
voc <- tkc[, .(d = uniqueN(id)), by = palabra][d >= 5, palabra]
tkc <- tkc[palabra %in% voc, .N, by = .(id, palabra)]
et <- et[id %in% tkc$id]
X <- Matrix::sparseMatrix(i = match(tkc$id, et$id), j = match(tkc$palabra, voc), x = 1, dims = c(nrow(et), length(voc)), dimnames = list(et$id, voc))
X@x[] <- 1
set.seed(CFG$semilla); folds <- sample(rep(1:5, length.out = nrow(et)))
nb_fit <- function(Xt, yt) { lp <- sapply(levels(yt), function(c) { cnt <- Matrix::colSums(Xt[yt == c, , drop = FALSE]) + 1; log(cnt / sum(cnt)) })
  list(lp = lp, prior = log(as.numeric(table(yt)) / length(yt))) }
nb_pred <- function(m, Xn) { s <- as.matrix(Xn %*% m$lp); s <- sweep(s, 2, m$prior, "+"); factor(colnames(m$lp)[max.col(s)], colnames(m$lp)) }
f1 <- function(pr, yv) { r <- sapply(levels(yv), function(c) { tp <- sum(pr == c & yv == c); p <- tp / max(1, sum(pr == c)); rr <- tp / max(1, sum(yv == c)); if (p + rr == 0) 0 else 2 * p * rr / (p + rr) }); mean(r) }
cvres <- rbindlist(lapply(1:5, function(k) {
  tr <- folds != k; te <- !tr
  m_nb <- nb_fit(X[tr, ], et$y[tr]); p_nb <- nb_pred(m_nb, X[te, ])
  m_gl <- glmnet::cv.glmnet(X[tr, ], et$y[tr], family = "binomial", alpha = 1, nfolds = 4)
  p_gl <- factor(ifelse(predict(m_gl, X[te, ], s = "lambda.1se", type = "response") > .5, "positivo", "negativo"), levels(et$y))
  p_base <- factor(rep(names(which.max(table(et$y[tr]))), sum(te)), levels(et$y))
  rbind(data.table(modelo = "Naive Bayes multinomial", pliegue = k, exactitud = mean(p_nb == et$y[te]), f1 = f1(p_nb, et$y[te])),
        data.table(modelo = "Regresión logística LASSO", pliegue = k, exactitud = mean(p_gl == et$y[te]), f1 = f1(p_gl, et$y[te])),
        data.table(modelo = "Línea base (clase mayoritaria)", pliegue = k, exactitud = mean(p_base == et$y[te]), f1 = f1(p_base, et$y[te])))
}))
res_cv <- cvres[, .(exactitud = mean(exactitud), f1 = mean(f1), sd_f1 = sd(f1)), by = modelo]
gl_full <- glmnet::cv.glmnet(X, et$y, family = "binomial", alpha = 1, nfolds = 5)
cf <- as.matrix(coef(gl_full, s = "lambda.min"))[-1, 1]
nuevas <- data.table(palabra = names(cf), coef = cf)[coef != 0][order(coef)]
nb_full <- nb_fit(X, et$y)
pr_full <- factor(ifelse(predict(gl_full, X, s = "lambda.1se", type = "response") > .5, "positivo", "negativo"), levels(et$y))
cm <- as.data.table(table(real = et$y, pred = nb_pred(nb_full, X)))
guardar_tabla(res_cv, "clasificador_cv"); guardar_tabla(nuevas, "palabras_aprendidas")
bit("6. Clasificador", sprintf("Mejor F1 macro en validación cruzada: %s = %.2f", res_cv[which.max(f1), modelo], max(res_cv$f1)),
    sprintf("%s mensajes etiquetados, %s palabras fuera del diccionario. %s",
            num(nrow(et)), num(length(voc)), paste(res_cv[, sprintf("%s: exactitud %s, F1 %.2f", modelo, pct(exactitud), f1)], collapse = "; ")),
    justificacion = "Si un modelo sin las palabras del diccionario predice la polaridad mejor que el azar, el contexto confirma las etiquetas y propone términos nuevos")

# ---- 7. ¿Qué mensajes reciben respuesta? ---------------------------------------------------
#> Regresión logística: la variable respuesta es si el mensaje recibió al
#> menos una respuesta; las explicativas son si es pregunta, la franja
#> horaria, la longitud, fin de semana, uso de emojis, polaridad y si es
#> temporada. Los odds ratios (exp del coeficiente) indican cuánto aumenta
#> o disminuye la probabilidad de respuesta; el AUC con validación cruzada
#> de 5 pliegues mide la capacidad de discriminación del modelo.
pr <- ds[analizable == TRUE & n_palabras >= 2, .(id, hora, dia_semana, n_palabras, es_pregunta, emojis, polaridad, periodo, n_reacciones)]
pr[, respondido := id %in% resp$orig]
pr[, `:=`(franja = cut(hora, c(-1, 6, 11, 13, 17, 21, 23), labels = c("Madrugada", "Mañana", "Mediodía", "Tarde", "Noche", "Noche tarde")),
          largo = cut(n_palabras, c(0, 5, 12, 25, Inf), labels = c("1-5 palabras", "6-12", "13-25", "26+")),
          finde = dia_semana %in% c("sábado", "domingo"), con_emoji = emojis != "", temporada = sub(" \\d{4}$", "", periodo) %in% c("Verano", "Invierno"))]
pr[, franja := relevel(franja, "Tarde")]; pr[, largo := relevel(largo, "1-5 palabras")]; pr[, polaridad := relevel(factor(polaridad), "neutral")]
lg <- glm(respondido ~ es_pregunta + franja + largo + finde + con_emoji + polaridad + temporada, data = pr, family = binomial)
orx <- data.table(var = names(coef(lg)), or = exp(coef(lg)), li = exp(confint.default(lg)[, 1]), ls = exp(confint.default(lg)[, 2]), p = summary(lg)$coefficients[, 4])[-1]
set.seed(CFG$semilla); fo <- sample(rep(1:5, length.out = nrow(pr)))
auc <- function(s, yv) { r <- rank(s); n1 <- sum(yv); n0 <- length(yv) - n1; (sum(r[yv]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
auc_cv <- mean(sapply(1:5, function(k) { m <- glm(formula(lg), data = pr[fo != k], family = binomial); auc(predict(m, pr[fo == k]), pr[fo == k, respondido]) }))
guardar_tabla(orx, "respuesta_odds_ratios")
bit("7. ¿Quién recibe respuesta?", sprintf("Regresión logística: AUC en validación cruzada = %.2f", auc_cv),
    sprintf("Ser pregunta multiplica las chances por %.2f; %s de los mensajes recibe respuesta", orx[var == "es_preguntaTRUE", or], pct(mean(pr$respondido))),
    justificacion = "Los odds ratios cuantifican qué hace que un mensaje obtenga ayuda: horario, longitud, tono")

# Exportables para el dashboard (analizador en vivo y asistente)
nb_export <- data.table(palabra = rownames(nb_full$lp), neg = round(nb_full$lp[, "negativo"], 4), pos = round(nb_full$lp[, "positivo"], 4))
guardar_chk(list(resp = resp, pq = pq, ph = ph, nodos = nodos, ed = ed, picos = picos, dd = dd, causas = causas, efe = efe, ccd = ccd, p_granger = p_granger,
                 ev = ev, pron = pron, sem = sem, mejor_m = mejor_m, res_cv = res_cv, nuevas = nuevas, cm = cm, orx = orx, auc_cv = auc_cv,
                 nb = list(lp = nb_export, prior = nb_full$prior), mejor = mejor), "f10_pat")
saveRDS(list(nb = nb_full, glmnet = gl_full, logit_respuesta = lg), file.path(DIR$mod, "modelos_f10.rds"))

# ---- 8. Figuras -------------------------------------------------------------------------
#> Doce figuras: mejor hora para preguntar, red de respuestas, miembros que
#> responden frente a los que preguntan, días pico, causas y efectos de la
#> incertidumbre, comparación de modelos de pronóstico, pronóstico, métricas
#> y palabras del clasificador, y factores asociados a recibir respuesta.
# F10-01 Mejor hora para preguntar
sc_ <- max(ph$mediana, na.rm = TRUE) / max(ph$tasa)
p <- ggplot(ph[n >= 20], aes(hora)) + geom_col(aes(y = tasa), fill = alpha(GRAF$color, .75), width = .75) +
  geom_line(aes(y = mediana / sc_), color = "#7C2D12", linewidth = 1.2) + geom_point(aes(y = mediana / sc_), color = "#7C2D12", size = 2.4) +
  geom_col(data = ph[hora == mejor$hora], aes(y = tasa), fill = "#3F6212", width = .75) +
  annotate_label(x = mejor$hora, y = mejor$tasa * 1.08, label = sprintf("Mejor hora: %02dh\n%s respondidas", mejor$hora, pct(mejor$tasa, 0)),
           family = FT_TXT, size = 3.4, fill = "white", label.size = 0) +
  scale_y_continuous(labels = percent, name = "Preguntas con respuesta", sec.axis = sec_axis(~ . * sc_, name = "Mediana de minutos hasta la 1ª respuesta (línea)")) +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02dh", 0:23)) +
  labs(title = "Mejor hora para preguntar", x = "Hora en que se publicó la pregunta",
       subtitle = sprintf("Barras: proporción de preguntas que recibió respuesta explícita de otra persona. Línea: tiempo mediano de espera. En total, %s de las preguntas se responden",
                          pct(mean(pq$respondida))))
guardar_fig(p, "mejor_hora_preguntar", "Mejor hora para preguntar: tasa y tiempo de respuesta",
            "Para cada hora del día, la proporción de preguntas que reciben respuesta explícita y la mediana de minutos hasta la primera respuesta. Es un hallazgo práctico para los estudiantes: preguntar a la hora en que el grupo está activo aumenta la probabilidad de ayuda.", w = 12.5, h = 6.4)

# F10-02 Red de respuestas
topn <- nodos[order(-(respondio + recibio))][1:min(.N, 70), autor]
sub <- igraph::induced_subgraph(gu, topn)
set.seed(CFG$semilla); lay <- igraph::layout_with_fr(sub, weights = igraph::E(sub)$weight, niter = 3000)
nd <- data.table(autor = igraph::V(sub)$name, x = lay[, 1], y = lay[, 2])
nd <- merge(nd, nodos, by = "autor")
.en <- igraph::ends(sub, igraph::E(sub), names = TRUE)
el <- data.table(from = .en[, 1], to = .en[, 2], weight = as.numeric(igraph::E(sub)$weight))
el <- cbind(el, nd[match(el$from, autor), .(x1 = x, y1 = y)], nd[match(el$to, autor), .(x2 = x, y2 = y)])
comc <- setNames(colorRampPalette(c("#65A30D", "#0EA5E9", "#F59E0B", "#E11D48", "#8B5CF6", "#14B8A6", "#F97316"))(max(nd$comunidad)), seq_len(max(nd$comunidad)))
p <- ggplot() + geom_segment(data = el, aes(x1, y1, xend = x2, yend = y2, linewidth = weight), color = alpha(TINTA3, .35)) +
  geom_point(data = nd, aes(x, y, size = respondio + 1, fill = factor(comunidad)), shape = 21, color = "white", stroke = 1) +
  ggrepel::geom_text_repel(data = nd[order(-respondio)][1:12], aes(x, y, label = sub("Miembro ", "M", autor)), family = FT_MONO, size = 3, max.overlaps = 30) +
  scale_fill_manual(values = comc, guide = "none") + scale_size(range = c(1.5, 11), guide = "none") + scale_linewidth(range = c(.2, 2.5), guide = "none") +
  theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Red de respuestas entre miembros", x = NULL, y = NULL,
       subtitle = sprintf("Los 70 miembros con más respuestas dadas o recibidas (anonimizados). Tamaño = respuestas dadas; color = comunidad detectada con Louvain (modularidad %.2f)", igraph::modularity(com)))
guardar_fig(p, "red_respuestas", "Red de respuestas entre miembros",
            "Grafo de quién responde a quién. Unos pocos nodos grandes (miembros que responden mucho) conectan comunidades distintas: son los líderes informales que sostienen la ayuda dentro del grupo.", w = 12, h = 9.4)

# F10-03 Miembros que responden frente a miembros que preguntan
n3 <- nodos[mensajes >= 15]
p <- ggplot(n3, aes(preguntas + 1, respondio + 1)) + geom_abline(linetype = "22", color = TINTA3) +
  geom_point(aes(size = mensajes, color = respondio > preguntas), alpha = .7) +
  ggrepel::geom_text_repel(data = n3[order(-respondio)][1:10], aes(label = sub("Miembro ", "M", autor)), family = FT_MONO, size = 3) +
  scale_x_log10() + scale_y_log10() + scale_size(range = c(1.5, 9), name = "Mensajes") +
  scale_color_manual(values = c(`TRUE` = "#3F6212", `FALSE` = "#F59E0B"), labels = c(`TRUE` = "Más responde que pregunta", `FALSE` = "Más pregunta que responde"), name = NULL) +
  labs(title = "Miembros que responden frente a miembros que preguntan", x = "Preguntas hechas + 1 (escala log)", y = "Respuestas dadas a otros + 1 (escala log)",
       subtitle = "Miembros con 15 o más mensajes; sobre la diagonal se ubican quienes responden más de lo que preguntan")
guardar_fig(p, "responden_frente_preguntan", "Miembros que responden frente a miembros que preguntan",
            sprintf("Cada punto es un miembro activo: preguntas realizadas frente a respuestas dadas. El %s de estos miembros responde más de lo que pregunta; los diez miembros que más responden concentran el %s de todas las respuestas.",
                    pct(mean(n3$respondio > n3$preguntas)), pct(sum(nodos[order(-respondio)][1:10, respondio]) / sum(nodos$respondio))), h = 7)

# F10-04 Días pico
p <- ggplot(dd, aes(fecha, N)) + geom_line(color = alpha(GRAF$color, .6), linewidth = .4) +
  geom_point(data = picos, aes(size = N), color = "#B91C1C") +
  ggrepel::geom_label_repel(data = picos, aes(label = sprintf("%s\n%s", format(fecha, "%d/%m/%y"), palabras)), family = FT_TXT, size = 2.8,
                            fill = alpha("white", .92), label.size = 0, max.overlaps = 30, box.padding = .5, min.segment.length = 0) +
  scale_size(range = c(2, 6), guide = "none") + scale_x_date(date_breaks = "3 months", labels = etiq_mes) + scale_y_continuous(labels = comma) +
  labs(title = "Días pico y sus palabras distintivas", x = NULL, y = "Mensajes por día",
       subtitle = "Días anómalos (z robusto > 2.5 frente a la mediana móvil de 31 días; uno por semana) con las palabras más propias de cada uno: así se reconstruye qué pasó")
guardar_fig(p, "dias_pico", "Días pico y su tema",
            "Serie diaria de mensajes con los días anómalos marcados y las palabras que los distinguen. Las palabras propias de cada pico (trámites, nombres de docentes, eventos) permiten reconstruir qué ocurrió ese día sin leer cientos de mensajes.", w = 13.5, h = 7.4)

# F10-05 Causas de la incertidumbre
c5 <- copy(causas)[order(z)][, palabra := factor(palabra, palabra)]
p <- ggplot(c5, aes(z, palabra)) + geom_col(fill = SENT_COL[["confusion"]], width = .7) +
  geom_text(aes(label = sprintf("%d mensajes", a)), hjust = -.1, family = FT_MONO, size = 3) +
  scale_x_continuous(expand = expansion(c(0, .3))) +
  labs(title = "Causas de la incertidumbre (log-odds)", x = "Fuerza de la asociación (z del log-odds)", y = NULL,
       subtitle = "Palabras sobre-representadas en mensajes de confusión o preocupación frente al resto: trámites, sistema y plazos")
guardar_fig(p, "causas_incertidumbre", "Causas de la incertidumbre (log-odds)",
            sprintf("Palabras (fuera del diccionario de sentimientos) que aparecen mucho más en los mensajes de confusión y preocupación que en el resto. Señalan las causas de la incertidumbre: %s.", paste(causas$palabra[1:8], collapse = ", ")), h = 7)

# F10-06 Efecto en el tiempo (CCF)
p <- ggplot(ccd, aes(lag, r, fill = r > 0)) + geom_col(width = .7) + geom_hline(yintercept = c(-1, 1) * 1.96 / sqrt(nrow(serie)), linetype = "22", color = TINTA3) +
  scale_fill_manual(values = c(`TRUE` = "#65A30D", `FALSE` = "#E11D48"), guide = "none") + scale_x_continuous(breaks = -7:7) +
  labs(title = "Correlación cruzada incertidumbre - malestar", x = "Rezago en días (positivo = la incertidumbre va antes)", y = "Correlación",
       subtitle = sprintf("Correlación cruzada diaria entre %% de incertidumbre (confusión + preocupación) y %% de malestar (frustración + enojo). Prueba de Granger con 2 rezagos: p = %.3f",
                          p_granger))
guardar_fig(p, "efecto_incertidumbre_malestar", "Correlación cruzada incertidumbre → malestar",
            "Correlación entre la incertidumbre de un día y el malestar de días anteriores y posteriores. Las líneas punteadas son el umbral de significancia aproximado. La prueba de Granger indica si la incertidumbre pasada ayuda a predecir el malestar.", h = 6)

# F10-07 Qué pasa después de una duda
efe[, grupo := ifelse(inc, "Respuestas a mensajes de incertidumbre", "Respuestas a otros mensajes")]
e7 <- efe[, sent_r := factor(sent_r, SENT_ORDEN)]
p <- ggplot(e7, aes(p, sent_r, fill = grupo)) + geom_col(position = position_dodge(.75), width = .7) +
  scale_fill_manual(values = c("#7E8CB8", "#D9F99D"), name = NULL) + scale_y_discrete(labels = SENT_LAB, limits = rev(SENT_ORDEN)) +
  scale_x_continuous(labels = percent) +
  labs(title = "Sentimiento de las respuestas a mensajes de incertidumbre", x = "Proporción de las respuestas", y = NULL,
       subtitle = "Sentimiento de las respuestas que recibe un mensaje de confusión o preocupación, comparado con las respuestas al resto")
guardar_fig(p, "efecto_respuestas", "Sentimiento de las respuestas a mensajes de incertidumbre",
            { dif <- dcast(efe, sent_r ~ inc, value.var = "p", fill = 0); setnames(dif, c("s", "otros", "inc")); dif[, d := inc - otros]
  sprintf("Distribución del sentimiento de las respuestas según el mensaje respondido. Las respuestas a mensajes de incertidumbre tienen más %s y menos %s que las respuestas al resto.",
          paste(tolower(SENT_LAB[as.character(dif[order(-d)][1:2, s])]), collapse = " y "), paste(tolower(SENT_LAB[as.character(dif[order(d)][1:2, s])]), collapse = " y ")) }, h = 7.4)

# F10-08 Comparación de modelos de pronóstico
e8 <- copy(ev)[order(MAE)][, modelo := factor(modelo, rev(modelo))]
p <- ggplot(e8, aes(MAE, modelo, fill = modelo == mejor_m)) + geom_col(width = .6) +
  geom_text(aes(label = sprintf("MAE %.1f · RMSE %.1f", MAE, RMSE)), hjust = -.08, family = FT_MONO, size = 3.4) +
  scale_fill_manual(values = c(`TRUE` = "#3F6212", `FALSE` = "#BEF264"), guide = "none") + scale_x_continuous(expand = expansion(c(0, .4))) +
  labs(title = "Comparación de modelos de pronóstico", x = "Error absoluto medio en las 26 semanas de prueba (mensajes por semana)", y = NULL,
       subtitle = "Los modelos se entrenaron sin ver las últimas 26 semanas y se compararon contra lo que realmente pasó")
guardar_fig(p, "comparacion_pronostico", "Comparación de modelos de pronóstico",
            sprintf("Error de cuatro modelos de series de tiempo en las 26 semanas reservadas. El menor error lo obtuvo %s (MAE %.1f), frente a %.1f del ingenuo estacional, que solo repite el año anterior.", mejor_m, min(ev$MAE), ev[modelo == "Ingenuo estacional", MAE]), h = 5)

# F10-09 Pronóstico
p <- ggplot() + geom_line(data = sem, aes(s, N), color = alpha(TINTA2, .7), linewidth = .5) +
  geom_ribbon(data = pron, aes(s, ymin = li95, ymax = ls95), fill = alpha("#65A30D", .18)) +
  geom_ribbon(data = pron, aes(s, ymin = li80, ymax = ls80), fill = alpha("#65A30D", .3)) +
  geom_line(data = pron, aes(s, media), color = "#3F6212", linewidth = 1.3) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes) + scale_y_continuous(labels = comma) +
  labs(title = "Pronóstico de mensajes semanales (26 semanas)", x = NULL, y = "Mensajes por semana",
       subtitle = sprintf("Modelo %s con intervalos de 80%% y 95%%. Se espera el próximo pico la semana del %s",
                          mejor_m, format(pron[which.max(media), s], "%d/%m/%Y")))
guardar_fig(p, "pronostico_actividad", "Pronóstico de mensajes semanales",
            "Historia semanal de mensajes y pronóstico para las siguientes 26 semanas con bandas de incertidumbre. Anticipa la próxima ola de actividad, útil para que delegados y docentes preparen información antes de las inscripciones.", w = 12.5, h = 6.4)

# F10-10 Clasificador: métricas y matriz
pa <- ggplot(res_cv, aes(f1, reorder(modelo, f1))) + geom_col(fill = "#84CC16", width = .6) +
  barra_error_h(aes(xmin = f1 - sd_f1, xmax = f1 + sd_f1), alto = .2) +
  geom_text(aes(label = sprintf("F1 %.2f · exactitud %s", f1, pct(exactitud, 0))), hjust = -.05, x = .02, family = FT_MONO, size = 3.2) +
  scale_x_continuous(limits = c(0, 1)) + labs(title = "Validación cruzada (5 pliegues)", x = "F1 macro", y = NULL)
cm[, p := N / sum(N), by = real]
pb <- ggplot(cm, aes(pred, real, fill = p)) + geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = sprintf("%s\n%s", num(N), pct(p, 0))), family = FT_MONO, color = "white") +
  scale_fill_gradient(low = "#BEF264", high = "#3F6212", guide = "none") + labs(title = "Naive Bayes (todo el set)", x = "Predicho", y = "Real")
p <- (pa | pb) + plot_layout(widths = c(1.6, 1)) + plot_annotation(title = "Clasificador de polaridad entrenado sin el diccionario",
  subtitle = "Se entrenó con las etiquetas del diccionario pero SIN sus palabras: si acierta más que la línea base, el contexto confirma las etiquetas")
guardar_fig(p, "clasificador_metricas", "Clasificador de polaridad: validación y matriz de confusión",
            "F1 macro y exactitud de Naive Bayes y regresión logística LASSO frente a una línea base, más la matriz de confusión. Los modelos superan a la línea base usando solo palabras que no están en el diccionario.", w = 13, h = 5.8)

# F10-11 Palabras aprendidas
nv <- rbind(nuevas[order(coef)][1:12], nuevas[order(-coef)][1:12])[!is.na(palabra)]
nv <- unique(nv)[order(coef)][, palabra := factor(palabra, palabra)]
p <- ggplot(nv, aes(coef, palabra, fill = coef > 0)) + geom_col(width = .7) +
  scale_fill_manual(values = c(`TRUE` = POL_COL[["positivo"]], `FALSE` = POL_COL[["negativo"]]), labels = c(`TRUE` = "Empuja a positivo", `FALSE` = "Empuja a negativo"), name = NULL) +
  geom_vline(xintercept = 0) +
  labs(title = "Palabras que el clasificador asoció a cada polaridad", x = "Coeficiente LASSO (log-odds)", y = NULL,
       subtitle = "Ninguna de estas palabras está en el diccionario: son candidatas para ampliarlo y revelan el contexto de lo negativo y lo positivo")
guardar_fig(p, "palabras_aprendidas", "Palabras que el clasificador aprendió fuera del diccionario",
            sprintf("Coeficientes de la regresión LASSO para palabras que no están en el diccionario. Empujan a lo negativo: %s; a lo positivo: %s. Son candidatas a revisar para ampliar el diccionario.",
                    paste(nuevas[order(coef)][1:5, palabra], collapse = ", "), paste(nuevas[order(-coef)][1:5, palabra], collapse = ", ")), h = 7.6)

# F10-12 ¿Qué mensajes reciben respuesta?
lab_or <- c(es_preguntaTRUE = "Es una pregunta", franjaMadrugada = "Publicado de madrugada", franjaMañana = "Publicado en la mañana",
            franjaMediodía = "Publicado a mediodía", franjaNoche = "Publicado de noche (18-21h)", `franjaNoche tarde` = "Publicado tarde (22-23h)",
            largo6.12 = "6-12 palabras", largo13.25 = "13-25 palabras", `largo26+` = "26 o más palabras", findeTRUE = "Fin de semana",
            con_emojiTRUE = "Lleva emoji", polaridadnegativo = "Tono negativo", polaridadpositivo = "Tono positivo", temporadaTRUE = "En temporada (verano/invierno)")
orx[, etiqueta := ifelse(var %in% names(lab_or), lab_or[var], var)]
orx[, etiqueta := factor(etiqueta, orx[order(or), etiqueta])]
p <- ggplot(orx, aes(or, etiqueta)) + geom_vline(xintercept = 1, linetype = "22") +
  barra_error_h(aes(xmin = li, xmax = ls), alto = 0, linewidth = 1.6, color = alpha(GRAF$color, .5)) +
  geom_point(aes(color = or > 1), size = 4) + scale_color_manual(values = c(`TRUE` = "#3F6212", `FALSE` = "#E11D48"), guide = "none") +
  scale_x_log10() +
  labs(title = "Factores asociados a recibir respuesta", x = "Odds ratio (escala log; >1 aumenta la probabilidad)", y = NULL,
       subtitle = sprintf("Regresión logística sobre %s mensajes (referencia: tarde, 1-5 palabras, tono neutro). AUC en validación cruzada = %.2f", num(nrow(pr)), auc_cv))
guardar_fig(p, "odds_respuesta", "Factores asociados a recibir respuesta (odds ratios)",
            "Odds ratios con intervalos de 95% de una regresión logística. Ser pregunta y tener más palabras aumentan la probabilidad de respuesta; el horario y el tono también influyen. Es una guía práctica de cómo preguntar para obtener ayuda.", h = 7)

fase_fin()
