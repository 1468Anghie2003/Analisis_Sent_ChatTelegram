# =====================================================================
#  F09 | ENTIDADES, LUGARES Y RELACIONES
#  Temas de clase: 1 (NER y extracción de relaciones) y 4 (Ej8: identificar
#  lugares en una frase). Como no hay un modelo NER entrenado para jerga
#  universitaria boliviana, se usa NER basado en reglas y diccionario
#  (Regex): DOCENTE, MATERIA (código y nombre), LUGAR, PLATAFORMA, TRÁMITE y
#  HORARIO. Relaciones: coocurrencia docente-materia en el mismo mensaje.
#  Externo: reglas de asociación (Apriori, paquete arules) para descubrir
#  qué palabras o entidades "disparan" cada sentimiento.
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
suppressPackageStartupMessages(library(arules))
fase_inicio("F09")

F7 <- cargar_chk("f07_sent"); F5 <- cargar_chk("f05_stop")
ds <- F7$ds[analizable == TRUE]
ent <- fread(file.path(DIR$dic, "dic_entidades.csv"), encoding = "UTF-8")

# ---- 1. NER por reglas ---------------------------------------------------
#> Cada patrón del diccionario de entidades se busca en el texto normalizado.
#> Los nombres de pila comunes (Juan, Carlos...) exigen un título delante
#> (lic., ing., doc., dra.) para no confundir docentes con estudiantes.
tx <- ds$texto_norm
men <- rbindlist(lapply(seq_len(nrow(ent)), function(i) {
  h <- which(stri_detect_regex(tx, ent$patron[i]))
  if (length(h)) data.table(id = ds$id[h], tipo = ent$tipo[i], entidad = ent$entidad[i]) else NULL
}))
#> Materias por código (INF-111, MAT 115, EST-133...)
cods <- stri_match_all_regex(tx, "\\b(inf|mat|fis|est|lab|sis|dat|com|seg|cpa|qmc|elt|tel|ind)\\s*-?\\s*(\\d{3})\\b")
cod <- rbindlist(lapply(seq_along(cods), function(i) { m <- cods[[i]]; if (is.na(m[1, 1])) NULL else
  data.table(id = ds$id[i], tipo = "codigo_materia", entidad = unique(paste0(toupper(m[, 2]), "-", m[, 3]))) }))
#> Horarios: rangos "de 8 a 10", horas "14:00" y franjas "en la mañana"
rng <- stri_match_all_regex(tx, "\\bde (\\d{1,2})(?::\\d{2})?(?: ?(?:am|pm|hrs?|h))? a (\\d{1,2})(?::\\d{2})?\\b")
hor <- rbindlist(lapply(seq_along(rng), function(i) { m <- rng[[i]]; if (is.na(m[1, 1])) NULL else
  data.table(id = ds$id[i], h_ini = as.integer(m[, 2]), h_fin = as.integer(m[, 3])) }))
hor <- hor[h_ini <= 22 & h_fin <= 23]
hor[, h_ini := fifelse(h_ini <= 6, h_ini + 12L, h_ini)]        # "de 2 a 4" es de 14 a 16
hor[, franja := fifelse(h_ini < 12, "Mañana (7-12)", fifelse(h_ini < 14, "Mediodía (12-14)", fifelse(h_ini < 18, "Tarde (14-18)", "Noche (18-22)")))]
fr <- stri_match_all_regex(tx, "\\b(?:en la|por la|de la|turno|horario de la|horario)\\s+(manana|tarde|noche)\\b")
frw <- rbindlist(lapply(seq_along(fr), function(i) { m <- fr[[i]]; if (is.na(m[1, 1])) NULL else
  data.table(id = ds$id[i], franja = c(manana = "Mañana (7-12)", tarde = "Tarde (14-18)", noche = "Noche (18-22)")[m[, 2]]) }))
hor_all <- unique(rbind(hor[, .(id, franja)], frw))
men <- rbind(men, cod, hor_all[, .(id, tipo = "horario", entidad = franja)])
men <- unique(men)
if (isTRUE(CFG$anonimizar_docentes)) {
  rk_d <- men[tipo == "docente", .N, by = entidad][order(-N)][, alias := sprintf("Docente %02d", seq_len(.N))]
  men[tipo == "docente", entidad := rk_d$alias[match(entidad, rk_d$entidad)]]
}
men <- merge(men, ds[, .(id, fecha, sentimiento, polaridad, autor)], by = "id")
res_ent <- men[, .(menciones = .N, distintas = uniqueN(entidad)), by = tipo][order(-menciones)]
guardar_tabla(men[, .N, by = .(tipo, entidad)][order(tipo, -N)], "entidades_conteo")
bit("1. NER por reglas", sprintf("%s menciones de entidades en %s mensajes", num(nrow(men)), num(uniqueN(men$id))),
    paste(res_ent[, sprintf("%s %s (%d distintas)", tipo, num(menciones), distintas)], collapse = "; "),
    archivo = "03_Diccionarios/dic_entidades.csv",
    justificacion = "NER con reglas y diccionario: transparente, reproducible y adaptado a la jerga local (lic., INF-111, kardex)")

# ---- 2. Percepción de docentes ----------------------------------------------
#> Para cada docente: mensajes positivos y negativos que lo mencionan. Se
#> usa una proporción suavizada (pos + 1) / (pos + neg + 2) con intervalo
#> de Wilson al 90%; así un docente con 2 menciones no aparece como "el mejor".
doc <- men[tipo == "docente", .(menciones = .N, pos = sum(polaridad == "positivo"), neg = sum(polaridad == "negativo"),
                                admiracion = sum(sentimiento == "admiracion"), curiosidad = sum(sentimiento == "curiosidad")), by = entidad]
doc[, `:=`(indice = (pos + 1) / (pos + neg + 2), n_pn = pos + neg)]
wil <- function(x, n, z = 1.645) { p <- x / n; c((p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n),
                                                  (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)) }
doc[, c("li", "ls") := as.list(wil(pos + 1, pos + neg + 2)), by = entidad]
doc <- doc[order(-menciones)]
guardar_tabla(doc, "percepcion_docentes")
dm <- doc[menciones >= CFG$min_menciones_docente]
bit("2. Docentes", sprintf("%d docentes identificados; %d con %d o más menciones", nrow(doc), nrow(dm), CFG$min_menciones_docente),
    sprintf("Más mencionados: %s", paste(doc[1:5, sprintf("%s (%d)", entidad, menciones)], collapse = ", ")),
    justificacion = "Responde a la pregunta '¿quiénes son los mejores licenciados según el chat?' como percepción, no como evaluación docente")

# ---- 3. Relaciones docente - materia ------------------------------------------
#> Extracción de relaciones por coocurrencia: cuando un docente y una
#> materia aparecen en el mismo mensaje se cuenta un vínculo entre ambos.
#> Los pares más repetidos indican qué docente se asocia con qué materia
#> en la conversación (no es un registro oficial de asignaciones).
rel <- merge(men[tipo == "docente", .(id, docente = entidad)], men[tipo %in% c("materia", "codigo_materia"), .(id, materia = entidad)], by = "id", allow.cartesian = TRUE)
rel <- rel[, .N, by = .(docente, materia)][order(-N)]
guardar_tabla(rel, "relaciones_docente_materia")
bit("3. Relaciones", sprintf("%d pares docente-materia distintos", nrow(rel)),
    sprintf("Los más fuertes: %s", paste(rel[1:5, sprintf("%s-%s (%d)", docente, materia, N)], collapse = ", ")),
    justificacion = "Extracción de relaciones por coocurrencia: si un docente y una materia aparecen en el mismo mensaje, se asume un vínculo")

# ---- 4. Reglas de asociación ------------------------------------------------------
#> Cada mensaje es una "canasta" con sus palabras de contenido (las 400
#> más frecuentes), sus tipos de entidad y su sentimiento. Apriori busca
#> reglas {X} => {SENT=...}: qué combinaciones anticipan un sentimiento.
vtop <- F5$tok_c[, .N, by = palabra][order(-N)][1:400, palabra]
it_w <- F5$tok_c[palabra %in% vtop & id %in% ds$id, .(item = unique(palabra)), by = id]
it_e <- men[tipo %in% c("lugar", "tramite", "plataforma"), .(item = paste0("[", entidad, "]")), by = id]
it_s <- ds[sentimiento %in% SENT_ORDEN, .(id, item = paste0("SENT=", sentimiento))]
canasta <- unique(rbind(it_w, it_e, it_s))
trans <- as(split(canasta$item, canasta$id), "transactions")
reglas <- apriori(trans, parameter = list(supp = 0.0015, conf = 0.15, minlen = 2, maxlen = 3),
                  appearance = list(rhs = paste0("SENT=", SENT_ORDEN), default = "lhs"), control = list(verbose = FALSE))
reglas <- subset(reglas, lift > 1.5)
rg <- as.data.table(DATAFRAME(reglas))
setnames(rg, tolower(names(rg)))
rg[, `:=`(lhs = stri_replace_all_regex(as.character(lhs), "[{}]", ""), rhs = sub("SENT=", "", stri_replace_all_regex(as.character(rhs), "[{}]", "")))]
rg <- rg[order(-lift)]
guardar_tabla(rg, "reglas_asociacion_sentimiento")
bit("4. Reglas de asociación", sprintf("%s reglas {palabras/entidades} => {sentimiento} con lift > 1.5", num(nrow(rg))),
    sprintf("%s canastas, soporte mínimo 0.15%%, confianza mínima 15%%. Ejemplos: %s", num(length(trans)),
            paste(rg[, .SD[1], by = rhs][order(-lift)][1:4, sprintf("{%s}=>%s (lift %.1f)", lhs, rhs, lift)], collapse = "; ")),
    archivo = "04_Resultados/tablas/F09_reglas_asociacion_sentimiento.csv",
    justificacion = "Las reglas muestran causas probables: qué temas acompañan a cada sentimiento más de lo esperado por azar (lift)")

guardar_chk(list(men = men, doc = doc, rel = rel, rg = rg, hor = hor), "f09_ent")

# ---- 5. Figuras -------------------------------------------------------------------------
#> Nueve figuras: entidades por tipo, docentes más mencionados, ranking de
#> percepción, red docente-materia, materias por código, lugares y su
#> carga negativa, horarios mencionados y reglas de asociación.
lab_tipo <- c(docente = "Docentes", materia = "Materias (nombre)", codigo_materia = "Materias (código)", lugar = "Lugares",
              plataforma = "Plataformas", tramite = "Trámites", horario = "Franjas horarias")
# F09-01 Entidades por tipo
e1 <- men[, .N, by = .(tipo, entidad)][order(tipo, -N)][, head(.SD, 7), by = tipo]
e1[, tipo := factor(lab_tipo[tipo], lab_tipo)]
e1[, ent2 := tidytext::reorder_within(entidad, N, tipo)]
p <- ggplot(e1, aes(N, ent2, fill = tipo)) + geom_col(width = .72, show.legend = FALSE) +
  facet_wrap(~tipo, scales = "free", ncol = 4) + tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#E11D48", "#F97316", "#F59E0B", "#0EA5E9", "#6366F1", "#16A34A", "#A855F7")) +
  labs(title = "Entidades más mencionadas por tipo", x = "Mensajes que las mencionan", y = NULL,
       subtitle = sprintf("NER por reglas: %s menciones en %s mensajes. Las 7 entidades más mencionadas de cada tipo", num(nrow(men)), num(uniqueN(men$id))))
guardar_fig(p, "entidades_por_tipo", "Entidades más mencionadas por tipo",
            sprintf("Resultado del reconocimiento de entidades por reglas. Los trámites más nombrados son %s; las materias, %s; el lugar más citado es %s y la plataforma, %s.",
                    paste(e1[tipo == "Trámites"][1:3, entidad], collapse = ", "), paste(e1[tipo == "Materias (nombre)"][1:3, entidad], collapse = ", "),
                    e1[tipo == "Lugares"][1, entidad], e1[tipo == "Plataformas"][1, entidad]), w = 14, h = 8.4)

# F09-02 Docentes más mencionados
d2 <- doc[1:min(.N, 20)][order(menciones)][, entidad := factor(entidad, entidad)]
d2[, neutro := menciones - pos - neg]
d2l <- melt(d2[, .(entidad, Positivo = pos, Neutro = neutro, Negativo = neg)], id.vars = "entidad")
d2l[, variable := factor(variable, c("Negativo", "Neutro", "Positivo"))]
p <- ggplot(d2l, aes(value, entidad, fill = variable)) + geom_col(width = .72) +
  scale_fill_manual(values = c(Positivo = POL_COL[["positivo"]], Neutro = "#CBD5E1", Negativo = POL_COL[["negativo"]]), name = NULL) +
  labs(title = "Docentes más mencionados", x = "Mensajes que lo mencionan", y = NULL,
       subtitle = "Menciones según la polaridad del mensaje; la mayoría son neutras porque son preguntas ('¿qué tal es el lic. ...?')")
guardar_fig(p, "docentes_mas_mencionados", "Docentes más mencionados y polaridad de las menciones",
            sprintf("Los 20 docentes más mencionados con la polaridad de los mensajes que los nombran. Predominan las menciones neutras (consultas sobre cómo enseña o evalúa): %s neutras, %s negativas y %s positivas en total.",
                    num(sum(doc$menciones - doc$pos - doc$neg)), num(sum(doc$neg)), num(sum(doc$pos))), h = 8)

# F09-03 Ranking de percepción
d3 <- dm[order(indice)][, entidad := factor(entidad, entidad)]
p <- ggplot(d3, aes(indice, entidad)) +
  geom_vline(xintercept = .5, linetype = "22", color = TINTA3) +
  barra_error_h(aes(xmin = li, xmax = ls), alto = 0, color = alpha(GRAF$color, .45), linewidth = 2.2) +
  geom_point(aes(size = menciones, color = indice)) +
  scale_color_gradient2(low = POL_COL[["negativo"]], mid = "#A3A3A3", high = POL_COL[["positivo"]], midpoint = .5, guide = "none") +
  scale_size(range = c(2.5, 7), name = "Menciones") + scale_x_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Percepción de los docentes en el chat", x = "Índice de percepción: (positivos + 1) / (positivos + negativos + 2)", y = NULL,
       subtitle = sprintf("Docentes con %d o más menciones; la barra es el intervalo de Wilson al 90%%. Es la percepción del chat, no una evaluación docente", CFG$min_menciones_docente))
guardar_fig(p, "percepcion_docentes", "Ranking de percepción de docentes",
            "Índice suavizado de percepción por docente con intervalo de confianza. A la derecha de la línea punteada predominan las menciones positivas. Los intervalos anchos indican pocas menciones polarizadas: el orden debe leerse con cautela.", h = 8.4)

# F09-04 Red docente-materia
r4 <- rel[N >= 2][order(-N)][1:min(.N, 60)]
g <- igraph::graph_from_data_frame(r4[, .(docente, materia, N)], directed = FALSE)
set.seed(CFG$semilla); lay <- igraph::layout_with_fr(g, niter = 3000)
nod <- data.table(nombre = igraph::V(g)$name, x = lay[, 1], y = lay[, 2])[, tipo := fifelse(nombre %in% r4$docente, "Docente", "Materia")]
nod[, grado := igraph::strength(g, weights = r4$N)]
ed <- cbind(r4, nod[match(r4$docente, nombre), .(x1 = x, y1 = y)], nod[match(r4$materia, nombre), .(x2 = x, y2 = y)])
p <- ggplot() + geom_segment(data = ed, aes(x1, y1, xend = x2, yend = y2, linewidth = N), color = alpha("#E11D48", .35)) +
  geom_point(data = nod, aes(x, y, size = grado, fill = tipo), shape = 21, color = "white", stroke = 1.2) +
  ggrepel::geom_text_repel(data = nod, aes(x, y, label = nombre, fontface = ifelse(tipo == "Materia", "bold", "plain")), family = FT_TXT, size = 3.2, max.overlaps = 50) +
  scale_fill_manual(values = c(Docente = "#E11D48", Materia = "#0EA5E9"), name = NULL) + scale_size(range = c(2, 9), guide = "none") +
  scale_linewidth(range = c(.4, 3), guide = "none") + theme(axis.text = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Red de relaciones docente-materia", x = NULL, y = NULL,
       subtitle = "Extracción de relaciones por coocurrencia en el mismo mensaje (pares con 2 o más menciones); el grosor indica cuántas veces aparecen juntos")
guardar_fig(p, "red_docente_materia", "Red de relaciones docente-materia",
            "Grafo bipartito entre docentes (rojo) y materias (azul) construido con las coocurrencias en un mismo mensaje. Reconstruye, solo a partir del chat, qué docente se asocia con qué materia.", w = 12.5, h = 9)

# F09-05 Materias por código
m5 <- men[tipo == "codigo_materia", .N, by = .(entidad, polaridad)]
ordm <- m5[, .(t = sum(N)), by = entidad][order(-t)][1:min(.N, 20)]
m5 <- m5[entidad %in% ordm$entidad][, entidad := factor(entidad, rev(ordm$entidad))]
p <- ggplot(m5, aes(N, entidad, fill = factor(polaridad, c("negativo", "neutral", "positivo")))) + geom_col(width = .72) +
  scale_fill_manual(values = POL_COL, name = NULL) + theme(axis.text.y = element_text(family = FT_MONO, color = TINTA)) +
  labs(title = "Las materias más discutidas (por código)", x = "Mensajes", y = NULL,
       subtitle = "Códigos de materia escritos en el chat (INF-111, MAT-115...) y la polaridad de esos mensajes")
guardar_fig(p, "materias_codigo", "Materias más discutidas por código",
            "Las 20 siglas de materia más mencionadas. Son las que generan más consultas para habilitar en temporada o conseguir paralelo; el color muestra la polaridad de los mensajes que las nombran.", h = 7.6)

# F09-06 Lugares y su sentimiento
l6 <- men[tipo == "lugar", .(n = .N, neg = mean(polaridad == "negativo"), pos = mean(polaridad == "positivo")), by = entidad][n >= 5]
p <- ggplot(l6, aes(n, neg)) + geom_point(aes(size = n, color = neg - pos), alpha = .85) +
  ggrepel::geom_text_repel(aes(label = entidad), family = FT_TIT, fontface = "bold.italic", size = 4, color = TINTA, max.overlaps = 30) +
  scale_color_gradient2(low = POL_COL[["positivo"]], mid = "#A3A3A3", high = POL_COL[["negativo"]], midpoint = 0, guide = "none") +
  scale_size(range = c(3, 16), guide = "none") + scale_x_log10(labels = comma) + scale_y_continuous(labels = percent) +
  labs(title = "Lugares mencionados y su carga negativa", x = "Menciones (escala log)", y = "Mensajes negativos",
       subtitle = "Ej8 de clase: identificar lugares en el texto. Más arriba, lugares asociados a quejas; el color resume positivo (verde) frente a negativo (rojo)")
guardar_fig(p, "lugares_sentimiento", "Lugares mencionados y su carga negativa",
            sprintf("Cada burbuja es un lugar del campus o institución detectado en el texto. %s es el más mencionado (%d veces); entre los lugares con 40 o más menciones, el de mayor proporción negativa es %s (%s).",
                    l6[order(-n)][1, entidad], l6[order(-n)][1, n], l6[n >= 40][order(-neg)][1, entidad], pct(l6[n >= 40][order(-neg)][1, neg])), h = 6.8)

# F09-07 Franjas horarias mencionadas
h7 <- men[tipo == "horario", .(n = .N, neg = mean(polaridad == "negativo")), by = entidad]
h7[, entidad := factor(entidad, c("Mañana (7-12)", "Mediodía (12-14)", "Tarde (14-18)", "Noche (18-22)"))]
pa <- ggplot(h7, aes(entidad, n, fill = entidad)) + geom_col(width = .6, show.legend = FALSE) + geom_text(aes(label = n), vjust = -.4, family = FT_MONO) +
  scale_fill_manual(values = c("#FDBA74", "#FB923C", "#F43F5E", "#7C3AED")) + scale_y_continuous(expand = expansion(c(0, .1))) +
  scale_x_discrete(labels = function(x) stri_replace_first_fixed(x, " (", "\n(")) +
  labs(title = "Franjas mencionadas", x = NULL, y = "Mensajes")
pb <- ggplot(hor[, .N, by = h_ini], aes(h_ini, N)) + geom_col(fill = GRAF$color, width = .8) +
  scale_x_continuous(breaks = 7:22, labels = sprintf("%02dh", 7:22)) + labs(title = "Hora de inicio en rangos 'de X a Y'", x = NULL, y = "Menciones")
p <- (pa | pb) + plot_layout(widths = c(1, 1.6)) + plot_annotation(title = "Horarios mencionados en el chat",
  subtitle = sprintf("Horarios mencionados al hablar de materias y paralelos. La franja más citada es %s", h7[order(-n)][1, entidad]))
guardar_fig(p, "horarios_mencionados", "Horarios mencionados en el chat",
            sprintf("Franjas horarias y horas de inicio mencionadas en rangos como 'de 14 a 16'. Responde cuáles son los horarios más discutidos: %s.", paste(h7[order(-n), sprintf("%s (%d)", entidad, n)], collapse = ", ")), w = 12.5, h = 5.8)

# F09-08 Reglas de asociación (dispersión)
p <- ggplot(rg, aes(support, confidence, color = rhs, size = lift)) + geom_point(alpha = .7) +
  escala_sent_col(name = "Consecuente") + scale_size(range = c(1.2, 7), name = "Lift") +
  scale_x_log10(labels = percent) + scale_y_continuous(labels = percent) + theme(legend.position = "right") +
  labs(title = "Reglas de asociación hacia los sentimientos", x = "Soporte (escala log)", y = "Confianza",
       subtitle = sprintf("%s reglas {palabras, entidades} ⇒ {sentimiento} con lift > 1.5 (algoritmo Apriori)", num(nrow(rg))))
guardar_fig(p, "reglas_dispersion", "Reglas de asociación: soporte, confianza y lift",
            "Cada punto es una regla del tipo {X} ⇒ {sentimiento}. Arriba y a la derecha están las reglas fuertes y frecuentes; el tamaño es el lift (cuántas veces más probable es el sentimiento cuando aparece X).", h = 6.8)

# F09-09 Disparadores de cada sentimiento
r9 <- rg[nchar(lhs) < 40][, .SD[order(-lift)][1:2], by = rhs][!is.na(lhs)]
r9[, et := sprintf("{%s} ⇒ %s", lhs, SENT_LAB[rhs])]
r9 <- r9[order(lift)][, et := factor(et, unique(et))]
p <- ggplot(r9, aes(lift, et, fill = rhs)) + geom_col(width = .72, show.legend = FALSE) +
  geom_text(aes(label = sprintf("lift %.1f · conf %s", lift, pct(confidence, 0))), hjust = -.08, family = FT_MONO, size = 3) +
  escala_sent() + scale_x_continuous(expand = expansion(c(0, .35))) +
  labs(title = "Disparadores de cada sentimiento", x = "Lift", y = NULL,
       subtitle = "Las dos reglas con mayor lift para cada sentimiento: combinaciones de palabras o entidades que lo anticipan")
guardar_fig(p, "disparadores_sentimiento", "Disparadores de cada sentimiento (reglas de mayor lift)",
            "Reglas de asociación con mayor lift por sentimiento. Funcionan como hipótesis de causa: por ejemplo, ciertos trámites o palabras del sistema anticipan frustración, y las menciones de enseñanza anticipan admiración.", w = 12.5, h = 9)

fase_fin()
