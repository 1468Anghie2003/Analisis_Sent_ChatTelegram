# =====================================================================
#  F07 | ANÁLISIS DE SENTIMIENTOS
#  Tema de clase: 1 (análisis de sentimientos como técnica de extracción) y
#  4 (Ej9: sentimientos de un texto con gráfica de frecuencia).
#  Cada mensaje recibe puntajes en los 15 sentimientos (motor de F06); el
#  sentimiento dominante es el de mayor puntaje y la polaridad es
#  positivo - negativo. Se analiza: distribución, evolución en el tiempo,
#  periodos académicos, horas, personas, reacciones, ironía y "reír para no
#  llorar". Se valida con dos señales independientes (texto vs emoji).
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
suppressPackageStartupMessages(library(ggridges))
fase_inicio("F07")

L <- cargar_chk("f03_limpio"); F6 <- cargar_chk("f06_dic")
lim <- L$limpio; W <- F6$R$W; H <- F6$R$H

# ---- 1. Dataset con sentimientos --------------------------------------
#> Se unen al dataset limpio el sentimiento dominante, la polaridad, el
#> puntaje (positivo - negativo), la intensidad, la ironía y los 15 puntajes.
sc <- copy(W); setnames(sc, SENT_ORDEN, paste0("s_", SENT_ORDEN))
ds <- merge(lim, sc, by = "id", all.x = TRUE)
ds[is.na(sentimiento), sentimiento := ifelse(analizable, "sin_sentimiento", "no_analizable")]
ds[is.na(polaridad), polaridad := "neutral"]
for (k in c("puntaje", "puntaje_pos", "puntaje_neg", "intensidad", "n_sentimientos", paste0("s_", SENT_ORDEN))) set(ds, which(is.na(ds[[k]])), k, 0)
ds[is.na(ironia), ironia := FALSE]
setorder(ds, fecha_hora_local)
fwrite(ds, file.path(DIR$dataset, "03_dataset_sentimientos.csv"), bom = TRUE)
cs <- ds[sentimiento %in% SENT_ORDEN]
dist <- cs[, .N, by = sentimiento][, p := N / sum(N)][order(-N)]
guardar_tabla(dist, "distribucion_sentimientos")
bit("1. Puntuación", "Sentimiento dominante, polaridad e intensidad por mensaje",
    sprintf("%s mensajes con sentimiento (%s de los analizables). Predominan: %s", num(nrow(cs)), pct(nrow(cs) / sum(ds$analizable)),
            paste(dist[1:5, sprintf("%s %s", SENT_LAB[sentimiento], pct(p))], collapse = ", ")),
    antes = nrow(lim), despues = nrow(ds), archivo = "01_Dataset/03_dataset_sentimientos.csv",
    justificacion = "El sentimiento dominante permite contar y graficar; los 15 puntajes conservan los mensajes con emociones mezcladas")

pol <- cs[, .N, by = polaridad][, p := N / sum(N)]
bit("1b. Polaridad", "Balance positivo / negativo",
    paste(pol[, sprintf("%s %s", polaridad, pct(p))], collapse = ", "),
    justificacion = "La polaridad resume el ánimo general; los 15 sentimientos explican su porqué")

# ---- 2. Índice de ánimo diario -----------------------------------------
#> Índice = (positivos - negativos) / (positivos + negativos) en una ventana
#> móvil de 7 días: va de -1 (todo negativo) a +1 (todo positivo).
dia <- cs[, .(pos = sum(polaridad == "positivo"), neg = sum(polaridad == "negativo"), n = .N), by = fecha][order(fecha)]
tod <- data.table(fecha = seq(min(dia$fecha), max(dia$fecha), by = "day"))
dia <- merge(tod, dia, all.x = TRUE); for (k in c("pos", "neg", "n")) set(dia, which(is.na(dia[[k]])), k, 0L)
dia[, `:=`(pos7 = frollsum(pos, 7, align = "center"), neg7 = frollsum(neg, 7, align = "center"))]
dia[, indice := fifelse(pos7 + neg7 >= 5, (pos7 - neg7) / (pos7 + neg7), NA_real_)]
guardar_tabla(dia, "indice_animo_diario")
bit("2. Índice de ánimo", "Índice diario (positivos - negativos) / (positivos + negativos), ventana de 7 días",
    sprintf("Promedio %.2f; día más positivo %s (%.2f); día más negativo %s (%.2f)", mean(dia$indice, na.rm = TRUE),
            format(dia[which.max(indice), fecha], "%d/%m/%Y"), max(dia$indice, na.rm = TRUE),
            format(dia[which.min(indice), fecha], "%d/%m/%Y"), min(dia$indice, na.rm = TRUE)),
    archivo = "04_Resultados/tablas/F07_indice_animo_diario.csv",
    justificacion = "La ventana de 7 días suaviza los días con pocos mensajes y permite seguir el ánimo a lo largo de cada temporada")

# ---- 3. Validación por señales independientes ------------------------------
#> En mensajes que tienen a la vez palabras y emojis con sentimiento se
#> compara la polaridad de cada fuente por separado (kappa de Cohen).
#> Se usan solo emojis (no emoticones ni signos) y solo mensajes donde
#> ambas fuentes tienen polaridad (positiva o negativa).
pol_fuente <- H[fuente != "emoticon", .(p = sum(peso * fifelse(sentimiento %in% SENT_ORDEN[1:6], 1, fifelse(sentimiento %in% SENT_ORDEN[9:15], -1, 0)))),
                by = .(id, fuente = fifelse(fuente == "emoji", "emoji", "texto"))]
vv <- dcast(pol_fuente, id ~ fuente, value.var = "p")[!is.na(texto) & !is.na(emoji) & texto != 0 & emoji != 0]
cls <- function(x) factor(fifelse(x > 0, "positivo", "negativo"), c("positivo", "negativo"))
vv[, `:=`(t = cls(texto), e = cls(emoji))]
tabv <- table(vv$t, vv$e); po <- sum(diag(tabv)) / sum(tabv); pe <- sum(rowSums(tabv) * colSums(tabv)) / sum(tabv)^2
kappa <- (po - pe) / (1 - pe)
bit("3. Validación interna", sprintf("Concordancia texto-emoji: %s (kappa de Cohen = %.2f)", pct(po), kappa),
    sprintf("%s mensajes con polaridad en palabras y en emojis a la vez", num(nrow(vv))),
    justificacion = "Si palabras y emojis, medidos por separado, coinciden más que por azar, el diccionario captura un sentimiento real")

# Muestra estratificada para validación manual del equipo
dir.create(file.path(DIR$res, "validacion"), showWarnings = FALSE)
set.seed(CFG$semilla)
mu <- ds[analizable == TRUE & n_palabras >= 3][, .SD[sample(.N, min(.N, 20))], by = sentimiento][, .(id, fecha, texto_mostrar, sentimiento_modelo = sentimiento, etiqueta_manual = "")]
fwrite(mu, file.path(DIR$res, "validacion", "muestra_para_etiquetar.csv"), bom = TRUE)
f_man <- file.path(DIR$res, "validacion", "etiquetas_manuales.csv")
if (file.exists(f_man)) {
  man <- fread(f_man, encoding = "UTF-8")[etiqueta_manual != ""]
  acc <- mean(man$etiqueta_manual == man$sentimiento_modelo)
  bit("3b. Validación manual", sprintf("Exactitud frente a etiquetas del equipo: %s (%d mensajes)", pct(acc), nrow(man)), justificacion = "Etiquetado humano")
} else bit("3b. Validación manual", "Muestra estratificada lista para etiquetar", sprintf("%d mensajes (20 por sentimiento) en 04_Resultados/validacion/", nrow(mu)),
           archivo = "04_Resultados/validacion/muestra_para_etiquetar.csv",
           justificacion = "Si el equipo completa la columna etiqueta_manual y guarda como etiquetas_manuales.csv, F07 calcula la exactitud")

guardar_chk(list(ds = ds, dia = dia, kappa = kappa, po = po, tabv = tabv), "f07_sent")

# ---- 4. Figuras -------------------------------------------------------------------------
#> Catorce figuras: distribución de los 15 sentimientos, polaridad global y
#> por año, evolución mensual, picos por mes, perfil por periodo académico,
#> distribución horaria, ánimo por día de la semana, perfil del miembro más
#> activo frente al grupo, reacciones por sentimiento, presencia de risa
#> según polaridad, validación texto-emoji, términos de cada sentimiento,
#> termómetro de ánimo e ironía detectada.
# F07-01 Distribución de los 15 sentimientos
d1 <- copy(dist)[, sentimiento := factor(sentimiento, rev(dist$sentimiento))][, y := as.numeric(sentimiento)]
p <- ggplot(d1, aes(p, y, fill = sentimiento)) + geom_col(width = .7, orientation = "y", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%s  (%s)", pct(p), num(N))), hjust = -.08, family = FT_MONO, size = 3.4) +
  escala_sent() + scale_x_continuous(labels = percent, expand = expansion(c(0, .2))) +
  scale_y_continuous(breaks = d1$y, labels = SENT_LAB[as.character(d1$sentimiento)]) +
  coord_cartesian(clip = "off") + theme(plot.margin = margin(22, 24, 16, 64), axis.text.y = element_text(size = 12, color = TINTA)) +
  labs(title = "Distribución de los 15 sentimientos", x = "Porcentaje de mensajes con sentimiento", y = NULL,
       subtitle = sprintf("Sentimiento dominante de %s mensajes con señal emocional. Encabezan %s, %s y %s",
                          num(nrow(cs)), SENT_LAB[dist$sentimiento[1]], SENT_LAB[dist$sentimiento[2]], SENT_LAB[dist$sentimiento[3]]))
p <- agregar_emojis(p, SENT_EMOJI[as.character(d1$sentimiento)], x = rep(-max(d1$p) * .2, 15), y = d1$y, w = max(d1$p) * .055, h = .8)
guardar_fig(p, "distribucion_15_sentimientos", "Distribución de los 15 sentimientos",
            sprintf("Porcentaje de mensajes según su sentimiento dominante. %s domina (%s); le siguen %s. Entre los sentimientos negativos sobresalen %s.",
                    SENT_LAB[dist$sentimiento[1]], pct(dist$p[1]), paste(tolower(SENT_LAB[dist$sentimiento[2:4]]), collapse = ", "),
                    paste(tolower(SENT_LAB[dist[sentimiento %in% SENT_ORDEN[9:15]][1:2, sentimiento]]), collapse = " y ")), h = 7.6)

# F07-02 Polaridad global y por año
pa <- ggplot(pol, aes(2, p, fill = polaridad)) + geom_col(width = 1, color = GRAF$fondo, linewidth = 2) +
  geom_text(aes(label = pct(p, 0)), position = position_stack(vjust = .5), color = "white", family = FT_TIT, fontface = "bold.italic", size = 6) +
  coord_polar(theta = "y") + xlim(.6, 2.5) + scale_fill_manual(values = POL_COL, name = NULL) +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(), legend.position = "bottom") + labs(title = "Global", x = NULL, y = NULL)
pyr <- cs[, .N, by = .(anio, polaridad)][, p := N / sum(N), by = anio]
pb <- ggplot(pyr, aes(factor(anio), p, fill = factor(polaridad, c("negativo", "neutral", "positivo")))) + geom_col(width = .6) +
  geom_text(aes(label = pct(p, 0)), position = position_stack(vjust = .5), color = "white", family = FT_TXT, fontface = "bold") +
  scale_fill_manual(values = POL_COL, guide = "none") + scale_y_continuous(labels = percent) + labs(title = "Por año", x = NULL, y = NULL)
p <- (pa | pb) + plot_layout(widths = c(1, 1.5)) +
  plot_annotation(title = "Balance de polaridad",
                  subtitle = "Mensajes con sentimiento clasificados como positivos, negativos o neutros (curiosidad y sorpresa son neutras)")
guardar_fig(p, "polaridad_global_anio", "Polaridad global y por año",
            sprintf("Proporción de mensajes positivos, neutros y negativos en todo el periodo y por año. Globalmente, %s son positivos y %s negativos (la confusión cuenta como negativa según la polaridad de clase). La parte positiva %s de %s a %s.",
                    pct(pol[polaridad == "positivo", p]), pct(pol[polaridad == "negativo", p]),
                    ifelse(pyr[polaridad == "positivo"][order(anio)][.N, p] < pyr[polaridad == "positivo"][order(anio)][1, p], "baja", "sube"),
                    min(pyr$anio), max(pyr$anio)), h = 6)

# F07-03 Evolución mensual (100%)
em <- cs[, .N, by = .(mes = as.Date(paste0(mes, "-01")), sentimiento)][, p := N / sum(N), by = mes]
em[, sentimiento := factor(sentimiento, SENT_ORDEN)]
p <- ggplot(em, aes(mes, p, fill = sentimiento)) + geom_area(position = "fill", color = "white", linewidth = .15) +
  escala_sent(name = NULL) + scale_y_continuous(labels = percent, expand = c(0, 0)) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes, expand = c(0, 0)) +
  theme(legend.position = "right") +
  labs(title = "Evolución mensual de los sentimientos (100%)", x = NULL, y = "Composición de los mensajes con sentimiento",
       subtitle = "Área apilada al 100%: cómo cambia la mezcla de los 15 sentimientos a lo largo de 40 meses")
guardar_fig(p, "evolucion_mensual_100", "Evolución mensual de los sentimientos (100%)",
            "Composición mensual de los sentimientos. La curiosidad es la base constante del chat; las bandas de frustración, confusión, gratitud y satisfacción cambian de grosor de un mes a otro según lo que ocurra con inscripciones, pagos y habilitación de materias.", w = 12.5, h = 7)

# F07-04 Mapa de calor sentimiento x mes (z-score)
z <- copy(em)[, z := (p - mean(p)) / sd(p), by = sentimiento]
p <- ggplot(z, aes(mes, sentimiento, fill = z)) + geom_tile(color = GRAF$fondo, linewidth = .5) +
  scale_fill_gradient2(low = "#2D3A8C", mid = "#FBF7FF", high = "#C2185B", midpoint = 0, name = "z-score", limits = c(-3, 3), oob = squish) +
  scale_y_discrete(labels = SENT_LAB, limits = rev(SENT_ORDEN)) + scale_x_date(date_breaks = "3 months", labels = etiq_mes, expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), legend.key.width = unit(1.6, "cm")) +
  labs(title = "Picos de cada sentimiento por mes (z-score)", x = NULL, y = NULL,
       subtitle = "Peso de cada sentimiento en el mes, estandarizado respecto de su propio promedio: magenta = mucho más de lo habitual")
guardar_fig(p, "picos_sentimiento_mes", "Picos de cada sentimiento por mes (z-score)",
            "Cada fila es un sentimiento y cada columna un mes; el color indica cuánto se aleja ese mes de lo habitual para ese sentimiento. Permite ver, por ejemplo, en qué meses la frustración o la preocupación fueron excepcionales.", w = 12.5, h = 7.4)

# F07-05 Huella emocional por periodo académico (radar)
rp <- cs[, .N, by = .(tipo = sub(" \\d{4}$", "", periodo), sentimiento)][, p := N / sum(N), by = tipo]
rp <- merge(CJ(tipo = unique(rp$tipo), sentimiento = SENT_ORDEN), rp, all.x = TRUE)[is.na(p), p := 0]
rp[, k := match(sentimiento, SENT_ORDEN)]
rp <- rbind(rp, rp[k == 1][, k := 16])
rp[, p2 := sqrt(p)]
setorder(rp, tipo, k)
p <- ggplot(rp, aes(k, p2, color = tipo, fill = tipo, group = tipo)) +
  geom_polygon(alpha = .12, linewidth = 1.1) + geom_point(size = 1.8) +
  coord_polar() + scale_x_continuous(breaks = 1:15, labels = SENT_LAB[SENT_ORDEN], limits = c(1, 16)) +
  scale_y_continuous(breaks = sqrt(c(.01, .05, .15, .3)), labels = c("1%", "5%", "15%", "30%")) +
  scale_color_manual(values = c(Verano = "#F59E0B", `Semestre I` = "#0EA5E9", Invierno = "#6366F1", `Semestre II` = "#14B8A6"), name = NULL) +
  scale_fill_manual(values = c(Verano = "#F59E0B", `Semestre I` = "#0EA5E9", Invierno = "#6366F1", `Semestre II` = "#14B8A6"), name = NULL) +
  theme(axis.text.x = element_text(size = 10.5, color = TINTA), axis.text.y = element_text(size = 8)) +
  labs(title = "Perfil de sentimientos por periodo académico", x = NULL, y = NULL,
       subtitle = "Radar con la proporción de cada sentimiento (escala raíz cuadrada) en verano, invierno y los dos semestres regulares")
guardar_fig(p, "perfil_periodos", "Perfil de sentimientos por periodo académico",
            sprintf("Gráfico de radar que compara la mezcla de sentimientos en los cuatro periodos. La frustración pesa más en las temporadas (verano %s, invierno %s) que en los semestres (I %s, II %s); los semestres tienen más curiosidad y el semestre I, más tristeza y gratitud.",
                    pct(rp[tipo == "Verano" & sentimiento == "frustracion" & k < 16, p]), pct(rp[tipo == "Invierno" & sentimiento == "frustracion" & k < 16, p]),
                    pct(rp[tipo == "Semestre I" & sentimiento == "frustracion" & k < 16, p]), pct(rp[tipo == "Semestre II" & sentimiento == "frustracion" & k < 16, p])), w = 10, h = 10.4)

# F07-06 Horas de cada sentimiento (ridgeline)
hr <- cs[, .(hora_dec = hora + minuto / 60, sentimiento = factor(sentimiento, rev(SENT_ORDEN)))]
med_h <- hr[, .(m = median(hora_dec)), by = sentimiento]
p <- ggplot(hr, aes(hora_dec, sentimiento, fill = sentimiento)) +
  geom_density_ridges(scale = 2.2, rel_min_height = .01, color = "white", alpha = .9, linewidth = .4, show.legend = FALSE, bandwidth = .9) +
  escala_sent() + scale_x_continuous(breaks = seq(0, 24, 3), labels = sprintf("%02dh", seq(0, 24, 3)), limits = c(0, 24), expand = c(0, 0)) +
  scale_y_discrete(labels = SENT_LAB) +
  labs(title = "Distribución horaria de cada sentimiento", x = "Hora del día (Bolivia)", y = NULL,
       subtitle = sprintf("Distribución horaria de cada sentimiento. Los que más aparecen de noche (22 a 6 h): %s",
                          paste(tolower(SENT_LAB[cs[, .(n = mean(hora >= 22 | hora < 6)), by = sentimiento][order(-n)][1:3, sentimiento]]), collapse = ", ")))
guardar_fig(p, "reloj_emociones", "Distribución horaria de cada sentimiento",
            sprintf("Curvas de densidad de la hora en que aparece cada sentimiento. La mayoría se concentra entre media mañana y la tarde (medianas entre %s y %s h); %s tienen la mayor proporción nocturna y %s son las más diurnas.",
                    floor(min(cs[, median(hora), by = sentimiento]$V1)), ceiling(max(cs[, median(hora), by = sentimiento]$V1)),
                    paste(tolower(SENT_LAB[cs[, .(n = mean(hora >= 22 | hora < 6)), by = sentimiento][order(-n)][1:2, sentimiento]]), collapse = " y "),
                    paste(tolower(SENT_LAB[cs[, .(n = mean(hora >= 22 | hora < 6)), by = sentimiento][order(n)][1:2, sentimiento]]), collapse = " y ")), h = 8.4)

# F07-07 Día de la semana: balance
dw <- cs[, .(pos = mean(polaridad == "positivo"), neg = mean(polaridad == "negativo"), n = .N), by = dia_semana]
dw[, dia_semana := factor(dia_semana, rev(DIAS_ES))]
dwl <- melt(dw, id.vars = c("dia_semana", "n"))[, value := ifelse(variable == "neg", -value, value)]
p <- ggplot(dwl, aes(value, dia_semana, fill = variable)) + geom_col(width = .62) +
  geom_text(aes(label = pct(abs(value), 0), hjust = ifelse(value > 0, -.15, 1.15)), family = FT_MONO, size = 3.4) +
  geom_vline(xintercept = 0, color = TINTA) +
  scale_fill_manual(values = c(pos = POL_COL[["positivo"]], neg = POL_COL[["negativo"]]), labels = c(pos = "Positivos", neg = "Negativos"), name = NULL) +
  scale_x_continuous(labels = function(x) pct(abs(x), 0), expand = expansion(.15)) +
  labs(title = "Balance de polaridad por día de la semana", x = "Proporción de mensajes con sentimiento", y = NULL,
       subtitle = "Positivos a la derecha y negativos a la izquierda; los neutros (curiosidad, sorpresa) completan el 100%")
guardar_fig(p, "animo_dia_semana", "Balance de polaridad por día de la semana",
            "Barras divergentes con la proporción de mensajes positivos y negativos por día. Entre semana hay más mensajes neutros (consultas); el sábado y el domingo, con menos mensajes, lo positivo y lo negativo pesan más.", h = 6)

# F07-08 Miembro más activo vs grupo
top1 <- ds[autor == ds[, .N, by = autor][order(-N)][1, autor]]
cmp <- rbind(cs[, .N, by = sentimiento][, .(sentimiento, p = N / sum(N), quien = "Todo el grupo")],
             top1[sentimiento %in% SENT_ORDEN, .N, by = sentimiento][, .(sentimiento, p = N / sum(N), quien = top1$autor[1])])
cmp <- merge(CJ(sentimiento = SENT_ORDEN, quien = unique(cmp$quien)), cmp, all.x = TRUE)[is.na(p), p := 0]
cw <- dcast(cmp, sentimiento ~ quien, value.var = "p")
cw <- cw[, .(sentimiento, a = `Todo el grupo`, b = get(top1$autor[1]))]
cw[, sentimiento := factor(sentimiento, rev(SENT_ORDEN))]
pa <- ggplot(cw) + geom_segment(aes(x = a, xend = b, y = sentimiento, yend = sentimiento), color = TINTA3, linewidth = 1.2) +
  geom_point(aes(a, sentimiento), size = 4, color = "#94A3B8") + geom_point(aes(b, sentimiento, color = sentimiento), size = 5, show.legend = FALSE) +
  escala_sent_col() + scale_y_discrete(labels = SENT_LAB) + scale_x_continuous(labels = percent) +
  labs(title = sprintf("%s (gris: %s)", unique(cmp$quien)[unique(cmp$quien) != "Todo el grupo"], "todo el grupo"), x = "Proporción de sus mensajes con sentimiento", y = NULL)
t10 <- ds[, .N, by = autor][order(-N)][1:10, autor]
t10d <- cs[autor %in% t10, .(pos = mean(polaridad == "positivo"), neg = mean(polaridad == "negativo"), n = .N), by = autor]
t10d <- merge(t10d, ds[autor %in% t10, .(msgs = .N), by = autor], by = "autor")[order(msgs)][, autor := factor(autor, autor)]
pb <- ggplot(t10d) + geom_col(aes(-neg, autor), fill = POL_COL[["negativo"]], width = .6) + geom_col(aes(pos, autor), fill = POL_COL[["positivo"]], width = .6) +
  geom_text(aes(0, autor, label = num(msgs)), family = FT_MONO, size = 3, color = "white") + geom_vline(xintercept = 0) +
  scale_x_continuous(labels = function(x) pct(abs(x), 0)) + labs(title = "Los 10 más activos: negativos | positivos", x = NULL, y = NULL)
p <- (pa | pb) + plot_annotation(title = "Perfil emocional del miembro más activo",
  subtitle = sprintf("%s escribió %s mensajes (el más activo del chat). Izquierda: su perfil frente al grupo. Derecha: polaridad de los 10 más activos (número = mensajes)",
                     top1$autor[1], num(nrow(top1))))
guardar_fig(p, "perfil_mas_activo", "Perfil emocional del miembro más activo",
            sprintf("A la izquierda, la proporción de cada sentimiento en los mensajes del miembro más activo comparada con todo el grupo; a la derecha, la polaridad de los diez miembros más activos (anonimizados). En %s predomina %s (%s de sus mensajes con sentimiento), frente a %s en el grupo.",
                    top1$autor[1], tolower(SENT_LAB[cw[order(-b)][1, as.character(sentimiento)]]), pct(cw[order(-b)][1, b]), pct(cw[order(-b)][1, a])), w = 13.5, h = 7)

# F07-09 Reacciones según sentimiento
rx <- ds[sentimiento %in% SENT_ORDEN, .(prom = mean(n_reacciones), con = mean(n_reacciones > 0),
                                        arriba = sum(stri_count_regex(reacciones, "👍:(\\d+)") > 0), n = .N), by = sentimiento]
rr <- ds[sentimiento %in% SENT_ORDEN & reacciones != "", {
  x <- unlist(strsplit(reacciones, "|", fixed = TRUE)); e <- sub(":\\d+$", "", x); k <- as.integer(sub("^.*:", "", x))
  .(up = sum(k[e == "👍"]), down = sum(k[e == "👎"]), risa = sum(k[e %in% c("🤣", "😁", "😂")]), total = sum(k)) }, by = sentimiento]
rx <- merge(rx, rr, by = "sentimiento", all.x = TRUE)
rx[, sentimiento := factor(sentimiento, rx[order(prom), sentimiento])]
p <- ggplot(rx, aes(prom, sentimiento)) +
  geom_segment(aes(x = 0, xend = prom, yend = sentimiento, color = sentimiento), linewidth = 2, show.legend = FALSE) +
  geom_point(aes(color = sentimiento, size = con), show.legend = FALSE) + escala_sent_col() +
  geom_text(aes(label = sprintf("%.2f  |  👍 %s  👎 %s", prom, num(up %||% 0), num(down %||% 0))), hjust = -.15, family = FT_MONO, size = 3.1) +
  scale_y_discrete(labels = SENT_LAB) + scale_x_continuous(expand = expansion(c(0, .55))) +
  labs(title = "Reacciones recibidas según el sentimiento del mensaje", x = "Reacciones promedio por mensaje", y = NULL,
       subtitle = "El tamaño del punto es la proporción de mensajes que recibió al menos una reacción; a la derecha, pulgares arriba y abajo recibidos")
guardar_fig(p, "reacciones_por_sentimiento", "Reacciones recibidas según el sentimiento del mensaje",
            sprintf("Promedio de reacciones por mensaje según su sentimiento dominante. %s y %s son los que más reacciones reciben en promedio: el grupo respalda o comenta con reacciones aunque no escriba; las consultas y los agradecimientos casi no reciben.",
                    SENT_LAB[rx[order(-prom)][1, as.character(sentimiento)]], tolower(SENT_LAB[rx[order(-prom)][2, as.character(sentimiento)]])), h = 7.4)

# F07-10 Reír para no llorar
ris <- ds[analizable == TRUE, .(risa = stri_detect_regex(texto_norm, "\\b(jaja\\w*|xd)\\b") | stri_detect_regex(emojis, "😂|🤣|💀|😅|🥲"),
                               pol_txt = fifelse(puntaje_neg > puntaje_pos, "Mensaje negativo", fifelse(puntaje_pos > puntaje_neg, "Mensaje positivo", "Sin sentimiento o neutro")),
                               tipo = sub(" \\d{4}$", "", periodo))]
r10 <- ris[, .(p = mean(risa), n = .N), by = .(pol_txt, tipo)]
r10[, tipo := factor(tipo, c("Verano", "Semestre I", "Invierno", "Semestre II"))]
p <- ggplot(r10, aes(tipo, p, fill = pol_txt)) + geom_col(position = position_dodge(.75), width = .7) +
  geom_text(aes(label = pct(p, 0)), position = position_dodge(.75), vjust = -.4, family = FT_MONO, size = 3.3) +
  scale_fill_manual(values = c(`Mensaje negativo` = "#E11D48", `Mensaje positivo` = "#16A34A", `Sin sentimiento o neutro` = "#CBD5E1"), name = NULL) +
  scale_y_continuous(labels = percent, expand = expansion(c(0, .12))) +
  labs(title = "Presencia de risa según la polaridad del mensaje", x = NULL, y = "Mensajes con marcadores de risa (jaja, xd, emojis de risa)",
       subtitle = sprintf("Proporción de mensajes con marcadores de risa según su polaridad: %s en los mensajes negativos y %s en los neutros",
                          pct(ris[pol_txt == "Mensaje negativo", mean(risa)]), pct(ris[pol_txt == "Sin sentimiento o neutro", mean(risa)])))
guardar_fig(p, "risa_por_polaridad", "Presencia de risa según la polaridad del mensaje",
            sprintf("Porcentaje de mensajes con marcadores de risa (jaja, xd o emojis de risa) según su polaridad. La risa aparece sobre todo en mensajes positivos (%s), pero también en el %s de los negativos, lo que indica que parte de las quejas se expresa en tono humorístico.",
                    pct(ris[pol_txt == "Mensaje positivo", mean(risa)]), pct(ris[pol_txt == "Mensaje negativo", mean(risa)])), h = 6.4)

# F07-11 Validación texto vs emoji
tv <- as.data.table(as.data.frame(tabv)); setnames(tv, c("texto", "emoji", "n"))
tv[, p := n / sum(n), by = texto]
p <- ggplot(tv, aes(emoji, texto, fill = p)) + geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = sprintf("%s\n(%s)", num(n), pct(p, 0))), family = FT_MONO, size = 4, color = ifelse(tv$p > .45, "white", TINTA)) +
  scale_fill_gradient(low = "#F3E8FF", high = "#7E22CE", labels = percent, name = "% de la fila") +
  scale_y_discrete(limits = rev(levels(tv$texto))) +
  theme(panel.grid.major = element_blank()) +
  labs(title = "Validación interna: polaridad del texto frente a la de los emojis", x = "Polaridad según los emojis", y = "Polaridad según las palabras",
       subtitle = sprintf("%s mensajes tienen polaridad en sus palabras y en sus emojis. Coinciden en %s; kappa de Cohen = %.2f (acuerdo moderado, mayor que el azar)", num(nrow(vv)), pct(po), kappa))
guardar_fig(p, "validacion_texto_emoji", "Validación interna: polaridad del texto frente a la de los emojis",
            "Matriz de concordancia entre la polaridad medida solo con palabras y solo con emojis en los mismos mensajes. La diagonal concentra la mayoría y el kappa positivo confirma que ambas fuentes miden el mismo fenómeno; los desacuerdos suelen ser humor o ironía.", w = 9, h = 7)

# F07-12 Términos que construyen cada sentimiento
tt <- H[fuente %in% c("frase", "palabra", "raiz", "emoji", "emoticon")][, .N, by = .(sentimiento, termino)][order(sentimiento, -N)][, head(.SD, 5), by = sentimiento]
tt[, termino2 := tidytext::reorder_within(termino, N, sentimiento)]
tt[, sentimiento := factor(sentimiento, SENT_ORDEN, SENT_LAB[SENT_ORDEN])]
p <- ggplot(tt, aes(N, termino2, fill = sentimiento)) + geom_col(width = .7, show.legend = FALSE) +
  facet_wrap(~sentimiento, scales = "free", ncol = 5) + tidytext::scale_y_reordered() +
  scale_fill_manual(values = setNames(SENT_COL, SENT_LAB)) +
  theme(strip.text = element_text(size = 12), axis.text.y = element_text(size = 9.5)) +
  labs(title = "Términos que más aportan a cada sentimiento", x = "Apariciones", y = NULL,
       subtitle = "Los cinco términos, emojis o emoticones que más aportan a cada uno de los 15 sentimientos")
guardar_fig(p, "terminos_por_sentimiento", "Términos que más aportan a cada sentimiento",
            "Para cada sentimiento, los cinco elementos del diccionario con más apariciones. Sirve para interpretar los resultados: la frustración se construye con 'no me deja', 'sin cupo' y 'error'; la admiración, con 'enseña bien' y 'buen docente'.", w = 14, h = 9.4)

# F07-13 Índice de ánimo diario
pk <- dia[!is.na(indice)][order(indice)][c(1:4, (.N - 3):.N)]
p <- ggplot(dia[!is.na(indice)], aes(fecha, indice)) +
  geom_hline(yintercept = 0, color = TINTA3) +
  geom_area(aes(y = pmax(indice, 0)), fill = alpha(POL_COL[["positivo"]], .35)) +
  geom_area(aes(y = pmin(indice, 0)), fill = alpha(POL_COL[["negativo"]], .35)) +
  geom_line(color = "#7E22CE", linewidth = .5) +
  geom_point(data = pk, color = "#7E22CE", size = 2.6) +
  ggrepel::geom_label_repel(data = pk, aes(label = format(fecha, "%d/%m/%Y")), family = FT_MONO, size = 2.9, fill = "white", label.size = 0, max.overlaps = 20) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes) + scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Termómetro de ánimo del grupo", x = NULL, y = "Índice de ánimo (-1 a +1)",
       subtitle = "Índice (positivos - negativos) / (positivos + negativos) en ventana móvil de 7 días; se marcan los 4 días más altos y los 4 más bajos")
guardar_fig(p, "termometro_animo", "Termómetro de ánimo diario (ventana de 7 días)",
            "Índice de ánimo diario suavizado a 7 días. Por encima de cero predomina lo positivo y por debajo lo negativo. Los días marcados (máximos y mínimos) son las semanas que conviene leer en detalle; el dashboard permite filtrar esas fechas y ver de qué se hablaba.", w = 12.5, h = 6.4)

# F07-14 Ironía
ir <- ds[ironia == TRUE]
ir_fr <- data.table(frase = c(F6$D$ironia, F6$D$ironia_emoji))[, n := vapply(frase, function(f) sum(stri_detect_fixed(paste(ir$texto_norm, ir$emojis), f)), 1L)][n > 0][order(n)]
ir_fr[, frase := factor(frase, frase)]
p <- ggplot(ir_fr, aes(n, frase)) + geom_col(fill = "#8E5572", width = .65) + geom_text(aes(label = n), hjust = -.3, family = FT_MONO) +
  scale_x_continuous(expand = expansion(c(0, .15))) +
  labs(title = "Ironía detectada", x = "Mensajes irónicos que contienen la señal", y = NULL,
       subtitle = sprintf("%d mensajes con señal de ironía y palabras positivas: su parte positiva se reinterpretó como decepción", nrow(ir)))
guardar_fig(p, "ironia", "Señales de ironía detectadas",
            "Frases y emojis de ironía encontrados en mensajes que además tenían palabras positivas. En esos casos ('excelente atención como siempre 🙃') lo positivo se trató como decepción para no sobrestimar el buen ánimo.", h = 5.4)

fase_fin()
