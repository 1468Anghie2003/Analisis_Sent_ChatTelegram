# =====================================================================
#  F02 | EXPLORACIÓN DEL DATASET ORIGINAL (ANTES DE LIMPIAR)
#  Objetivo: conocer el volumen, la cobertura temporal por año, el ritmo
#  de la conversación (horas, días, temporadas), quién participa y cómo
#  se interactúa (respuestas, reacciones), para decidir la limpieza.
#  Métodos: estadística descriptiva, curva de Lorenz e índice de Gini,
#  mapas de calor temporales.
# =====================================================================
source(file.path(if (dir.exists("02_Codigo")) "02_Codigo" else ".", "00_config.R"))
fase_inicio("F02")

# ---- 1. Variables temporales ------------------------------------------
#> Se usa date_unixtime (segundos UTC) convertido a la hora de Bolivia;
#> de ahí salen fecha, año, mes, semana ISO, día de la semana, hora,
#> semestre (I/II) y periodo académico (verano, invierno, semestres).
orig <- cargar_chk("f01_original")
orig[, fh := as.POSIXct(as.numeric(date_unixtime), origin = "1970-01-01", tz = CFG$zona_horaria)]
orig[, `:=`(fecha = as.Date(fh, tz = CFG$zona_horaria), anio = as.integer(format(fh, "%Y")),
            hora = as.integer(format(fh, "%H")), dsem = as.integer(format(fh, "%u")))]
orig[, mes := as.Date(format(fecha, "%Y-%m-01"))]
msg <- orig[type == "message"]
bit("1. Variables temporales", "Fecha y hora local (America/La_Paz) derivadas de date_unixtime",
    sprintf("%s mensajes y %s avisos del sistema", num(nrow(msg)), num(sum(orig$type == "service"))),
    antes = nrow(orig), despues = nrow(orig),
    justificacion = "La hora local es necesaria para analizar horarios reales de conversación")

# ---- 2. Cobertura por año ---------------------------------------------
#> Para cada año se calcula la primera y la última fecha con mensajes, el
#> total de mensajes, los días con actividad y los autores distintos. Así
#> se sabe qué años están completos (2024 y 2025) y cuáles no (2023 empieza
#> con el primer mensaje del historial; 2026 termina en la descarga), lo que
#> obliga a comparar años con porcentajes y no con totales.
cob <- msg[, .(desde = min(fh), hasta = max(fh), mensajes = .N, dias_activos = uniqueN(fecha),
               autores = uniqueN(from_id)), by = anio][order(anio)]
cob[, dias_calendario := as.integer(as.Date(hasta) - as.Date(desde)) + 1L]
guardar_tabla(cob[, .(anio, desde = format(desde, "%d/%m/%Y %H:%M"), hasta = format(hasta, "%d/%m/%Y %H:%M"),
                      mensajes, dias_activos, dias_calendario, autores)], "cobertura_por_anio")
for (i in seq_len(nrow(cob)))
  bit("2. Cobertura por año", sprintf("%d: del %s al %s", cob$anio[i], format(cob$desde[i], "%d/%m/%Y"), format(cob$hasta[i], "%d/%m/%Y")),
      sprintf("%s mensajes, %d días con mensajes, %s autores", num(cob$mensajes[i]), cob$dias_activos[i], num(cob$autores[i])),
      justificacion = "Cada año del dataset tiene un rango distinto: 2023 y 2026 están incompletos")

d <- copy(cob)[, `:=`(y = factor(anio, rev(anio)),
                      ini = as.Date(paste0(anio, "-01-01")) + (as.numeric(format(desde, "%j")) - 1),
                      fin = as.Date(paste0(anio, "-01-01")) + (as.numeric(format(hasta, "%j")) - 1))]
d[, `:=`(x0 = as.numeric(format(desde, "%j")), x1 = as.numeric(format(hasta, "%j")))]
p <- ggplot(d) +
  geom_rect(aes(xmin = 1, xmax = 366, ymin = as.numeric(y) - .32, ymax = as.numeric(y) + .32), fill = alpha(GRAF$color, .10)) +
  geom_rect(aes(xmin = x0, xmax = x1, ymin = as.numeric(y) - .32, ymax = as.numeric(y) + .32), fill = GRAF$color) +
  geom_text(aes(x = x0, y = as.numeric(y) + .48, label = format(desde, "desde %d/%m")), hjust = 0, size = 3.4, family = FT_MONO, color = TINTA2) +
  geom_text(aes(x = x1, y = as.numeric(y) + .48, label = format(hasta, "hasta %d/%m")), hjust = 1, size = 3.4, family = FT_MONO, color = TINTA2) +
  geom_text(aes(x = (x0 + x1) / 2, y = as.numeric(y), label = sprintf("%s mensajes · %d días activos · %s autores",
                                                                      num(mensajes), dias_activos, num(autores))),
            color = "white", family = FT_TXT, fontface = "bold", size = 3.6) +
  scale_y_continuous(breaks = seq_len(nrow(d)), labels = levels(d$y)) +
  scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                     labels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")) +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(family = FT_TIT, face = "bold.italic", size = 16, color = TINTA)) +
  labs(title = "Cobertura del dataset por año", x = NULL, y = NULL,
       subtitle = "La barra coloreada marca desde qué fecha hasta qué fecha hay registros en cada año\n2023 empieza en junio (primer mensaje del historial) y 2026 llega hasta octubre (fecha de la descarga)")
guardar_fig(p, "cobertura_por_anio", "Cobertura temporal del dataset por año",
            "Rango de fechas con mensajes en cada año, con volumen, días activos y autores distintos. 2024 y 2025 están completos; 2023 empieza el 22 de junio (primer mensaje disponible en el historial exportado) y 2026 llega hasta mediados de octubre (fecha de la descarga).", h = 5.6)

# ---- 3. Actividad mensual y temporadas ------------------------------------
#> Se cuentan los mensajes por mes. Los cambios de nombre del grupo
#> (acción edit_group_title) marcan el inicio de cada temporada (verano,
#> invierno): son etiquetas que el propio dataset trae y que permiten
#> ubicar en la serie cuándo empieza cada curso de temporada.
tit <- orig[action == "edit_group_title", .(fecha, title = stri_trim_both(stri_replace_all_regex(title, "(?i)curso de temporada|[^\\p{L}\\p{N} ]", "")))]
setorder(tit, fecha)
tit <- tit[, .SD[c(TRUE, diff(as.numeric(fecha)) > 30)], by = title][order(fecha)][, nivel := rep_len(c(1.10, 1.02), .N)]
mens <- msg[, .N, by = mes]
p <- ggplot(mens, aes(mes, N)) +
  geom_area(fill = alpha(GRAF$color, .25), color = GRAF$color, linewidth = 1) +
  geom_point(color = GRAF$color, size = 1.8) +
  geom_vline(data = tit, aes(xintercept = fecha), linetype = "22", color = TINTA2, linewidth = .5) +
  geom_label_xd(data = tit, mapping = aes(x = fecha, y = max(mens$N) * nivel, label = title), family = FT_TIT, fontface = "bold.italic",
             size = 3.6, fill = "white", color = TINTA, label.size = 0, label.r = unit(6, "pt")) +
  scale_x_date(date_breaks = "3 months", labels = etiq_mes) + scale_y_continuous(labels = comma, expand = expansion(c(0, .14))) +
  labs(title = "Mensajes por mes y cambios de temporada", x = NULL, y = "Mensajes",
       subtitle = "Las líneas punteadas son los cambios de nombre del grupo, que anuncian cada nueva temporada (verano / invierno)")
mes_max <- mens[which.max(N)]
bit("3. Actividad mensual", sprintf("Serie mensual con %d cambios de nombre del grupo (temporadas)", nrow(tit)),
    sprintf("Mes con más mensajes: %s (%s); mediana mensual: %s", etiq_mes1(mes_max$mes), num(mes_max$N), num(median(mens$N))),
    justificacion = "Ubicar las temporadas en el tiempo explica los picos y define los periodos académicos usados en todo el análisis")
guardar_fig(p, "actividad_mensual_temporadas", "Mensajes por mes y cambios de temporada",
            "Serie mensual de mensajes con los cambios de nombre del grupo. La actividad se concentra entre junio y agosto y entre diciembre y febrero, cuando se inscriben y cursan las materias de invierno y verano; entre temporadas el chat casi se apaga.", h = 6.4)

# ---- 4. Calendario diario ---------------------------------------------------
#> Se completa la serie con todos los días del rango (los días sin
#> mensajes valen 0) y se dibuja como calendario: semana del año en el eje
#> horizontal y día de la semana en el vertical, un panel por año. Permite
#> ver de un vistazo las semanas de inscripción y los días aislados con picos.
dia <- msg[, .N, by = fecha]
todos <- data.table(fecha = seq(min(dia$fecha), max(dia$fecha), by = "day"))
dia <- merge(todos, dia, all.x = TRUE)[is.na(N), N := 0L]
dia[, `:=`(anio = as.integer(format(fecha, "%Y")), sem = as.integer(format(fecha, "%U")), dsem = as.integer(format(fecha, "%u")))]
p <- ggplot(dia, aes(sem, dsem, fill = N)) +
  geom_tile(color = GRAF$fondo, linewidth = .6) +
  facet_wrap(~anio, ncol = 1) +
  scale_fill_gradientn(colors = c("#FFF6DA", "#FFD25E", "#F59E0B", "#D9480F", "#8A1C0F"), trans = "sqrt",
                       name = "Mensajes en el día", breaks = c(0, 10, 50, 150, 300)) +
  scale_y_reverse(breaks = 1:7, labels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom")) +
  scale_x_continuous(breaks = c(0, 9, 17, 26, 35, 44), labels = c("ene", "mar", "may", "jul", "sep", "nov")) +
  theme(panel.grid = element_blank(), panel.grid.major = element_blank(), legend.key.width = unit(2.2, "cm")) +
  labs(title = "Calendario diario de mensajes", x = NULL, y = NULL,
       subtitle = "Cada celda es un día; los colores intensos revelan las semanas de inscripción de cada temporada")
bit("4. Calendario diario", sprintf("%s días en el rango; %s con al menos un mensaje", num(nrow(dia)), num(sum(dia$N > 0))),
    sprintf("Día con más mensajes: %s (%s)", format(dia[which.max(N), fecha], "%d/%m/%Y"), num(max(dia$N))),
    justificacion = "La vista diaria muestra que la actividad es estacional y se concentra en pocas semanas")
guardar_fig(p, "calendario_diario", "Calendario diario de mensajes (2023-2026)",
            "Mapa de calor tipo calendario: cada celda es un día. Se ven bloques intensos a fines de junio, inicios de agosto, enero y febrero (inscripciones y adición/retiro de cada temporada) y días aislados con picos que se analizan en F10.", h = 8.6)

# ---- 5. Hora x día de la semana ------------------------------------------------
#> Se cruzan la hora local (0 a 23) y el día de la semana en una tabla de
#> frecuencias y se representa como mapa de calor. Sirve para saber en qué
#> momentos se conversa más y, más adelante (F10), si la hora influye en
#> que una pregunta reciba respuesta.
hd <- msg[, .N, by = .(hora, dsem)]
p <- ggplot(hd, aes(hora, factor(dsem), fill = N)) +
  geom_tile(color = GRAF$fondo, linewidth = 1.1) +
  geom_text(aes(label = ifelse(N >= quantile(N, .9), num(N), "")), size = 2.8, family = FT_MONO, color = "white") +
  scale_fill_gradientn(colors = c("#FFF6DA", "#FFC93C", "#F08A24", "#C2410C", "#6B1D0B"), name = "Mensajes") +
  scale_y_discrete(labels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"), limits = rev) +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02dh", 0:23), expand = c(0, 0)) +
  theme(panel.grid = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "Mensajes por hora y día de la semana", x = NULL, y = NULL,
       subtitle = "Mensajes acumulados por hora y día de la semana (hora de Bolivia); los números marcan el 10% de celdas más activas")
hd_max <- hd[which.max(N)]
bit("5. Hora y día", "Tabla de frecuencias hora x día de la semana",
    sprintf("Celda más activa: %s a las %02d h (%s mensajes)", DIAS_ES[hd_max$dsem], hd_max$hora, num(hd_max$N)),
    justificacion = "Describe el ritmo diario de la conversación y prepara el análisis de la mejor hora para preguntar")
guardar_fig(p, "hora_dia_semana", "Mensajes por hora y día de la semana",
            "Mapa de calor hora × día. La conversación se concentra entre las 9 y las 13 horas de lunes a miércoles, con un segundo repunte cerca de las 19 horas; la madrugada y el fin de semana son los momentos más tranquilos.", h = 5.8)

# ---- 6. Crecimiento de miembros ---------------------------------------------------
#> Cada aviso del sistema join_group_by_link o invite_members corresponde a
#> una persona que entra al grupo. Se cuentan por mes y se acumulan: así se
#> ve cómo crece la comunidad y si las llegadas coinciden con las temporadas.
ent <- orig[action %in% c("join_group_by_link", "invite_members"), .N, by = mes][order(mes)]
ent[, acumulado := cumsum(N)]
p1 <- ggplot(ent, aes(mes, acumulado)) + geom_step(color = GRAF$color, linewidth = 1.3) +
  geom_area(fill = alpha(GRAF$color, .12), stat = "identity") +
  scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "6 months", labels = etiq_mes1) +
  labs(title = "Llegadas acumuladas al grupo", x = NULL, y = "Personas que entraron")
p2 <- ggplot(ent, aes(mes, N)) + geom_col(fill = "#F59E0B", width = 25) +
  scale_x_date(date_breaks = "6 months", labels = etiq_mes1) +
  labs(title = "Llegadas por mes", x = NULL, y = "Personas")
p <- (p1 / p2) + plot_annotation(title = "Crecimiento de la comunidad del grupo",
                                 subtitle = sprintf("%s ingresos registrados por enlace o invitación; cada temporada atrae una nueva ola de estudiantes", num(sum(ent$N))))
bit("6. Crecimiento de miembros", sprintf("%s ingresos registrados por enlace o invitación", num(sum(ent$N))),
    sprintf("Mes con más ingresos: %s (%s personas)", etiq_mes1(ent[which.max(N), mes]), num(max(ent$N))),
    justificacion = "Los avisos del sistema no son conversación, pero sí informan cuándo llega gente nueva al grupo")
guardar_fig(p, "crecimiento_miembros", "Crecimiento de la comunidad del grupo",
            "Arriba, el total acumulado de personas que entraron al grupo; abajo, las llegadas por mes. Las olas de ingreso coinciden con las semanas de inscripción a los cursos de temporada.", h = 7.4)

# ---- 7. Concentración de la participación (Lorenz y Gini) ---------------------------
#> Para qué se usa: para medir si la conversación la sostienen todos los
#> miembros o solo unos pocos. La curva de Lorenz ordena a los autores de
#> menos a más activos y acumula el porcentaje de mensajes; si todos
#> escribieran lo mismo, la curva sería la diagonal. El índice de Gini
#> resume la distancia a esa diagonal: 0 = participación igualitaria,
#> 1 = una sola persona escribe todo.
#> A qué se quiere llegar: si el Gini es alto, los porcentajes de
#> sentimientos del grupo están muy influidos por los miembros más activos.
#> Por eso en F07 se compara el perfil del miembro más activo con el del
#> grupo, en el dashboard se puede ver el perfil de cada miembro y los
#> resultados se interpretan como "lo que expresa el chat", no como la
#> opinión de todos los estudiantes.
aut <- msg[!is.na(from_id), .N, by = from_id][order(N)]
aut[, `:=`(pm = seq_len(.N) / .N, pc = cumsum(N) / sum(N))]
gini <- 1 - 2 * sum((aut$pc - aut$N / sum(aut$N) / 2) / nrow(aut))
top10 <- aut[order(-N)][seq_len(ceiling(.1 * .N)), sum(N)] / sum(aut$N)
top1  <- aut[order(-N)][seq_len(ceiling(.01 * .N)), sum(N)] / sum(aut$N)
p <- ggplot(aut, aes(pm, pc)) +
  geom_abline(linetype = "22", color = TINTA3) +
  geom_area(fill = alpha(GRAF$color, .18)) + geom_line(color = GRAF$color, linewidth = 1.5) +
  annotate_label(x = .30, y = .72, family = FT_TXT, size = 4, fill = "white", label.size = 0, hjust = 0,
           label = sprintf("Índice de Gini = %.2f\nEl 10%% más activo escribe el %s\nEl 1%% más activo escribe el %s", gini, pct(top10), pct(top1))) +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "Concentración de la participación (curva de Lorenz)", x = "Miembros ordenados de menos a más activos", y = "Mensajes acumulados",
       subtitle = sprintf("Curva de Lorenz de %s autores; la diagonal representa una participación igualitaria. Cuanto más se aleja la curva de la diagonal, más concentrada está la conversación en pocos miembros", num(nrow(aut))))
bit("7. Concentración (Lorenz y Gini)", sprintf("Índice de Gini = %.2f sobre %s autores", gini, num(nrow(aut))),
    sprintf("El 10%% más activo escribe el %s de los mensajes y el 1%% más activo el %s", pct(top10), pct(top1)),
    justificacion = "Una participación concentrada obliga a interpretar los sentimientos como los del chat, no los de todos los estudiantes")
guardar_fig(p, "lorenz_participacion", "Concentración de la participación (curva de Lorenz)",
            sprintf("Curva de Lorenz e índice de Gini (%.2f) de los mensajes por autor. Se usa para medir si la conversación la sostienen todos o unos pocos: el 10%% más activo escribe el %s de los mensajes. En consecuencia, los porcentajes de sentimientos describen lo que expresa el chat y están influidos por sus miembros más activos; por eso en F07 y en el dashboard se compara el perfil de cada miembro (anonimizado) con el del grupo.", gini, pct(top10)), w = 10, h = 8.4)

# ---- 8. Interacción por año ----------------------------------------------------------
#> Se calcula, para cada año, qué proporción de mensajes responde a otro,
#> recibe reacciones, se edita, se reenvía o trae foto. Las respuestas
#> permiten reconstruir los hilos de conversación (F10) y las reacciones
#> son una señal de acuerdo o desacuerdo que no depende del texto.
inter <- msg[, .(`Responde a otro mensaje` = mean(!is.na(reply_to_message_id)), `Recibió reacciones` = mean(!is.na(reactions)),
                 `Fue editado` = mean(!is.na(edited)), `Es reenviado` = mean(!is.na(forwarded_from) | !is.na(forwarded_from_id)),
                 `Tiene foto` = mean(!is.na(photo))), by = anio]
inter <- melt(inter, id.vars = "anio")
p <- ggplot(inter, aes(value, variable, color = factor(anio))) +
  geom_line(aes(group = variable), color = alpha(TINTA3, .5), linewidth = 1.2) +
  geom_point(size = 5) +
  scale_color_manual(values = c("#FFC93C", "#F59E0B", "#D9480F", "#7C2D12"), name = "Año") +
  scale_x_continuous(labels = percent) +
  labs(title = "Formas de interacción por año", x = "Porcentaje de mensajes del año", y = NULL,
       subtitle = "Cada punto es un año: cuántos mensajes responden a otro, reciben reacciones, se editan, se reenvían o traen foto")
bit("8. Interacción por año", "Proporción de respuestas, reacciones, ediciones, reenvíos y fotos por año",
    sprintf("Respuestas a otro mensaje: %s del total; mensajes con reacciones: %s", pct(mean(!is.na(msg$reply_to_message_id))), pct(mean(!is.na(msg$reactions)))),
    justificacion = "Define qué señales de interacción existen para los análisis de hilos y reacciones")
guardar_fig(p, "interaccion_por_anio", "Formas de interacción por año",
            "Proporción de mensajes que son respuestas, que reciben reacciones, que se editan, que se reenvían o que traen foto, por año. Las respuestas permiten reconstruir hilos de conversación (F10) y las reacciones son una señal directa del ánimo del grupo.", h = 5.6)

# ---- 9. Reacciones más usadas (emojis 3D) -------------------------------------------
#> El campo reactions (texto JSON) se lee mensaje por mensaje y se suman
#> las veces que se usó cada emoji como reacción. Las reacciones son la
#> forma de expresar acuerdo o desacuerdo sin escribir y se incorporan al
#> diccionario de emojis en F06.
reac <- rbindlist(lapply(msg[!is.na(reactions), reactions], function(j) {
  r <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  data.table(emoji = vapply(r, function(z) z$emoji %||% z$type %||% "?", ""), n = vapply(r, function(z) as.integer(z$count %||% 0L), 1L))
}))
reac <- reac[, .(n = sum(n)), by = .(emoji = norm_emoji(emoji))][order(-n)][1:min(.N, 15)]
reac[, `:=`(pos = rev(seq_len(.N)), rz = sqrt(n))]
#> La escala raíz cuadrada se aplica a mano (x = raíz de n) para que las
#> imágenes de los emojis, ubicadas a la izquierda del cero, no queden en
#> una zona sin definir de la escala (falla en ggplot2 4.x).
xmax <- max(reac$rz)
cortes <- c(10, 100, 500, 1000, 2000, 4000); cortes <- cortes[cortes <= max(reac$n) * 1.1]
p <- ggplot(reac, aes(rz, pos)) +
  geom_col(fill = GRAF$color, width = .62, orientation = "y") +
  geom_text(aes(label = num(n)), hjust = -0.25, family = FT_MONO, size = 3.6) +
  scale_x_continuous(breaks = sqrt(cortes), labels = comma(cortes), expand = expansion(c(0, .1))) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(clip = "off") + theme(plot.margin = margin(22, 24, 16, 70)) +
  labs(title = "Reacciones más usadas en el chat", x = "Veces usada (escala raíz cuadrada)", y = NULL,
       subtitle = sprintf("%s reacciones en %s mensajes; el pulgar arriba domina como señal de acuerdo y el pulgar abajo como desacuerdo",
                          num(sum(reac$n)), num(sum(!is.na(msg$reactions)))))
p <- agregar_emojis(p, reac$emoji, x = rep(-xmax * .045, nrow(reac)), y = reac$pos, w = xmax * .045, h = .85)
bit("9. Reacciones", sprintf("%s reacciones en %s mensajes", num(sum(reac$n)), num(sum(!is.na(msg$reactions)))),
    sprintf("Más usadas: %s", paste(sprintf("%s %s", reac$emoji[1:min(5, nrow(reac))], num(reac$n[1:min(5, nrow(reac))])), collapse = ", ")),
    justificacion = "Las reacciones son una señal de ánimo independiente del texto")
guardar_fig(p, "reacciones_top", "Reacciones más usadas en el chat",
            "Las 15 reacciones más frecuentes (emojis 3D Fluent). El pulgar arriba concentra la mayoría de reacciones: funciona como un 'me sirve / de acuerdo'. El pulgar abajo es la principal forma de desaprobar sin escribir.", h = 7)

guardar_chk(orig[, .(id, fh, fecha, anio, hora, dsem, mes)], "f02_tiempos")
fase_fin()
