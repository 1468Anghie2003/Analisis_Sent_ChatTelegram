# =====================================================================
#  Análisis de Sentimientos (carpeta GrupoXDMSent) | GENERAR_DASHBOARD.R
#  Construye los datos de la página web (carpeta docs/) a partir de las
#  salidas del pipeline. Ninguna cifra del dashboard se escribe a mano.
#  Uso: source("GENERAR_DASHBOARD.R") después de EJECUTAR_TODO.R
#  Crea: docs/data/datos.js      (resultados, modelos, bitácora, scripts)
#        docs/data/mensajes.js   (un registro compacto por mensaje: permite
#                                 filtrar por cualquier rango de fechas)
#        docs/img/*.png          (las figuras de todas las fases)
#        docs/emoji/*.png        (emojis 3D Fluent usados en el chat)
# =====================================================================
source(file.path("02_Codigo", "00_config.R"), encoding = "UTF-8")
library(jsonlite)
dir.create(file.path(DIR$web, "data"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DIR$web, "img"), showWarnings = FALSE)
dir.create(file.path(DIR$web, "emoji"), showWarnings = FALSE)

F3 <- cargar_chk("f03_limpio"); F4 <- cargar_chk("f04_tokens"); F5 <- cargar_chk("f05_stop"); F6 <- cargar_chk("f06_dic")
F7 <- cargar_chk("f07_sent"); F8 <- cargar_chk("f08_lda"); F9 <- cargar_chk("f09_ent"); F10 <- cargar_chk("f10_pat")
ORIG <- cargar_chk("f01_original")
ds <- copy(F7$ds)[order(fecha_hora_local)]
ds[, fh := as.POSIXct(fecha_hora_local, tz = CFG$zona_horaria)]
d0 <- min(ds$fecha)

# ---- 1. Vocabularios ----------------------------------------------------
#> Para que el navegador pueda recalcular todo por rango de fechas sin un
#> servidor, cada palabra, bigrama, trigrama, entidad y autor se guarda una
#> sola vez en un vocabulario y los mensajes se refieren a ellos por número.
aut <- ds[, .N, by = autor][order(-N)]
tok <- F5$tok_c[id %in% ds$id]
voc_t <- tok[, .N, by = palabra][N >= CFG$min_freq_vocab][order(-N), palabra]
big_v <- F5$big_f[1:min(.N, 1500), bigrama]; tri_v <- F5$tri_f[1:min(.N, 1500), trigrama]
big <- F4$big[bigrama %in% big_v & id %in% ds$id]; tri <- F4$tri[trigrama %in% tri_v & id %in% ds$id]
DE <- F6$DE
emo_v <- DE$emoji
jer <- fread(file.path(DIR$dic, "dic_jerga_expresiones.csv"), encoding = "UTF-8")
ent <- F9$men[, .(id, k = paste(tipo, entidad, sep = "|"))]
ent_v <- ent[, .N, by = k][order(-N), k]
ctipos <- c("texto", "foto", "sticker", "video", "audio", "archivo", "encuesta")

# ---- 2. Registro compacto por mensaje -----------------------------------
#> Una fila por mensaje en formato columnar: día, hora, día de la semana,
#> autor, sentimiento, polaridad, si es pregunta y si fue respondida,
#> tópico, ironía, reacciones y los índices de sus palabras, frases, emojis,
#> expresiones y entidades. Con esto el dashboard filtra cualquier rango.
lista_idx <- function(dt_id_val, vocab, ids) {          # id -> vector de índices (base 0)
  m <- dt_id_val[, .(v = list(match(val, vocab) - 1L)), by = id]
  out <- vector("list", length(ids)); names(out) <- ids
  out[m$id] <- m$v; lapply(out, function(z) if (is.null(z)) integer() else z[!is.na(z)])
}
ids <- ds$id
L_t <- lista_idx(tok[palabra %in% voc_t][order(id, pos)][, .(id, val = palabra)], voc_t, ids)
L_b <- lista_idx(big[, .(id, val = bigrama)], big_v, ids)
L_g <- lista_idx(tri[, .(id, val = trigrama)], tri_v, ids)
em_l <- ds[emojis != "", .(val = c(strsplit(emojis, " ", fixed = TRUE)[[1]])), by = id]
em_l <- rbind(em_l, ds[!is.na(emoji_sticker), .(id, val = emoji_sticker)])
L_e <- lista_idx(em_l[val %in% emo_v], emo_v, ids)
ex_l <- ds[expresiones != "", .(val = strsplit(expresiones, "|", fixed = TRUE)[[1]]), by = id]
L_x <- lista_idx(ex_l, jer$expresion, ids)
L_n <- lista_idx(ent[, .(id, val = k)], ent_v, ids)
rx_l <- ds[reacciones != "", { x <- strsplit(reacciones, "|", fixed = TRUE)[[1]]; .(e = sub(":\\d+$", "", x), n = as.integer(sub("^.*:", "", x))) }, by = id]
rx_l[, ei := match(e, emo_v) - 1L]
rx_m <- rx_l[!is.na(ei), .(v = list(as.integer(rbind(ei, n)))), by = id]
L_r <- setNames(vector("list", length(ids)), ids); L_r[rx_m$id] <- rx_m$v; L_r <- lapply(L_r, function(z) if (is.null(z)) integer() else z)
smask <- as.integer(Reduce(`+`, lapply(seq_along(SENT_ORDEN), function(i) (ds[[paste0("s_", SENT_ORDEN[i])]] > 0) * 2^(i - 1))))
pq <- F10$pq
ans <- ifelse(ds$id %in% pq$orig, as.integer(pq$respondida[match(ds$id, pq$orig)]), -1L)
rt <- ifelse(ds$id %in% pq$orig & !is.na(pq$minutos[match(ds$id, pq$orig)]), round(pq$minutos[match(ds$id, pq$orig)]), -1)
tp <- F8$msg_top$topico[match(ds$id, F8$msg_top$id)] - 1L; tp[is.na(tp)] <- -1L
txt <- if (CFG$publicar_textos) stri_sub(stri_replace_all_regex(ds$texto_mostrar, "\\s+", " "), 1, 300) else rep("", nrow(ds))
M <- list(
  d = as.integer(ds$fecha - d0), h = ds$hora, w = match(ds$dia_semana, DIAS_ES) - 1L, a = match(ds$autor, aut$autor) - 1L,
  s = ifelse(ds$sentimiento %in% SENT_ORDEN, match(ds$sentimiento, SENT_ORDEN) - 1L, ifelse(ds$sentimiento == "sin_sentimiento", -1L, -2L)),
  sm = smask, p = ifelse(ds$polaridad == "positivo", 1L, ifelse(ds$polaridad == "negativo", -1L, 0L)),
  q = as.integer(ds$es_pregunta), ans = ans, rt = as.integer(rt), tp = tp, ir = as.integer(ds$ironia),
  ct = match(ds$tipo_contenido, ctipos) - 1L, nr = ds$n_reacciones, np = ds$n_palabras,
  t = unname(L_t), b = unname(L_b), g = unname(L_g), e = unname(L_e), x = unname(L_x), n = unname(L_n), r = unname(L_r), txt = txt)
for (k in c("t", "b", "g", "e", "x", "n", "r")) M[[k]] <- lapply(M[[k]], function(z) I(as.integer(z)))
XDM <- list(d0 = format(d0), n = nrow(ds), cols = M,
            vocab = list(tok = voc_t, big = big_v, tri = tri_v, ent = ent_v, aut = aut$autor, aut_n = aut$N, ct = ctipos))
js <- paste0("window.XDM = ", toJSON(XDM, auto_unbox = TRUE, digits = NA, na = "null"), ";")
writeBin(charToRaw(enc2utf8(js)), file.path(DIR$web, "data", "mensajes.js"))
cat(sprintf("docs/data/mensajes.js: %.1f MB (%s mensajes)\n", file.size(file.path(DIR$web, "data", "mensajes.js")) / 1e6, num(nrow(ds))))

# ---- 3. Emojis e imágenes ---------------------------------------------------
#> Se copian a docs/ las imágenes 3D de los emojis usados y todas las
#> figuras del pipeline, para que la carpeta docs funcione sola (GitHub Pages).
DE[, img := ifelse(imagen_3d, paste0("emoji/", codigo, ".png"), "")]
for (i in which(DE$imagen_3d)) file.copy(file.path(DIR$emoji, paste0(DE$codigo[i], ".png")), file.path(DIR$web, "emoji", paste0(DE$codigo[i], ".png")), overwrite = TRUE)
for (e in SENT_EMOJI) { f <- emoji_png(e); if (!is.na(f)) file.copy(f, file.path(DIR$web, "emoji", basename(f)), overwrite = TRUE) }
file.copy(file.path(DIR$emoji, "LICENSE_FluentEmoji_MIT.txt"), file.path(DIR$web, "emoji", "LICENSE_FluentEmoji_MIT.txt"), overwrite = TRUE)
unlink(list.files(file.path(DIR$web, "img"), pattern = "\\.png$", full.names = TRUE))
idx <- fread(file.path(DIR$fig, "indice_figuras.csv"), encoding = "UTF-8")
for (r in idx$ruta) file.copy(file.path(RAIZ, r), file.path(DIR$web, "img", basename(r)), overwrite = TRUE)

# ---- 4. Textos del proyecto (objetivos y fases) -------------------------------
#> Objetivos y, para cada fase, su objetivo, el tema de clase que aplica,
#> los métodos y la justificación. Se acompañan del texto completo de
#> cada script, de los pasos de la bitácora y de las figuras de la fase.
OBJ <- list(
  general = "Analizar los sentimientos, temas y patrones de conversación del grupo de Telegram del Curso de Temporada de la Carrera de Informática (UMSA) entre 2023 y 2026, con un pipeline reproducible en R y un dashboard interactivo.",
  especificos = c("Extraer el historial completo del chat desde JSON a un dataset tabular sin pérdida de información.",
                  "Limpiar y normalizar los mensajes, excluyendo avisos del sistema y publicidad, y tratar emojis, jerga juvenil y expresiones bolivianas.",
                  "Construir diccionarios de los 15 sentimientos acordados en clase, de emojis y de jerga, ampliados con el propio dataset.",
                  "Medir la distribución y la evolución de los sentimientos por día, semana, mes, semestre, periodo académico y año.",
                  "Descubrir temas latentes (LDA, TF-IDF, log-odds), entidades (docentes, materias, lugares, horarios) y sus relaciones.",
                  "Hallar patrones ocultos: causas de la incertidumbre, la mejor hora para preguntar, días pico y la red de ayuda del grupo.",
                  "Pronosticar la actividad del chat y validar el diccionario con señales independientes y un clasificador supervisado.",
                  "Documentar cada decisión en una bitácora y publicar los resultados en un dashboard interactivo y reproducible."))
FASE_TXT <- list(
  F01 = list(obj = "Convertir el export JSON de Telegram en una tabla con un registro por mensaje, sin perder ningún dato.",
             clase = "Tema 1: introducción a la extracción de texto (extracción estructurada y Regex).",
             metodos = "Parseo JSON con jsonlite, aplanado de claves, campos anidados como texto JSON y auditoría de no pérdida.",
             porque = "El JSON es el formato 'legible por máquina' de Telegram Desktop; aplanarlo en tuplas permite filtrar por fecha, autor o tipo de registro."),
  F02 = list(obj = "Conocer el dataset antes de limpiarlo: cobertura por año, ritmo de conversación, participación e interacción.",
             clase = "Exploración inicial (paso previo de todo proceso de minería de datos).",
             metodos = "Estadística descriptiva, mapas de calor temporales, curva de Lorenz e índice de Gini.",
             porque = "Las decisiones de limpieza dependen de lo que hay en los datos: avisos automáticos y picos de temporada. La curva de Lorenz y el índice de Gini miden cuán concentrada está la participación; como una minoría escribe gran parte de los mensajes, los sentimientos se interpretan como lo que expresa el chat y se comparan los perfiles individuales con el del grupo."),
  F03 = list(obj = "Dejar solo la conversación real y normalizar su texto.",
             clase = "Tema 1: pasos generales de extracción (preprocesamiento: limpieza y normalización).",
             metodos = "Exclusión de avisos y publicidad con 5 reglas, normalización en 8 pasos, extracción de emojis y jerga, anonimización.",
             porque = "Sin limpieza, la publicidad de grupos y eventos y las variantes de escritura (graciaaas, q, xq) distorsionan cualquier conteo."),
  F04 = list(obj = "Partir el texto en unidades contables: oraciones, palabras, caracteres, subpalabras y n-gramas.",
             clase = "Temas 2 y 3: tokenización, bigramas, tokenización de caracteres, Ej5 (traducción carácter a carácter) y Ej6 (frecuencia por palabra).",
             metodos = "unnest_tokens() de tidytext; leyes de Zipf y Heaps (externas) para verificar que el chat se comporta como lenguaje natural.",
             porque = "La tokenización es la base de frecuencias, n-gramas, diccionarios y modelos."),
  F05 = list(obj = "Quitar palabras vacías y encontrar lo distintivo de cada periodo.",
             clase = "Temas 5, 5.1, 6, 7 y 9: palabras vacías, shallow linguistics, anti_join y TF-IDF.",
             metodos = "Comparación de listas (Snowball, ISO, personalizada, automática por frecuencia), TF-IDF por año y periodo, log-odds ponderado por mes (externo).",
             porque = "La lista ISO borraba palabras con carga emocional; el log-odds revela el 'tema latente' de cada mes que no se ve mirando el año completo."),
  F06 = list(obj = "Construir los diccionarios que permiten medir sentimientos en un chat juvenil boliviano.",
             clase = "Tema 4 (Ej9: sentimientos con léxico) y los 15 sentimientos acordados en clase con la estructura S().",
             metodos = "Base de clase + ampliación desde el dataset, diccionario de emojis curado y validado con el Emoji Sentiment Ranking, jerga y emoticones.",
             porque = "Un léxico general no reconoce expresiones como 'sin cupo', 'me jaló' o 'ni modo', ni que el emoji de calavera se usa como risa; por eso el diccionario se adaptó al contexto del chat."),
  F07 = list(obj = "Medir qué siente el grupo, cuándo y por qué.",
             clase = "Tema 1 (análisis de sentimientos) y tema 4 (gráfica de sentimientos y frecuencia).",
             metodos = "Motor de puntuación con frases, palabras, raíces, emojis, negación, intensificadores e ironía; validación con kappa de Cohen.",
             porque = "El sentimiento dominante permite contar; los 15 puntajes conservan las emociones mezcladas de cada mensaje."),
  F08 = list(obj = "Descubrir de qué temas habla el grupo y cómo cambian en el tiempo.",
             clase = "Temas 5.2, 8 y 10: modelado de tópicos con LDA (tidytext y tm).",
             metodos = "Días como documentos, elección de k con perplejidad y coherencia UMass, posterior por mensaje, comparación tidytext vs tm.",
             porque = "Los mensajes son demasiado cortos para LDA; agruparlos por día da documentos con contexto suficiente."),
  F09 = list(obj = "Reconocer docentes, materias, lugares, plataformas, trámites y horarios, y relacionarlos.",
             clase = "Tema 1 (NER y extracción de relaciones) y tema 4 (Ej8: lugares en una frase).",
             metodos = "NER por reglas y diccionario, percepción de docentes con intervalo de Wilson, red docente-materia y reglas de asociación (Apriori).",
             porque = "No existe un modelo NER para la jerga universitaria local; las reglas son transparentes y reproducibles."),
  F10 = list(obj = "Identificar patrones no evidentes: causas y efectos de la incertidumbre, días pico, red de respuestas, pronóstico de actividad y modelos supervisados.",
             clase = "Tema 7 (chatbot: insumo del asistente del dashboard) + métodos externos.",
             metodos = "Hilos de respuesta, red y comunidades de Louvain, anomalías con MAD, Granger, pronóstico (4 modelos), Naive Bayes y LASSO, regresión logística.",
             porque = "El análisis no se limita a describir: busca explicar por qué ocurren los sentimientos observados y anticipar el comportamiento del chat."),
  F11 = list(obj = "Cerrar el ciclo: hallazgos automáticos, índices y bitácora en Excel.",
             clase = "Documentación y reproducibilidad.",
             metodos = "Redacción automática de hallazgos con las cifras de cada fase; bitácora con diseño; diagrama del pipeline.",
             porque = "Todo resultado debe poder rastrearse hasta el paso y el script que lo generó."))
SCRIPT_FASE <- setNames(paste0(FASES$cod, "_", FASES$nombre, ".R"), FASES$cod)
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
scripts <- list()
for (f in c(list.files(DIR$codigo, pattern = "\\.R$", full.names = TRUE), file.path(RAIZ, "EJECUTAR_TODO.R"), file.path(RAIZ, "GENERAR_DASHBOARD.R")))
  scripts[[basename(f)]] <- leer(f)
bit_df <- fread(file.path(DIR$bit, "bitacora.csv"), encoding = "UTF-8", colClasses = "character")
fases <- lapply(seq_len(nrow(FASES)), function(i) {
  cod <- FASES$cod[i]; b <- bit_df[startsWith(fase, cod)]
  c(as.list(FASES[i, ]), FASE_TXT[[cod]], list(script = SCRIPT_FASE[[cod]], bit_ids = as.integer(b$id),
    figuras = idx[fase == cod, .(nro, archivo, titulo, descripcion)]))
})

# ---- 5. Resultados y modelos ---------------------------------------------------------
#> Resultados que no dependen del rango de fechas (modelos, validaciones,
#> redes, pronóstico, hallazgos, bitácora, índice de figuras) se exportan
#> tal como salieron de cada fase.
cob <- ds[, .(desde = format(min(fh), "%d/%m/%Y"), hasta = format(max(fh), "%d/%m/%Y"), mensajes = .N, dias = uniqueN(fecha)), by = anio][order(anio)]
dic_s <- merge(F6$D$dic, F6$uso_term[, .(mensajes = sum(mensajes)), by = termino], by = "termino", all.x = TRUE)[is.na(mensajes), mensajes := 0]
nod <- F10$nodos[order(-(respondio + recibio))][1:min(.N, 90)]
edg <- F10$ed[de %in% nod$autor & a %in% nod$autor]
red_dm <- F9$rel[N >= 2][1:min(.N, 70)]
modelos <- list(
  topicos = F8$etq[, .(id = topic - 1L, nombre, etiqueta, top)],
  topicos_terminos = F8$beta[order(topic, -beta)][, head(.SD, 12), by = topic][, .(t = topic - 1L, termino = term, beta = round(beta, 5))],
  seleccion_k = F8$sel[, .(k, perplejidad = round(perplejidad, 1), coherencia = round(coherencia, 3))], k = F8$K,
  pronostico = F10$pron[, .(s = format(s), media = round(media, 1), li80 = round(li80, 1), ls80 = round(ls80, 1), li95 = round(li95, 1), ls95 = round(ls95, 1))],
  semanal = F10$sem[, .(s = format(s), n = N)], pron_modelos = F10$ev[, .(modelo, MAE = round(MAE, 1), RMSE = round(RMSE, 1))], pron_mejor = F10$mejor_m,
  cv = F10$res_cv[, .(modelo, exactitud = round(exactitud, 3), f1 = round(f1, 3))], nuevas = F10$nuevas[order(coef)][, .(palabra, coef = round(coef, 3))],
  odds = F10$orx[, .(var, or = round(or, 3), li = round(li, 3), ls = round(ls, 3), p = signif(p, 3))], auc = round(F10$auc_cv, 3),
  reglas = F9$rg[1:min(.N, 400), .(lhs, rhs, soporte = round(support, 4), confianza = round(confidence, 3), lift = round(lift, 2), n = count)],
  causas = F10$causas[, .(palabra, n = a, z = round(z, 2))], ccf = F10$ccd[, .(lag, r = round(r, 3))], granger = signif(F10$p_granger, 3),
  efecto = F10$efe[, .(inc, sentimiento = sent_r, p = round(p, 3))],
  picos = F10$picos[, .(fecha = format(fecha), n = N, z = round(z, 2), palabras, sent = sent_dom)],
  red = list(nodos = nod[, .(id = autor, respondio, recibio, comunidad, mensajes)], aristas = edg[, .(de, a, peso)]),
  miembros = F10$nodos[, .(a = autor, r = respondio, rc = recibio, c = comunidad)],
  red_dm = red_dm, kappa = round(F7$kappa, 3), concordancia = round(F7$po, 3),
  esr = round(mean(F6$val$polaridad == F6$val$esr_pol | F6$val$esr_pol == "neutral"), 3),
  nb = list(palabras = F10$nb$lp$palabra, neg = F10$nb$lp$neg, pos = F10$nb$lp$pos, prior = round(F10$nb$prior, 4)))
mot <- list(dic = F6$D$dic[, .(t = termino, s = match(sentimiento, SENT_ORDEN) - 1L, k = tipo, o = origen)],
            emo = DE[sentimiento %in% SENT_ORDEN, .(e = emoji, s = match(sentimiento, SENT_ORDEN) - 1L, i = intensidad)],
            jer = merge(F6$JS, jer[, .(expresion, patron)], by = "expresion")[, .(x = expresion, s = match(sentimiento, SENT_ORDEN) - 1L,
                     re = stri_replace_all_fixed(patron, "(?i)", ""), ci = stri_detect_fixed(patron, "(?i)"))],
            norm = jer[normaliza_a != "", .(re = stri_replace_all_fixed(patron, "(?i)", ""), a = normaliza_a)],
            neg = F6$D$negadores, int = F6$D$intensificadores, iro = F6$D$ironia, iro_e = F6$D$ironia_emoji)
corr <- { f <- file.path(DIR$bit, "corridas.csv"); if (file.exists(f)) fread(f, encoding = "UTF-8") else NULL }
XD <- list(
  meta = list(proyecto = CFG$proyecto, titulo = CFG$titulo, subtitulo = CFG$subtitulo, materia = CFG$materia, chat = ORIG$chat_nombre[1],
              generado = format(Sys.time(), "%d/%m/%Y %H:%M"), d0 = format(d0), d1 = format(max(ds$fecha)),
              registros = nrow(ORIG), avisos = nrow(F3$serv), publicidad = nrow(F3$publi), mensajes = nrow(ds),
              analizables = sum(ds$analizable), con_sentimiento = sum(ds$sentimiento %in% SENT_ORDEN), miembros = nrow(aut),
              dias_activos = uniqueN(ds$fecha), version = CFG$version, anonimizado = CFG$anonimizar),
  cobertura = cob, objetivos = OBJ,
  sentimientos = data.table(id = SENT_ORDEN, nombre = unname(SENT_LAB[SENT_ORDEN]), color = unname(SENT_COL[SENT_ORDEN]),
                            emoji = unname(SENT_EMOJI[SENT_ORDEN]), img = paste0("emoji/", emoji_codigo(norm_emoji(SENT_EMOJI[SENT_ORDEN])), ".png"),
                            polaridad = F6$D$defs$polaridad[match(SENT_ORDEN, F6$D$defs$sentimiento)],
                            definicion = F6$D$defs$definicion[match(SENT_ORDEN, F6$D$defs$sentimiento)]),
  periodos = PERIODOS, fases = fases, hallazgos = fread(file.path(DIR$res, "hallazgos.csv"), encoding = "UTF-8"),
  equipo = fread(file.path(DIR$codigo, "equipo.csv"), encoding = "UTF-8"),
  bitacora = bit_df, figuras = idx[, .(fase, nro, archivo, titulo, descripcion)], corridas = corr, scripts = scripts,
  emojis = DE[, .(e = emoji, nombre = nombre_es, sig = significado_chat, fam = familia, s = ifelse(sentimiento %in% SENT_ORDEN, match(sentimiento, SENT_ORDEN) - 1L, -1L),
                  int = intensidad, img, txt = usos_texto, rea = usos_reaccion, stk = usos_sticker, esr = esr_puntaje, curado)],
  jerga = jer[, .(x = expresion, tipo, sig = significado, s = ifelse(sentimiento %in% SENT_ORDEN, match(sentimiento, SENT_ORDEN) - 1L, -1L), norm = normaliza_a)],
  dic_sent = dic_s[, .(t = termino, s = match(sentimiento, SENT_ORDEN) - 1L, k = tipo, o = origen, m = mensajes)],
  docentes = F9$doc[, .(nombre = entidad, menciones, pos, neg, indice = round(indice, 3), li = round(li, 3), ls = round(ls, 3))],
  modelos = modelos, motor = mot,
  datasets = list(original = ncol(ORIG), limpio = ncol(F3$limpio), sentimientos = ncol(ds)),
  columnas = names(ds))
js <- paste0("window.XD = ", toJSON(XD, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA, null = "null"), ";")
writeBin(charToRaw(enc2utf8(js)), file.path(DIR$web, "data", "datos.js"))
cat(sprintf("docs/data/datos.js: %.1f MB | figuras: %d | emojis: %d\nAbrir: %s\n", file.size(file.path(DIR$web, "data", "datos.js")) / 1e6,
            nrow(idx), sum(DE$imagen_3d), normalizePath(file.path(DIR$web, "index.html"), mustWork = FALSE)))
