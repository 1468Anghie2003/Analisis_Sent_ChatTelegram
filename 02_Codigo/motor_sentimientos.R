# =====================================================================
#  MOTOR DE SENTIMIENTOS (lo usan F06, F07 y GENERAR_DASHBOARD)
#  Puntúa cada mensaje en los 15 sentimientos combinando 4 fuentes:
#   1. Frases del diccionario (2 a 5 palabras)   peso 1.5
#   2. Palabras exactas                           peso 1.0
#   3. Raíces (inicio de palabra)                 peso 0.8
#   4. Emojis y stickers (diccionario de emojis)  peso 0.5 + 0.25 x intensidad
#   5. Emoticones y signos (:v, :'v, :(, ??, F)   peso 0.8
#  Reglas: las frases se buscan primero y "consumen" sus palabras (no se
#  cuentan dos veces); una palabra negada (no, nunca, ni, sin, jamás a una o
#  dos posiciones) no se cuenta; un intensificador (muy, re, full, súper,
#  harto...) multiplica por 1.5; la ironía pasa lo positivo a decepción.
# =====================================================================

construir_diccionario <- function() {
  source(file.path(DIR$codigo, "diccionario_sentimientos.R"), local = TRUE, encoding = "UTF-8")
  filas <- list()
  for (s in names(BASE_CLASE)) {
    b <- BASE_CLASE[[s]]
    filas[[length(filas) + 1]] <- data.table(sentimiento = s, polaridad = b$polaridad, termino = b$terminos, tipo = "termino", origen = "Clase")
    if (length(b$raices)) filas[[length(filas) + 1]] <- data.table(sentimiento = s, polaridad = b$polaridad, termino = b$raices, tipo = "raiz", origen = "Clase")
  }
  for (s in names(REASIGNADOS))
    filas[[length(filas) + 1]] <- data.table(sentimiento = s, polaridad = BASE_CLASE[[s]]$polaridad, termino = REASIGNADOS[[s]], tipo = "termino",
                                             origen = ifelse(s == "admiracion", "Clase (reasignado de amor)", "Clase (reasignado de vergüenza)"))
  for (s in names(AMPLIACION))
    filas[[length(filas) + 1]] <- data.table(sentimiento = s, polaridad = BASE_CLASE[[s]]$polaridad, termino = AMPLIACION[[s]], tipo = "termino", origen = "Ampliación (dataset)")
  d <- rbindlist(filas)
  d[, termino := normalizar_base(termino)]
  d[, termino := stri_trim_both(stri_replace_all_regex(termino, "[^\\p{L} ]", " "))]
  d <- d[termino != ""]
  d[tipo == "termino", tipo := ifelse(stri_detect_fixed(termino, " "), "frase", "palabra")]
  d[, n_pal := stri_count_words(termino)]
  # Conflictos: un término en dos sentimientos se queda en el primero (la clase manda)
  d[, prio := fifelse(startsWith(origen, "Clase"), 1L, 2L)]
  setorder(d, prio)
  conflictos <- d[duplicated(d[, .(termino, tipo)]) & !duplicated(d[, .(termino, tipo, sentimiento)])]
  d <- d[!duplicated(d[, .(termino, tipo)])]
  d[, prio := NULL]
  setorder(d, sentimiento, tipo, termino)
  defs <- data.table(sentimiento = names(BASE_CLASE), polaridad = vapply(BASE_CLASE, `[[`, 1, "polaridad"),
                     definicion = vapply(BASE_CLASE, `[[`, "", "definicion"))
  list(dic = d, defs = defs, conflictos = conflictos, ironia = normalizar_base(IRONIA), ironia_emoji = IRONIA_EMOJI,
       negadores = NEGADORES, intensificadores = INTENSIFICADORES)
}

#> D: resultado de construir_diccionario(); DE: diccionario de emojis
#> (emoji, sentimiento, intensidad); JS: jerga con sentimiento (expresion, sentimiento)
puntuar_mensajes <- function(ids, texto_limpio, texto_norm, emojis, sticker, expresiones, D, DE, JS) {
  dic <- D$dic
  tok <- data.table(id = rep(ids, lengths(w <- strsplit(texto_limpio, " ", fixed = TRUE))), w = unlist(w))
  tok <- tok[!is.na(w) & w != ""]
  tok[, pos := seq_len(.N), by = id]
  setkey(tok, id, pos)
  usados <- data.table(id = ids[0], pos = integer())
  hits <- list()
  # 1. Frases (de la más larga a la más corta)
  frases <- dic[tipo == "frase"]
  for (n in sort(unique(frases$n_pal), decreasing = TRUE)) {
    g <- copy(tok)
    for (k in seq_len(n - 1)) g[, paste0("w", k) := shift(w, -k), by = id]
    cols <- c("w", if (n > 1) paste0("w", seq_len(n - 1)))
    g <- g[complete.cases(g[, ..cols])]
    g[, gram := do.call(paste, c(.SD, sep = " ")), .SDcols = cols]
    m <- merge(g[, .(id, pos, gram)], frases[n_pal == n, .(gram = termino, sentimiento, termino)], by = "gram")
    if (!nrow(m)) next
    span <- m[, .(p = pos + 0:(n - 1)), by = .(id, ini = pos)]
    choca <- unique(span[usados, on = .(id, p = pos), nomatch = 0L][, .(id, ini)])
    if (nrow(choca)) m <- m[!choca, on = .(id, pos = ini)]
    if (!nrow(m)) next
    setorder(m, id, pos)
    m <- m[!duplicated(m[, .(id, pos)])]
    m[, fin_prev := shift(pos + n - 1L, fill = -1L), by = id]
    m <- m[pos > fin_prev]
    hits[[length(hits) + 1]] <- m[, .(id, pos, sentimiento, termino, fuente = "frase", peso = 1.5)]
    usados <- rbind(usados, m[, .(pos = pos + 0:(n - 1)), by = .(id, ini = pos)][, .(id, pos)])
  }
  libres <- tok[!usados, on = .(id, pos)]
  # 2. Palabras exactas
  pal <- merge(libres, dic[tipo == "palabra", .(w = termino, sentimiento, termino)], by = "w")
  # 3. Raíces (solo palabras que no coincidieron exactas)
  rest <- libres[!pal, on = .(id, pos)]
  raices <- dic[tipo == "raiz"][order(-nchar(termino))]
  uw <- unique(rest$w)
  mapa <- rbindlist(lapply(seq_len(nrow(raices)), function(i) {
    x <- uw[startsWith(uw, raices$termino[i])]
    if (length(x)) data.table(w = x, sentimiento = raices$sentimiento[i], termino = raices$termino[i]) else NULL }))
  if (nrow(mapa)) mapa <- mapa[!duplicated(w)]
  rai <- if (nrow(mapa)) merge(rest, mapa, by = "w") else rest[0][, `:=`(sentimiento = character(), termino = character())]
  pr <- rbind(pal[, .(id, pos, sentimiento, termino, fuente = "palabra", peso = 1.0)],
              rai[, .(id, pos, sentimiento, termino, fuente = "raiz", peso = 0.8)])
  # negación e intensificación (mirando 1 y 2 posiciones atrás)
  pr <- merge(pr, tok[, .(id, pos, w1 = shift(w, 1), w2 = shift(w, 2)), by = id][, .(id, pos, w1, w2)], by = c("id", "pos"), all.x = TRUE)
  pr[, negado := (w1 %in% D$negadores) | (w2 %in% D$negadores & w1 %in% c(D$intensificadores, "es", "esta", "estoy", "fue", "era", "me", "te", "se", "lo", "la", "tan", "muy"))]
  pr[, intensificado := w1 %in% D$intensificadores]
  pr[intensificado == TRUE, peso := peso * 1.5]
  negados <- pr[negado == TRUE]
  pr <- pr[negado == FALSE, .(id, pos, sentimiento, termino, fuente, peso)]
  hits[[length(hits) + 1]] <- pr
  # 4. Emojis y stickers
  em <- data.table(id = rep(ids, lengths(e <- strsplit(ifelse(is.na(emojis), "", emojis), " ", fixed = TRUE))), emoji = unlist(e))[!is.na(emoji) & emoji != ""]
  st <- data.table(id = ids, emoji = sticker)[!is.na(emoji) & emoji != ""]
  em <- rbind(em, st)[, .(n = pmin(.N, 3L)), by = .(id, emoji)]
  em <- merge(em, DE[sentimiento %in% SENT_ORDEN, .(emoji, sentimiento, intensidad)], by = "emoji")
  if (nrow(em)) hits[[length(hits) + 1]] <- em[, .(id, pos = NA_integer_, sentimiento, termino = emoji, fuente = "emoji", peso = n * (0.5 + 0.25 * intensidad))]
  # 5. Emoticones y signos expresivos
  ex <- data.table(id = rep(ids, lengths(x <- strsplit(ifelse(is.na(expresiones), "", expresiones), "|", fixed = TRUE))), expresion = unlist(x))[!is.na(expresion) & expresion != ""]
  ex <- merge(ex, JS, by = "expresion")
  if (nrow(ex)) hits[[length(hits) + 1]] <- ex[, .(id, pos = NA_integer_, sentimiento, termino = expresion, fuente = "emoticon", peso = 0.8)]
  H <- rbindlist(hits, use.names = TRUE)
  # 6. Ironía
  iro_txt <- stri_detect_regex(texto_norm, paste0("\\b(", paste(D$ironia, collapse = "|"), ")\\b"))
  iro_emo <- stri_detect_regex(ifelse(is.na(emojis), "", emojis), paste(D$ironia_emoji, collapse = "|"))
  pos_txt <- H[sentimiento %in% c("alegria", "admiracion", "satisfaccion", "tranquilidad", "gratitud") & fuente %in% c("frase", "palabra", "raiz"), unique(id)]
  ironicos <- ids[(iro_txt | iro_emo) & ids %in% pos_txt]
  H[id %in% ironicos & sentimiento %in% c("alegria", "admiracion", "satisfaccion", "tranquilidad", "gratitud") & fuente != "emoji", `:=`(sentimiento = "decepcion", fuente = paste0(fuente, "+ironia"))]
  # 7. Agregación por mensaje
  W <- dcast(H[, .(peso = sum(peso)), by = .(id, sentimiento)], id ~ sentimiento, value.var = "peso", fill = 0)
  for (s in setdiff(SENT_ORDEN, names(W))) W[, (s) := 0]
  W <- merge(data.table(id = ids), W, by = "id", all.x = TRUE)
  for (s in SENT_ORDEN) set(W, which(is.na(W[[s]])), s, 0)
  M <- as.matrix(W[, ..SENT_ORDEN])
  txtpeso <- dcast(H[fuente %in% c("frase", "palabra", "raiz")][, .(p = sum(peso)), by = .(id, sentimiento)], id ~ sentimiento, value.var = "p", fill = 0)
  Mt <- matrix(0, nrow(W), 15, dimnames = list(NULL, SENT_ORDEN))
  if (nrow(txtpeso)) { idx <- match(txtpeso$id, W$id); for (s in intersect(SENT_ORDEN, names(txtpeso))) Mt[idx, s] <- txtpeso[[s]] }
  desempate <- M + 0.01 * Mt + matrix(rep(seq(0.0015, 0.0001, length.out = 15), each = nrow(M)), nrow(M))
  tiene <- rowSums(M) > 0
  W[, sentimiento := ifelse(tiene, SENT_ORDEN[max.col(desempate, ties.method = "first")], NA_character_)]
  W[, puntaje_pos := rowSums(M[, 1:6, drop = FALSE])]
  W[, puntaje_neg := rowSums(M[, 9:15, drop = FALSE])]
  W[, puntaje := round(puntaje_pos - puntaje_neg, 3)]
  W[, polaridad := fifelse(puntaje > 0, "positivo", fifelse(puntaje < 0, "negativo", "neutral"))]
  W[, intensidad := round(rowSums(M), 3)]
  W[, ironia := id %in% ironicos]
  W[, n_sentimientos := rowSums(M > 0)]
  list(W = W, H = H, negados = negados)
}
