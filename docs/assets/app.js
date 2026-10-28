/* =====================================================================
   Análisis de Sentimientos · app.js
   Dashboard interactivo. Todos los datos vienen de data/datos.js y
   data/mensajes.js (generados por GENERAR_DASHBOARD.R). Los cálculos por
   rango de fechas se hacen en el navegador sobre un registro compacto
   por mensaje, de modo que cualquier periodo se puede analizar sin servidor.
   ===================================================================== */
(() => {
"use strict";
const X = window.XD, M = window.XDM, C = M.cols, V = M.vocab, N = M.n;
const $ = s => document.querySelector(s), $$ = s => [...document.querySelectorAll(s)];
const SENT = X.sentimientos, S_COL = SENT.map(s => s.color), S_NOM = SENT.map(s => s.nombre);
const EMO = X.emojis, EMO_IDX = new Map(EMO.map((e, i) => [e.e, i]));
const D0 = new Date(M.d0 + "T12:00:00");
const NDAYS = C.d[N - 1] + 1;
const MESES = ["ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"];
const MESES_L = ["enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"];
const DIAS = ["lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"];
const POL_COL = { pos: "#16A34A", neu: "#94A3B8", neg: "#E11D48" };
const COL_A = "#16131F", COL_B = "#0D9488";
const esc = s => String(s ?? "").replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
const fmt = n => (n ?? 0).toLocaleString("es-BO");
const pct = (x, d = 1) => (isFinite(x) ? (100 * x).toFixed(d) : "0") + "%";
const dayDate = d => new Date(D0.getTime() + d * 864e5);
const iso = dt => dt.toISOString().slice(0, 10);
const dayIso = d => iso(dayDate(d));
const fromIso = s => Math.round((new Date(s + "T12:00:00") - D0) / 864e5);
const fdia = d => { const t = dayDate(d); return `${t.getDate()} ${MESES[t.getMonth()]} ${t.getFullYear()}`; };
const fcorta = d => { const t = dayDate(d); return `${String(t.getDate()).padStart(2, "0")}/${String(t.getMonth() + 1).padStart(2, "0")}/${t.getFullYear()}`; };
const imgEmo = (e, est = "") => { const i = EMO_IDX.get(e); const m = i != null ? EMO[i].img : ""; return m ? `<img src="${m}" alt="${esc(e)}" style="${est}">` : `<span class="gl" style="${est}">${e}</span>`; };
const css = v => getComputedStyle(document.documentElement).getPropertyValue(v).trim();

/* ---------- Tema de ECharts ---------- */
echarts.registerTheme("xd", {
  color: ["#FF6B3D", "#9333EA", "#0D9488", "#2563EB", "#E8A400", "#DB2777", "#16A34A", "#E11D48", "#4F46E5", "#0891B2"],
  backgroundColor: "transparent", textStyle: { fontFamily: "Manrope", color: "#4A4760" },
  title: { textStyle: { fontFamily: "Instrument Serif", fontStyle: "italic", fontWeight: 400, color: "#16131F" } },
  categoryAxis: { axisLine: { lineStyle: { color: "rgba(22,19,31,.18)" } }, axisTick: { show: false }, axisLabel: { color: "#4A4760" }, splitLine: { lineStyle: { color: "rgba(22,19,31,.06)" } } },
  valueAxis: { axisLine: { show: false }, axisLabel: { color: "#7C7993" }, splitLine: { lineStyle: { color: "rgba(22,19,31,.07)" } } },
  legend: { textStyle: { color: "#4A4760", fontFamily: "Manrope" }, icon: "roundRect", itemWidth: 12, itemHeight: 8 },
  tooltip: { backgroundColor: "rgba(255,255,255,.92)", borderColor: "rgba(255,255,255,.9)", borderWidth: 1, textStyle: { color: "#16131F", fontFamily: "Manrope" },
    extraCssText: "backdrop-filter:blur(14px);border-radius:14px;box-shadow:0 14px 34px -14px rgba(36,22,70,.4);padding:10px 12px;" }
});
const charts = {};
function ch(id) {
  const el = typeof id === "string" ? document.getElementById(id) : id; if (!el) return null;
  const k = el.id || id;
  if (!charts[k] || charts[k].isDisposed()) charts[k] = echarts.init(el, "xd", { renderer: "canvas" });
  return charts[k];
}
window.addEventListener("resize", () => Object.values(charts).forEach(c => c && !c.isDisposed() && c.resize()));
const tt = (extra = {}) => Object.assign({ trigger: "item", confine: true }, extra);

/* ---------- Índices por fecha ---------- */
const S = { a: 0, b: NDAYS - 1, lab: "Todo el periodo", cmp: false, A2: null, gran: "auto", tab: "inicio", sucio: new Set() };
function lowerB(d) { let lo = 0, hi = N; while (lo < hi) { const m = (lo + hi) >> 1; if (C.d[m] < d) lo = m + 1; else hi = m; } return lo; }
function slice(a, b) { return [lowerB(a), lowerB(b + 1)]; }

/* ---------- Agregación de un rango (con caché) ---------- */
const VT = V.tok.length, VB = V.big.length, VG = V.tri.length, VE = EMO.length, VN = V.ent.length, VA = V.aut.length, NT = X.modelos.topicos.length;
const iTok = w => V.tok.indexOf(w);
const RISA_T = new Set(["xd", "xdxd", "xdxdxd"].map(iTok).filter(i => i >= 0));
const RISA_X = new Set(X.jerga.map((j, k) => /^risa/.test(j.tipo) ? k : -1).filter(k => k >= 0));
const RISA_E = new Set(["😂", "🤣", "💀", "😅", "🥲"].map(e => EMO_IDX.get(e)).filter(i => i != null));
const cache = new Map();
function agg(a, b) {
  const key = a + "-" + b; if (cache.has(key)) return cache.get(key);
  const [i0, i1] = slice(a, b);
  const R = { a, b, i0, i1, n: i1 - i0, nan: 0, ns: 0, sent: new Float64Array(15), pol: [0, 0, 0], porDia: new Map(),
    hw: new Float64Array(7 * 24), sh: new Float64Array(15 * 24), tok: new Float64Array(VT), big: new Float64Array(VB), tri: new Float64Array(VG),
    emoT: new Float64Array(VE), emoR: new Float64Array(VE), expr: new Float64Array(X.jerga.length), ent: new Float64Array(VN), entPol: new Float64Array(VN * 3),
    entSent: new Float64Array(VN * 15), aut: new Float64Array(VA), autSent: new Map(), top: new Float64Array(NT), topSent: new Float64Array(NT * 15),
    tokSent: new Float64Array(VT * 15), emoSent: new Float64Array(VE * 15), qh: new Float64Array(24), qa: new Float64Array(24), qt: Array.from({ length: 24 }, () => []),
    risa: [[0, 0], [0, 0], [0, 0]], reacS: new Float64Array(15), reacN: new Float64Array(15), nir: 0, preguntas: 0, respondidas: 0, tiempos: [], nreac: 0 };
  for (let i = i0; i < i1; i++) {
    const d = C.d[i], s = C.s[i], p = C.p[i];
    let pd = R.porDia.get(d); if (!pd) { pd = { n: 0, pos: 0, neg: 0, s: new Float64Array(15) }; R.porDia.set(d, pd); }
    pd.n++; R.hw[C.w[i] * 24 + C.h[i]]++; R.aut[C.a[i]]++; R.nreac += C.nr[i];
    if (s !== -2) R.nan++;
    if (s >= 0) {
      R.ns++; R.sent[s]++; pd.s[s]++; R.sh[s * 24 + C.h[i]]++; R.pol[p + 1]++; if (p > 0) pd.pos++; else if (p < 0) pd.neg++;
      let as = R.autSent.get(C.a[i]); if (!as) { as = new Float64Array(15); R.autSent.set(C.a[i], as); } as[s]++;
      R.reacS[s] += C.nr[i]; R.reacN[s]++;
      if (C.tp[i] >= 0) R.topSent[C.tp[i] * 15 + s]++;
    }
    if (C.tp[i] >= 0) R.top[C.tp[i]]++;
    let risa = false;
    for (const k of C.t[i]) { R.tok[k]++; if (s >= 0) R.tokSent[k * 15 + s]++; if (RISA_T.has(k)) risa = true; }
    for (const k of C.b[i]) R.big[k]++;
    for (const k of C.g[i]) R.tri[k]++;
    for (const k of C.e[i]) { R.emoT[k]++; if (s >= 0) R.emoSent[k * 15 + s]++; if (RISA_E.has(k)) risa = true; }
    const r = C.r[i]; for (let j = 0; j < r.length; j += 2) R.emoR[r[j]] += r[j + 1];
    for (const k of C.x[i]) { R.expr[k]++; if (RISA_X.has(k)) risa = true; }
    for (const k of C.n[i]) { R.ent[k]++; R.entPol[k * 3 + p + 1]++; if (s >= 0) R.entSent[k * 15 + s]++; }
    if (s !== -2) { R.risa[p + 1][0]++; if (risa) R.risa[p + 1][1]++; }
    if (C.q[i] === 1 && C.ans[i] >= 0) { R.preguntas++; R.qh[C.h[i]]++; if (C.ans[i] === 1) { R.respondidas++; R.qa[C.h[i]]++; if (C.rt[i] >= 0) { R.qt[C.h[i]].push(C.rt[i]); R.tiempos.push(C.rt[i]); } } }
    if (C.ir[i]) R.nir++;
  }
  if (cache.size > 40) cache.delete(cache.keys().next().value);
  cache.set(key, R); return R;
}
const TODO = () => agg(0, NDAYS - 1);
const RA = () => agg(S.a, S.b);
const RB = () => (S.cmp && S.A2 ? agg(S.A2.a, S.A2.b) : null);
const topK = (arr, k, labels) => { const ix = []; for (let i = 0; i < arr.length; i++) if (arr[i] > 0) ix.push(i); ix.sort((x, y) => arr[y] - arr[x]); return ix.slice(0, k).map(i => ({ i, v: arr[i], l: labels ? labels[i] : i })); };
const median = a => { if (!a.length) return NaN; const s = [...a].sort((x, y) => x - y), m = s.length >> 1; return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2; };
const domSent = R => { let b = -1, v = -1; R.sent.forEach((x, i) => { if (x > v) { v = x; b = i; } }); return v > 0 ? b : -1; };
const animo = R => { const t = R.pol[2] + R.pol[0]; return t ? (R.pol[2] - R.pol[0]) / t : 0; };
const emoTodo = R => R.emoT.map((v, i) => v + R.emoR[i]);
const sumA = a => a.reduce((x, y) => x + y, 0);
function logOdds(R, base, min = 3, excl = null) {
  const tr = sumA(R.tok), tb = sumA(base.tok), out = [];
  for (let k = 0; k < VT; k++) { const y = R.tok[k]; if (y < min || (excl && excl.has(k))) continue; const a = Math.max(base.tok[k] - y, 0); const r = Math.max(tb - tr, 1);
    const lo = Math.log((y + .5) / (tr - y + .5)) - Math.log((a + .5) / (r - a + .5)); const se = Math.sqrt(1 / (y + .5) + 1 / (tr - y + .5) + 1 / (a + .5) + 1 / (r - a + .5)); out.push({ i: k, z: lo / se, v: y }); }
  return out.sort((x, y) => y.z - x.z);
}
// log-odds entre dos conjuntos disjuntos (A frente a B)
function logOddsAB(A, B, min = 4) {
  const ta = sumA(A.tok), tb = sumA(B.tok), out = [];
  for (let k = 0; k < VT; k++) { const y = A.tok[k], z = B.tok[k]; if (y + z < min) continue;
    const lo = Math.log((y + .5) / (ta - y + .5)) - Math.log((z + .5) / (tb - z + .5)); const se = Math.sqrt(1 / (y + .5) + 1 / (ta - y + .5) + 1 / (z + .5) + 1 / (tb - z + .5)); out.push({ i: k, z: lo / se, a: y, b: z }); }
  return out.sort((x, y) => y.z - x.z);
}

/* ---------- Granularidad de las series ---------- */
function gran(a, b) { return S.gran !== "auto" ? S.gran : (b - a <= 62 ? "dia" : b - a <= 540 ? "semana" : "mes"); }
function bucket(d, g) {
  const t = dayDate(d);
  if (g === "dia") return iso(t);
  if (g === "semana") { const w = (t.getDay() + 6) % 7; return iso(new Date(t.getTime() - w * 864e5)); }
  return `${t.getFullYear()}-${String(t.getMonth() + 1).padStart(2, "0")}`;
}
function buckets(R, g) {
  const keys = []; const map = new Map(); let last = null;
  for (let d = R.a; d <= R.b; d++) { const k = bucket(d, g); if (k !== last) { keys.push(k); map.set(k, { n: 0, pos: 0, neg: 0, s: new Float64Array(15), a: d, b: d }); last = k; } map.get(k).b = d; }
  R.porDia.forEach((v, d) => { const o = map.get(bucket(d, g)); if (!o) return; o.n += v.n; o.pos += v.pos; o.neg += v.neg; for (let j = 0; j < 15; j++) o.s[j] += v.s[j]; });
  return { keys, map };
}
const etiqB = (k, g) => g === "mes" ? `${MESES[+k.slice(5, 7) - 1]} ${k.slice(0, 4)}` : (g === "semana" ? "sem. " + k.slice(8, 10) + "/" + k.slice(5, 7) + "/" + k.slice(2, 4) : k.slice(8, 10) + "/" + k.slice(5, 7) + "/" + k.slice(2, 4));

/* =====================================================================
   PERIODOS: selección por modo (excluyente) para A y, opcionalmente, B
   ===================================================================== */
const PER = X.periodos;
function rangoAnio(y) { return [fromIso(`${y}-01-01`), fromIso(`${y}-12-31`)]; }
function rangoSem(lbl) { const [s, y] = lbl.split("-"); return s === "I" ? [fromIso(`${y}-01-01`), fromIso(`${y}-06-30`)] : [fromIso(`${y}-07-01`), fromIso(`${y}-12-31`)]; }
function rangoPer(lbl) {
  const y = +lbl.slice(-4), p = lbl.slice(0, -5);
  const ini = { "Verano": `${y - 1}-${PER[0].inicio}`, "Semestre I": `${y}-${PER[1].inicio}`, "Invierno": `${y}-${PER[2].inicio}`, "Semestre II": `${y}-${PER[3].inicio}` }[p];
  const fin = { "Verano": `${y}-${PER[1].inicio}`, "Semestre I": `${y}-${PER[2].inicio}`, "Invierno": `${y}-${PER[3].inicio}`, "Semestre II": `${y}-${PER[0].inicio}` }[p];
  return [fromIso(ini), fromIso(fin) - 1];
}
function rangoMes(k) { const [y, m] = k.split("-").map(Number); const last = new Date(y, m, 0).getDate(); return [fromIso(`${k}-01`), fromIso(`${k}-${String(last).padStart(2, "0")}`)]; }
function rangoSemana(d) { const w = (dayDate(d).getDay() + 6) % 7; return [d - w, d - w + 6]; }
const clip = ([a, b]) => [Math.max(0, a), Math.min(NDAYS - 1, b)];
let PRE = null;
function listaPresets() {
  const anios = [], sems = [], pers = [], meses = [];
  const y0 = dayDate(0).getFullYear(), y1 = dayDate(NDAYS - 1).getFullYear();
  for (let y = y0; y <= y1; y++) { anios.push(y); sems.push(`I-${y}`, `II-${y}`); ["Verano", "Semestre I", "Invierno", "Semestre II"].forEach(p => pers.push(`${p} ${y}`)); }
  pers.push(`Verano ${y1 + 1}`);
  const T = TODO(); const ms = new Map(); T.porDia.forEach((v, d) => { const k = bucket(d, "mes"); ms.set(k, (ms.get(k) || 0) + v.n); });
  [...ms.keys()].sort().forEach(k => meses.push(k));
  const conDatos = r => { const [a, b] = clip(r); return b >= a && agg(a, b).n > 0; };
  return { anios: anios.filter(y => conDatos(rangoAnio(y))), sems: sems.filter(s => conDatos(rangoSem(s))), pers: pers.filter(p => conDatos(rangoPer(p))), meses, ms };
}
const MODOS = [["todo", "Todo"], ["anio", "Año"], ["sem", "Semestre"], ["per", "Periodo académico"], ["mes", "Mes"], ["semana", "Semana"], ["dia", "Día"], ["rango", "Rango"]];
const P = { A: { modo: "todo", val: null }, B: { modo: "anio", val: null } };
function etiqueta(modo, val, a, b) {
  if (modo === "todo") return "Todo el periodo";
  if (modo === "anio") return "Año " + val;
  if (modo === "sem") return "Semestre " + val;
  if (modo === "per") return val;
  if (modo === "mes") return `${MESES_L[+val.slice(5, 7) - 1]} ${val.slice(0, 4)}`;
  if (modo === "semana") return "Semana del " + fcorta(a);
  if (modo === "dia") return "Día " + fcorta(a);
  return `${fcorta(a)} – ${fcorta(b)}`;
}
function rangoDe(modo, val) {
  if (modo === "todo") return [0, NDAYS - 1];
  if (modo === "anio") return clip(rangoAnio(+val));
  if (modo === "sem") return clip(rangoSem(val));
  if (modo === "per") return clip(rangoPer(val));
  if (modo === "mes") return clip(rangoMes(val));
  if (modo === "semana") return clip(rangoSemana(fromIso(val)));
  if (modo === "dia") { const d = fromIso(val); return clip([d, d]); }
  return clip([fromIso(val[0]), fromIso(val[1])]);
}
function defecto(modo, p) {
  const ult = PRE.anios[PRE.anios.length - 1], pen = PRE.anios[PRE.anios.length - 2] ?? ult;
  if (modo === "anio") return String(p === "B" ? pen : ult);
  if (modo === "sem") return PRE.sems[PRE.sems.length - 1 - (p === "B" ? 1 : 0)] ?? PRE.sems[0];
  if (modo === "per") return PRE.pers[PRE.pers.length - 1 - (p === "B" ? 1 : 0)] ?? PRE.pers[0];
  if (modo === "mes") return PRE.meses[PRE.meses.length - 1 - (p === "B" ? 1 : 0)] ?? PRE.meses[0];
  if (modo === "semana" || modo === "dia") { const T = TODO(); let d = NDAYS - 1; while (d > 0 && !T.porDia.has(d)) d--; return dayIso(Math.max(0, d - (p === "B" ? 7 : 0))); }
  if (modo === "rango") { const base = p === "A" ? [S.a, S.b] : (S.A2 ? [S.A2.a, S.A2.b] : [S.a, S.b]); return [dayIso(base[0]), dayIso(base[1])]; }
  return null;
}
function pintarModos(p) {
  const cont = $(`.modos[data-p="${p}"]`);
  cont.innerHTML = MODOS.map(([k, l]) => `<button data-m="${k}" class="${P[p].modo === k ? "on" : ""}">${l}</button>`).join("");
  cont.querySelectorAll("button").forEach(b => b.onclick = () => { const m = b.dataset.m; aplicar(p, m, defecto(m, p)); });
}
function pintarValor(p) {
  const cont = $("#val" + p), { modo, val } = P[p], opt = (arr, f, v) => arr.map(x => `<option value="${x}" ${String(x) === String(v) ? "selected" : ""}>${f ? f(x) : x}</option>`).join("");
  const cobY = Object.fromEntries(X.cobertura.map(c => [c.anio, c]));
  const mn = dayIso(0), mx = dayIso(NDAYS - 1);
  let h = "";
  if (modo === "anio") h = `<select class="sel">${opt(PRE.anios, y => cobY[y] ? `${y} (${cobY[y].desde.slice(0, 5)} a ${cobY[y].hasta.slice(0, 5)})` : y, val)}</select>`;
  else if (modo === "sem") h = `<select class="sel">${opt(PRE.sems, s => `Semestre ${s} (${s.startsWith("I-") ? "ene-jun" : "jul-dic"})`, val)}</select>`;
  else if (modo === "per") h = `<select class="sel">${opt(PRE.pers, null, val)}</select>`;
  else if (modo === "mes") h = `<select class="sel">${opt(PRE.meses, k => `${MESES_L[+k.slice(5, 7) - 1]} ${k.slice(0, 4)} (${fmt(PRE.ms.get(k))} mensajes)`, val)}</select>`;
  else if (modo === "semana") h = `<span class="lbl">Cualquier día de la semana</span><input type="date" class="fecha" min="${mn}" max="${mx}" value="${val}">`;
  else if (modo === "dia") h = `<input type="date" class="fecha" min="${mn}" max="${mx}" value="${val}">`;
  else if (modo === "rango") h = `<span class="lbl">Desde</span><input type="date" class="fecha" id="r${p}0" min="${mn}" max="${mx}" value="${val[0]}"><span class="lbl">hasta</span><input type="date" class="fecha" id="r${p}1" min="${mn}" max="${mx}" value="${val[1]}"><button class="chip">Aplicar</button>`;
  cont.innerHTML = h;
  const sel = cont.querySelector("select"), inp = cont.querySelector("input[type=date]");
  if (sel) sel.onchange = e => aplicar(p, modo, e.target.value);
  if (modo === "semana" || modo === "dia") inp.onchange = e => e.target.value && aplicar(p, modo, e.target.value);
  if (modo === "rango") cont.querySelector("button").onclick = () => { const a = $(`#r${p}0`).value, b = $(`#r${p}1`).value; if (a && b) aplicar(p, "rango", a <= b ? [a, b] : [b, a]); };
}
function aplicar(p, modo, val, labForzada) {
  P[p].modo = modo; P[p].val = val;
  const [a, b] = rangoDe(modo, val), lab = labForzada || etiqueta(modo, val, a, b), R = agg(a, b);
  if (p === "A") { S.a = a; S.b = b; S.lab = lab; } else S.A2 = { a, b, lab };
  pintarModos(p); pintarValor(p);
  $("#res" + p).innerHTML = `<b>${esc(lab)}</b> · ${modo !== "rango" && modo !== "dia" ? `${fcorta(a)} – ${fcorta(b)} · ` : ""}${fmt(R.n)} mensajes`;
  marcarSucio(); renderActiva();
}
function aplicarRango(a, b, lab) { [a, b] = clip([a, b]); if (a === b) aplicar("A", "dia", dayIso(a), lab); else aplicar("A", "rango", [dayIso(a), dayIso(b)], lab); }
function initFiltro() {
  PRE = listaPresets();
  aplicar("A", "todo", null);
  $("#btnComparar").onclick = () => {
    S.cmp = !S.cmp; $("#filtro").classList.toggle("cmp-on", S.cmp); document.body.classList.toggle("cmp-on-global", S.cmp);
    $("#btnComparar").classList.toggle("on", S.cmp); $("#btnComparar").textContent = S.cmp ? "Quitar comparación" : "Comparar con otro periodo";
    $("#etqA").textContent = S.cmp ? "Periodo A" : "Periodo";
    if (S.cmp) aplicar("B", P.B.modo, P.B.val ?? defecto(P.B.modo, "B")); else { S.A2 = null; marcarSucio(); renderActiva(); }
  };
  $("#btnRestablecer").onclick = () => { if (S.cmp) $("#btnComparar").click(); S.gran = "auto"; $("#selGran").value = "auto"; aplicar("A", "todo", null); };
  $("#btnAyudaF").onclick = () => $("#ayudaF").classList.toggle("on");
  $("#selGran").onchange = e => { S.gran = e.target.value; marcarSucio(); renderActiva(); };
}

/* =====================================================================
   PESTAÑAS: burbuja, flechas y menú de todas las secciones
   ===================================================================== */
const CON_FILTRO = new Set(["explorador", "sentimientos", "emojis", "lenguaje", "temas", "comunidad", "docentes", "causas"]);
function marcarSucio() { CON_FILTRO.forEach(t => S.sucio.add(t)); }
function moverBurbuja(btn) { const b = $("#burbuja"); b.style.left = btn.offsetLeft + "px"; b.style.width = btn.offsetWidth + "px"; }
function flechas() { const n = $("#tabs"); $("#tabIzq").disabled = n.scrollLeft <= 4; $("#tabDer").disabled = n.scrollLeft + n.clientWidth >= n.scrollWidth - 4; }
function irA(tab) {
  const btn = $(`.tab[data-tab="${tab}"]`); if (!btn) return;
  $$(".tab").forEach(t => t.classList.toggle("on", t === btn)); moverBurbuja(btn);
  const nav = $("#tabs"); nav.scrollTo({ left: btn.offsetLeft - nav.clientWidth / 2 + btn.offsetWidth / 2, behavior: "smooth" });
  const c = btn.dataset.c, c2 = btn.dataset.c2, r = document.documentElement.style;
  r.setProperty("--accent", c); r.setProperty("--accent2", c2); r.setProperty("--accent-soft", c + "24");
  $$(".vista").forEach(v => v.classList.toggle("on", v.id === "v-" + tab));
  $("#filtro").classList.toggle("on", CON_FILTRO.has(tab));
  $("#menuTabs").classList.remove("on");
  S.tab = tab; history.replaceState(null, "", "#" + tab);
  if (!RENDER[tab].hecho || S.sucio.has(tab)) renderActiva();
  setTimeout(() => { Object.values(charts).forEach(c => c && !c.isDisposed() && c.resize()); flechas(); }, 80);
  window.scrollTo({ top: 0, behavior: "smooth" });
}
let rTimer = null;
function renderActiva() {
  clearTimeout(rTimer);
  rTimer = setTimeout(() => { const t = S.tab, f = RENDER[t]; if (!f) return; try { f(); if (CON_FILTRO.has(t)) panelCmp(t); } catch (e) { console.error(t, e); } f.hecho = true; S.sucio.delete(t); }, 30);
}
function initNav() {
  $$(".tab").forEach(b => b.onclick = () => irA(b.dataset.tab));
  const nav = $("#tabs");
  $("#tabIzq").onclick = () => nav.scrollBy({ left: -nav.clientWidth * .7, behavior: "smooth" });
  $("#tabDer").onclick = () => nav.scrollBy({ left: nav.clientWidth * .7, behavior: "smooth" });
  nav.addEventListener("scroll", flechas);
  nav.addEventListener("wheel", e => { if (Math.abs(e.deltaY) > Math.abs(e.deltaX)) { nav.scrollLeft += e.deltaY; e.preventDefault(); } }, { passive: false });
  $("#menuTabs").innerHTML = $$(".tab").map(b => `<button data-t="${b.dataset.tab}" style="--c:${b.dataset.c}"><i>${b.querySelector("i").textContent}</i><span>${esc(b.textContent.replace(/^\d+/, ""))}<small>${esc(b.dataset.d || "")}</small></span></button>`).join("");
  $$("#menuTabs button").forEach(b => b.onclick = () => irA(b.dataset.t));
  $("#btnMenu").onclick = e => { e.stopPropagation(); $("#menuTabs").classList.toggle("on"); };
  document.addEventListener("click", e => { if (!e.target.closest("#menuTabs") && !e.target.closest("#btnMenu")) $("#menuTabs").classList.remove("on"); });
  flechas();
}

/* =====================================================================
   UTILIDADES DE GRÁFICOS
   ===================================================================== */
const richEmo = (lista, size = 22) => { const r = {}; lista.forEach((e, i) => { const k = EMO_IDX.get(e); if (k != null && EMO[k].img) r["e" + i] = { backgroundColor: { image: EMO[k].img }, width: size, height: size }; }); return r; };
const sentRich = (size = 20) => { const r = {}; SENT.forEach((s, i) => r["s" + i] = { backgroundColor: { image: s.img }, width: size, height: size }); return r; };
function barH(id, items, color, opts = {}) {
  const c = ch(id); if (!c) return; const it = [...items].reverse();
  c.setOption({ grid: { left: opts.left ?? 10, right: 58, top: 8, bottom: 8, containLabel: true }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, formatter: opts.tip }),
    xAxis: { type: "value", axisLabel: { show: !opts.noX } }, yAxis: { type: "category", data: it.map(x => x.l), axisLabel: Object.assign({ fontSize: 12, color: "#16131F", interval: 0 }, opts.yLabel || {}) },
    series: [{ type: "bar", data: it.map(x => ({ value: x.v, itemStyle: { color: x.c || color, borderRadius: [0, 8, 8, 0] } })), barMaxWidth: 18,
      label: { show: true, position: "right", fontFamily: "JetBrains Mono", fontSize: 11, color: "#4A4760", formatter: opts.lab || (p => fmt(Math.round(p.value))) } }] }, true);
}
// barras divergentes: valores positivos favorecen a A y negativos a B
function barDiv(id, items, opts = {}) {
  const c = ch(id); if (!c) return; if (!items.length) return vacio(id, "Sin diferencias para mostrar");
  const it = [...items].reverse();
  c.setOption({ grid: { left: 10, right: 30, top: 26, bottom: 8, containLabel: true }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, formatter: opts.tip }),
    legend: { show: false }, xAxis: { type: "value", axisLabel: { formatter: opts.ejeX || (v => v) } },
    yAxis: { type: "category", data: it.map(x => x.l), axisLabel: Object.assign({ interval: 0, fontSize: 12, color: "#16131F" }, opts.yLabel || {}) },
    graphic: [{ type: "text", left: 10, top: 2, style: { text: opts.izq || "◀ mayor en B", fill: COL_B, font: "600 11px Manrope" } }, { type: "text", right: 30, top: 2, style: { text: opts.der || "mayor en A ▶", fill: COL_A, font: "600 11px Manrope" } }],
    series: [{ type: "bar", data: it.map(x => ({ value: x.v, itemStyle: { color: x.c || (x.v >= 0 ? COL_A : COL_B), borderRadius: x.v >= 0 ? [0, 8, 8, 0] : [8, 0, 0, 8] } })), barMaxWidth: 16,
      label: { show: true, position: "outside", formatter: opts.lab || (p => (p.value > 0 ? "+" : "") + p.value), fontFamily: "JetBrains Mono", fontSize: 10.5 } }] }, true);
}
function vacio(id, txt = "Sin datos para este periodo") { const c = ch(id); if (c) c.setOption({ title: { text: txt, left: "center", top: "middle", textStyle: { fontSize: 18, color: "#7C7993" } }, xAxis: { show: false }, yAxis: { show: false }, series: [], graphic: [] }, true); }
function msgsHtml(idx, max = 30) {
  if (!idx.length) return `<div class="vacio">No hay mensajes con texto para mostrar.</div>`;
  return idx.slice(0, max).map(i => { const s = C.s[i];
    return `<div class="msg">${C.nr[i] ? `<span class="rx">${C.nr[i]} reacciones</span>` : ""}<div class="m">${fdia(C.d[i])} · ${String(C.h[i]).padStart(2, "0")}h · ${esc(V.aut[C.a[i]])}${s >= 0 ? ` <span class="tag" style="background:${S_COL[s]}">${S_NOM[s]}</span>` : ""}</div>${esc(C.txt[i])}</div>`; }).join("");
}
function muestra(cond, a, b, k = 30) { const [i0, i1] = slice(a, b); const out = []; for (let i = i0; i < i1; i++) if (C.txt[i] && C.txt[i].length > 8 && cond(i)) out.push(i);
  for (let i = out.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [out[i], out[j]] = [out[j], out[i]]; } return out.slice(0, k); }
function nubePalabras(id, items, opts = {}) {
  const c = ch(id); if (!c) return; if (!items.length) return vacio(id);
  c.setOption({ tooltip: tt({ formatter: p => `<b>${esc(p.name)}</b>: ${fmt(p.value)}${opts.unidad || ""}` }), series: [{ type: "wordCloud", shape: opts.forma || "circle", gridSize: opts.grid || 6, sizeRange: opts.tam || [12, 62],
    rotationRange: [0, 0], width: "100%", height: "100%", drawOutOfBound: false, textStyle: { fontFamily: "Manrope", fontWeight: 800 },
    emphasis: { textStyle: { shadowBlur: 10, shadowColor: "rgba(0,0,0,.2)" } }, data: items }] }, true);
}
function nubeTok(R, k = 130) {
  return topK(R.tok, k).map(t => { let b = -1, v = 0; for (let s = 0; s < 15; s++) if (R.tokSent[t.i * 15 + s] > v) { v = R.tokSent[t.i * 15 + s]; b = s; }
    return { name: V.tok[t.i], value: t.v, textStyle: { color: b >= 0 && v / t.v > .25 ? S_COL[b] : "#4A4760" } }; });
}
// Nube de emojis: imágenes ubicadas en espiral, del más usado (centro) al menos usado
function nubeEmojis(el, cuenta, max = 60) {
  if (!el) return; el.innerHTML = "";
  const items = topK(cuenta, max); const W = el.clientWidth || 600, H = el.clientHeight || 420;
  if (!items.length) { el.innerHTML = `<div class="vacio">Sin emojis en este periodo</div>`; return; }
  const top = items[0].v, lado = Math.min(W, H), puestos = [];
  items.forEach(it => {
    const s = Math.round(20 + (lado * .26 - 20) * Math.pow(it.v / top, .42)), m = s * .1;
    for (let st = 0; st < 4000; st++) {
      const th = st * .13, r = 2.6 * th, x = W / 2 + r * Math.cos(th) * (W / H) - s / 2, y = H / 2 + r * Math.sin(th) - s / 2;
      if (x < 0 || y < 0 || x + s > W || y + s > H) { if (r > Math.max(W, H)) break; continue; }
      if (puestos.some(p => x + m < p.x + p.s - p.m && x + s - m > p.x + p.m && y + m < p.y + p.s - p.m && y + s - m > p.y + p.m)) continue;
      puestos.push({ x, y, s, m }); const e = EMO[it.i];
      const img = e.img ? document.createElement("img") : document.createElement("span");
      if (e.img) img.src = e.img; else { img.className = "gl"; img.textContent = e.e; img.style.fontSize = s * .85 + "px"; }
      Object.assign(img.style, { left: x + "px", top: y + "px", width: s + "px", height: s + "px" });
      img.dataset.v = it.v; tipEmo(img, it.i, it.v); el.appendChild(img); break;
    }
  });
}
const tip = document.createElement("div"); tip.className = "card"; Object.assign(tip.style, { position: "fixed", zIndex: 500, pointerEvents: "none", padding: "12px 14px", display: "none", maxWidth: "320px" }); document.body.appendChild(tip);
function tipEmo(el, i, usos) {
  el.onmouseenter = () => { const e = EMO[i]; tip.innerHTML = `<div class="tooltip-emo">${imgEmo(e.e)}<div><b style="font-family:var(--serif);font-style:italic;font-size:20px;font-weight:400">${esc(e.nombre || "")}</b><br><span style="font-size:12.5px">${esc(e.sig)}</span><br>
    <span style="font-size:11.5px;color:var(--ink2)">Familia: ${esc(e.fam)} · ${e.s >= 0 ? `<b style="color:${S_COL[e.s]}">${S_NOM[e.s]}</b>` : "neutral"}${usos != null ? ` · ${fmt(usos)} usos` : ""}${e.esr != null ? ` · ESR ${e.esr}` : ""}</span></div></div>`; tip.style.display = "block"; };
  el.onmousemove = ev => { tip.style.left = Math.min(innerWidth - 330, ev.clientX + 16) + "px"; tip.style.top = Math.min(innerHeight - 140, ev.clientY + 16) + "px"; };
  el.onmouseleave = () => tip.style.display = "none";
}
function gridEmojis(el, cuenta, k, conRango = true) {
  const top = topK(cuenta, k);
  el.innerHTML = top.length ? top.map((t, r) => { const e = EMO[t.i]; return `<div class="emo" data-i="${t.i}" data-v="${t.v}">${conRango ? `<span class="rk">${r + 1}</span>` : ""}${imgEmo(e.e)}<div class="n">${fmt(t.v)}</div>
    <span class="s" style="background:${e.s >= 0 ? S_COL[e.s] : "#94A3B8"}">${e.s >= 0 ? S_NOM[e.s] : "informativo"}</span></div>`; }).join("") : `<div class="vacio">Sin emojis en este periodo</div>`;
  el.querySelectorAll(".emo").forEach(x => tipEmo(x, +x.dataset.i, +x.dataset.v));
}

/* =====================================================================
   RENDER POR PESTAÑA
   ===================================================================== */
const RENDER = {};
const hallazgoVal = h => /\p{Extended_Pictographic}/u.test(h.valor) ? imgEmo(h.valor.trim(), "width:44px;height:44px") : esc(h.valor);

/* ---------- 01 Inicio ---------- */
RENDER.inicio = () => {
  const m = X.meta, T = TODO();
  $("#heroLead").innerHTML = `${esc(m.subtitulo)}. Periodo analizado: del <b>${fdia(0)}</b> al <b>${fdia(NDAYS - 1)}</b>; ${fmt(m.registros)} registros exportados, ${fmt(m.mensajes)} mensajes de conversación tras la limpieza y ${fmt(m.miembros)} miembros (anonimizados).`;
  const nBit = X.bitacora.filter(b => !String(b.paso).startsWith("Figura ")).length;
  $("#heroMeta").innerHTML = [`Chat: ${m.chat}`, `${X.fases.length} fases en R`, `${X.figuras.length} figuras`, `${nBit} pasos en la bitácora`, `Generado: ${m.generado}`].map(t => `<span class="pill">${esc(t)}</span>`).join("");
  $("#objetivos").innerHTML = `<div style="font:500 11px var(--mono);letter-spacing:.14em;color:var(--accent);text-transform:uppercase">Objetivo general</div>
    <div class="gen">${esc(X.objetivos.general)}</div><ol>${X.objetivos.especificos.map(o => `<li>${esc(o)}</li>`).join("")}</ol>`;
  const ds = domSent(T);
  const k = [
    ["Mensajes analizados", fmt(m.mensajes), `de ${fmt(m.registros)} registros exportados`, m.mensajes / m.registros],
    ["Con sentimiento", fmt(m.con_sentimiento), `${pct(m.con_sentimiento / m.analizables)} de los mensajes con texto`, m.con_sentimiento / m.analizables],
    ["Sentimiento predominante", S_NOM[ds], `${pct(T.sent[ds] / T.ns)} de los mensajes con sentimiento`, T.sent[ds] / T.ns],
    ["Índice de ánimo", animo(T).toFixed(2), "de -1 (todo negativo) a +1 (todo positivo)", (animo(T) + 1) / 2],
    ["Miembros", fmt(m.miembros), `escribieron en ${fmt(m.dias_activos)} días distintos`, 1],
    ["Publicidad excluida", fmt(m.publicidad), `y ${fmt(m.avisos)} avisos del sistema`, m.publicidad / m.registros],
    ["Validación", "κ " + X.modelos.kappa.toFixed(2), `palabras y emojis coinciden en ${pct(X.modelos.concordancia)}`, X.modelos.concordancia],
    ["Tópicos (LDA)", X.modelos.k, X.modelos.topicos.map(t => t.nombre).slice(0, 3).join(", ") + "...", 1]];
  $("#kpis").innerHTML = k.map(([t, v, d, p]) => `<div class="card kpi c3"><div class="k">${t}</div><div class="v">${esc(v)}</div><div class="d">${esc(d)}</div><div class="barra-k"><span style="width:${Math.max(4, Math.min(100, 100 * p))}%"></span></div></div>`).join("");
  $("#curiosos").innerHTML = X.hallazgos.slice(0, 12).map(h => `<div class="card curioso c3"><div class="cat">${esc(h.categoria)}</div><div class="val">${hallazgoVal(h)}</div><div class="tit">${esc(h.titulo)}</div><p>${esc(h.texto)}</p></div>`).join("");
  $("#cobertura").innerHTML = X.cobertura.map(c => `<div style="display:flex;align-items:center;gap:12px;padding:10px 0;border-bottom:1px dashed rgba(22,19,31,.1)">
    <div class="cifra-g" style="font-size:34px;min-width:84px">${c.anio}</div><div style="font-size:13px;color:var(--ink2);line-height:1.5"><b style="color:var(--ink)">${c.desde} → ${c.hasta}</b><br>${fmt(c.mensajes)} mensajes · ${c.dias} días activos</div></div>`).join("");
  const { keys, map } = buckets(T, "semana");
  const c = ch("chInicioLinea");
  c.setOption({ grid: { left: 40, right: 16, top: 20, bottom: 30 }, tooltip: tt({ trigger: "axis", formatter: ps => { const o = map.get(keys[ps[0].dataIndex]); const s = domSent({ sent: o.s });
      return `<b>Semana del ${keys[ps[0].dataIndex].split("-").reverse().join("/")}</b><br>${fmt(o.n)} mensajes${s >= 0 ? `<br>Predomina: <b style="color:${S_COL[s]}">${S_NOM[s]}</b>` : ""}`; } }),
    xAxis: { type: "category", data: keys.map(k => `${MESES[+k.slice(5, 7) - 1]} ${k.slice(2, 4)}`), axisLabel: { interval: Math.ceil(keys.length / 12) } }, yAxis: { type: "value" },
    series: [{ type: "line", smooth: .35, symbol: "circle", symbolSize: 5, data: keys.map(k => { const o = map.get(k), s = domSent({ sent: o.s }); return { value: o.n, itemStyle: { color: s >= 0 ? S_COL[s] : "#94A3B8" } }; }),
      lineStyle: { width: 2.2, color: css("--accent") }, areaStyle: { color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [{ offset: 0, color: css("--accent") + "55" }, { offset: 1, color: css("--accent") + "00" }]) } }] }, true);
  c.off("click"); c.on("click", p => { const d = fromIso(keys[p.dataIndex]); irA("explorador"); aplicar("A", "semana", dayIso(d)); });
};

/* ---------- 02 Hilo del proyecto: visor de código ---------- */
function seccionesScript(txt) {
  const L = txt.split("\n"), secs = [];
  let cur = { t: "Encabezado y propósito", l0: 0, exp: [], num: null, sub: false }, sub = null;
  const cerrar = (o, i) => { if (o) { o.l1 = i - 1; secs.push(o); } };
  L.forEach((ln, i) => {
    let m = ln.match(/^#\s*-{3,}\s*(.+?)\s*-{3,}\s*$/);
    if (m && !/\p{L}/u.test(m[1])) m = null;
    const mc = !m && ln.match(/^#\s*(\d+\.\d+)\s+(.+?)\s*-{3,}\s*$/);      // formato de 00_config.R
    const mf = ln.match(/^#\s*(F\d{2})-(\d{2})\s+(.+)$/);                   // subsección de figura
    if (m || mc) {
      cerrar(sub, i); sub = null; cerrar(cur, i);
      const t = m ? m[1].replace(/-+$/, "").trim() : `${mc[1]} ${mc[2].charAt(0) + mc[2].slice(1).toLowerCase()}`;
      const n = t.match(/^(\d+[a-z]?)\./);
      cur = { t, l0: i, exp: [], num: n ? n[1] : null, sub: false }; return;
    }
    if (mf) { cerrar(sub, i); sub = { t: `Figura ${mf[1]}-${mf[2]}`, l0: i, exp: [], fig: { fase: mf[1], nro: +mf[2] }, sub: true }; return; }
    const destino = sub || cur;
    if (/^\s*#>/.test(ln)) destino.exp.push(ln.replace(/^\s*#>\s?/, ""));
    else if (cur.t === "Encabezado y propósito" && !sub && /^#\s{1,3}\S/.test(ln) && !/^#\s*=+/.test(ln)) cur.exp.push(ln.replace(/^#\s*/, ""));
  });
  cerrar(sub, L.length); cerrar(cur, L.length);
  // las secciones principales abarcan también sus subsecciones
  const out = secs.filter(s => !s.sub).sort((a, b) => a.l0 - b.l0);
  out.forEach((s, k) => { s.l1 = (out[k + 1] ? out[k + 1].l0 : L.length) - 1; });
  const subs = secs.filter(s => s.sub);
  const final = []; out.forEach(s => { final.push(s); subs.filter(x => x.l0 > s.l0 && x.l0 <= s.l1).sort((a, b) => a.l0 - b.l0).forEach(x => final.push(x)); });
  return { lineas: L, secs: final };
}
function visorCodigo(cont, nombre, color, cod) {
  const txt = X.scripts[nombre]; if (!txt) { cont.innerHTML = `<div class="vacio">Script no disponible</div>`; return; }
  const { lineas, secs } = seccionesScript(txt);
  const bits = cod ? X.bitacora.filter(b => String(b.fase).startsWith(cod)) : [];
  const hl = lineas.map(l => `<span class="ln">${Prism.highlight(l || " ", Prism.languages.r, "r")}</span>`).join("");
  cont.innerHTML = `<div class="codigo" style="--c:${color}"><div class="bar"><span class="marca-f"></span><span class="nombre">${nombre.includes("TODO") || nombre.startsWith("GENERAR") ? "" : "02_Codigo/"}${esc(nombre)}</span><span>${lineas.length} líneas · ${secs.filter(s => !s.sub).length} secciones</span><button>Copiar</button></div>
    <div class="cuerpo"><div class="secciones">${secs.map((s, i) => `<button data-i="${i}" class="${s.sub ? "sub" : ""}">${esc(s.t)}</button>`).join("")}</div><pre>${hl}</pre></div>
    <div class="explica"><b>Script principal</b>Al elegir una sección de la izquierda se resalta su código y se muestra qué hace, por qué y dónde quedó registrada en la bitácora.</div></div>`;
  const pre = cont.querySelector("pre"), spans = pre.querySelectorAll(".ln"), ex = cont.querySelector(".explica");
  cont.querySelectorAll(".secciones button").forEach(b => b.onclick = () => {
    const s = secs[+b.dataset.i]; cont.querySelectorAll(".secciones button").forEach(x => x.classList.toggle("on", x === b));
    spans.forEach((sp, i) => sp.classList.toggle("hl", i >= s.l0 && i <= s.l1));
    pre.scrollTo({ top: spans[s.l0].offsetTop - 12, behavior: "smooth" });
    let h = `<b>${esc(s.t)} · líneas ${s.l0 + 1} a ${s.l1 + 1}</b>`;
    if (s.fig) {
      const f = X.figuras.find(x => x.fase === s.fig.fase && +x.nro === s.fig.nro);
      h += f ? `<div class="ex-fig"><img src="img/${f.archivo}" data-a="${f.archivo}" alt=""><div><b style="color:var(--ink);text-transform:none;letter-spacing:0;font:700 13px var(--sans)">${esc(f.titulo)}</b>${esc(f.descripcion)}</div></div>` : esc(s.exp.join(" "));
      const bf = bits.find(x => x.paso === s.t.replace("Figura ", "Figura "));
      if (bf) h += `<div class="ex-bit">Registrada en la bitácora: ID ${bf.id} · ${esc(f ? "04_Resultados/figuras/" + bf.archivo.split("/").slice(-2).join("/") : "")}</div>`;
    } else {
      h += s.exp.length ? esc(s.exp.join(" ")) : "Sección auxiliar del script; los detalles están en los comentarios del código.";
      if (s.num) { const bs = bits.filter(x => new RegExp(`^${s.num}[a-z]?\\.`).test(x.paso)); if (bs.length) h += `<div class="ex-bit">Registrado en la bitácora: ${bs.map(x => `ID ${x.id} (${esc(x.paso)}: ${esc(x.accion)})`).join(" · ")}</div>`; }
    }
    ex.innerHTML = h;
    const im = ex.querySelector("img[data-a]"); if (im) im.onclick = () => abrirLB(im.dataset.a);
  });
  cont.querySelector(".bar button").onclick = e => { navigator.clipboard?.writeText(txt); e.target.textContent = "Copiado"; setTimeout(() => e.target.textContent = "Copiar", 1400); };
}
RENDER.hilo = () => {
  const B = new Map(X.bitacora.map(b => [+b.id, b]));
  $("#hilo").innerHTML = X.fases.map((f, k) => { const pasos = f.bit_ids.map(id => B.get(id)).filter(b => b && !String(b.paso).startsWith("Figura "));
    return `<div class="nodo" style="--c:${f.color}" data-k="${k}"><span class="punto"></span>
    <div class="card"><div class="fcab"><span class="cod">${f.cod}</span><h4>${esc(f.titulo)}</h4><span class="pill">${f.figuras.length} figuras · ${pasos.length} pasos en la bitácora${f.bit_ids.length ? ` (ID ${Math.min(...f.bit_ids)}–${Math.max(...f.bit_ids)})` : ""}</span></div>
    <p class="resumen">${esc(f.obj)}</p><div class="det"></div></div></div>`; }).join("");
  $$("#hilo .nodo").forEach(n => n.querySelector(".card").addEventListener("click", e => {
    if (e.target.closest(".det")) return; const k = +n.dataset.k, f = X.fases[k], det = n.querySelector(".det");
    const abierto = n.classList.toggle("abierto"); if (!abierto || det.dataset.ok) return;
    det.dataset.ok = 1;
    const pasos = f.bit_ids.map(id => B.get(id)).filter(b => b && !String(b.paso).startsWith("Figura "));
    det.innerHTML = `<div class="det-grid"><div><dl class="ficha"><dt>Tema de clase aplicado</dt><dd>${esc(f.clase)}</dd><dt>Métodos</dt><dd>${esc(f.metodos)}</dd><dt>Justificación</dt><dd>${esc(f.porque)}</dd>
      <dt>Pasos registrados en la bitácora</dt></dl><ul class="pasos">${pasos.map(b => `<li><span class="bid">ID ${b.id}</span><b>${esc(b.paso)}: ${esc(b.accion)}</b>${esc(b.detalle)}${b.justificacion ? `<br><i>Justificación:</i> ${esc(b.justificacion)}` : ""}</li>`).join("")}</ul>
      <p class="sub" style="margin-top:8px">Además, cada una de las ${f.figuras.length} figuras de la fase quedó registrada en la bitácora con su número.</p></div>
      <div class="visor"></div></div><div class="galeria">${f.figuras.map(g => `<figure data-a="${g.archivo}"><img loading="lazy" src="img/${g.archivo}" alt=""><figcaption><b>Figura ${f.cod}-${String(g.nro).padStart(2, "0")}</b>${esc(g.titulo)}</figcaption></figure>`).join("")}</div>`;
    visorCodigo(det.querySelector(".visor"), f.script, f.color, f.cod);
    det.querySelectorAll("figure").forEach(fg => fg.onclick = () => abrirLB(fg.dataset.a, f.figuras.map(x => x.archivo)));
  }));
  const apoyo = ["00_config.R", "diccionario_sentimientos.R", "motor_sentimientos.R", "EJECUTAR_TODO.R", "GENERAR_DASHBOARD.R"];
  $("#scriptsApoyo").innerHTML = apoyo.map(a => `<button data-a="${a}">${a}</button>`).join("");
  $$("#scriptsApoyo button").forEach(b => b.onclick = () => visorCodigo($("#visorApoyo"), b.dataset.a, "#475569"));
};

/* ---------- Visor de figuras (limitado a la fase) ---------- */
const LB = { lista: [], i: 0 };
function abrirLB(arch, lista) {
  const f0 = X.figuras.find(f => f.archivo === arch); if (!f0) return;
  LB.lista = lista && lista.length ? lista : X.figuras.filter(f => f.fase === f0.fase).map(f => f.archivo);
  LB.i = Math.max(0, LB.lista.indexOf(arch)); pintarLB(); $("#lightbox").classList.add("on");
}
function cerrarLB() { $("#lightbox").classList.remove("on"); }
function pintarLB() {
  const f = X.figuras.find(x => x.archivo === LB.lista[LB.i]); if (!f) return;
  $("#lbImg").src = "img/" + f.archivo;
  $("#lbCap").innerHTML = `<code>Figura ${f.fase}-${String(f.nro).padStart(2, "0")} · 04_Resultados/figuras</code><b>${esc(f.titulo)}</b>${esc(f.descripcion)}`;
  const n = LB.lista.length, ult = LB.i === n - 1;
  $("#lbCont").textContent = `Figura ${LB.i + 1} de ${n} · Fase ${f.fase}`;
  $("#lbPrev").disabled = LB.i === 0;
  $("#lbNext").textContent = ult ? "Volver a la fase" : "Siguiente";
}
function lbSig() { if (LB.i >= LB.lista.length - 1) return cerrarLB(); LB.i++; pintarLB(); }
function lbAnt() { if (LB.i > 0) { LB.i--; pintarLB(); } }
$("#lbCerrar").onclick = cerrarLB;
$("#lightbox").onclick = e => { if (e.target.id === "lightbox") cerrarLB(); };
$("#lbPrev").onclick = lbAnt; $("#lbNext").onclick = lbSig;
document.addEventListener("keydown", e => { if (!$("#lightbox").classList.contains("on")) return; if (e.key === "Escape") cerrarLB(); if (e.key === "ArrowRight") lbSig(); if (e.key === "ArrowLeft") lbAnt(); });

/* ---------- 03 Explorador ---------- */
function kpisRango(R) {
  const ds = domSent(R), dias = R.porDia.size, topE = topK(emoTodo(R), 1)[0], topW = topK(R.tok, 1)[0];
  const k = [["Mensajes", fmt(R.n), `${fmt(dias)} días con mensajes · ${(R.n / Math.max(1, R.b - R.a + 1)).toFixed(1)} por día`],
    ["Sentimiento predominante", ds >= 0 ? S_NOM[ds] : "—", ds >= 0 ? `${pct(R.sent[ds] / R.ns)} de ${fmt(R.ns)} mensajes con sentimiento` : ""],
    ["Índice de ánimo", animo(R).toFixed(2), `${fmt(R.pol[2])} positivos · ${fmt(R.pol[0])} negativos`],
    ["Preguntas", fmt(R.preguntas), `${pct(R.respondidas / Math.max(1, R.preguntas))} recibieron respuesta`],
    ["Miembros activos", fmt(R.aut.filter(v => v > 0).length), `${fmt(R.nreac)} reacciones en el periodo`],
    ["Emoji más usado", "", topE ? `${fmt(topE.v)} usos · ${EMO[topE.i].sig.split(";")[0]}` : ""],
    ["Palabra más usada", topW ? V.tok[topW.i] : "—", topW ? `${fmt(topW.v)} apariciones` : ""],
    ["Ironía detectada", fmt(R.nir), "mensajes con señales de ironía"]];
  $("#kpisRango").innerHTML = k.map(([t, v, d], i) => `<div class="card kpi c3"><div class="k">${t}</div><div class="v">${i === 5 && topE ? imgEmo(EMO[topE.i].e, "width:48px;height:48px;vertical-align:middle") : esc(v)}</div><div class="d">${esc(d)}</div></div>`).join("");
}
RENDER.explorador = () => {
  const R = RA(); kpisRango(R);
  const g = gran(S.a, S.b), { keys, map } = buckets(R, g);
  const c = ch("chTimeline");
  c.setOption({ grid: { left: 46, right: 50, top: 30, bottom: 70 }, legend: { top: 0, data: ["Mensajes", "Índice de ánimo"] },
    tooltip: tt({ trigger: "axis", formatter: ps => { const o = map.get(keys[ps[0].dataIndex]), s = domSent({ sent: o.s }); return `<b>${etiqB(keys[ps[0].dataIndex], g)}</b><br>${fmt(o.n)} mensajes<br>${fmt(o.pos)} positivos · ${fmt(o.neg)} negativos${s >= 0 ? `<br>Predomina <b style="color:${S_COL[s]}">${S_NOM[s]}</b>` : ""}`; } }),
    xAxis: { type: "category", data: keys.map(k => etiqB(k, g)) }, yAxis: [{ type: "value", name: "mensajes" }, { type: "value", min: -1, max: 1, name: "ánimo", splitLine: { show: false } }],
    dataZoom: [{ type: "slider", bottom: 14, height: 26, borderColor: "transparent", backgroundColor: "rgba(255,255,255,.5)", fillerColor: css("--accent") + "33", handleStyle: { color: css("--accent") } }, { type: "inside", zoomOnMouseWheel: false }],
    series: [{ name: "Mensajes", type: "bar", data: keys.map(k => map.get(k).n), itemStyle: { color: css("--accent"), borderRadius: [6, 6, 0, 0] }, barMaxWidth: 26 },
      { name: "Índice de ánimo", type: "line", yAxisIndex: 1, smooth: .3, symbol: "none", data: keys.map(k => { const o = map.get(k); return o.pos + o.neg >= 3 ? +((o.pos - o.neg) / (o.pos + o.neg)).toFixed(3) : null; }), lineStyle: { color: "#9333EA", width: 2 }, itemStyle: { color: "#9333EA" }, connectNulls: true }] }, true);
  c.off("datazoom"); let zt = null;
  c.on("datazoom", () => { clearTimeout(zt); zt = setTimeout(() => { const o = c.getOption().dataZoom[0]; if (o.start <= 0.5 && o.end >= 99.5) return;
    const i0 = Math.max(0, Math.round(o.start / 100 * (keys.length - 1))), i1 = Math.min(keys.length - 1, Math.round(o.end / 100 * (keys.length - 1)));
    aplicarRango(map.get(keys[i0]).a, map.get(keys[i1]).b); }, 650); });
  // sentimientos, emojis y nubes del periodo
  const ord = [...Array(15).keys()].filter(i => R.sent[i] > 0).sort((x, y) => R.sent[y] - R.sent[x]);
  ord.length ? barH("chExpSent", ord.map(i => ({ l: `{s${i}|} ${S_NOM[i]}`, v: +(R.sent[i] / R.ns * 100).toFixed(1), c: S_COL[i] })), null, { lab: p => p.value + "%", yLabel: { rich: sentRich(18) } }) : vacio("chExpSent");
  gridEmojis($("#expEmojis"), emoTodo(R), 12);
  nubePalabras("chExpNube", nubeTok(R, 110));
  nubeEmojis($("#expNubeEmo"), emoTodo(R), 55);
  // calendario
  const calCol = ["#F3FBF5", "#9BE3B0", "#22A55B", "#0B5D2E"];
  if (S.b - S.a <= 200) {
    const sem = [], vals = []; let mx = 1;
    for (let d = S.a; d <= S.b; d++) { const t = dayDate(d), w = (t.getDay() + 6) % 7, k = iso(new Date(t.getTime() - w * 864e5)); if (!sem.length || sem[sem.length - 1] !== k) sem.push(k); const n = R.porDia.get(d)?.n || 0; mx = Math.max(mx, n); vals.push([sem.length - 1, w, n, d]); }
    ch("chCalendario").setOption({ grid: { left: 70, right: 20, top: 10, bottom: 50 }, tooltip: tt({ formatter: p => `${fdia(p.data[3])} (${DIAS[p.data[1]]})<br><b>${fmt(p.data[2])} mensajes</b>` }),
      xAxis: { type: "category", data: sem.map(k => k.slice(8, 10) + "/" + k.slice(5, 7)) }, yAxis: { type: "category", data: DIAS, inverse: true },
      visualMap: { min: 0, max: mx, calculable: true, orient: "horizontal", left: "center", bottom: 0, itemHeight: 140, inRange: { color: calCol }, textStyle: { fontSize: 10 } },
      series: [{ type: "heatmap", data: vals, itemStyle: { borderColor: "rgba(255,255,255,.8)", borderWidth: 1.5, borderRadius: 3 } }] }, true);
  } else {
    const y0 = dayDate(S.a).getFullYear(), y1 = dayDate(S.b).getFullYear(), anios = []; for (let y = y0; y <= y1; y++) anios.push(String(y));
    const cel = new Map(); let mx = 1;
    for (let d = S.a; d <= S.b; d++) { const t = dayDate(d), y = t.getFullYear(), sw = Math.min(52, Math.floor(Math.round((t - new Date(y, 0, 1, 12)) / 864e5) / 7)), k = (y - y0) * 53 + sw;
      const o = cel.get(k) || { n: 0, d0: d, d1: d }; o.n += R.porDia.get(d)?.n || 0; o.d1 = d; cel.set(k, o); mx = Math.max(mx, o.n); }
    const vals = [...cel.entries()].map(([k, o]) => [k % 53, Math.floor(k / 53), o.n, o.d0, o.d1]);
    const ejeX = [...Array(53).keys()].map(w => { const t = new Date(2025, 0, 1 + w * 7); return w % 4 === 0 ? `${t.getDate()} ${MESES[t.getMonth()]}` : ""; });
    ch("chCalendario").setOption({ grid: { left: 54, right: 20, top: 10, bottom: 50 }, tooltip: tt({ formatter: p => `Semana ${fdia(p.data[3])} – ${fdia(p.data[4])}<br><b>${fmt(p.data[2])} mensajes</b>` }),
      xAxis: { type: "category", data: ejeX, axisLabel: { interval: 0, fontSize: 10 } }, yAxis: { type: "category", data: anios, inverse: true, axisLabel: { fontFamily: "Fraunces", fontStyle: "italic", fontSize: 15, color: "#16131F" } },
      visualMap: { min: 0, max: mx, calculable: true, orient: "horizontal", left: "center", bottom: 0, itemHeight: 140, inRange: { color: calCol }, textStyle: { fontSize: 10 } },
      series: [{ type: "heatmap", data: vals, itemStyle: { borderColor: "rgba(255,255,255,.85)", borderWidth: 2, borderRadius: 4 } }] }, true);
  }
  const hw = []; let mh = 1; for (let w = 0; w < 7; w++) for (let h = 0; h < 24; h++) { const v = R.hw[w * 24 + h]; mh = Math.max(mh, v); hw.push([h, w, v]); }
  ch("chHoraDia").setOption({ grid: { left: 70, right: 10, top: 10, bottom: 50 }, tooltip: tt({ formatter: p => `${DIAS[p.data[1]]} ${p.data[0]}h: <b>${fmt(p.data[2])}</b>` }),
    xAxis: { type: "category", data: [...Array(24).keys()].map(h => h + "h") }, yAxis: { type: "category", data: DIAS, inverse: true },
    visualMap: { min: 0, max: mh, orient: "horizontal", left: "center", bottom: 0, itemHeight: 120, inRange: { color: ["#F3FBF5", "#FDE68A", "#F97316", "#9A3412"] }, textStyle: { fontSize: 10 } },
    series: [{ type: "heatmap", data: hw, itemStyle: { borderColor: "rgba(255,255,255,.8)", borderWidth: 1.5, borderRadius: 3 } }] }, true);
  // temas y mensajes destacados
  const TP = X.modelos.topicos;
  sumA(R.top) ? ch("chExpTemas").setOption({ tooltip: tt({ formatter: p => `${p.name}<br><b>${fmt(p.value)}</b> mensajes (${p.percent}%)` }), legend: { bottom: 0, type: "scroll" },
    series: [{ type: "pie", radius: ["40%", "70%"], center: ["50%", "44%"], itemStyle: { borderColor: "#fff", borderWidth: 3, borderRadius: 8 }, label: { formatter: "{d}%", fontFamily: "Fraunces", fontStyle: "italic", fontWeight: 600 },
      data: TP.map((t, i) => ({ name: t.nombre, value: R.top[i], itemStyle: { color: TOP_COL[i] } })) }] }, true) : vacio("chExpTemas");
  const dest = []; for (let i = R.i0; i < R.i1; i++) if (C.nr[i] > 0 && C.txt[i] && C.txt[i].length > 3) dest.push(i);
  dest.sort((x, y) => C.nr[y] - C.nr[x]);
  $("#expDestacados").innerHTML = msgsHtml(dest, 12);
  // días top y palabras distintivas
  const T = TODO(); const top = [...R.porDia.entries()].sort((x, y) => y[1].n - x[1].n).slice(0, 8);
  $("#diasTop").innerHTML = top.length ? top.map(([d, o]) => { const s = domSent({ sent: o.s }); const w = logOdds(agg(d, d), T, 2).slice(0, 5).map(x => V.tok[x.i]).join(", ");
    return `<div style="display:flex;gap:12px;align-items:center;padding:9px 6px;border-bottom:1px dashed rgba(22,19,31,.1);cursor:pointer" data-d="${d}"><div class="cifra-g" style="font-size:28px;min-width:60px">${fmt(o.n)}</div>
      <div style="font-size:12.8px;line-height:1.45"><b>${fdia(d)}</b> (${DIAS[(dayDate(d).getDay() + 6) % 7]})${s >= 0 ? ` <span class="tag" style="background:${S_COL[s]}">${S_NOM[s]}</span>` : ""}<br><span style="color:var(--ink2)">${esc(w)}</span></div></div>`; }).join("") : `<div class="vacio">Sin mensajes</div>`;
  $$("#diasTop [data-d]").forEach(el => el.onclick = () => aplicar("A", "dia", dayIso(+el.dataset.d)));
  const lo = (S.a === 0 && S.b === NDAYS - 1) ? [] : logOdds(R, T, 3).slice(0, 18);
  if (!lo.length) vacio("chDistintivas", "Disponible al elegir un periodo");
  else barH("chDistintivas", lo.map(x => ({ l: V.tok[x.i], v: +x.z.toFixed(2) })), css("--accent"), { lab: p => "z " + p.value, tip: ps => `<b>${ps[0].name}</b><br>z = ${ps[0].value}<br>${fmt(R.tok[V.tok.indexOf(ps[0].name)])} apariciones` });
};

/* ---------- 04 Sentimientos ---------- */
let sentPick = 0;
RENDER.sentimientos = () => {
  const R = RA(), B = RB();
  $("#subSent").textContent = `${fmt(R.ns)} mensajes con sentimiento en ${S.lab}${B ? ` frente a ${fmt(B.ns)} en ${S.A2.lab}` : ""}.`;
  const ord = [...Array(15).keys()].sort((x, y) => R.sent[y] - R.sent[x]).reverse();
  const ser = [{ name: B ? "A · " + S.lab : S.lab, type: "bar", data: ord.map(i => ({ value: R.ns ? +(R.sent[i] / R.ns * 100).toFixed(2) : 0, itemStyle: { color: S_COL[i], borderRadius: [0, 8, 8, 0] } })), barMaxWidth: 16,
    label: { show: true, position: "right", formatter: p => p.value.toFixed(1) + "%", fontFamily: "JetBrains Mono", fontSize: 11 } }];
  if (B) ser.push({ name: "B · " + S.A2.lab, type: "bar", data: ord.map(i => ({ value: B.ns ? +(B.sent[i] / B.ns * 100).toFixed(2) : 0, itemStyle: { color: S_COL[i] + "55", borderColor: S_COL[i], borderWidth: 1, borderRadius: [0, 8, 8, 0] } })), barMaxWidth: 16,
    label: { show: true, position: "right", formatter: p => p.value.toFixed(1) + "%", fontFamily: "JetBrains Mono", fontSize: 10, color: "#7C7993" } });
  ch("chSent15").setOption({ grid: { left: 10, right: 60, top: B ? 28 : 6, bottom: 6, containLabel: true }, legend: { show: !!B, top: 0 }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" },
      formatter: ps => { const i = ord[ps[0].dataIndex]; return `<b style="color:${S_COL[i]}">${S_NOM[i]}</b><br>${esc(SENT[i].definicion)}<br>` + ps.map(p => `${p.seriesName}: <b>${p.value.toFixed(1)}%</b>`).join("<br>") + `<br>${fmt(R.sent[i])} mensajes en A`; } }),
    xAxis: { type: "value", axisLabel: { formatter: "{value}%" } },
    yAxis: { type: "category", data: ord.map(i => `{s${i}|} ${S_NOM[i]}`), axisLabel: { rich: sentRich(20), fontSize: 12.5, color: "#16131F", interval: 0 } }, series: ser }, true);
  const pol = (X_, nombre, radio) => ({ name: nombre, type: "pie", radius: radio, center: ["50%", "54%"], itemStyle: { borderColor: "rgba(255,255,255,.9)", borderWidth: 3, borderRadius: 8 },
    label: { formatter: "{d}%", fontFamily: "Fraunces", fontStyle: "italic", fontSize: 15, fontWeight: 600 },
    data: [{ name: "Positivo", value: X_.pol[2], itemStyle: { color: POL_COL.pos } }, { name: "Neutro", value: X_.pol[1], itemStyle: { color: POL_COL.neu } }, { name: "Negativo", value: X_.pol[0], itemStyle: { color: POL_COL.neg } }] });
  ch("chPolaridad").setOption({ legend: { bottom: 0 }, tooltip: tt({ formatter: p => `${p.seriesName}<br>${p.name}: <b>${fmt(p.value)}</b> (${p.percent}%)` }),
    title: B ? { text: "Anillo exterior: A · interior: B", left: "center", top: 0, textStyle: { fontSize: 13, fontFamily: "Manrope", fontStyle: "normal", color: "#7C7993" } } : undefined,
    series: B ? [pol(R, "A · " + S.lab, ["52%", "72%"]), Object.assign(pol(B, "B · " + S.A2.lab, ["24%", "44%"]), { label: { show: false } })] : [pol(R, S.lab, ["42%", "72%"])] }, true);
  $("#notaAnimo").innerHTML = `Índice de ánimo de <b>${esc(S.lab)}</b>: <b>${animo(R).toFixed(2)}</b>${B ? ` · <b>${esc(S.A2.lab)}</b>: <b>${animo(B).toFixed(2)}</b>` : ""}. Ironía detectada en ${fmt(R.nir)} mensajes.`;
  const g = gran(S.a, S.b), { keys, map } = buckets(R, g);
  ch("chSentTiempo").setOption({ grid: { left: 46, right: 20, top: 60, bottom: 40 }, legend: { top: 0, type: "scroll" }, tooltip: tt({ trigger: "axis", order: "valueDesc", valueFormatter: v => v == null ? "-" : v.toFixed(1) + "%" }),
    xAxis: { type: "category", data: keys.map(k => etiqB(k, g)), boundaryGap: false }, yAxis: { type: "value", max: 100, axisLabel: { formatter: "{value}%" } }, dataZoom: [{ type: "inside", zoomOnMouseWheel: false }],
    series: SENT.map((s, i) => ({ name: s.nombre, type: "line", stack: "t", smooth: .3, symbol: "none", lineStyle: { width: 0 }, itemStyle: { color: s.color }, areaStyle: { color: s.color, opacity: .9 }, emphasis: { focus: "series" },
      data: keys.map(k => { const o = map.get(k), t = sumA(o.s); return t >= 5 ? +(o.s[i] / t * 100).toFixed(2) : null; }) })) }, true);
  const rmax = Math.max(...SENT.map((_, i) => Math.max(R.ns ? R.sent[i] / R.ns : 0, B && B.ns ? B.sent[i] / B.ns : 0))) * 100;
  ch("chRadar").setOption({ legend: { bottom: 0 }, tooltip: tt(), radar: { radius: "64%", center: ["50%", "48%"], indicator: SENT.map(s => ({ name: s.nombre, max: Math.ceil(Math.sqrt(rmax)) })), axisName: { color: "#4A4760", fontSize: 11 },
      splitArea: { areaStyle: { color: ["rgba(255,255,255,.35)", "rgba(255,255,255,.1)"] } }, splitLine: { lineStyle: { color: "rgba(22,19,31,.1)" } } },
    series: [{ type: "radar", data: [{ name: B ? "A · " + S.lab : S.lab, value: SENT.map((_, i) => +Math.sqrt(R.ns ? R.sent[i] / R.ns * 100 : 0).toFixed(3)), areaStyle: { color: css("--accent") + "44" }, lineStyle: { color: css("--accent"), width: 2 }, itemStyle: { color: css("--accent") } }]
      .concat(B ? [{ name: "B · " + S.A2.lab, value: SENT.map((_, i) => +Math.sqrt(B.ns ? B.sent[i] / B.ns * 100 : 0).toFixed(3)), areaStyle: { color: COL_B + "33" }, lineStyle: { color: COL_B, width: 2 }, itemStyle: { color: COL_B } }] : []),
      tooltip: { formatter: p => `<b>${p.name}</b><br>` + SENT.map((s, i) => `${s.nombre}: ${(p.value[i] ** 2).toFixed(1)}%`).join("<br>") } }] }, true);
  $("#subRadar").textContent = "Escala raíz cuadrada, para que también se vean los sentimientos poco frecuentes" + (B ? "; el periodo B se muestra en verde azulado." : ".");
  const sh = []; for (let s = 0; s < 15; s++) { let t = 0; for (let h = 0; h < 24; h++) t += R.sh[s * 24 + h]; for (let h = 0; h < 24; h++) sh.push([h, s, t ? +(R.sh[s * 24 + h] / t * 100).toFixed(2) : 0]); }
  ch("chSentHora").setOption({ grid: { left: 96, right: 10, top: 10, bottom: 50 }, tooltip: tt({ formatter: p => `${S_NOM[p.data[1]]} a las ${p.data[0]}h: <b>${p.data[2]}%</b> de sus mensajes` }),
    xAxis: { type: "category", data: [...Array(24).keys()].map(h => h + "h") }, yAxis: { type: "category", data: S_NOM, inverse: true, axisLabel: { interval: 0 } },
    visualMap: { min: 0, max: Math.max(8, ...sh.map(x => x[2])), orient: "horizontal", left: "center", bottom: 0, itemHeight: 120, inRange: { color: ["#FBF7FF", "#E9D5FF", "#A855F7", "#581C87"] }, textStyle: { fontSize: 10 } },
    series: [{ type: "heatmap", data: sh, itemStyle: { borderColor: "rgba(255,255,255,.8)", borderWidth: 1, borderRadius: 2 } }] }, true);
  const an = keys.map(k => { const o = map.get(k); return o.pos + o.neg >= 3 ? (o.pos - o.neg) / (o.pos + o.neg) : null; });
  const sm = an.map((_, i) => { const w = an.slice(Math.max(0, i - 2), i + 3).filter(v => v != null); return w.length ? +(w.reduce((a, b) => a + b, 0) / w.length).toFixed(3) : null; });
  ch("chAnimo").setOption({ grid: { left: 40, right: 16, top: 16, bottom: 30 }, tooltip: tt({ trigger: "axis", valueFormatter: v => v == null ? "-" : v.toFixed(2) }), xAxis: { type: "category", data: keys.map(k => etiqB(k, g)), boundaryGap: false }, yAxis: { type: "value", min: -1, max: 1 },
    series: [{ name: "Ánimo positivo", type: "line", data: sm.map(v => v == null ? null : Math.max(0, v)), smooth: .4, symbol: "none", connectNulls: true, lineStyle: { width: 2.2, color: POL_COL.pos }, areaStyle: { color: POL_COL.pos, opacity: .22, origin: 0 } },
      { name: "Ánimo negativo", type: "line", data: sm.map(v => v == null ? null : Math.min(0, v)), smooth: .4, symbol: "none", connectNulls: true, lineStyle: { width: 2.2, color: POL_COL.neg }, areaStyle: { color: POL_COL.neg, opacity: .22, origin: 0 } }] }, true);
  $("#pickSent").innerHTML = SENT.map((s, i) => `<span style="background:${s.color};cursor:pointer;${i === sentPick ? "outline:3px solid rgba(22,19,31,.35)" : ""}" data-i="${i}">${s.nombre} ${R.sent[i] ? "· " + fmt(R.sent[i]) : ""}</span>`).join("");
  $$("#pickSent span").forEach(sp => sp.onclick = () => { sentPick = +sp.dataset.i; RENDER.sentimientos(); });
  $("#msgsSent").innerHTML = msgsHtml(muestra(i => C.s[i] === sentPick, S.a, S.b, 30));
  const rs = ["Negativo", "Neutro", "Positivo"].map((n, k) => ({ l: n, v: R.risa[k][0] ? +(R.risa[k][1] / R.risa[k][0] * 100).toFixed(2) : 0, c: [POL_COL.neg, POL_COL.neu, POL_COL.pos][k] }));
  barH("chRisa", rs.reverse(), null, { lab: p => p.value.toFixed(1) + "%", tip: ps => `${ps[0].name}: ${ps[0].value}% de los mensajes incluye risa` });
  const rq = [...Array(15).keys()].map(i => ({ l: S_NOM[i], v: R.reacN[i] ? +(R.reacS[i] / R.reacN[i]).toFixed(2) : 0, c: S_COL[i] })).sort((x, y) => y.v - x.v);
  barH("chSentReac", rq, null, { lab: p => p.value.toFixed(2) });
};

/* ---------- 05 Emojis ---------- */
let emoFuente = "texto";
function emoCuenta(R) { return emoFuente === "texto" ? R.emoT : emoFuente === "reaccion" ? R.emoR : emoTodo(R); }
RENDER.emojis = () => {
  const R = RA(), cnt = emoCuenta(R);
  $("#emoFuente").innerHTML = [["texto", "Texto y stickers"], ["reaccion", "Reacciones"], ["todo", "Todo"]].map(([k, l]) => `<button data-k="${k}" style="${k === emoFuente ? "background:var(--accent);color:#fff" : ""}">${l}</button>`).join("");
  $$("#emoFuente button").forEach(b => b.onclick = () => { emoFuente = b.dataset.k; RENDER.emojis(); panelCmp("emojis"); });
  gridEmojis($("#emoGrid"), cnt, 36);
  nubeEmojis($("#nubeEmo"), cnt, 70);
  const fam = new Map(); EMO.forEach((e, i) => { if (!e.curado || !cnt[i]) return; const f = fam.get(e.fam) || { s: e.s, n: 0, l: [] }; f.n += cnt[i]; f.l.push([i, cnt[i]]); fam.set(e.fam, f); });
  $("#familias").innerHTML = [...fam.entries()].sort((a, b) => b[1].n - a[1].n).slice(0, 10).map(([k, f]) => `<div class="fam"><div class="nom"><b>${esc(k)}</b><span>${f.s >= 0 ? S_NOM[f.s] : ""} · ${fmt(f.n)} usos</span></div>
    <div class="imgs">${f.l.sort((a, b) => b[1] - a[1]).slice(0, 9).map(([i, n]) => `${imgEmo(EMO[i].e)}<em>${fmt(n)}</em>`).join("")}</div></div>`).join("") || `<div class="vacio">Sin datos</div>`;
  const tE = topK(R.emoT, 14).filter(t => EMO[t.i].s >= 0); const nodes = [], links = [], usados = new Set();
  tE.forEach(t => { const e = EMO[t.i], nm = e.e + " " + (e.nombre || "").slice(0, 28); nodes.push({ name: nm, itemStyle: { color: S_COL[e.s] } }); links.push({ source: nm, target: S_NOM[e.s], value: t.v }); usados.add(e.s); });
  usados.forEach(s => nodes.push({ name: S_NOM[s], itemStyle: { color: S_COL[s] } }));
  links.length ? ch("chSankey").setOption({ tooltip: tt(), series: [{ type: "sankey", left: 10, right: 110, top: 10, bottom: 10, nodeGap: 9, data: nodes, links, lineStyle: { color: "gradient", opacity: .35, curveness: .5 },
    label: { fontFamily: "Manrope", fontSize: 12, color: "#16131F" }, emphasis: { focus: "adjacency" } }] }, true) : vacio("chSankey");
  const tR = topK(R.emoR, 12);
  if (tR.length) { const it = [...tR].reverse(); ch("chReacciones").setOption({ grid: { left: 60, right: 50, top: 6, bottom: 6 }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, formatter: ps => { const e = EMO[it[ps[0].dataIndex].i]; return `<b>${e.e} ${esc(e.nombre)}</b><br>${esc(e.sig)}<br>${fmt(ps[0].value)} reacciones`; } }),
    xAxis: { type: "value" }, yAxis: { type: "category", data: it.map((t, k) => `{e${k}|}`), axisLabel: { interval: 0, rich: richEmo(it.map(t => EMO[t.i].e), 26) } },
    series: [{ type: "bar", data: it.map(t => ({ value: t.v, itemStyle: { color: EMO[t.i].s >= 0 ? S_COL[EMO[t.i].s] : "#94A3B8", borderRadius: [0, 8, 8, 0] } })), barMaxWidth: 18, label: { show: true, position: "right", fontFamily: "JetBrains Mono", fontSize: 11 } }] }, true); } else vacio("chReacciones");
  const ms = new Map(); for (let i = R.i0; i < R.i1; i++) { if (!C.e[i].length) continue; const k = bucket(C.d[i], "mes"); let m = ms.get(k); if (!m) { m = new Map(); ms.set(k, m); } for (const e of C.e[i]) if (EMO[e].s >= 0) m.set(e, (m.get(e) || 0) + 1); }
  const pts = [...ms.keys()].sort().map(k => { const best = [...ms.get(k).entries()].sort((a, b) => b[1] - a[1])[0]; return best ? { k, e: best[0], n: best[1] } : null; }).filter(Boolean);
  pts.length ? ch("chEmojiMes").setOption({ grid: { left: 40, right: 20, top: 30, bottom: 40 }, tooltip: tt({ formatter: p => { const o = pts[p.dataIndex]; if (!o) return ""; const e = EMO[o.e]; return `<b>${etiqB(o.k, "mes")}</b><br>${e.e} ${esc(e.nombre)} · ${o.n} usos<br>${esc(e.sig)}`; } }),
    xAxis: { type: "category", data: pts.map(o => etiqB(o.k, "mes")) }, yAxis: { type: "value", name: "usos" },
    series: [{ type: "line", data: pts.map(o => o.n), smooth: .4, symbol: "none", lineStyle: { color: css("--accent") + "40", width: 6 }, z: 0, tooltip: { show: false } },
      { type: "scatter", data: pts.map(o => ({ value: o.n, symbol: EMO[o.e].img ? "image://" + EMO[o.e].img : "circle", symbolSize: 30 })) }] }, true) : vacio("chEmojiMes");
  const opts = topK(emoTodo(R), 60);
  const sa = $("#emoA"), sb = $("#emoB"); const prevA = sa.value, prevB = sb.value;
  sa.innerHTML = sb.innerHTML = opts.map(t => `<option value="${t.i}">${EMO[t.i].e} ${esc(EMO[t.i].nombre || "")} (${fmt(t.v)})</option>`).join("");
  const def = (k, v) => opts.some(t => t.i == v) ? v : (opts[k] ? opts[k].i : "");
  sa.value = def(0, prevA || EMO_IDX.get("😂")); sb.value = def(1, prevB || EMO_IDX.get("🤣"));
  const comp = () => {
    const ia = +sa.value, ib = +sb.value; if (isNaN(ia) || isNaN(ib)) return;
    const ficha = i => { const e = EMO[i]; return `<div class="fam" style="align-items:flex-start">${imgEmo(e.e, "width:64px;height:64px")}<div><b style="font-family:var(--serif);font-style:italic;font-size:22px;font-weight:400">${esc(e.nombre)}</b><br>
      <span style="font-size:12.5px">${esc(e.sig)}</span><br><span style="font-size:11.5px;color:var(--ink2)">Familia <b>${esc(e.fam)}</b> · ${e.s >= 0 ? `<b style="color:${S_COL[e.s]}">${S_NOM[e.s]}</b>` : "neutral"} · texto ${fmt(R.emoT[i])} · reacciones ${fmt(R.emoR[i])}${e.esr != null ? ` · Emoji Sentiment Ranking ${e.esr}` : ""}</span>
      ${EMO[ia].fam === EMO[ib].fam && ia !== ib ? `<br><span class="tag" style="background:var(--accent)">Misma familia: expresan lo mismo en el chat</span>` : ""}</div></div>`; };
    $("#emoFichaA").innerHTML = ficha(ia); $("#emoFichaB").innerHTML = ficha(ib);
    const g = gran(S.a, S.b), { keys } = buckets(R, g); const sA = new Map(), sB = new Map();
    for (let i = R.i0; i < R.i1; i++) { const k = bucket(C.d[i], g); let a = 0, b = 0; for (const e of C.e[i]) { if (e === ia) a++; if (e === ib) b++; } const r = C.r[i]; for (let j = 0; j < r.length; j += 2) { if (r[j] === ia) a += r[j + 1]; if (r[j] === ib) b += r[j + 1]; } if (a) sA.set(k, (sA.get(k) || 0) + a); if (b) sB.set(k, (sB.get(k) || 0) + b); }
    ch("chEmoComp").setOption({ grid: { left: 40, right: 20, top: 34, bottom: 30 }, legend: { top: 0 }, tooltip: tt({ trigger: "axis" }), xAxis: { type: "category", data: keys.map(k => etiqB(k, g)) }, yAxis: { type: "value" },
      series: [{ name: EMO[ia].e + " " + EMO[ia].nombre, type: "line", smooth: .3, data: keys.map(k => sA.get(k) || 0), areaStyle: { opacity: .15 }, symbol: "none", lineStyle: { width: 2.4 } },
        { name: EMO[ib].e + " " + EMO[ib].nombre, type: "line", smooth: .3, data: keys.map(k => sB.get(k) || 0), areaStyle: { opacity: .15 }, symbol: "none", lineStyle: { width: 2.4 } }] }, true);
  };
  sa.onchange = sb.onchange = comp; comp();
};

/* ---------- 06 Lenguaje ---------- */
let palSent = 0, nubeSent = 8;
function palabrasDeSent(R, s) { const sub = { tok: new Float64Array(VT) }; for (let k = 0; k < VT; k++) sub.tok[k] = R.tokSent[k * 15 + s]; return logOdds(sub, R, 3).filter(x => x.z > 0); }
RENDER.lenguaje = () => {
  const R = RA(), col = css("--accent");
  const tw = topK(R.tok, 25, V.tok), tb = topK(R.big, 25, V.big), tg = topK(R.tri, 25, V.tri);
  tw.length ? barH("chPalabras", tw, col) : vacio("chPalabras"); tb.length ? barH("chBigramas", tb, "#14B8A6") : vacio("chBigramas"); tg.length ? barH("chTrigramas", tg, "#5EEAD4") : vacio("chTrigramas");
  nubePalabras("chNube", nubeTok(R, 140));
  const pal = ["#0D9488", "#0891B2", "#2563EB", "#7C3AED", "#DB2777", "#EA580C", "#65A30D"];
  const fr = topK(R.big, 60, V.big).map((x, k) => ({ name: x.l, value: x.v, textStyle: { color: pal[k % pal.length] } }))
    .concat(topK(R.tri, 40, V.tri).map((x, k) => ({ name: x.l, value: x.v * 1.6, real: x.v, textStyle: { color: pal[(k + 3) % pal.length] } })));
  nubePalabras("chNubeFrases", fr, { tam: [11, 40], grid: 8, forma: "square" });
  const ex = X.jerga.map((j, k) => ({ j, n: R.expr[k] })).filter(o => o.n > 0).sort((a, b) => b.n - a.n);
  $("#tablaExpr").innerHTML = `<table class="t"><tr><th>Expresión</th><th>Mensajes</th><th>Significado</th></tr>${ex.map(o => `<tr><td class="mono" style="font-size:13px"><b>${esc(o.j.x)}</b><br><span style="font:500 10px var(--sans);color:var(--ink3)">${esc(o.j.tipo)}</span></td><td class="mono">${fmt(o.n)}</td><td>${esc(o.j.sig)}${o.j.s >= 0 ? ` <span class="tag" style="background:${S_COL[o.j.s]}">${S_NOM[o.j.s]}</span>` : ""}</td></tr>`).join("")}</table>`;
  const pick = (id, sel, f) => { $(id).innerHTML = SENT.map((s, i) => `<span style="background:${s.color};cursor:pointer;${i === sel ? "outline:3px solid rgba(22,19,31,.35)" : ""}" data-i="${i}">${s.nombre}</span>`).join(""); $$(id + " span").forEach(sp => sp.onclick = () => f(+sp.dataset.i)); };
  pick("#pickSentNube", nubeSent, i => { nubeSent = i; RENDER.lenguaje(); });
  const ns = palabrasDeSent(R, nubeSent).slice(0, 80);
  nubePalabras("chNubeSent", ns.map(x => ({ name: V.tok[x.i], value: +x.z.toFixed(2), n: x.v, textStyle: { color: S_COL[nubeSent] } })), { tam: [12, 54], unidad: " (z)" });
  pick("#pickSentPal", palSent, i => { palSent = i; RENDER.lenguaje(); });
  const it = palabrasDeSent(R, palSent).slice(0, 30);
  it.length ? ch("chPalSent").setOption({ grid: { left: 40, right: 20, top: 30, bottom: 70 }, tooltip: tt({ trigger: "axis", formatter: ps2 => { const x = it[ps2[0].dataIndex]; return `<b>${V.tok[x.i]}</b><br>${fmt(x.v)} veces en mensajes de ${S_NOM[palSent]}<br>${pct(x.v / Math.max(1, R.tok[x.i]))} de todos sus usos<br>log-odds z = ${x.z.toFixed(2)}`; } }),
    xAxis: { type: "category", data: it.map(x => V.tok[x.i]), axisLabel: { rotate: 40, color: "#16131F", interval: 0 } }, yAxis: { type: "value", name: "log-odds z" },
    series: [{ type: "bar", data: it.map(x => +x.z.toFixed(2)), itemStyle: { color: S_COL[palSent], borderRadius: [8, 8, 0, 0] }, barMaxWidth: 24 }] }, true) : vacio("chPalSent");
};

/* ---------- 07 Temas ---------- */
const TOP_COL = ["#DB2777", "#F97316", "#EAB308", "#16A34A", "#0EA5E9", "#6366F1", "#A855F7", "#E11D48", "#14B8A6", "#64748B", "#F43F5E", "#84CC16"];
RENDER.temas = () => {
  const R = RA(), TP = X.modelos.topicos, TT = X.modelos.topicos_terminos, tot = sumA(R.top);
  $("#subTopicos").textContent = `LDA con k = ${X.modelos.k} (elegido por perplejidad y coherencia UMass). El nombre de cada tópico se asigna automáticamente comparando sus palabras con un léxico de temas universitarios.`;
  $("#topicosCards").innerHTML = TP.map((t, i) => { const ws = TT.filter(x => x.t === t.id); const mx = Math.max(...ws.map(w => w.beta));
    return `<div class="card c4" style="border-top:5px solid ${TOP_COL[i]}"><div style="font:500 11px var(--mono);color:${TOP_COL[i]}">TÓPICO ${i + 1} · ${tot ? pct(R.top[i] / tot) : "0%"} de los mensajes del periodo</div>
    <h3>${esc(t.nombre)}</h3><div style="display:flex;flex-wrap:wrap;gap:5px;margin-top:8px">${ws.map(w => `<span class="tag" style="background:${TOP_COL[i]};opacity:${(.45 + .55 * w.beta / mx).toFixed(2)};font-size:${(11 + 5 * w.beta / mx).toFixed(1)}px">${esc(w.termino)}</span>`).join("")}</div></div>`; }).join("");
  ch("chTopDist").setOption({ tooltip: tt({ formatter: p => `${p.name}<br><b>${fmt(p.value)}</b> mensajes (${p.percent}%)` }), legend: { bottom: 0, type: "scroll" },
    series: [{ type: "pie", radius: ["38%", "70%"], center: ["50%", "45%"], itemStyle: { borderColor: "#fff", borderWidth: 3, borderRadius: 10 }, label: { formatter: "{d}%", fontFamily: "Fraunces", fontStyle: "italic", fontWeight: 600 },
      data: TP.map((t, i) => ({ name: t.nombre, value: R.top[i], itemStyle: { color: TOP_COL[i] } })) }] }, true);
  const g = gran(S.a, S.b), { keys } = buckets(R, g); const m = new Map(keys.map(k => [k, new Float64Array(NT)]));
  for (let i = R.i0; i < R.i1; i++) if (C.tp[i] >= 0) { const o = m.get(bucket(C.d[i], g)); if (o) o[C.tp[i]]++; }
  ch("chTopTiempo").setOption({ grid: { left: 40, right: 16, top: 40, bottom: 30 }, legend: { top: 0, type: "scroll" }, tooltip: tt({ trigger: "axis", valueFormatter: v => v == null ? "-" : v.toFixed(1) + "%" }),
    xAxis: { type: "category", data: keys.map(k => etiqB(k, g)), boundaryGap: false }, yAxis: { type: "value", max: 100, axisLabel: { formatter: "{value}%" } },
    series: TP.map((t, i) => ({ name: t.nombre, type: "line", stack: "t", smooth: .3, symbol: "none", lineStyle: { width: 0 }, itemStyle: { color: TOP_COL[i] }, areaStyle: { color: TOP_COL[i] }, emphasis: { focus: "series" },
      data: keys.map(k => { const o = m.get(k), s = sumA(o); return s >= 3 ? +(o[i] / s * 100).toFixed(2) : null; }) })) }, true);
  const hm = []; TP.forEach((t, i) => { let s = 0; for (let j = 0; j < 15; j++) s += R.topSent[i * 15 + j]; for (let j = 0; j < 15; j++) hm.push([j, i, s ? +(R.topSent[i * 15 + j] / s * 100).toFixed(1) : 0]); });
  ch("chTopSent").setOption({ grid: { left: 10, right: 20, top: 10, bottom: 60, containLabel: true }, tooltip: tt({ formatter: p => `${TP[p.data[1]].nombre}<br>${S_NOM[p.data[0]]}: <b>${p.data[2]}%</b>` }),
    xAxis: { type: "category", data: S_NOM, axisLabel: { rotate: 35, interval: 0 } }, yAxis: { type: "category", data: TP.map(t => t.nombre), inverse: true },
    visualMap: { min: 0, max: 50, show: false, inRange: { color: ["#FFF7FB", "#FBCFE8", "#DB2777", "#831843"] } },
    series: [{ type: "heatmap", data: hm, label: { show: true, formatter: p => p.data[2] >= 2 ? p.data[2] + "%" : "", fontFamily: "JetBrains Mono", fontSize: 10.5 }, itemStyle: { borderColor: "#fff", borderWidth: 2, borderRadius: 4 } }] }, true);
};

/* ---------- 08 Comunidad: ranking, perfil de miembro y red ---------- */
let autSel = null;
const MIEMBRO = new Map((X.modelos.miembros || []).map(m => [m.a, m]));
function perfilMiembro(a, R) {
  const o = { n: 0, dias: new Set(), d0: null, d1: null, h: new Float64Array(24), s: new Float64Array(15), tok: new Map(), emo: new Map(), q: 0, qr: 0, nr: 0, idx: [] };
  for (let i = R.i0; i < R.i1; i++) { if (C.a[i] !== a) continue;
    o.n++; o.dias.add(C.d[i]); if (o.d0 == null) o.d0 = C.d[i]; o.d1 = C.d[i]; o.h[C.h[i]]++; if (C.s[i] >= 0) o.s[C.s[i]]++;
    for (const k of C.t[i]) o.tok.set(k, (o.tok.get(k) || 0) + 1); for (const k of C.e[i]) o.emo.set(k, (o.emo.get(k) || 0) + 1);
    if (C.q[i] === 1) { o.q++; if (C.ans[i] === 1) o.qr++; } o.nr += C.nr[i]; if (C.txt[i] && C.txt[i].length > 6) o.idx.push(i); }
  return o;
}
RENDER.comunidad = () => {
  const R = RA(), ta = topK(R.aut, 25);
  if (autSel == null || !R.aut[autSel]) autSel = ta[0]?.i ?? 0;
  const c = ch("chAutores"); const it = [...ta].reverse();
  c.setOption({ grid: { left: 10, right: 50, top: 6, bottom: 6, containLabel: true }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, formatter: ps => `<b>${ps[0].name}</b><br>${fmt(ps[0].value)} mensajes en el periodo<br>Clic para abrir su perfil` }), xAxis: { type: "value" },
    yAxis: { type: "category", data: it.map(t => V.aut[t.i]), axisLabel: { fontFamily: "JetBrains Mono", fontSize: 11, interval: 0 } },
    series: [{ type: "bar", data: it.map(t => { const s = R.autSent.get(t.i); const mx = s ? Math.max(...s) : 0; const d = mx > 0 ? s.indexOf(mx) : -1; return { value: t.v, itemStyle: { color: t.i === autSel ? "#16131F" : (d >= 0 ? S_COL[d] : "#94A3B8"), borderRadius: [0, 8, 8, 0] } }; }), barMaxWidth: 16,
      label: { show: true, position: "right", fontFamily: "JetBrains Mono", fontSize: 11 } }] }, true);
  c.off("click"); c.on("click", p => { autSel = it[p.dataIndex].i; RENDER.comunidad(); $("#cardPerfil").scrollIntoView({ behavior: "smooth", block: "center" }); });
  // selector con todos los miembros activos del periodo
  const todos = topK(R.aut, 400);
  $("#selMiembro").innerHTML = todos.map(t => `<option value="${t.i}" ${t.i === autSel ? "selected" : ""}>${esc(V.aut[t.i])} (${fmt(t.v)})</option>`).join("");
  $("#selMiembro").onchange = e => { autSel = +e.target.value; RENDER.comunidad(); };
  const pf = perfilMiembro(autSel, R), net = MIEMBRO.get(V.aut[autSel]) || {}, tot = sumA(pf.s);
  const d = tot ? pf.s.indexOf(Math.max(...pf.s)) : -1, hmax = pf.h.indexOf(Math.max(...pf.h));
  $("#subPerfil").textContent = ` · ${S.lab}`;
  $("#perfilCab").innerHTML = `<div class="perfil-cab"><span class="alias">${esc(V.aut[autSel])}</span>${d >= 0 ? `<span class="tag" style="background:${S_COL[d]}">Sentimiento más frecuente: ${S_NOM[d]} (${pct(pf.s[d] / tot)})</span>` : ""}${net.c ? `<span class="tag" style="background:#2563EB">Comunidad ${net.c} (Louvain)</span>` : ""}</div>
    <div class="perfil-kpis">${[[fmt(pf.n), "mensajes en el periodo"], [fmt(V.aut_n[autSel]), "mensajes en total"], [fmt(pf.dias.size), "días con mensajes"],
      [pf.d0 != null ? fcorta(pf.d0) : "—", "primer mensaje del periodo"], [pf.d1 != null ? fcorta(pf.d1) : "—", "último mensaje del periodo"], [`${hmax}h`, "hora en que más escribe"],
      [fmt(pf.q), `preguntas (${pct(pf.qr / Math.max(1, pf.q), 0)} respondidas)`], [fmt(pf.nr), "reacciones recibidas"], [fmt(net.r || 0), "respuestas dadas a otros (total)"], [fmt(net.rc || 0), "respuestas recibidas (total)"]]
      .map(([v, l]) => `<div><b>${v}</b><span>${l}</span></div>`).join("")}</div>`;
  const rmax = Math.min(100, Math.ceil(1.15 * Math.max(...SENT.map((_, i) => Math.max(R.ns ? R.sent[i] / R.ns * 100 : 0, tot ? pf.s[i] / tot * 100 : 0))) / 5) * 5 || 10);
  ch("chPerfil").setOption({ legend: { bottom: 0, textStyle: { fontSize: 10 } }, tooltip: tt(), radar: { radius: "60%", center: ["50%", "46%"], indicator: SENT.map(s => ({ name: s.nombre, max: rmax })), axisName: { color: "#4A4760", fontSize: 9.5 }, splitArea: { areaStyle: { color: ["rgba(255,255,255,.3)", "rgba(255,255,255,.08)"] } } },
    series: [{ type: "radar", data: [{ name: "Todo el grupo", value: SENT.map((_, i) => R.ns ? +(R.sent[i] / R.ns * 100).toFixed(1) : 0), areaStyle: { color: "#94A3B844" }, lineStyle: { color: "#94A3B8" }, itemStyle: { color: "#94A3B8" } },
      { name: V.aut[autSel], value: SENT.map((_, i) => tot ? +(pf.s[i] / tot * 100).toFixed(1) : 0), areaStyle: { color: css("--accent") + "44" }, lineStyle: { color: css("--accent"), width: 2.4 }, itemStyle: { color: css("--accent") } }] }] }, true);
  ch("chPerfilHoras").setOption({ grid: { left: 34, right: 8, top: 24, bottom: 24 }, title: { text: "Mensajes por hora", left: "center", top: 0, textStyle: { fontSize: 13, fontFamily: "Manrope", fontStyle: "normal", color: "#7C7993" } },
    tooltip: tt({ trigger: "axis" }), xAxis: { type: "category", data: [...Array(24).keys()].map(h => h + "h"), axisLabel: { fontSize: 9 } }, yAxis: { type: "value", axisLabel: { fontSize: 9 } },
    series: [{ type: "bar", data: [...pf.h], itemStyle: { color: css("--accent"), borderRadius: [4, 4, 0, 0] } }] }, true);
  const pw = [...pf.tok.entries()].sort((a, b) => b[1] - a[1]).slice(0, 15);
  $("#subPerfilPal").textContent = `Palabras con contenido y emojis más usados por ${V.aut[autSel]} en ${S.lab}.`;
  pw.length ? barH("chPerfilPal", pw.map(([k, v]) => ({ l: V.tok[k], v })), css("--accent")) : vacio("chPerfilPal");
  const pe = [...pf.emo.entries()].sort((a, b) => b[1] - a[1]).slice(0, 14);
  $("#perfilEmo").innerHTML = pe.length ? pe.map(([k, v]) => `<span>${imgEmo(EMO[k].e)}${fmt(v)}</span>`).join("") : `<span>Sin emojis en el periodo</span>`;
  $("#perfilMsgs").innerHTML = msgsHtml(pf.idx.slice().sort(() => Math.random() - .5), 20);
  const tasa = [...Array(24).keys()].map(h => R.qh[h] >= 5 ? +(R.qa[h] / R.qh[h] * 100).toFixed(1) : null), med = [...Array(24).keys()].map(h => R.qt[h].length >= 3 ? +median(R.qt[h]).toFixed(1) : null);
  const minQ = Math.max(15, .03 * R.preguntas); let best = -1; tasa.forEach((v, h) => { if (v != null && R.qh[h] >= minQ && (best < 0 || v > tasa[best])) best = h; });
  ch("chMejorHora").setOption({ grid: { left: 46, right: 56, top: 40, bottom: 30 }, legend: { top: 0 }, tooltip: tt({ trigger: "axis", formatter: ps => `<b>${ps[0].name}</b><br>${R.qh[ps[0].dataIndex]} preguntas<br>` + ps.map(p => `${p.seriesName}: ${p.value ?? "-"}`).join("<br>") }),
    xAxis: { type: "category", data: [...Array(24).keys()].map(h => h + "h") }, yAxis: [{ type: "value", name: "% respondidas", max: 100 }, { type: "value", name: "minutos", splitLine: { show: false } }],
    series: [{ name: "Respondidas (%)", type: "bar", data: tasa.map((v, h) => ({ value: v, itemStyle: { color: h === best ? "#16131F" : css("--accent"), borderRadius: [6, 6, 0, 0] } })), barMaxWidth: 20,
      markPoint: best >= 0 ? { data: [{ coord: [best, tasa[best]], value: "mejor", symbolSize: 46, itemStyle: { color: "#16131F" } }], label: { fontSize: 10, color: "#fff" } } : undefined },
      { name: "Mediana de minutos hasta responder", type: "line", yAxisIndex: 1, data: med, smooth: .3, symbol: "circle", symbolSize: 5, connectNulls: true, lineStyle: { color: "#F59E0B", width: 2 }, itemStyle: { color: "#F59E0B" } }] }, true);
  const arr = [...R.aut].filter(v => v > 0).sort((a, b) => b - a), tt2 = sumA(arr), sh = p => tt2 ? arr.slice(0, Math.max(1, Math.ceil(arr.length * p))).reduce((a, b) => a + b, 0) / tt2 : 0;
  barH("chConcentra", [{ l: "1% más activo", v: +(sh(.01) * 100).toFixed(1) }, { l: "5% más activo", v: +(sh(.05) * 100).toFixed(1) }, { l: "10% más activo", v: +(sh(.1) * 100).toFixed(1) }, { l: "25% más activo", v: +(sh(.25) * 100).toFixed(1) }],
    css("--accent"), { lab: p => p.value + "%" });
  if (!RENDER.comunidad.red) { RENDER.comunidad.red = true; const red = X.modelos.red; const comms = [...new Set(red.nodos.map(n => n.comunidad))];
    const cr = ch("chRed");
    cr.setOption({ tooltip: tt({ formatter: p => p.dataType === "node" ? `<b>${p.data.name}</b><br>respondió ${p.data.r} veces · recibió ${p.data.rc}<br>${p.data.m} mensajes<br>Clic para abrir su perfil` : `${p.data.source} → ${p.data.target}: ${p.data.value}` }),
      series: [{ type: "graph", layout: "force", roam: true, draggable: true, force: { repulsion: 260, edgeLength: [40, 160], gravity: .06 },
        categories: comms.map(c => ({ name: "C" + c })), data: red.nodos.map(n => ({ name: n.id, value: n.respondio, r: n.respondio, rc: n.recibio, m: n.mensajes, category: comms.indexOf(n.comunidad), symbolSize: 6 + Math.sqrt(n.respondio) * 2.2,
          label: { show: n.respondio > 60, formatter: n.id.replace("Miembro ", "M"), fontFamily: "JetBrains Mono", fontSize: 10 } })),
        links: red.aristas.map(a => ({ source: a.de, target: a.a, value: a.peso, lineStyle: { width: Math.min(6, .5 + Math.sqrt(a.peso)), opacity: .16, curveness: .15 } })),
        emphasis: { focus: "adjacency", lineStyle: { opacity: .8 } } }] }, true);
    cr.on("click", p => { if (p.dataType !== "node") return; const k = V.aut.indexOf(p.data.name); if (k >= 0) { autSel = k; RENDER.comunidad(); $("#cardPerfil").scrollIntoView({ behavior: "smooth", block: "center" }); } });
  }
};

/* ---------- 09 Docentes y campus ---------- */
const ENT = V.ent.map(k => { const [t, ...e] = k.split("|"); return { t, e: e.join("|") }; });
let docSel = null;
const indiceDoc = (R, k) => (R.entPol[k * 3 + 2] + 1) / (R.entPol[k * 3 + 2] + R.entPol[k * 3] + 2);
RENDER.docentes = () => {
  const R = RA(), mn = +$("#minDoc").value;
  const docs = []; ENT.forEach((o, k) => { if (o.t !== "docente" || R.ent[k] < mn) return; docs.push({ k, n: R.ent[k], pos: R.entPol[k * 3 + 2], neg: R.entPol[k * 3], ind: indiceDoc(R, k) }); });
  docs.sort((a, b) => b.ind - a.ind || b.n - a.n);
  const c = ch("chDocentes");
  if (docs.length) { const it = [...docs].reverse().slice(-40); c.setOption({ grid: { left: 10, right: 90, top: 6, bottom: 20, containLabel: true },
    tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, formatter: ps => { const d = it[ps[0].dataIndex]; return `<b>${esc(ENT[d.k].e)}</b><br>índice ${pct(d.ind)}<br>${d.n} menciones: ${d.pos} positivas, ${d.neg} negativas`; } }),
    xAxis: { type: "value", min: 0, max: 1, axisLabel: { formatter: v => Math.round(v * 100) + "%" } }, yAxis: { type: "category", data: it.map(d => ENT[d.k].e), axisLabel: { interval: 0, fontSize: 11 } },
    visualMap: { show: false, dimension: 0, min: 0.2, max: 0.8, inRange: { color: [POL_COL.neg, "#A3A3A3", POL_COL.pos] } },
    series: [{ type: "bar", data: it.map(d => +d.ind.toFixed(3)), barMaxWidth: 14, itemStyle: { borderRadius: [0, 8, 8, 0] }, label: { show: true, position: "right", formatter: p => `${pct(p.value, 0)} · ${it[p.dataIndex].n}`, fontFamily: "JetBrains Mono", fontSize: 10.5 } },
      { type: "line", data: it.map(() => .5), symbol: "none", silent: true, lineStyle: { type: "dashed", color: "#7C7993", width: 1 }, tooltip: { show: false } }] }, true);
    c.off("click"); c.on("click", p => { if (p.seriesIndex) return; docSel = it[p.dataIndex].k; RENDER.docentes(); }); } else vacio("chDocentes", "Pocas menciones en este periodo");
  const todosD = ENT.map((o, k) => ({ o, k })).filter(x => x.o.t === "docente" && R.ent[x.k] > 0).sort((a, b) => R.ent[b.k] - R.ent[a.k]);
  if (docSel == null || !R.ent[docSel]) docSel = todosD[0]?.k ?? null;
  $("#selDoc").innerHTML = todosD.map(x => `<option value="${x.k}" ${x.k === docSel ? "selected" : ""}>${esc(x.o.e)} (${R.ent[x.k]})</option>`).join("");
  $("#selDoc").onchange = e => { docSel = +e.target.value; RENDER.docentes(); };
  if (docSel != null) { const it = [...Array(15).keys()].map(i => ({ l: S_NOM[i], v: R.entSent[docSel * 15 + i], c: S_COL[i] })).filter(x => x.v > 0).sort((a, b) => b.v - a.v);
    it.length ? barH("chDocSent", it, null) : vacio("chDocSent", "Menciones sin sentimiento");
    $("#msgsDoc").innerHTML = msgsHtml(muestra(i => C.n[i].includes(docSel), S.a, S.b, 20), 20); } else { vacio("chDocSent"); $("#msgsDoc").innerHTML = ""; }
  const mats = ENT.map((o, k) => ({ o, k })).filter(x => (x.o.t === "materia" || x.o.t === "codigo_materia") && R.ent[x.k] > 0).sort((a, b) => R.ent[b.k] - R.ent[a.k]).slice(0, 20).reverse();
  mats.length ? ch("chMaterias").setOption({ grid: { left: 10, right: 20, top: 30, bottom: 6, containLabel: true }, legend: { top: 0 }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" } }), xAxis: { type: "value" },
    yAxis: { type: "category", data: mats.map(x => x.o.e), axisLabel: { interval: 0, fontSize: 11 } }, series: [["Negativo", 0, POL_COL.neg], ["Neutro", 1, POL_COL.neu], ["Positivo", 2, POL_COL.pos]].map(([n, j, c]) => ({ name: n, type: "bar", stack: "p", barMaxWidth: 14, itemStyle: { color: c }, data: mats.map(x => R.entPol[x.k * 3 + j]) })) }, true) : vacio("chMaterias");
  const lug = ENT.map((o, k) => ({ o, k })).filter(x => x.o.t === "lugar" && R.ent[x.k] >= 3);
  lug.length ? ch("chLugares").setOption({ grid: { left: 50, right: 30, top: 20, bottom: 40 }, tooltip: tt({ formatter: p => `<b>${p.data[3]}</b><br>${p.data[0]} menciones<br>${p.data[1]}% negativas · ${p.data[2]}% positivas` }),
    xAxis: { type: "log", name: "menciones", nameLocation: "middle", nameGap: 26 }, yAxis: { type: "value", name: "% negativas" },
    series: [{ type: "scatter", data: lug.map(x => { const n = R.ent[x.k]; return [n, +(R.entPol[x.k * 3] / n * 100).toFixed(1), +(R.entPol[x.k * 3 + 2] / n * 100).toFixed(1), x.o.e]; }),
      symbolSize: d => 10 + Math.sqrt(d[0]) * 2.2, itemStyle: { color: p => p.data[1] > p.data[2] ? POL_COL.neg + "cc" : POL_COL.pos + "cc", borderColor: "#fff", borderWidth: 2 },
      label: { show: true, formatter: p => p.data[3], position: "top", fontFamily: "Instrument Serif", fontStyle: "italic", fontSize: 15, color: "#16131F" }, labelLayout: { hideOverlap: true } }] }, true) : vacio("chLugares");
  const hor = ENT.map((o, k) => ({ o, k })).filter(x => x.o.t === "horario");
  const ord = ["Mañana (7-12)", "Mediodía (12-14)", "Tarde (14-18)", "Noche (18-22)"]; hor.sort((a, b) => ord.indexOf(a.o.e) - ord.indexOf(b.o.e));
  ch("chHorarios").setOption({ grid: { left: 40, right: 20, top: 20, bottom: 30 }, tooltip: tt({ trigger: "axis" }), xAxis: { type: "category", data: hor.map(x => x.o.e), axisLabel: { interval: 0 } }, yAxis: { type: "value" },
    series: [{ type: "bar", data: hor.map((x, j) => ({ value: R.ent[x.k], itemStyle: { color: ["#FDBA74", "#FB923C", "#F43F5E", "#7C3AED"][j % 4], borderRadius: [10, 10, 0, 0] } })), barMaxWidth: 60, label: { show: true, position: "top", fontFamily: "JetBrains Mono" } }] }, true);
  const tr = ENT.map((o, k) => ({ o, k })).filter(x => (x.o.t === "tramite" || x.o.t === "plataforma") && R.ent[x.k] > 0).sort((a, b) => R.ent[b.k] - R.ent[a.k]).slice(0, 16);
  tr.length ? barH("chTramites", tr.map(x => ({ l: x.o.e, v: R.ent[x.k], c: x.o.t === "tramite" ? "#E11D48" : "#F59E0B" })), null) : vacio("chTramites");
  if (!RENDER.docentes.red) { RENDER.docentes.red = true; const rd = X.modelos.red_dm; const ns = new Map();
    rd.forEach(r => { ns.set(r.docente, (ns.get(r.docente) || 0) + r.N); ns.set("§" + r.materia, (ns.get("§" + r.materia) || 0) + r.N); });
    ch("chRedDM").setOption({ tooltip: tt(), series: [{ type: "graph", layout: "force", roam: true, draggable: true, zoom: .85, force: { repulsion: 220, edgeLength: [50, 150], gravity: .1 }, labelLayout: { hideOverlap: true }, categories: [{ name: "Docente" }, { name: "Materia" }],
      data: [...ns.entries()].map(([k, v]) => ({ name: k.replace("§", ""), value: v, category: k.startsWith("§") ? 1 : 0, symbolSize: 8 + Math.sqrt(v) * 3, itemStyle: { color: k.startsWith("§") ? "#0EA5E9" : "#E11D48" },
        label: { show: true, position: "right", color: "#16131F", fontFamily: k.startsWith("§") ? "Manrope" : "Instrument Serif", fontStyle: k.startsWith("§") ? "normal" : "italic", fontWeight: k.startsWith("§") ? 800 : 400, fontSize: k.startsWith("§") ? 11 : 14 } })),
      links: rd.map(r => ({ source: r.docente, target: r.materia, value: r.N, lineStyle: { width: Math.min(7, Math.sqrt(r.N)), opacity: .35, color: "#E11D48" } })) }] }, true); }
};

/* ---------- 10 Causas y predicción ---------- */
const DIC_TOK = new Set(); X.dic_sent.forEach(d => d.t.split(" ").forEach(w => { const k = V.tok.indexOf(w); if (k >= 0) DIC_TOK.add(k); }));
function incertidumbre(R) { const inc = { tok: new Float64Array(VT) }; for (let i = R.i0; i < R.i1; i++) if (C.s[i] === 8 || C.s[i] === 9) for (const k of C.t[i]) inc.tok[k]++; return logOdds(inc, R, 3, DIC_TOK); }
RENDER.causas = () => {
  const R = RA(); const lo = incertidumbre(R).slice(0, 16);
  lo.length ? barH("chCausas", lo.map(x => ({ l: V.tok[x.i], v: +x.z.toFixed(2) })), "#6E7FA8", { lab: p => "z " + p.value }) : vacio("chCausas", "Pocos mensajes de incertidumbre en este periodo");
  const sel = $("#selRegla"); if (!sel.options.length) { sel.innerHTML = `<option value="">Todos los sentimientos</option>` + SENT.map(s => `<option value="${s.id}">${s.nombre}</option>`).join(""); sel.onchange = () => RENDER.causas(); }
  const rg = X.modelos.reglas.filter(r => !sel.value || r.rhs === sel.value).slice(0, 80);
  $("#tablaReglas").innerHTML = `<table class="t"><tr><th>Si aparece...</th><th>⇒ sentimiento</th><th>Lift</th><th>Confianza</th><th>Mensajes</th></tr>${rg.map(r => { const s = SENT.findIndex(x => x.id === r.rhs);
    return `<tr><td>{${esc(r.lhs)}}</td><td><span class="tag" style="background:${S_COL[s]}">${S_NOM[s]}</span></td><td class="mono">${r.lift}</td><td class="mono">${pct(r.confianza, 0)}</td><td class="mono">${r.n}</td></tr>`; }).join("")}</table>`;
  if (RENDER.causas.ok) return; RENDER.causas.ok = true;
  const Mo = X.modelos;
  $("#subCcf").innerHTML = `Correlación cruzada diaria entre el % de incertidumbre (confusión + preocupación) y el % de malestar (frustración + enojo). Prueba de Granger con 2 rezagos: <b>p = ${Mo.granger}</b>${Mo.granger < .05 ? " (la incertidumbre pasada ayuda a predecir el malestar)." : " (sin evidencia de que la incertidumbre anticipe el malestar)."}`;
  ch("chCcf").setOption({ grid: { left: 40, right: 10, top: 20, bottom: 40 }, tooltip: tt({ trigger: "axis" }), xAxis: { type: "category", data: Mo.ccf.map(c => c.lag), name: "rezago (días)", nameLocation: "middle", nameGap: 26 }, yAxis: { type: "value" },
    series: [{ type: "bar", data: Mo.ccf.map(c => ({ value: c.r, itemStyle: { color: c.r > 0 ? "#65A30D" : "#E11D48", borderRadius: 6 } })), barMaxWidth: 22 }] }, true);
  const ef = Mo.efecto; const sents = SENT.map(s => s.id);
  ch("chEfecto").setOption({ grid: { left: 10, right: 20, top: 30, bottom: 6, containLabel: true }, legend: { top: 0 }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, valueFormatter: v => v + "%" }),
    xAxis: { type: "value", axisLabel: { formatter: "{value}%" } }, yAxis: { type: "category", data: S_NOM, inverse: true, axisLabel: { interval: 0 } },
    series: [[true, "Respuestas a incertidumbre", "#6E7FA8"], [false, "Respuestas al resto", "#BEF264"]].map(([v, n, c]) => ({ name: n, type: "bar", barMaxWidth: 10, itemStyle: { color: c, borderRadius: [0, 6, 6, 0] },
      data: sents.map(s => { const o = ef.find(e => e.inc === v && e.sentimiento === s); return o ? +(o.p * 100).toFixed(1) : 0; }) })) }, true);
  const hist = Mo.semanal, pr = Mo.pronostico;
  $("#subPron").innerHTML = `Modelo <b>${esc(Mo.pron_mejor)}</b> (el de menor error en 26 semanas de prueba). Bandas de 80% y 95%. Próximo pico esperado: semana del <b>${pr.reduce((a, b) => b.media > a.media ? b : a).s.split("-").reverse().join("/")}</b>.`;
  const xs = hist.map(h => h.s).concat(pr.map(p => p.s));
  ch("chPronostico").setOption({ grid: { left: 46, right: 20, top: 30, bottom: 60 }, legend: { top: 0, data: ["Histórico", "Banda 95%", "Banda 80%", "Pronóstico"] }, tooltip: tt({ trigger: "axis" }), dataZoom: [{ type: "slider", bottom: 10, height: 22, start: 55 }, { type: "inside", zoomOnMouseWheel: false }],
    xAxis: { type: "category", data: xs.map(s => s.slice(8, 10) + "/" + s.slice(5, 7) + "/" + s.slice(2, 4)), boundaryGap: false }, yAxis: { type: "value", name: "mensajes/semana" },
    series: [{ name: "Histórico", type: "line", data: hist.map(h => h.n).concat(pr.map(() => null)), symbol: "none", lineStyle: { color: "#4A4760", width: 1.4 }, itemStyle: { color: "#4A4760" } },
      { name: "ic95_base", type: "line", stack: "b95", data: hist.map(() => null).concat(pr.map(p => p.li95)), symbol: "none", lineStyle: { opacity: 0 }, tooltip: { show: false } },
      { name: "Banda 95%", type: "line", stack: "b95", data: hist.map(() => null).concat(pr.map(p => +(p.ls95 - p.li95).toFixed(1))), symbol: "none", lineStyle: { opacity: 0 }, itemStyle: { color: "#65A30D55" }, areaStyle: { color: "#65A30D22" } },
      { name: "ic80_base", type: "line", stack: "b80", data: hist.map(() => null).concat(pr.map(p => p.li80)), symbol: "none", lineStyle: { opacity: 0 }, tooltip: { show: false } },
      { name: "Banda 80%", type: "line", stack: "b80", data: hist.map(() => null).concat(pr.map(p => +(p.ls80 - p.li80).toFixed(1))), symbol: "none", lineStyle: { opacity: 0 }, itemStyle: { color: "#65A30D99" }, areaStyle: { color: "#65A30D44" } },
      { name: "Pronóstico", type: "line", data: hist.map(() => null).concat(pr.map(p => p.media)), symbol: "none", lineStyle: { color: "#3F6212", width: 2.6 }, itemStyle: { color: "#3F6212" } }] }, true);
  barH("chModelos", Mo.pron_modelos.map(m => ({ l: m.modelo, v: m.MAE, c: m.modelo === Mo.pron_mejor ? "#3F6212" : "#BEF264" })).sort((a, b) => a.v - b.v), null, { lab: p => "MAE " + p.value });
  barH("chCv", Mo.cv.map(m => ({ l: m.modelo, v: m.f1, c: m.modelo.startsWith("Línea") ? "#CBD5E1" : "#84CC16" })).sort((a, b) => b.v - a.v), null, { lab: p => "F1 " + p.value.toFixed(2) });
  const nv = Mo.nuevas.slice(0, 12).concat(Mo.nuevas.slice(-12));
  ch("chNuevas").setOption({ grid: { left: 10, right: 20, top: 6, bottom: 6, containLabel: true }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" } }), xAxis: { type: "value" }, yAxis: { type: "category", data: nv.map(n => n.palabra), axisLabel: { interval: 0, fontSize: 11 } },
    series: [{ type: "bar", data: nv.map(n => ({ value: n.coef, itemStyle: { color: n.coef > 0 ? POL_COL.pos : POL_COL.neg, borderRadius: 6 } })), barMaxWidth: 12 }] }, true);
  const LAB = { es_preguntaTRUE: "Es una pregunta", franjaMadrugada: "Madrugada", "franjaMañana": "Mañana", "franjaMediodía": "Mediodía", franjaNoche: "Noche (18-21h)", "franjaNoche tarde": "Noche tarde (22-23h)", "largo6-12": "6-12 palabras",
    "largo13-25": "13-25 palabras", "largo26+": "26+ palabras", findeTRUE: "Fin de semana", con_emojiTRUE: "Lleva emoji", polaridadnegativo: "Tono negativo", polaridadpositivo: "Tono positivo", temporadaTRUE: "En temporada" };
  const od = [...Mo.odds].sort((a, b) => a.or - b.or);
  $("#subOdds").innerHTML = `Regresión logística (referencia: tarde, 1-5 palabras, tono neutro). AUC en validación cruzada = <b>${Mo.auc}</b>. Un odds ratio mayor que 1 aumenta la probabilidad de recibir respuesta.`;
  ch("chOdds").setOption({ grid: { left: 10, right: 30, top: 10, bottom: 30, containLabel: true }, tooltip: tt({ formatter: p => { const o = od[p.dataIndex]; return o ? `<b>${LAB[o.var] || o.var}</b><br>OR ${o.or} (IC95% ${o.li}–${o.ls})<br>p = ${o.p}` : ""; } }),
    xAxis: { type: "log", min: .5, max: 4 }, yAxis: { type: "category", data: od.map(o => LAB[o.var] || o.var), axisLabel: { interval: 0 } },
    series: [{ type: "custom", renderItem: (pa, api) => { const y = api.coord([1, api.value(0)])[1], x0 = api.coord([api.value(1), 0])[0], x1 = api.coord([api.value(2), 0])[0]; return { type: "line", shape: { x1: x0, y1: y, x2: x1, y2: y }, style: { stroke: "#4D7C0F88", lineWidth: 4 } }; },
      data: od.map((o, i) => [i, o.li, o.ls]), encode: { y: 0 }, tooltip: { show: false } },
      { type: "scatter", data: od.map((o, i) => [o.or, i]), symbolSize: 13, itemStyle: { color: p => od[p.dataIndex].or > 1 ? "#3F6212" : "#E11D48" } },
      { type: "line", data: od.map((o, i) => [1, i]), symbol: "none", silent: true, lineStyle: { type: "dashed", color: "#7C7993", width: 1 }, tooltip: { show: false } }] }, true);
};

/* =====================================================================
   PANEL DE COMPARACIÓN A / B (pestañas 03 a 10)
   ===================================================================== */
function cabCmp(t) { return `<h3>Periodo A frente a Periodo B</h3><div class="cab-cmp"><span class="pa">A · ${esc(S.lab)}</span><span class="pb">B · ${esc(S.A2.lab)}</span><span class="sub" style="margin:0">${esc(t)}</span></div>`; }
function filaCmp(nombre, va, vb, tipo = "n") {
  const f = v => tipo === "p" ? pct(v) : tipo === "i" ? (+v).toFixed(2) : tipo === "t" ? v : fmt(Math.round(v * 10) / 10);
  let dl = "", cl = "";
  if (tipo === "n" && vb) { const d = (va - vb) / vb; dl = (d >= 0 ? "+" : "") + pct(d); cl = d >= 0 ? "d-pos" : "d-neg"; }
  else if (tipo === "p") { const d = (va - vb) * 100; dl = (d >= 0 ? "+" : "") + d.toFixed(1) + " pp"; cl = d >= 0 ? "d-pos" : "d-neg"; }
  else if (tipo === "i") { const d = va - vb; dl = (d >= 0 ? "+" : "") + d.toFixed(2); cl = d >= 0 ? "d-pos" : "d-neg"; }
  return `<tr><td>${nombre}</td><td class="mono">${f(va)}</td><td class="mono">${f(vb)}</td><td class="mono ${cl}">${dl || "—"}</td></tr>`;
}
const tablaCmp = filas => `<div class="tabla-wrap" style="max-height:none"><table class="t tabla-cmp"><tr><th>Indicador</th><th>Periodo A</th><th>Periodo B</th><th>Diferencia (A − B)</th></tr>${filas.join("")}</table></div>`;
const listaCmp = (tit, items, color) => `<div class="lista-cmp"><h5 style="color:${color}">${tit}</h5><ol>${items.join("") || "<li>Sin datos</li>"}</ol></div>`;
function panelCmp(tab) {
  const el = $("#cmp-" + tab); if (!el) return;
  if (!S.cmp || !S.A2) { el.innerHTML = ""; return; }
  const A = RA(), B = RB();
  const id = (s) => `cmpch-${tab}-${s}`;
  if (tab === "explorador") {
    const dA = domSent(A), dB = domSent(B), eA = topK(emoTodo(A), 1)[0], eB = topK(emoTodo(B), 1)[0], wA = topK(A.tok, 1)[0], wB = topK(B.tok, 1)[0], tA = topK(A.top, 1)[0], tB = topK(B.top, 1)[0];
    const lenA = A.b - A.a + 1, lenB = B.b - B.a + 1;
    el.innerHTML = cabCmp("Las diferencias se calculan como A − B; en cantidades, como variación porcentual respecto de B.") + `<div class="grid"><div class="c7">${tablaCmp([
      filaCmp("Mensajes", A.n, B.n), filaCmp("Días con mensajes", A.porDia.size, B.porDia.size), filaCmp("Mensajes por día", A.n / lenA, B.n / lenB),
      filaCmp("Miembros activos", A.aut.filter(v => v > 0).length, B.aut.filter(v => v > 0).length), filaCmp("Mensajes con sentimiento", A.ns / Math.max(1, A.nan), B.ns / Math.max(1, B.nan), "p"),
      filaCmp("Positivos", A.pol[2] / Math.max(1, A.ns), B.pol[2] / Math.max(1, B.ns), "p"), filaCmp("Negativos", A.pol[0] / Math.max(1, A.ns), B.pol[0] / Math.max(1, B.ns), "p"),
      filaCmp("Índice de ánimo", animo(A), animo(B), "i"), filaCmp("Preguntas respondidas", A.respondidas / Math.max(1, A.preguntas), B.respondidas / Math.max(1, B.preguntas), "p"),
      filaCmp("Sentimiento predominante", dA >= 0 ? S_NOM[dA] : "—", dB >= 0 ? S_NOM[dB] : "—", "t"),
      filaCmp("Emoji más usado", eA ? imgEmo(EMO[eA.i].e, "width:22px;height:22px;vertical-align:middle") : "—", eB ? imgEmo(EMO[eB.i].e, "width:22px;height:22px;vertical-align:middle") : "—", "t"),
      filaCmp("Palabra más usada", wA ? V.tok[wA.i] : "—", wB ? V.tok[wB.i] : "—", "t"),
      filaCmp("Tópico principal", tA ? X.modelos.topicos[tA.i].nombre : "—", tB ? X.modelos.topicos[tB.i].nombre : "—", "t")])}</div>
      <div class="c5"><div class="chart" id="${id("dias")}" style="height:420px"></div></div></div>`;
    const sA = [], sB = [], L = Math.max(lenA, lenB), gg = L > 120 ? 7 : 1;
    for (let k = 0; k < L; k += gg) { let a = 0, b = 0; for (let j = k; j < Math.min(L, k + gg); j++) { a += A.porDia.get(A.a + j)?.n || 0; b += B.porDia.get(B.a + j)?.n || 0; } sA.push(k < lenA ? a : null); sB.push(k < lenB ? b : null); }
    ch(id("dias")).setOption({ title: { text: `Mensajes por ${gg === 7 ? "semana" : "día"} desde el inicio de cada periodo`, left: "center", textStyle: { fontSize: 13, fontFamily: "Manrope", fontStyle: "normal", color: "#4A4760" } },
      grid: { left: 40, right: 14, top: 60, bottom: 30 }, legend: { top: 24 }, tooltip: tt({ trigger: "axis" }), xAxis: { type: "category", data: sA.map((_, k) => `${gg === 7 ? "sem" : "día"} ${k + 1}`) }, yAxis: { type: "value" },
      series: [{ name: "A", type: "line", data: sA, smooth: .3, symbol: "none", lineStyle: { color: COL_A, width: 2.2 }, itemStyle: { color: COL_A }, areaStyle: { color: COL_A, opacity: .08 } },
        { name: "B", type: "line", data: sB, smooth: .3, symbol: "none", lineStyle: { color: COL_B, width: 2.2 }, itemStyle: { color: COL_B }, areaStyle: { color: COL_B, opacity: .1 } }] }, true);
  } else if (tab === "sentimientos") {
    el.innerHTML = cabCmp("Diferencia en puntos porcentuales (pp) del peso de cada sentimiento: hacia la derecha, mayor en A; hacia la izquierda, mayor en B.") + `<div class="chart" id="${id("d")}" style="height:440px"></div>`;
    const it = [...Array(15).keys()].map(i => ({ l: S_NOM[i], v: +(((A.ns ? A.sent[i] / A.ns : 0) - (B.ns ? B.sent[i] / B.ns : 0)) * 100).toFixed(1), c: S_COL[i] })).sort((a, b) => b.v - a.v);
    barDiv(id("d"), it, { lab: p => (p.value > 0 ? "+" : "") + p.value + " pp", tip: ps => { const i = S_NOM.indexOf(ps[0].name); return `<b>${ps[0].name}</b><br>A: ${pct(A.sent[i] / Math.max(1, A.ns))} · B: ${pct(B.sent[i] / Math.max(1, B.ns))}<br>Diferencia: ${ps[0].value} pp`; } });
  } else if (tab === "emojis") {
    const ca = emoCuenta(A), cb = emoCuenta(B), ta = sumA(ca) || 1, tb = sumA(cb) || 1;
    const uni = [...new Set(topK(ca, 25).map(x => x.i).concat(topK(cb, 25).map(x => x.i)))];
    const it = uni.map(i => ({ i, v: +((ca[i] / ta - cb[i] / tb) * 100).toFixed(2) })).sort((a, b) => Math.abs(b.v) - Math.abs(a.v)).slice(0, 16).sort((a, b) => b.v - a.v);
    el.innerHTML = cabCmp("Cambio en la participación de cada emoji (pp) según la fuente elegida en la pestaña; abajo, los diez más usados en cada periodo.") + `<div class="grid"><div class="c7"><div class="chart" id="${id("d")}" style="height:460px"></div></div><div class="c5"><div class="listas-cmp">
      ${listaCmp("Periodo A", topK(ca, 10).map(x => `<li>${imgEmo(EMO[x.i].e)}${esc(EMO[x.i].nombre)} · ${fmt(x.v)} (${pct(x.v / ta)})</li>`), COL_A)}
      ${listaCmp("Periodo B", topK(cb, 10).map(x => `<li>${imgEmo(EMO[x.i].e)}${esc(EMO[x.i].nombre)} · ${fmt(x.v)} (${pct(x.v / tb)})</li>`), COL_B)}</div></div></div>`;
    barDiv(id("d"), it.map((x, k) => ({ l: `{e${k}|}`, v: x.v, c: EMO[x.i].s >= 0 ? S_COL[EMO[x.i].s] : "#94A3B8" })), { yLabel: { rich: richEmo(it.map(x => EMO[x.i].e).reverse(), 24) }, lab: p => (p.value > 0 ? "+" : "") + p.value + " pp",
      tip: ps => { const x = it[it.length - 1 - ps[0].dataIndex]; return `<b>${EMO[x.i].e} ${esc(EMO[x.i].nombre)}</b><br>A: ${pct(ca[x.i] / ta)} · B: ${pct(cb[x.i] / tb)}<br>Diferencia: ${x.v} pp`; } });
  } else if (tab === "lenguaje") {
    const lo = logOddsAB(A, B, 5), mas = lo.slice(0, 12), menos = lo.slice(-12).reverse();
    el.innerHTML = cabCmp("Palabras que distinguen a cada periodo (log-odds entre A y B): a la derecha, propias de A; a la izquierda, propias de B.") + `<div class="grid"><div class="c7"><div class="chart" id="${id("d")}" style="height:520px"></div></div><div class="c5"><div class="listas-cmp">
      ${listaCmp("Frases más usadas en A", topK(A.big, 10, V.big).map(x => `<li>${esc(x.l)} · ${fmt(x.v)}</li>`), COL_A)}${listaCmp("Frases más usadas en B", topK(B.big, 10, V.big).map(x => `<li>${esc(x.l)} · ${fmt(x.v)}</li>`), COL_B)}</div></div></div>`;
    barDiv(id("d"), mas.concat(menos.reverse()).map(x => ({ l: V.tok[x.i], v: +x.z.toFixed(2) })).sort((a, b) => b.v - a.v), { lab: p => "z " + p.value,
      tip: ps => { const x = lo.find(o => V.tok[o.i] === ps[0].name); return `<b>${ps[0].name}</b><br>A: ${fmt(x.a)} veces · B: ${fmt(x.b)} veces<br>z = ${ps[0].value}`; } });
  } else if (tab === "temas") {
    const TP = X.modelos.topicos, ta = sumA(A.top) || 1, tb = sumA(B.top) || 1;
    el.innerHTML = cabCmp("Peso de cada tópico en cada periodo y su diferencia en puntos porcentuales.") + `<div class="grid"><div class="c7"><div class="chart" id="${id("g")}" style="height:340px"></div></div><div class="c5">${tablaCmp(TP.map((t, i) => filaCmp(esc(t.nombre), A.top[i] / ta, B.top[i] / tb, "p")))}</div></div>`;
    ch(id("g")).setOption({ grid: { left: 10, right: 40, top: 30, bottom: 6, containLabel: true }, legend: { top: 0 }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, valueFormatter: v => v + "%" }), xAxis: { type: "value", axisLabel: { formatter: "{value}%" } },
      yAxis: { type: "category", data: TP.map(t => t.nombre), inverse: true, axisLabel: { interval: 0 } },
      series: [["A", A.top, ta, COL_A], ["B", B.top, tb, COL_B]].map(([n, v, t, c]) => ({ name: n, type: "bar", barMaxWidth: 14, itemStyle: { color: c, borderRadius: [0, 6, 6, 0] }, data: TP.map((_, i) => +(v[i] / t * 100).toFixed(1)), label: { show: true, position: "right", formatter: "{c}%", fontSize: 10 } })) }, true);
  } else if (tab === "comunidad") {
    const conc = R0 => { const arr = [...R0.aut].filter(v => v > 0).sort((a, b) => b - a), t = sumA(arr); return t ? arr.slice(0, Math.max(1, Math.ceil(arr.length * .1))).reduce((a, b) => a + b, 0) / t : 0; };
    const act = R0 => R0.aut.filter(v => v > 0).length;
    el.innerHTML = cabCmp("Participación, concentración y respuesta en cada periodo; a la derecha, los miembros más activos de cada uno.") + `<div class="grid"><div class="c7">${tablaCmp([
      filaCmp("Miembros activos", act(A), act(B)), filaCmp("Mensajes por miembro activo", A.n / Math.max(1, act(A)), B.n / Math.max(1, act(B))), filaCmp("Mensajes del 10% más activo", conc(A), conc(B), "p"),
      filaCmp("Preguntas", A.preguntas, B.preguntas), filaCmp("Preguntas respondidas", A.respondidas / Math.max(1, A.preguntas), B.respondidas / Math.max(1, B.preguntas), "p"),
      filaCmp("Mediana de minutos hasta la respuesta", median(A.tiempos) || 0, median(B.tiempos) || 0), filaCmp("Reacciones", A.nreac, B.nreac)])}</div>
      <div class="c5"><div class="listas-cmp">${listaCmp("Más activos en A", topK(A.aut, 8).map(x => `<li>${esc(V.aut[x.i])} · ${fmt(x.v)}</li>`), COL_A)}${listaCmp("Más activos en B", topK(B.aut, 8).map(x => `<li>${esc(V.aut[x.i])} · ${fmt(x.v)}</li>`), COL_B)}</div></div></div>`;
  } else if (tab === "docentes") {
    const ks = ENT.map((o, k) => k).filter(k => ENT[k].t === "docente" && A.ent[k] >= 3 && B.ent[k] >= 3).sort((x, y) => (A.ent[y] + B.ent[y]) - (A.ent[x] + B.ent[x])).slice(0, 18);
    el.innerHTML = cabCmp("Índice de percepción de los docentes mencionados al menos 3 veces en ambos periodos (A en negro, B en verde azulado).") + `<div class="grid"><div class="c7"><div class="chart" id="${id("d")}" style="height:${Math.max(260, ks.length * 26 + 60)}px"></div></div><div class="c5"><div class="listas-cmp">
      ${listaCmp("Más mencionados en A", ENT.map((o, k) => k).filter(k => ENT[k].t === "docente" && A.ent[k] > 0).sort((x, y) => A.ent[y] - A.ent[x]).slice(0, 8).map(k => `<li>${esc(ENT[k].e)} · ${A.ent[k]}</li>`), COL_A)}
      ${listaCmp("Más mencionados en B", ENT.map((o, k) => k).filter(k => ENT[k].t === "docente" && B.ent[k] > 0).sort((x, y) => B.ent[y] - B.ent[x]).slice(0, 8).map(k => `<li>${esc(ENT[k].e)} · ${B.ent[k]}</li>`), COL_B)}</div></div></div>`;
    if (!ks.length) return vacio(id("d"), "No hay docentes con 3+ menciones en ambos periodos");
    const nom = ks.map(k => ENT[k].e).reverse(), kr = [...ks].reverse();
    ch(id("d")).setOption({ grid: { left: 10, right: 30, top: 30, bottom: 20, containLabel: true }, legend: { top: 0, data: ["A", "B"] }, tooltip: tt({ trigger: "axis", axisPointer: { type: "shadow" }, formatter: ps => { const k = kr[ps[0].dataIndex]; return `<b>${esc(ENT[k].e)}</b><br>A: ${pct(indiceDoc(A, k))} (${A.ent[k]} menciones)<br>B: ${pct(indiceDoc(B, k))} (${B.ent[k]} menciones)`; } }),
      xAxis: { type: "value", min: 0, max: 1, axisLabel: { formatter: v => Math.round(v * 100) + "%" } }, yAxis: { type: "category", data: nom, axisLabel: { interval: 0 } },
      series: [{ type: "custom", renderItem: (pa, api) => { const y = api.coord([0, api.value(0)])[1], x0 = api.coord([api.value(1), 0])[0], x1 = api.coord([api.value(2), 0])[0]; return { type: "line", shape: { x1: x0, y1: y, x2: x1, y2: y }, style: { stroke: "rgba(22,19,31,.25)", lineWidth: 3 } }; },
        data: kr.map((k, i) => [i, indiceDoc(A, k), indiceDoc(B, k)]), encode: { y: 0 }, tooltip: { show: false } },
        { name: "A", type: "scatter", data: kr.map((k, i) => [indiceDoc(A, k), i]), symbolSize: 12, itemStyle: { color: COL_A } },
        { name: "B", type: "scatter", data: kr.map((k, i) => [indiceDoc(B, k), i]), symbolSize: 12, itemStyle: { color: COL_B } }] }, true);
  } else if (tab === "causas") {
    const inc = R0 => (R0.sent[8] + R0.sent[9]) / Math.max(1, R0.ns), mal = R0 => (R0.sent[12] + R0.sent[13] + R0.sent[14]) / Math.max(1, R0.ns);
    el.innerHTML = cabCmp("Peso de la incertidumbre y del malestar en cada periodo, y las palabras más asociadas a la incertidumbre en cada uno.") + `<div class="grid"><div class="c6">${tablaCmp([
      filaCmp("Incertidumbre (confusión + preocupación)", inc(A), inc(B), "p"), filaCmp("Malestar (decepción + cansancio + frustración)", mal(A), mal(B), "p"), filaCmp("Enojo", A.sent[11] / Math.max(1, A.ns), B.sent[11] / Math.max(1, B.ns), "p"),
      filaCmp("Preguntas respondidas", A.respondidas / Math.max(1, A.preguntas), B.respondidas / Math.max(1, B.preguntas), "p")])}</div>
      <div class="c6"><div class="listas-cmp">${listaCmp("Causas en A", incertidumbre(A).slice(0, 10).map(x => `<li>${esc(V.tok[x.i])} · z ${x.z.toFixed(1)}</li>`), COL_A)}${listaCmp("Causas en B", incertidumbre(B).slice(0, 10).map(x => `<li>${esc(V.tok[x.i])} · z ${x.z.toFixed(1)}</li>`), COL_B)}</div></div></div>`;
  }
}

/* ---------- 11 Laboratorio: motor de sentimientos en el navegador ---------- */
const MOT = X.motor;
const RX_EMO = /(?:\p{Regional_Indicator}{2})|(?:\p{Extended_Pictographic}️?[\u{1F3FB}-\u{1F3FF}]?(?:‍\p{Extended_Pictographic}️?[\u{1F3FB}-\u{1F3FF}]?)*)/gu;
const normE = e => e.replace(/[️\u{1F3FB}-\u{1F3FF}]/gu, "");
function normBase(s) {
  return s.normalize("NFKC").replace(/[   ​⁠]/g, " ").toLowerCase().normalize("NFD").replace(/\p{M}/gu, "")
    .replace(/(?<=\p{L})(\p{L})\1{2,}|(\p{L})\2{2,}(?=\p{L})/gu, "$1$2").replace(/\s+/g, " ").trim();
}
const reJS = (re, ci) => { try { return new RegExp(re, "gu" + (ci ? "i" : "")); } catch (e) { return null; } };
const NORM = MOT.norm.map(n => ({ re: reJS(n.re, true), a: n.a })).filter(n => n.re);
const JER = MOT.jer.map(j => ({ x: j.x, s: j.s, re: reJS(j.re, j.ci) })).filter(j => j.re);
const DIC = { frase: new Map(), palabra: new Map(), raiz: [] }; let maxN = 1;
MOT.dic.forEach(d => { if (d.k === "frase") { DIC.frase.set(d.t, d.s); maxN = Math.max(maxN, d.t.split(" ").length); } else if (d.k === "palabra") DIC.palabra.set(d.t, d.s); else DIC.raiz.push([d.t, d.s]); });
DIC.raiz.sort((a, b) => b[0].length - a[0].length);
const EMOD = new Map(MOT.emo.map(e => [e.e, e])); const NEG = new Set(MOT.neg), INT = new Set(MOT.int);
const NB = X.modelos.nb, NBI = new Map(NB.palabras.map((w, i) => [w, i]));
function analizar(texto) {
  const hits = [], sc = new Float64Array(15);
  const emos = (texto.match(RX_EMO) || []).map(normE);
  const exprs = JER.filter(j => { j.re.lastIndex = 0; return j.re.test(texto); });
  let t = texto.replace(/(?:https?:\/\/|www\.)\S+/g, " ").replace(RX_EMO, " ");
  t = normBase(t); NORM.forEach(n => { n.re.lastIndex = 0; t = t.replace(n.re, n.a); });
  t = t.replace(/(?<![\p{L}\d])x+d+(?![\p{L}\d])/gu, "xd");
  const limpio = t.replace(/[^\p{L}\s]/gu, " ").replace(/\s+/g, " ").trim(); const tk = limpio ? limpio.split(" ") : [];
  const marca = new Array(tk.length).fill(null);
  for (let n = maxN; n >= 2; n--) for (let i = 0; i + n <= tk.length; i++) { if (marca.slice(i, i + n).some(Boolean)) continue; const g = tk.slice(i, i + n).join(" ");
    if (DIC.frase.has(g)) { const s = DIC.frase.get(g); for (let j = i; j < i + n; j++) marca[j] = { s, ini: j === i, n, t: g, f: "frase" }; sc[s] += 1.5; hits.push({ t: g, s, f: "frase", w: 1.5 }); } }
  for (let i = 0; i < tk.length; i++) { if (marca[i]) continue; let s = DIC.palabra.get(tk[i]), f = "palabra", w = 1;
    if (s == null) { const r = DIC.raiz.find(([rz]) => tk[i].startsWith(rz)); if (r) { s = r[1]; f = "raíz"; w = .8; } }
    if (s == null) continue;
    const w1 = tk[i - 1], w2 = tk[i - 2]; const neg = NEG.has(w1) || (NEG.has(w2) && (INT.has(w1) || ["es", "esta", "estoy", "fue", "era", "me", "te", "se", "lo", "la", "tan", "muy"].includes(w1)));
    if (INT.has(w1)) w *= 1.5;
    marca[i] = { s, ini: true, n: 1, t: tk[i], f, neg }; if (neg) { hits.push({ t: tk[i], s, f: f + " (negada)", w: 0 }); continue; } sc[s] += w; hits.push({ t: tk[i], s, f: f + (w > 1 ? " ×1.5" : ""), w }); }
  const ce = new Map(); emos.forEach(e => ce.set(e, Math.min(3, (ce.get(e) || 0) + 1)));
  ce.forEach((n, e) => { const d = EMOD.get(e); if (d) { const w = n * (.5 + .25 * d.i); sc[d.s] += w; hits.push({ t: e, s: d.s, f: "emoji", w }); } });
  exprs.forEach(j => { sc[j.s] += .8; hits.push({ t: j.x, s: j.s, f: "emoticón", w: .8 }); });
  const iro = MOT.iro.some(p => new RegExp(`(^|\\s)${p}(\\s|$)`).test(t)) || emos.some(e => MOT.iro_e.includes(e));
  let ironia = false;
  if (iro && hits.some(h => [0, 1, 3, 4, 5].includes(h.s) && h.w > 0 && h.f !== "emoji")) { ironia = true;
    hits.forEach(h => { if ([0, 1, 3, 4, 5].includes(h.s) && h.w > 0 && h.f !== "emoji") { sc[h.s] -= h.w; sc[12] += h.w; h.f += " → ironía"; h.s2 = 12; } }); }
  let lp0 = NB.prior[0], lp1 = NB.prior[1], usadas = 0; tk.forEach(w => { const k = NBI.get(w); if (k != null) { lp0 += NB.neg[k]; lp1 += NB.pos[k]; usadas++; } });
  const pPos = usadas ? 1 / (1 + Math.exp(lp0 - lp1)) : null;
  return { sc, hits, tk, marca, ironia, pPos, usadas, emos };
}
function renderAnalisis() {
  const txt = $("#txtAnaliza").value, A = analizar(txt);
  $("#resaltado").innerHTML = (A.tk.length ? A.tk.map((w, i) => { const m = A.marca[i]; if (!m) return esc(w); const s = m.s; return `<mark class="${m.neg ? "neg" : ""}" style="background:${S_COL[s]}" title="${S_NOM[s]} (${m.f})">${esc(w)}</mark>`; }).join(" ") : "<span style='color:var(--ink3)'>Escribir un mensaje para analizarlo.</span>")
    + " " + A.emos.map(e => imgEmo(e, "width:28px;height:28px;vertical-align:middle")).join(" ");
  const ord = [...Array(15).keys()].filter(i => A.sc[i] > 0).sort((a, b) => A.sc[b] - A.sc[a]);
  if (ord.length) barH("chAnaliza", ord.map(i => ({ l: S_NOM[i], v: +A.sc[i].toFixed(2), c: S_COL[i] })), null, { lab: p => p.value.toFixed(2) }); else vacio("chAnaliza", "Sin señal de sentimiento: mensaje neutral o informativo");
  const pos = [0, 1, 2, 3, 4, 5].reduce((a, i) => a + A.sc[i], 0), neg = [8, 9, 10, 11, 12, 13, 14].reduce((a, i) => a + A.sc[i], 0);
  $("#notaAnaliza").innerHTML = `${ord.length ? `Sentimiento dominante: <b style="color:${S_COL[ord[0]]}">${S_NOM[ord[0]]}</b> · ` : ""}polaridad <b>${pos > neg ? "positiva" : neg > pos ? "negativa" : "neutral"}</b> (${(pos - neg).toFixed(2)}).
    ${A.ironia ? "<b>Ironía detectada</b>: lo positivo pasó a decepción. " : ""}${A.hits.length ? "Señales: " + A.hits.map(h => `${esc(h.t)} <i>(${h.f})</i>`).join(", ") + "." : ""}
    ${A.pPos != null ? `<br>Clasificador Naive Bayes (palabras fuera del diccionario, ${A.usadas} reconocidas): <b>${pct(A.pPos, 0)} positivo</b>.` : ""}`;
}


/* ---------- 11 Laboratorio: fechas en lenguaje natural ---------- */
const MESN = { enero: 1, ene: 1, febrero: 2, feb: 2, marzo: 3, mar: 3, abril: 4, abr: 4, mayo: 5, may: 5, junio: 6, jun: 6, julio: 7, jul: 7, agosto: 8, ago: 8, septiembre: 9, setiembre: 9, sep: 9, octubre: 10, oct: 10, noviembre: 11, nov: 11, diciembre: 12, dic: 12 };
function fechasDe(q) {
  const out = []; const y1 = dayDate(NDAYS - 1).getFullYear();
  let m;
  const rd = /(\d{1,2})[\/\-](\d{1,2})[\/\-](\d{2,4})/g; const fechas = []; while ((m = rd.exec(q))) { const y = +m[3] < 100 ? 2000 + +m[3] : +m[3]; fechas.push(fromIso(`${y}-${String(m[2]).padStart(2, "0")}-${String(m[1]).padStart(2, "0")}`)); }
  const rl = /(\d{1,2}) de (enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)(?: de(?:l)? (20\d\d))?/g; while ((m = rl.exec(q))) fechas.push(fromIso(`${m[3] || y1}-${String(MESN[m[2]]).padStart(2, "0")}-${String(m[1]).padStart(2, "0")}`));
  if (/semana/.test(q) && fechas.length) { const d = fechas[0], w = (dayDate(d).getDay() + 6) % 7; out.push({ a: d - w, b: d - w + 6, lab: "semana del " + fdia(d - w) }); return out; }
  if (fechas.length >= 2 && /\b(del|desde|entre)\b/.test(q)) { out.push({ a: Math.min(fechas[0], fechas[1]), b: Math.max(fechas[0], fechas[1]), lab: `${fdia(Math.min(fechas[0], fechas[1]))} – ${fdia(Math.max(fechas[0], fechas[1]))}` }); return out; }
  fechas.forEach(d => out.push({ a: d, b: d, lab: "el " + fdia(d) }));
  const rp = /(verano|invierno|semestre (?:i{1,2}|1|2|uno|dos)|(?:primer|segundo) semestre)\s*(?:de(?:l)?\s*)?(20\d\d)?/g;
  while ((m = rp.exec(q))) { let p = m[1]; const y = m[2] ? +m[2] : null;
    let lab; if (/verano/.test(p)) lab = "Verano"; else if (/invierno/.test(p)) lab = "Invierno"; else if (/(ii|2|dos|segundo)/.test(p)) lab = "II"; else lab = "I";
    if (!y) { out.push({ falta: lab }); continue; }
    if (lab === "I" || lab === "II") { const [a, b] = clip(rangoSem(`${lab}-${y}`)); out.push({ a, b, lab: `el semestre ${lab}-${y}` }); } else { const [a, b] = clip(rangoPer(`${lab} ${y}`)); out.push({ a, b, lab: `${lab} ${y}` }); } }
  if (!out.length) { const rm = /\b(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)\b(?:\s+(?:de(?:l)?\s+)?(20\d\d))?/g;
    while ((m = rm.exec(q))) { if (!m[2]) { out.push({ faltaMes: MESN[m[1]] }); continue; } const k = `${m[2]}-${String(MESN[m[1]]).padStart(2, "0")}`; const [a, b] = clip(rangoMes(k)); out.push({ a, b, lab: `${m[1]} de ${m[2]}` }); } }
  if (!out.length) { const ry = /\b(20(?:2[3-9]))\b/g; while ((m = ry.exec(q))) { const [a, b] = clip(rangoAnio(+m[1])); out.push({ a, b, lab: "el año " + m[1] }); } }
  if (!out.length && /(ultimo mes|último mes)/.test(q)) out.push({ a: NDAYS - 30, b: NDAYS - 1, lab: "los últimos 30 días" });
  if (!out.length && /(este ano|ultimo ano)/.test(q)) { const [a, b] = clip(rangoAnio(y1)); out.push({ a, b, lab: "el año " + y1 }); }
  return out;
}

/* ---------- 11 Laboratorio: asistente basado en reglas ---------- */
const INTENT = [
  { k: "saludo", re: /^(hola|buenas|buenos dias|buen dia|buenas tardes|buenas noches|hey|que tal|saludos)\b/, oculto: true },
  { k: "ayuda", re: /(ayuda|que puedes|que sabes|que preguntas|como funciona|que haces|opciones|menu|que se puede)/, titulo: "Ayuda", desc: "Lista de consultas disponibles.", claves: "ayuda, qué puedes, opciones", ej: "¿Qué se puede consultar?" },
  { k: "cmp", re: /(compar|versus|\bvs\b|frente a|diferencia)/, titulo: "Comparar periodos", desc: "Sentimiento predominante e índice de ánimo de dos periodos.", claves: "compara, versus, frente a, diferencia + dos fechas", ej: "Compara verano 2024 y verano 2025" },
  { k: "docente", re: /(docente|\blic\b|licenciad|profe|ingenier|docentes)/, titulo: "Docentes", desc: "Docentes más mencionados y con mejor percepción en el periodo.", claves: "docente, lic, licenciado, profe", ej: "¿Qué docentes se mencionaron en 2025?" },
  { k: "emoji", re: /(emoji|emoticon|carita)/, titulo: "Emojis", desc: "Emojis más usados en el texto y su significado.", claves: "emoji, emoticón, carita", ej: "¿Qué emojis se usaron en enero 2026?" },
  { k: "reac", re: /(reaccion|pulgar|like)/, titulo: "Reacciones", desc: "Reacciones más usadas en el periodo.", claves: "reacción, pulgar, like", ej: "¿Qué reacciones hubo en invierno 2024?" },
  { k: "persona", re: /(quien|quienes|persona|miembro|usuario|mas activo|hablo mas|hablaron mas|escribio mas)/, titulo: "Miembros", desc: "Miembros más activos (anonimizados) y su sentimiento más frecuente.", claves: "quién, miembro, más activo, habló más", ej: "¿Quién habló más en 2024?" },
  { k: "hora", re: /(hora|horario|a que hora|dia de la semana|cuando se escribe)/, titulo: "Horarios", desc: "Hora y día con más mensajes, y hora con mayor tasa de respuesta.", claves: "hora, horario, día de la semana", ej: "¿Cuál es la mejor hora para preguntar?" },
  { k: "pico", re: /(pico|dia con mas|mas mensajes|exploto|dia mas activo|mayor actividad)/, titulo: "Días pico", desc: "Día con más mensajes y sus palabras distintivas.", claves: "pico, día con más mensajes, mayor actividad", ej: "¿Cuál fue el día con más mensajes en 2025?" },
  { k: "resp", re: /(respuesta|respond|contest)/, titulo: "Respuestas", desc: "Porcentaje de preguntas que recibieron respuesta.", claves: "respuesta, responden, contestan", ej: "¿Cuántas preguntas se respondieron en 2026?" },
  { k: "causa", re: /(por que|causa|incertidumbre|ocasion|motivo)/, titulo: "Causas de la incertidumbre", desc: "Palabras asociadas a la confusión y la preocupación.", claves: "por qué, causa, incertidumbre, motivo", ej: "¿Qué causó la incertidumbre en verano 2025?" },
  { k: "tema", re: /(tema|topico|de que se hablo|de que hablaban|sobre que|conversa)/, titulo: "Temas", desc: "Tópico LDA principal y palabras distintivas del periodo.", claves: "tema, tópico, de qué se habló", ej: "¿De qué se habló la semana del 30/06/2025?" },
  { k: "pal", re: /(palabra|frase|que se dec|termino|expresion|jerga)/, titulo: "Palabras y frases", desc: "Palabras y frases más usadas.", claves: "palabra, frase, expresión, jerga", ej: "¿Qué palabras se usaron en julio 2025?" },
  { k: "sent", re: /(sentimiento|sentia|siente|sentian|emocion|animo|predomin|feliz|triste|enoj|frustr|positiv|negativ)/, titulo: "Sentimientos", desc: "Sentimientos predominantes e índice de ánimo.", claves: "sentimiento, emoción, ánimo, predominó", ej: "¿Qué sentimiento predominó en invierno 2025?" },
  { k: "resumen", re: /(resumen|cuentame|que paso|como estuvo|como fue|informacion de)/, titulo: "Resumen", desc: "Mensajes, sentimiento, emoji y palabra principales de un periodo.", claves: "resumen, qué pasó, cómo estuvo", ej: "Resumen del semestre II 2025" },
  { k: "gracias", re: /\b(gracias|genial|perfecto|excelente)\b/, oculto: true }];
const NO_PUEDE = ["Opiniones o valoraciones propias sobre docentes o personas.", "Nombres reales de los miembros (solo se usan alias).", "Fechas fuera del rango de los datos (" + fdia(0) + " a " + fdia(NDAYS - 1) + ").", "Contenido de fotos, audios o stickers (solo su emoji asociado).", "Predicciones individuales sobre una persona."];
const EJ_INICIO = ["¿Qué sentimiento predominó en invierno 2025?", "¿Qué emojis se usaron en enero 2026?", "Compara verano 2024 y verano 2025", "¿Quién habló más en 2024?", "¿De qué se habló la semana del 30/06/2025?", "¿Cuál es la mejor hora para preguntar?"];
function responder(qRaw) {
  const q = normBase(qRaw);
  let F = fechasDe(q); const falta = F.find(f => f.falta || f.faltaMes); F = F.filter(f => f.a != null && f.b >= f.a).map(f => { const [a, b] = clip([f.a, f.b]); return Object.assign(f, { a, b }); });
  const intents = INTENT.filter(o => o.re.test(q)).map(o => o.k);
  const intent = intents.find(k => !["saludo", "gracias"].includes(k)) || intents[0] || null;
  if (intent === "saludo" && !F.length) return { html: `Hola. El asistente responde consultas sobre el chat entre el ${fdia(0)} y el ${fdia(NDAYS - 1)}: sentimientos, emojis, palabras, temas, miembros (anonimizados), docentes, horarios, días pico, respuestas, causas de la incertidumbre y comparaciones entre periodos. Ejemplos:`, sugs: EJ_INICIO.slice(0, 4) };
  if (intent === "gracias" && !F.length) return { html: "Con gusto. Se puede hacer otra consulta en cualquier momento.", sugs: EJ_INICIO.slice(2, 5) };
  if (intent === "ayuda") return { html: `Consultas disponibles: ${INTENT.filter(o => !o.oculto && o.k !== "ayuda").map(o => `<b>${o.titulo}</b>`).join(", ")}. Todas aceptan una fecha (año, mes y año, periodo académico, semana, día o rango). La lista completa, con ejemplos, está en la tarjeta "Consultas que puede responder el asistente".`, sugs: INTENT.filter(o => !o.oculto && o.k !== "ayuda").slice(0, 5).map(o => o.ej) };
  if (falta) { const anios = X.cobertura.map(c => c.anio); const que = falta.faltaMes ? MESES_L[falta.faltaMes - 1] : (falta.falta.length < 3 ? "el semestre " + falta.falta : falta.falta.toLowerCase());
    return { html: `La consulta menciona ${que}, pero no el año. Consultas posibles:`, sugs: anios.slice(-4).map(y => `${qRaw.trim()} ${y}`) }; }
  const P0 = F[0] || { a: 0, b: NDAYS - 1, lab: "todo el periodo de datos" }; const R = agg(P0.a, P0.b);
  if (!R.n) return { html: `No hay mensajes en ${P0.lab}. Los datos van del ${fdia(0)} al ${fdia(NDAYS - 1)}.`, sugs: ["Resumen del año 2025", "¿Qué sentimiento predominó en invierno 2024?"] };
  const ver = { a: P0.a, b: P0.b, lab: P0.lab.replace(/^el /, "") };
  const top3 = arr => [...Array(15).keys()].sort((x, y) => arr[y] - arr[x]).slice(0, 3);
  const nota = F.length ? "" : " <i>(No se indicaron fechas: se usa todo el periodo.)</i>";
  switch (intent) {
    case "cmp": { if (F.length < 2) return { html: "Para comparar se necesitan dos periodos, por ejemplo: <i>compara verano 2024 y verano 2025</i>.", sugs: ["Compara invierno 2024 y invierno 2025", "Compara 2024 y 2025"] };
      const R2 = agg(F[1].a, F[1].b), d1 = domSent(R), d2 = domSent(R2);
      return { html: `<b>${P0.lab}</b>: ${fmt(R.n)} mensajes; predomina ${S_NOM[d1]} (${pct(R.sent[d1] / R.ns)}); índice de ánimo ${animo(R).toFixed(2)}.<br><b>${F[1].lab}</b>: ${fmt(R2.n)} mensajes; predomina ${S_NOM[d2]} (${pct(R2.sent[d2] / R2.ns)}); índice de ánimo ${animo(R2).toFixed(2)}.<br>${animo(R) > animo(R2) ? P0.lab : F[1].lab} presenta un ánimo más positivo.`,
        chart: { type: "cmp", R, R2, l1: P0.lab, l2: F[1].lab }, ver, cmpB: { a: F[1].a, b: F[1].b, lab: F[1].lab }, tab: "sentimientos" }; }
    case "emoji": { const t = topK(R.emoT, 5); return { html: `En ${P0.lab} los emojis más usados fueron ${t.map(x => `${imgEmo(EMO[x.i].e, "width:22px;height:22px;vertical-align:middle")} (${fmt(x.v)}; ${esc(EMO[x.i].sig.split(";")[0])})`).join(", ")}.${nota}`, ver, tab: "emojis" }; }
    case "reac": { const t = topK(R.emoR, 5); return { html: `Reacciones en ${P0.lab}: ${t.map(x => `${imgEmo(EMO[x.i].e, "width:22px;height:22px;vertical-align:middle")} ${fmt(x.v)}`).join(" · ") || "ninguna"}.${nota}`, ver, tab: "emojis" }; }
    case "persona": { const t = topK(R.aut, 5); const a = t[0]; const s = R.autSent.get(a.i); const mx = s ? Math.max(...s) : 0; const d = mx > 0 ? s.indexOf(mx) : -1;
      return { html: `En ${P0.lab} el miembro con más mensajes fue <b>${V.aut[a.i]}</b> (${fmt(a.v)} mensajes)${d >= 0 ? `; su sentimiento más frecuente fue <b style="color:${S_COL[d]}">${S_NOM[d]}</b>` : ""}. Le siguen ${t.slice(1).map(x => `${V.aut[x.i]} (${fmt(x.v)})`).join(", ")}.${nota}`, chart: { type: "bar", items: t.map(x => ({ l: V.aut[x.i], v: x.v })) }, ver, tab: "comunidad" }; }
    case "hora": { const porH = [...Array(24).keys()].map(h => { let s = 0; for (let w = 0; w < 7; w++) s += R.hw[w * 24 + h]; return s; }); const bh = porH.indexOf(Math.max(...porH));
      let bw = 0, vw = -1; for (let w = 0; w < 7; w++) { let s = 0; for (let h = 0; h < 24; h++) s += R.hw[w * 24 + h]; if (s > vw) { vw = s; bw = w; } }
      let best = -1; for (let h = 0; h < 24; h++) if (R.qh[h] >= Math.max(10, .03 * R.preguntas) && (best < 0 || R.qa[h] / R.qh[h] > R.qa[best] / R.qh[best])) best = h;
      return { html: `En ${P0.lab} la hora con más mensajes fue las <b>${bh}h</b> y el día, el <b>${DIAS[bw]}</b>.${best >= 0 ? ` Las preguntas de las <b>${best}h</b> tuvieron la mayor tasa de respuesta (${pct(R.qa[best] / R.qh[best])}).` : ""}${nota}`,
        chart: { type: "bar", items: porH.map((v, h) => ({ l: h + "h", v })), vertical: true }, ver, tab: "comunidad" }; }
    case "pico": { const t = [...R.porDia.entries()].sort((a, b) => b[1].n - a[1].n)[0]; const w = logOdds(agg(t[0], t[0]), TODO(), 2).slice(0, 5).map(x => V.tok[x.i]).join(", ");
      return { html: `El día con más mensajes en ${P0.lab} fue el <b>${fdia(t[0])}</b>, con ${fmt(t[1].n)} mensajes. Palabras distintivas de ese día: <i>${esc(w)}</i>.`, ver: { a: t[0], b: t[0], lab: "Día " + fcorta(t[0]) }, tab: "explorador" }; }
    case "resp": return { html: `En ${P0.lab}, ${fmt(R.respondidas)} de ${fmt(R.preguntas)} preguntas recibieron una respuesta explícita (${pct(R.respondidas / Math.max(1, R.preguntas))}); mediana de ${fmt(median(R.tiempos) || 0)} minutos hasta la primera respuesta.${nota}`, ver, tab: "comunidad" };
    case "docente": { const ds = ENT.map((o, k) => ({ o, k })).filter(x => x.o.t === "docente" && R.ent[x.k] >= 3).map(x => ({ e: x.o.e, n: R.ent[x.k], ind: indiceDoc(R, x.k) }));
      if (!ds.length) return { html: `Casi no se mencionan docentes en ${P0.lab}.`, ver };
      const mas = [...ds].sort((a, b) => b.n - a.n).slice(0, 3), mej = ds.filter(d => d.n >= 5).sort((a, b) => b.ind - a.ind).slice(0, 3);
      return { html: `En ${P0.lab} los docentes más mencionados fueron ${mas.map(d => `<b>${esc(d.e)}</b> (${d.n})`).join(", ")}.${mej.length ? ` Con mejor percepción (5 o más menciones): ${mej.map(d => `${esc(d.e)} (${pct(d.ind, 0)})`).join(", ")}.` : ""} Se trata de la percepción expresada en el chat, no de una evaluación docente.${nota}`, ver, tab: "docentes" }; }
    case "causa": { const lo = incertidumbre(R).slice(0, 6).map(x => V.tok[x.i]);
      return { html: `En ${P0.lab} hubo ${fmt(R.sent[8] + R.sent[9])} mensajes de confusión o preocupación. Palabras más asociadas: <i>${esc(lo.join(", ") || "sin datos suficientes")}</i>.${nota}`, ver, tab: "causas" }; }
    case "tema": { const tot = sumA(R.top); const ord = [...R.top.keys()].sort((a, b) => R.top[b] - R.top[a]);
      const w = (P0.a === 0 && P0.b === NDAYS - 1) ? [] : logOdds(R, TODO(), 3).slice(0, 6).map(x => V.tok[x.i]);
      return { html: `En ${P0.lab} el tópico principal fue <b>${esc(X.modelos.topicos[ord[0]].nombre)}</b> (${pct(R.top[ord[0]] / tot)}), seguido de ${esc(X.modelos.topicos[ord[1]].nombre)} (${pct(R.top[ord[1]] / tot)}).${w.length ? ` Palabras distintivas del periodo: <i>${esc(w.join(", "))}</i>.` : ""}${nota}`, ver, tab: "temas" }; }
    case "pal": { const tw = topK(R.tok, 6, V.tok), tb = topK(R.big, 4, V.big); return { html: `Palabras más usadas en ${P0.lab}: ${tw.map(x => `<b>${esc(x.l)}</b> (${fmt(x.v)})`).join(", ")}. Frases: ${tb.map(x => `"${esc(x.l)}"`).join(", ")}.${nota}`, chart: { type: "bar", items: tw.map(x => ({ l: x.l, v: x.v })) }, ver, tab: "lenguaje" }; }
    case "sent": { const t = top3(R.sent); return { html: `En ${P0.lab} predominó <b style="color:${S_COL[t[0]]}">${S_NOM[t[0]]}</b> (${pct(R.sent[t[0]] / R.ns)} de ${fmt(R.ns)} mensajes con sentimiento), seguido de ${S_NOM[t[1]]} (${pct(R.sent[t[1]] / R.ns)}) y ${S_NOM[t[2]]} (${pct(R.sent[t[2]] / R.ns)}). Índice de ánimo: ${animo(R).toFixed(2)}.${nota}`,
      chart: { type: "sent", R }, ver, tab: "sentimientos" }; }
    default: {
      if (!F.length && intent !== "resumen") return { html: `La consulta no corresponde a ninguna de las intenciones reconocidas. Se pueden consultar: ${INTENT.filter(o => !o.oculto && o.k !== "ayuda").map(o => o.titulo.toLowerCase()).join(", ")}. Ejemplos:`, sugs: EJ_INICIO.slice(0, 4) };
      const d = domSent(R), te = topK(R.emoT, 1)[0], tw = topK(R.tok, 1, V.tok)[0];
      return { html: `Resumen de ${P0.lab}: <b>${fmt(R.n)}</b> mensajes en ${fmt(R.porDia.size)} días; predomina <b style="color:${S_COL[d]}">${S_NOM[d]}</b>; índice de ánimo ${animo(R).toFixed(2)}${te ? `; emoji más usado ${imgEmo(EMO[te.i].e, "width:20px;height:20px;vertical-align:middle")}` : ""}${tw ? `; palabra más usada "${tw.l}"` : ""}.${nota}`,
        sugs: [`¿Qué sentimiento predominó en ${P0.lab.replace(/^el /, "")}?`, `¿De qué se habló en ${P0.lab.replace(/^el /, "")}?`, `¿Quién habló más en ${P0.lab.replace(/^el /, "")}?`], chart: { type: "sent", R }, ver, tab: "explorador" }; }
  }
}
let nChat = 0;
function chatMsg(html, yo, extra) {
  const el = document.createElement("div"); el.className = "bur " + (yo ? "yo" : "bot"); el.innerHTML = html; $("#chat").appendChild(el);
  if (extra) {
    if (extra.chart) { const div = document.createElement("div"); div.className = "mini"; div.id = "mini" + (++nChat); el.appendChild(div);
      setTimeout(() => { const c = echarts.init(div, "xd"); const E = extra.chart;
        if (E.type === "sent") { const o = [...Array(15).keys()].filter(i => E.R.sent[i]).sort((a, b) => E.R.sent[b] - E.R.sent[a]).slice(0, 8);
          c.setOption({ grid: { left: 90, right: 40, top: 4, bottom: 4 }, xAxis: { show: false }, yAxis: { type: "category", inverse: true, data: o.map(i => S_NOM[i]) }, series: [{ type: "bar", data: o.map(i => ({ value: +(E.R.sent[i] / E.R.ns * 100).toFixed(1), itemStyle: { color: S_COL[i], borderRadius: 6 } })), label: { show: true, position: "right", formatter: "{c}%" } }] }); }
        else if (E.type === "cmp") { c.setOption({ legend: { top: 0, textStyle: { fontSize: 10 } }, grid: { left: 40, right: 10, top: 26, bottom: 50 }, xAxis: { type: "category", data: S_NOM, axisLabel: { rotate: 50, fontSize: 9, interval: 0 } }, yAxis: { type: "value" },
          series: [[E.R, E.l1, COL_A], [E.R2, E.l2, COL_B]].map(([r, l, col]) => ({ name: l, type: "bar", itemStyle: { color: col }, data: SENT.map((_, i) => +(r.sent[i] / Math.max(1, r.ns) * 100).toFixed(1)) })) }); }
        else { const it = E.items; c.setOption(E.vertical ? { grid: { left: 30, right: 6, top: 6, bottom: 20 }, xAxis: { type: "category", data: it.map(x => x.l), axisLabel: { fontSize: 9 } }, yAxis: { type: "value", axisLabel: { fontSize: 9 } }, series: [{ type: "bar", data: it.map(x => x.v), itemStyle: { color: css("--accent"), borderRadius: 4 } }] }
          : { grid: { left: 100, right: 40, top: 4, bottom: 4 }, xAxis: { show: false }, yAxis: { type: "category", inverse: true, data: it.map(x => x.l) }, series: [{ type: "bar", data: it.map(x => x.v), itemStyle: { color: css("--accent"), borderRadius: 6 }, label: { show: true, position: "right" } }] }); }
      }, 30); }
    const sg = document.createElement("div"); sg.className = "sugs";
    if (extra.ver) { const b = document.createElement("button"); b.textContent = extra.cmpB ? "Ver la comparación en el dashboard" : "Ver este periodo en el dashboard";
      b.onclick = () => { irA(extra.tab || "explorador"); aplicarRango(extra.ver.a, extra.ver.b, extra.ver.lab);
        if (extra.cmpB) { if (!S.cmp) $("#btnComparar").click(); aplicar("B", "rango", [dayIso(extra.cmpB.a), dayIso(extra.cmpB.b)], extra.cmpB.lab); } }; sg.appendChild(b); }
    (extra.sugs || []).forEach(s => { const b = document.createElement("button"); b.textContent = s; b.onclick = () => preguntar(s); sg.appendChild(b); });
    if (sg.children.length) el.appendChild(sg);
  }
  $("#chat").scrollTop = $("#chat").scrollHeight;
}
function preguntar(q) { if (!q.trim()) return; chatMsg(esc(q), true); const r = responder(q); setTimeout(() => chatMsg(r.html, false, r), 220); $("#pregunta").value = ""; }
RENDER.lab = () => {
  if (RENDER.lab.ok) return; RENDER.lab.ok = true;
  const ej = ["no puedo inscribirme, otra vez se cayó el sistema 😭", "gracias lic, enseña muy bien 👏", "excelente atención de kardex como siempre 🙃", "alguien sabe si habilitaron cálculo 2??", "no está mal la materia, tranqui", "me jaló en el final, ni modo :'v"];
  $("#ejemplosLab").innerHTML = ej.map(e => `<button>${esc(e)}</button>`).join("");
  $$("#ejemplosLab button").forEach((b, i) => b.onclick = () => { $("#txtAnaliza").value = ej[i]; renderAnalisis(); });
  $("#txtAnaliza").addEventListener("input", renderAnalisis); renderAnalisis();
  chatMsg(`Asistente basado en reglas. Responde consultas sobre el chat entre el ${fdia(0)} y el ${fdia(NDAYS - 1)}. Ejemplos:`, false, { sugs: EJ_INICIO });
  $("#btnPreguntar").onclick = () => preguntar($("#pregunta").value); $("#pregunta").addEventListener("keydown", e => e.key === "Enter" && preguntar(e.target.value));
  $("#capacidades").innerHTML = INTENT.filter(o => !o.oculto).map(o => `<div><b>${o.titulo}</b>${esc(o.desc)}<br><span class="mono" style="font-size:10.5px;color:var(--ink3)">Palabras clave: ${esc(o.claves)}</span><br><button data-q="${esc(o.ej)}">${esc(o.ej)}</button></div>`).join("")
    + `<div><b>Saludos y cierre</b>Responde a "hola" con una presentación y a "gracias" con un cierre breve.<br><button data-q="Hola">Hola</button></div>`
    + `<div class="no"><b>No puede responder</b>${NO_PUEDE.map(t => "· " + esc(t)).join("<br>")}</div>`;
  $$("#capacidades button").forEach(b => b.onclick = () => { preguntar(b.dataset.q); $("#chat").scrollIntoView({ behavior: "smooth", block: "center" }); });
};

/* ---------- 12 Bitácora y datos ---------- */
let faseBit = "";
RENDER.bitacora = () => {
  const m = X.meta;
  const dsets = [["01_Dataset/", "ChatTelegramJSON.json", "Original (sucio)", `${fmt(m.registros)} registros · JSON`, "Export de Telegram Desktop en formato JSON legible por máquina. No se modifica nunca.", "#F26B1D"],
    ["01_Dataset/", "01_dataset_original.csv", "Original en tabla", `${fmt(m.registros)} filas × ${X.datasets.original} columnas`, "El JSON aplanado sin pérdida (F01): todas las claves como columnas y los campos anidados como texto JSON. Auditoría de no pérdida superada.", "#E0A100"],
    ["01_Dataset/", "02_dataset_limpio.csv", "Limpio", `${fmt(m.mensajes)} filas × ${X.datasets.limpio} columnas`, `Sin ${fmt(m.avisos)} avisos del sistema ni ${fmt(m.publicidad)} mensajes de publicidad (F03). Texto normalizado, emojis y jerga en columnas propias, autor anonimizado y variables de tiempo.`, "#17A34A"],
    ["01_Dataset/", "03_dataset_sentimientos.csv", "Limpio + sentimientos", `${fmt(m.mensajes)} filas × ${X.datasets.sentimientos} columnas`, "El dataset limpio con sentimiento dominante, polaridad, ironía y los 15 puntajes (F07).", "#9333EA"],
    ["01_Dataset/", "04_registros_excluidos.csv", "Excluidos", `${fmt(m.avisos + m.publicidad)} filas`, "Cada registro excluido con su motivo: aviso del sistema o la regla de publicidad que cumplió (R1 a R5).", "#E11D48"],
    ["05_Bitacora/privado/", "tabla_anonimizacion.csv", "Uso interno", `${fmt(m.miembros)} alias`, "Correspondencia entre cada alias (Miembro 0001...) y su nombre de Telegram, para identificar a un miembro si el docente lo solicita. No se publica en el dashboard ni se sube a GitHub.", "#475569"]];
  $("#datasets").innerHTML = dsets.map(([dir, n, e, f, d, c]) => `<div class="card c4" style="border-top:5px solid ${c}"><div style="font:500 10.5px var(--mono);color:var(--ink3)">${dir}</div><h3 style="font-size:21px">${n}</h3><span class="tag" style="background:${c}">${e}</span><p class="sub" style="margin-top:8px"><b>${f}</b><br>${esc(d)}</p></div>`).join("");
  $("#filtroFaseBit").innerHTML = `<button data-f="" style="${!faseBit ? "background:var(--accent);color:#fff" : ""}">Todas</button>` + X.fases.map(f => `<button data-f="${f.cod}" style="border-color:${f.color};${faseBit === f.cod ? `background:${f.color};color:#fff` : ""}">${f.cod}</button>`).join("")
    + `<button data-f="sinfig" style="${faseBit === "sinfig" ? "background:var(--accent);color:#fff" : ""}">Ocultar figuras</button>`;
  $$("#filtroFaseBit button").forEach(b => b.onclick = () => { faseBit = b.dataset.f; RENDER.bitacora(); });
  const pint = () => { const q = normBase($("#buscarBit").value || ""); const rows = X.bitacora.filter(b => (!faseBit || (faseBit === "sinfig" ? !String(b.paso).startsWith("Figura ") : b.fase.startsWith(faseBit))) && (!q || normBase(Object.values(b).join(" ")).includes(q)));
    $("#tablaBit").innerHTML = `<table class="t"><tr><th>ID</th><th>Fase</th><th>Paso</th><th>Acción y detalle</th><th>Filas</th><th>Archivo</th><th>Justificación</th></tr>${rows.map(b => { const f = X.fases.find(x => b.fase.startsWith(x.cod));
      return `<tr><td class="mono">${b.id}</td><td><span class="tag" style="background:${f ? f.color : "#475569"}">${b.fase.slice(0, 3)}</span></td><td><b>${esc(b.paso)}</b><br><span class="mono" style="font-size:10px;color:var(--ink3)">${esc(b.fecha_hora)}</span></td><td><b>${esc(b.accion)}</b><br>${esc(b.detalle)}</td><td class="mono">${b.filas_antes && b.filas_antes !== "NA" ? fmt(+b.filas_antes) + " → " : ""}${b.filas_despues && b.filas_despues !== "NA" ? fmt(+b.filas_despues) : ""}</td><td class="mono" style="font-size:11px">${esc(b.archivo)}</td><td>${esc(b.justificacion)}</td></tr>`; }).join("")}</table>`; };
  $("#buscarBit").oninput = pint; pint();
  const cr = X.corridas || []; const cols = cr.length ? Object.keys(cr[0]) : [];
  $("#tablaCorr").innerHTML = cr.length ? `<table class="t"><tr>${cols.map(c => `<th>${esc(c)}</th>`).join("")}</tr>${cr.map(r => `<tr>${cols.map(c => `<td class="mono" style="font-size:11px">${esc(r[c])}</td>`).join("")}</tr>`).join("")}</table>` : `<div class="vacio">Ejecutar EJECUTAR_TODO.R para registrar corridas.</div>`;
  const pf = () => { const q = normBase($("#buscarFig").value || ""); const rows = X.figuras.filter(f => !q || normBase(f.titulo + " " + f.descripcion + " " + f.archivo).includes(q));
    $("#tablaFig").innerHTML = `<table class="t"><tr><th>Figura</th><th>Título</th></tr>${rows.map(f => `<tr style="cursor:zoom-in" data-a="${f.archivo}"><td class="mono">${f.fase}-${String(f.nro).padStart(2, "0")}</td><td><b>${esc(f.titulo)}</b><br><span style="color:var(--ink2)">${esc(f.descripcion.slice(0, 160))}...</span></td></tr>`).join("")}</table>`;
    $$("#tablaFig tr[data-a]").forEach(tr => tr.onclick = () => abrirLB(tr.dataset.a)); };
  $("#buscarFig").oninput = pf; pf();
};

/* ---------- 13 Diccionarios ---------- */
let dicSent = -1;
RENDER.diccionarios = () => {
  const cuenta = new Array(15).fill(0), cl = new Array(15).fill(0); X.dic_sent.forEach(d => { cuenta[d.s]++; if (d.o.startsWith("Clase")) cl[d.s]++; });
  $("#sent15").innerHTML = SENT.map((s, i) => `<div class="card c3" style="border-left:5px solid ${s.color};padding:14px 16px"><div style="display:flex;gap:10px;align-items:center"><img src="${s.img}" style="width:40px;height:40px" alt=""><div><h3 style="font-size:22px;margin:0">${s.nombre}</h3>
    <span class="tag" style="background:${s.polaridad > 0 ? POL_COL.pos : s.polaridad < 0 ? POL_COL.neg : POL_COL.neu}">polaridad ${s.polaridad > 0 ? "+1" : s.polaridad}</span></div></div><p class="sub" style="margin:8px 0 4px">${esc(s.definicion)}</p><div class="mono" style="font-size:11px;color:var(--ink3)">${cuenta[i]} términos (${cl[i]} de clase, ${cuenta[i] - cl[i]} ampliados)</div></div>`).join("");
  $("#pickSentDic").innerHTML = `<span style="background:#16131F;cursor:pointer;${dicSent < 0 ? "outline:3px solid rgba(22,19,31,.3)" : ""}" data-i="-1">Todos</span>` + SENT.map((s, i) => `<span style="background:${s.color};cursor:pointer;${i === dicSent ? "outline:3px solid rgba(22,19,31,.35)" : ""}" data-i="${i}">${s.nombre}</span>`).join("");
  $$("#pickSentDic span").forEach(sp => sp.onclick = () => { dicSent = +sp.dataset.i; RENDER.diccionarios(); });
  const pd = () => { const q = normBase($("#buscarDic").value || ""); const rows = X.dic_sent.filter(d => (dicSent < 0 || d.s === dicSent) && (!q || d.t.includes(q))).sort((a, b) => b.m - a.m);
    $("#tablaDic").innerHTML = `<table class="t"><tr><th>Término</th><th>Sentimiento</th><th>Tipo</th><th>Origen</th><th>Mensajes</th></tr>${rows.slice(0, 600).map(d => `<tr><td><b>${esc(d.t)}</b></td><td><span class="tag" style="background:${S_COL[d.s]}">${S_NOM[d.s]}</span></td><td>${d.k}</td><td>${esc(d.o)}</td><td class="mono">${fmt(d.m)}</td></tr>`).join("")}</table>`; };
  $("#buscarDic").oninput = pd; pd();
  const pe = () => { const q = normBase($("#buscarEmo").value || ""); const rows = EMO.filter(e => !q || normBase(`${e.nombre} ${e.sig} ${e.fam}`).includes(q) || e.e === $("#buscarEmo").value.trim()).sort((a, b) => (b.txt + b.rea + b.stk) - (a.txt + a.rea + a.stk));
    $("#tablaEmo").innerHTML = `<table class="t"><tr><th></th><th>Significado en el chat</th><th>Usos</th></tr>${rows.slice(0, 300).map(e => `<tr><td>${imgEmo(e.e, "width:34px;height:34px;font-size:28px")}</td><td><b>${esc(e.nombre)}</b> · <span style="color:var(--ink3)">${esc(e.fam)}</span><br>${esc(e.sig)}${e.s >= 0 ? ` <span class="tag" style="background:${S_COL[e.s]}">${S_NOM[e.s]}</span>` : ""}</td><td class="mono">${fmt(e.txt + e.rea + e.stk)}</td></tr>`).join("")}</table>`; };
  $("#buscarEmo").oninput = pe; pe();
};

/* ---------- 14 Resultados ---------- */
RENDER.resultados = () => {
  $("#hallazgosRes").innerHTML = X.hallazgos.map(h => `<div class="card curioso c4"><div class="cat">${esc(h.categoria)}</div><div class="val">${hallazgoVal(h)}</div><div class="tit">${esc(h.titulo)}</div><p>${esc(h.texto)}</p></div>`).join("");
  const nBit = X.bitacora.filter(b => !String(b.paso).startsWith("Figura ")).length;
  const ev = [["F01", "JSON aplanado sin pérdida y auditado (01_dataset_original.csv)."], ["F03", "Publicidad y avisos excluidos; texto normalizado en 8 pasos (02_dataset_limpio.csv)."], ["F06", `${fmt(X.dic_sent.length)} términos, ${EMO.length} emojis y ${X.jerga.length} expresiones en los diccionarios.`],
    ["F07", "Distribución, evolución y perfiles de los 15 sentimientos para cualquier rango de fechas."], ["F05 · F08 · F09", `${X.modelos.k} tópicos LDA, TF-IDF, temas latentes por mes y ${fmt(X.docentes.length)} docentes reconocidos.`],
    ["F10", "Causas de la incertidumbre, mejor hora para preguntar, días pico y red de respuestas."], ["F06 · F07 · F10", `Kappa ${X.modelos.kappa}, coincidencia con el ESR ${pct(X.modelos.esr)}, pronóstico ${X.modelos.pron_mejor} y clasificador con F1 ${Math.max(...X.modelos.cv.map(c => c.f1)).toFixed(2)}.`],
    ["F11 · Dashboard", `${nBit} pasos y ${X.figuras.length} figuras registrados en la bitácora, y este dashboard interactivo.`]];
  $("#tablaObj").innerHTML = `<table class="t"><tr><th>#</th><th>Objetivo específico</th><th>Fase</th><th>Evidencia</th></tr>${X.objetivos.especificos.map((o, i) => `<tr><td class="mono">${i + 1}</td><td>${esc(o)}</td><td class="mono">${ev[i] ? ev[i][0] : ""}</td><td>${esc(ev[i] ? ev[i][1] : "")}</td></tr>`).join("")}</table>`;
  $("#equipo").innerHTML = X.equipo.map(p => `<div class="card persona"><div class="ini">${esc(p.apellidos[0])}${esc(p.nombres[0])}</div><b>${esc(p.apellidos)}</b><span>${esc(p.nombres)}</span></div>`).join("");
};

/* ---------- Efectos: cursor, fondo, tarjetas, ondas ---------- */
function efectos() {
  document.addEventListener("click", e => { const b = e.target.closest(".btn,.chip,.tab,.sugs button,.modos button"); if (!b) return; const r = b.getBoundingClientRect(), s = document.createElement("span"); s.className = "ripple";
    const d = Math.max(r.width, r.height); Object.assign(s.style, { width: d + "px", height: d + "px", left: (e.clientX - r.left - d / 2) + "px", top: (e.clientY - r.top - d / 2) + "px" }); b.style.position = b.style.position || "relative"; b.style.overflow = "hidden"; b.appendChild(s); setTimeout(() => s.remove(), 650); });
  const dot = $("#cur-dot"), ring = $("#cur-ring"); let mx = innerWidth / 2, my = innerHeight / 2, rx = mx, ry = my;
  if (!matchMedia("(hover:hover) and (pointer:fine)").matches) { dot.style.display = ring.style.display = "none"; return; }
  document.body.classList.add("no-cursor");
  document.addEventListener("mousemove", e => { mx = e.clientX; my = e.clientY; dot.style.transform = `translate(${mx}px,${my}px) translate(-50%,-50%)`;
    $$(".blob").forEach((b, i) => { const k = (i + 1) * 14; b.style.translate = `${(mx / innerWidth - .5) * k * 2}px ${(my / innerHeight - .5) * k * 2}px`; }); });
  (function loop() { rx += (mx - rx) * .18; ry += (my - ry) * .18; ring.style.transform = `translate(${rx}px,${ry}px) translate(-50%,-50%)`; requestAnimationFrame(loop); })();
  document.addEventListener("mouseover", e => ring.classList.toggle("grande", !!e.target.closest("button,a,select,input,textarea,.emo,figure,.chip,[data-d],.nodo .card,canvas,.nube-emo img")));
  document.addEventListener("mousemove", e => { const c = e.target.closest(".card"); if (!c) return; const r = c.getBoundingClientRect(); c.style.setProperty("--x", (e.clientX - r.left) + "px"); c.style.setProperty("--y", (e.clientY - r.top) + "px"); });
}

/* ---------- Inicio de la app ---------- */
function pie() {
  $("#pie").innerHTML = `<b>${esc(X.meta.proyecto)}</b> · ${esc(X.meta.materia)} · Integrantes: ${X.equipo.map(p => esc(p.nombre)).join(" · ")}.<br>
    Datos: chat de Telegram "${esc(X.meta.chat)}" (${fdia(0)} – ${fdia(NDAYS - 1)}), autores anonimizados; teléfonos, correos y menciones ocultos. Generado con R el ${esc(X.meta.generado)} (versión ${esc(X.meta.version)}).<br>
    Créditos: emojis <a href="emoji/LICENSE_FluentEmoji_MIT.txt">Fluent Emoji 3D</a> (Microsoft, MIT) · Emoji Sentiment Ranking (Kralj Novak et al., 2015, CC BY-SA 4.0) · nombres CLDR (Unicode) · gráficos Apache ECharts 5.5 (Apache 2.0) · resaltado Prism (MIT) · fuentes Instrument Serif, Manrope, Fraunces y JetBrains Mono (OFL).`;
}
function init() {
  initNav(); initFiltro(); efectos(); pie();
  $("#minDoc").onchange = () => RENDER.docentes();
  const ini = (location.hash || "#inicio").slice(1);
  irA(RENDER[ini] ? ini : "inicio");
  setTimeout(() => { moverBurbuja($(".tab.on")); flechas(); $("#carga").classList.add("fuera"); }, 250);
  window.addEventListener("resize", () => { moverBurbuja($(".tab.on")); flechas(); });
  window.addEventListener("hashchange", () => { const h = location.hash.slice(1); if (RENDER[h] && h !== S.tab) irA(h); });
}
document.readyState === "loading" ? document.addEventListener("DOMContentLoaded", init) : init();
})();
