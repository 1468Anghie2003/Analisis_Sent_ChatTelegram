# Análisis de Sentimientos: chat "Curso de Temporada" (carpeta GrupoXDMSent)

Proyecto de Minería de Datos (Carrera de Informática, UMSA). Pipeline reproducible en R que extrae, limpia y analiza el chat de Telegram del Curso de Temporada (15 sentimientos de clase, emojis, jerga, tópicos LDA, entidades, patrones y predicción). Incluye un dashboard web interactivo.

**Equipo:** Alba Orellana Ethan · Claros Flores Eliane Sarai · López Salgueiro Jhamil · Mamani Castro Angela Luz

## Estructura

| Carpeta | Contenido |
|---|---|
| `01_Dataset/` | JSON exportado de Telegram (entrada) y datasets generados: original, limpio, con sentimientos y registros excluidos |
| `02_Codigo/` | Configuración, diccionario y motor de sentimientos, y un script por fase (F01 a F11) |
| `03_Diccionarios/` | Diccionarios de sentimientos, emojis, jerga y entidades (CSV y Excel), stopwords e imágenes de emojis |
| `04_Resultados/` | `figuras/` (una subcarpeta por fase, numeradas Fxx-nn), `tablas/`, `modelos/`, `validacion/`, `hallazgos.csv` |
| `05_Bitacora/` | `Bitacora_GrupoXDMSent.xlsx`, `bitacora.csv`, `corridas.csv`, `log_consola.txt` y `privado/tabla_anonimizacion.csv` (alias y nombre real; uso interno, no se publica) |
| `docs/` | Dashboard web (HTML + JS); GitHub Pages lo publica desde aquí |
| `EJECUTAR_TODO.R` | Corre las 11 fases en orden y registra la corrida |
| `GENERAR_DASHBOARD.R` | Exporta los resultados a `docs/data/` y copia figuras y emojis |

## Fases

| Fase | Script | Tema |
|---|---|---|
| F01 | `F01_extraccion.R` | Extracción: JSON a tabla sin pérdida |
| F02 | `F02_exploracion.R` | Exploración del dataset original |
| F03 | `F03_limpieza.R` | Preprocesamiento y limpieza (solo se quita publicidad y avisos del sistema) |
| F04 | `F04_tokenizacion.R` | Tokenización, n-gramas, caracteres |
| F05 | `F05_stopwords_tfidf.R` | Stopwords, shallow linguistics, TF-IDF |
| F06 | `F06_diccionarios.R` | Diccionarios de sentimientos, emojis y jerga |
| F07 | `F07_sentimientos.R` | Análisis de los 15 sentimientos y validación |
| F08 | `F08_topicos.R` | Modelado de tópicos (LDA, comparación con tm) |
| F09 | `F09_entidades_relaciones.R` | NER por reglas, lugares, docentes, reglas de asociación |
| F10 | `F10_patrones_prediccion.R` | Patrones ocultos, causas, pronóstico, clasificador |
| F11 | `F11_cierre.R` | Hallazgos automáticos y bitácora en Excel |

## Cómo ejecutar (VS Code, terminal en la carpeta del proyecto)

Requisitos: R 4.3 o superior. Los paquetes que falten se instalan solos la primera vez.

```bash
# 1. Pipeline completo (unos 4 minutos)
Rscript EJECUTAR_TODO.R

# 2. Datos del dashboard
Rscript GENERAR_DASHBOARD.R

# 3. Ver el dashboard en localhost
python -m http.server 8000 -d docs
# abrir http://localhost:8000
```

Si `Rscript` no se reconoce en Windows, usar la ruta completa, por ejemplo:
`& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" EJECUTAR_TODO.R`

El dashboard también abre con doble clic en `docs/index.html` (no necesita servidor).

## Cambiar de dataset

Reemplazar el JSON en `01_Dataset/` por otro export de Telegram (formato JSON legible por máquina) y volver a correr los dos comandos. Fechas, periodos, figuras, hallazgos y dashboard se recalculan solos. Parámetros en `02_Codigo/00_config.R` (`CFG`).

## Publicar en GitHub Pages

1. Subir el repositorio a GitHub.
2. Settings → Pages → Source: *Deploy from a branch* → Branch `main`, carpeta `/docs`.
3. El dashboard queda en `https://<usuario>.github.io/<repositorio>/`.

Antes de publicar: en `00_config.R` poner `CFG$anonimizar_docentes <- TRUE` y volver a correr ambos scripts. El `.gitignore` deja fuera el JSON original, los CSV con texto, los checkpoints y la carpeta `05_Bitacora/privado`.

## Créditos

Emojis Fluent Emoji 3D (Microsoft, MIT) · Emoji Sentiment Ranking (Kralj Novak et al., 2015, CC BY-SA 4.0) · Apache ECharts (Apache 2.0) · Prism (MIT) · fuentes Instrument Serif, Fraunces, Manrope y JetBrains Mono (OFL).
