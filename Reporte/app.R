# =============================================================================
# app.R — Dashboard Shiny · Fundación Pachamama
# Conservación Biocultural Achuar · Cuencas del Pastaza
#
# Estructura:
#   ui()   → layout con bslib + filtros globales + 7 pestañas
#   server() → lógica reactiva por pestaña
#
# Ejecutar desde la carpeta Reporte/:
#   shiny::runApp("app.R", port = 3838, launch.browser = TRUE)
# =============================================================================

suppressPackageStartupMessages({
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
library(forcats)
library(scales)
library(DT)
library(glue)
library(purrr)
                          })

# ══════════════════════════════════════════════════════════════════════════════
# 0. CONFIGURACIÓN GLOBAL
# ══════════════════════════════════════════════════════════════════════════════

RUTA_DATOS <- "../Data"

PAL <- c(
  bosque   = "#2D6A4F",
  medio    = "#40916C",
  claro    = "#74C69D",
  suave    = "#B7E4C7",
  tierra   = "#8B5E3C",
  t_claro  = "#D4A373",
  amarillo = "#FFD166",
  naranja  = "#EF8C4B",
  azul     = "#264653",
  gris     = "#6B7280",
  rojo     = "#DC2626"
)

tema_plot <- function() {
  list(
    plot_bgcolor  = "#FAFAFA",
    paper_bgcolor = "white",
    font          = list(family = "Inter, Segoe UI, sans-serif", color = "#374151"),
    margin        = list(t = 50, r = 20, b = 40, l = 20)
  )
}

layout_base <- function(p, titulo = "", ...) {
  p |> layout(
    title       = list(text = titulo, font = list(color = PAL["bosque"], size = 15)),
    plot_bgcolor  = "#FAFAFA",
    paper_bgcolor = "white",
    font          = list(family = "Inter, Segoe UI, sans-serif"),
    ...
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 1. CARGA Y PREPARACIÓN DE DATOS
# ══════════════════════════════════════════════════════════════════════════════

cargar_datos <- function() {
  leer <- function(f) {
    path <- file.path(RUTA_DATOS, f)
    if (file.exists(path)) read.csv(path, stringsAsFactors = FALSE) else data.frame()
  }

  asm <- leer("Asambleas.csv") |>
    mutate(
      Fecha         = as.Date(Fecha),
      Anio          = year(Fecha),
      YearMon       = floor_date(Fecha, "month"),
      Participantes  = coalesce(as.numeric(Numero_hombres), 0) +
                       coalesce(as.numeric(Numero_mujeres), 0),
      Hombres        = coalesce(as.numeric(Numero_hombres), 0),
      Mujeres        = coalesce(as.numeric(Numero_mujeres), 0),
      Tipo_asamblea  = coalesce(Tipo_asamblea, "Sin clasificar"),
      Comunidad      = str_trim(Comunidad)
    ) |> filter(!is.na(Fecha))

  cap <- leer("Capacitaciones.csv") |>
    mutate(
      Fecha         = as.Date(Fecha),
      Anio          = year(Fecha),
      YearMon       = floor_date(Fecha, "month"),
      Participantes  = coalesce(as.numeric(Numero_hombres), 0) +
                       coalesce(as.numeric(Numero_mujeres), 0),
      Hombres        = coalesce(as.numeric(Numero_hombres), 0),
      Mujeres        = coalesce(as.numeric(Numero_mujeres), 0),
      Horas          = coalesce(as.numeric(Numero_horas_capacitacion), 0),
      Comunidad      = str_trim(Comunidad),
      Tema = case_when(
        str_detect(str_to_lower(Capacidad_adquirida), "dron|drone")               ~ "Drone / Fotografía",
        str_detect(str_to_lower(Capacidad_adquirida), "cámara|camara|trampa")     ~ "Cámaras trampa",
        str_detect(str_to_lower(Capacidad_adquirida), "gps")                      ~ "GPS",
        str_detect(str_to_lower(Capacidad_adquirida), "monitoreo|monitor")        ~ "Monitoreo",
        str_detect(str_to_lower(Capacidad_adquirida), "piscicult|piscin|alivines")~ "Piscicultura",
        str_detect(str_to_lower(Capacidad_adquirida), "avicult|pollo|aves")       ~ "Avicultura",
        str_detect(str_to_lower(Capacidad_adquirida), "comunicac|radio|audio")    ~ "Comunicación",
        str_detect(str_to_lower(Capacidad_adquirida), "acuerdo|manejo|recurso")   ~ "Manejo de recursos",
        str_detect(str_to_lower(Capacidad_adquirida), "arquitectura|casa|tipica") ~ "Arquitectura tradicional",
        str_detect(str_to_lower(Capacidad_adquirida), "limite|lindero|territorio") ~ "Territorio / Límites",
        str_detect(str_to_lower(Capacidad_adquirida), "forest|watcher")           ~ "Forest Watcher",
        TRUE ~ "Otros"
      )
    ) |> filter(!is.na(Fecha))

  seg  <- leer("df_seguridad_alimentaria.csv")
  ins  <- leer("df_entrega_insumos.csv") |>
    mutate(N_insumos = as.numeric(N_insumos)) |>
    filter(!is.na(Insumos_entregados) & Insumos_entregados != "" & !is.na(N_insumos))

  jag  <- leer("Evidencias_jaguar.csv")
  soc  <- leer("Socializaci\u00f3n_acuerdos.csv")
  if (nrow(soc) == 0) soc <- leer("Socializacion_acuerdos.csv")

  list(asm = asm, cap = cap, seg = seg, ins = ins, jag = jag, soc = soc)
}

DATOS <- cargar_datos()

# Valores para filtros
COMUNIDADES <- sort(unique(c(
  DATOS$asm$Comunidad, DATOS$cap$Comunidad
)))
COMUNIDADES <- COMUNIDADES[!is.na(COMUNIDADES) & COMUNIDADES != ""]

ANIOS <- sort(unique(c(DATOS$asm$Anio, DATOS$cap$Anio)))
ANIOS <- ANIOS[!is.na(ANIOS)]
ANIO_MIN <- if (length(ANIOS) > 0) min(ANIOS) else 2023
ANIO_MAX <- if (length(ANIOS) > 0) max(ANIOS) else 2026

# ══════════════════════════════════════════════════════════════════════════════
# 2. HELPERS UI
# ══════════════════════════════════════════════════════════════════════════════

kpi_box <- function(id_val, label, icon_name, color = PAL["bosque"]) {
  div(
    class = "kpi-card",
    style = glue("border-top: 4px solid {color};"),
    div(class = "kpi-icon",
        tags$i(class = glue("fas {icon_name}"), style = glue("color:{color}"))),
    div(class = "kpi-val", textOutput(id_val, inline = TRUE)),
    div(class = "kpi-lbl", label)
  )
}

semaforo <- function(val, meta_ok, meta_warn, unidad = "%") {
  if (is.na(val)) return(tags$span("—", class = "badge bg-secondary"))
  if (val >= meta_ok)   return(tags$span(glue("{val}{unidad} ✅"), class = "badge", style = "background:#D1FAE5;color:#065F46"))
  if (val >= meta_warn) return(tags$span(glue("{val}{unidad} 🟡"), class = "badge", style = "background:#FEF3C7;color:#92400E"))
  tags$span(glue("{val}{unidad} 🔴"), class = "badge", style = "background:#FEE2E2;color:#991B1B")
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- page_navbar(
  title = tags$span(
    tags$img(
      src = "https://img.icons8.com/fluency/32/leaf.png",
      height = "28px",
      style = "margin-right:8px; vertical-align:middle;"
    ),
    "Monitoreo Biocultural · Pachamama"
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2D6A4F",
    secondary = "#40916C",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  bg = "#2D6A4F",
  fillable = FALSE,
  collapsible = TRUE,
  
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$style(HTML("
      body { background: #F9FAFB; font-family: 'Inter', sans-serif; }

      /* Sidebar filtros */
      .filtros-panel {
        background: white;
        border-right: 1px solid #E5E7EB;
        padding: 1.25rem 1rem;
        min-height: 100vh;
      }
      .filtros-panel h6 {
        color:#2D6A4F;
        font-weight:700;
        font-size:.75rem;
        text-transform:uppercase;
        letter-spacing:.06em;
        margin-bottom:.75rem;
      }

      /* KPIs */
      .kpi-grid {
        display:grid;
        grid-template-columns:repeat(auto-fit,minmax(140px,1fr));
        gap:1rem;
        margin-bottom:1.5rem;
      }
      .kpi-card {
        background:white;
        border-radius:.6rem;
        padding:1rem;
        box-shadow:0 1px 4px rgba(0,0,0,.08);
        text-align:center;
        transition:transform .15s;
      }
      .kpi-card:hover {
        transform:translateY(-2px);
        box-shadow:0 4px 12px rgba(0,0,0,.12);
      }
      .kpi-icon {
        font-size:1.6rem;
        margin-bottom:.4rem;
      }
      .kpi-val {
        font-size:1.9rem;
        font-weight:700;
        color:#111827;
        line-height:1.1;
      }
      .kpi-lbl {
        font-size:.72rem;
        color:#6B7280;
        text-transform:uppercase;
        letter-spacing:.04em;
        margin-top:.25rem;
      }

      /* Secciones */
      .seccion-titulo {
        color:#2D6A4F;
        font-weight:700;
        border-bottom:2px solid #B7E4C7;
        padding-bottom:.4rem;
        margin:1.5rem 0 1rem;
        font-size:1.05rem;
      }
      .card {
        border:none;
        box-shadow:0 1px 4px rgba(0,0,0,.06);
        border-radius:.6rem;
        margin-bottom:1rem;
      }
      .card-header {
        background:#F0FDF4;
        color:#2D6A4F;
        font-weight:600;
        border-bottom:1px solid #D1FAE5;
      }

      /* Tabla indicadores */
      .ind-table td, .ind-table th {
        vertical-align:middle;
        font-size:.875rem;
      }
      .ind-table th {
        background:#2D6A4F !important;
        color:white !important;
      }

      /* Alerta info */
      .info-box {
        background:#EFF6FF;
        border-left:4px solid #3B82F6;
        padding:.75rem 1rem;
        border-radius:0 .4rem .4rem 0;
        font-size:.85rem;
        margin-bottom:1rem;
      }

      /* Nuevo indicador badge */
      .badge-new {
        background:#FEF3C7;
        color:#92400E;
        font-size:.7rem;
        padding:.2rem .5rem;
        border-radius:999px;
        font-weight:600;
      }

      /* Plotly hover */
      .js-plotly-plot {
        border-radius:.5rem;
      }

      /* scrollable tabs */
      .nav-tabs {
        flex-wrap: nowrap;
        overflow-x: auto;
      }
    "))
  ),
  
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-house me-1"), "Resumen"),
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 220,
        class = "filtros-panel",
        tags$h6("🎛 Filtros"),
        selectInput(
          "f_comunidad", "Comunidad",
          choices = c("Todas", COMUNIDADES),
          selected = "Todas"
        ),
        sliderInput(
          "f_anios", "Período",
          min = ANIO_MIN, max = ANIO_MAX,
          value = c(ANIO_MIN, ANIO_MAX),
          step = 1, sep = "", ticks = FALSE
        ),
        hr(),
        tags$small(
          class = "text-muted",
          "Filtros aplicados a todas las pestañas"
        ),
        hr(),
        actionButton(
          "btn_reset", "Reiniciar filtros",
          class = "btn btn-outline-secondary btn-sm w-100"
        )
      ),
      
      div(
        class = "p-3",
        
        div(
          class = "kpi-grid",
          kpi_box("kpi_asm",   "Asambleas",           "fa-users",          PAL["bosque"]),
          kpi_box("kpi_cap",   "Capacitaciones",      "fa-graduation-cap", PAL["medio"]),
          kpi_box("kpi_part",  "Participaciones",     "fa-people-group",   PAL["tierra"]),
          kpi_box("kpi_horas", "Horas capacitación",  "fa-clock",          PAL["naranja"]),
          kpi_box("kpi_com",   "Comunidades activas", "fa-map-marker-alt", PAL["azul"]),
          kpi_box("kpi_eqgen", "% Mujeres asambleas", "fa-venus",          PAL["t_claro"])
        ),
        
        card(
          card_header("📈 Actividad mensual — Asambleas y Capacitaciones"),
          plotlyOutput("res_serie", height = "300px")
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header("🏘 Actividad por Comunidad"),
              plotlyOutput("res_comunidad", height = "260px")
            )
          ),
          column(
            6,
            card(
              card_header("📅 Distribución por Año"),
              plotlyOutput("res_anual", height = "260px")
            )
          )
        ),
        
        card(
          card_header("🗺 Mapa de Calor — Actividad Mensual por Comunidad"),
          plotlyOutput("res_heatmap", height = "280px")
        )
      )
    )
  ),
  
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-users me-1"), "Gobernanza"),
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 220,
        class = "filtros-panel",
        tags$h6("🎛 Filtros"),
        selectInput(
          "a_comunidad", "Comunidad",
          choices = c("Todas", COMUNIDADES), selected = "Todas"
        ),
        sliderInput(
          "a_anios", "Período",
          min = ANIO_MIN, max = ANIO_MAX,
          value = c(ANIO_MIN, ANIO_MAX), step = 1, sep = ""
        ),
        hr(),
        radioButtons(
          "a_tipo_grafico", "Serie temporal",
          choices = c("Barras apiladas" = "bar", "Líneas" = "line"),
          selected = "bar"
        )
      ),
      
      div(
        class = "p-3",
        
        div(
          class = "info-box",
          tags$b("Gobernanza Comunitaria — Asambleas"), " | Fuente: ",
          tags$code("Asambleas.csv")
        ),
        
        div(
          class = "kpi-grid",
          kpi_box("a_kpi_n",    "Asambleas",           "fa-calendar-check", PAL["bosque"]),
          kpi_box("a_kpi_ord",  "Ordinarias",          "fa-circle-check",   PAL["medio"]),
          kpi_box("a_kpi_ext",  "Extraordinarias",     "fa-bolt",           PAL["naranja"]),
          kpi_box("a_kpi_hom",  "Hombres",             "fa-mars",           PAL["azul"]),
          kpi_box("a_kpi_muj",  "Mujeres",             "fa-venus",          PAL["t_claro"]),
          kpi_box("a_kpi_prom", "Prom. participantes", "fa-chart-bar",      PAL["tierra"])
        ),
        
        fluidRow(
          column(
            8,
            card(
              card_header("📅 Serie Temporal de Asambleas"),
              plotlyOutput("a_serie", height = "280px")
            )
          ),
          column(
            4,
            card(
              card_header("🏷 Tipo de Asamblea"),
              plotlyOutput("a_tipo", height = "280px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header("👥 Participación por Género y Comunidad"),
              plotlyOutput("a_genero", height = "260px")
            )
          ),
          column(
            6,
            card(
              card_header(tagList(
                "⚖️ Índice de Equidad de Género",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("a_equidad", height = "260px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header("📊 Asambleas por Monitor"),
              plotlyOutput("a_monitor", height = "260px")
            )
          ),
          column(
            6,
            card(
              card_header(tagList(
                "📆 Regularidad Mensual por Comunidad",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("a_regularidad", height = "260px")
            )
          )
        ),
        
        card(
          card_header("📋 Tabla de Asambleas (filtrable + exportable)"),
          DTOutput("a_tabla")
        )
      )
    )
  ),
  
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-graduation-cap me-1"), "Capacitaciones"),
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 220,
        class = "filtros-panel",
        tags$h6("🎛 Filtros"),
        selectInput(
          "c_comunidad", "Comunidad",
          choices = c("Todas", COMUNIDADES), selected = "Todas"
        ),
        sliderInput(
          "c_anios", "Período",
          min = ANIO_MIN, max = ANIO_MAX,
          value = c(ANIO_MIN, ANIO_MAX), step = 1, sep = ""
        ),
        hr(),
        checkboxGroupInput(
          "c_temas", "Temas a mostrar",
          choices = sort(unique(DATOS$cap$Tema)),
          selected = sort(unique(DATOS$cap$Tema))
        )
      ),
      
      div(
        class = "p-3",
        
        div(
          class = "info-box",
          tags$b("Generación de Capacidades"), " | Fuente: ",
          tags$code("Capacitaciones.csv")
        ),
        
        div(
          class = "kpi-grid",
          kpi_box("c_kpi_n",     "Sesiones",            "fa-chalkboard-teacher", PAL["bosque"]),
          kpi_box("c_kpi_horas", "Horas totales",       "fa-clock",              PAL["medio"]),
          kpi_box("c_kpi_part",  "Participantes",       "fa-users",              PAL["tierra"]),
          kpi_box("c_kpi_ic",    "Hrs / participante",  "fa-gauge-high",         PAL["naranja"]),
          kpi_box("c_kpi_temas", "Temas distintos",     "fa-tags",               PAL["azul"]),
          kpi_box("c_kpi_ef",    "Horas-persona total", "fa-fire",               PAL["t_claro"])
        ),
        
        fluidRow(
          column(
            8,
            card(
              card_header("📅 Sesiones por Mes"),
              plotlyOutput("c_serie", height = "280px")
            )
          ),
          column(
            4,
            card(
              card_header("🏷 Distribución por Tema"),
              plotlyOutput("c_temas_plot", height = "280px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header("⏫ Horas Acumuladas por Comunidad"),
              plotlyOutput("c_horas_acum", height = "260px")
            )
          ),
          column(
            6,
            card(
              card_header(tagList(
                "🔥 Esfuerzo de Capacitación (Horas × Personas)",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("c_esfuerzo", height = "260px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header(tagList(
                "📈 Intensidad de Capacitación por Comunidad",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("c_intensidad", height = "260px")
            )
          ),
          column(
            6,
            card(
              card_header(tagList(
                "👥 Ratio Género en Capacitaciones",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("c_genero", height = "260px")
            )
          )
        ),
        
        card(
          card_header("📋 Tabla de Capacitaciones"),
          DTOutput("c_tabla")
        )
      )
    )
  ),
  
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-seedling me-1"), "Seg. Alimentaria"),
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 220,
        class = "filtros-panel",
        tags$h6("🎛 Filtros"),
        selectInput(
          "s_comunidad", "Comunidad",
          choices = c("Todas", COMUNIDADES), selected = "Todas"
        ),
        sliderInput(
          "s_anios", "Período",
          min = ANIO_MIN, max = ANIO_MAX,
          value = c(ANIO_MIN, ANIO_MAX), step = 1, sep = ""
        )
      ),
      
      div(
        class = "p-3",
        
        div(
          class = "info-box",
          tags$b("Seguridad Alimentaria e Insumos"), " | Fuentes: ",
          tags$code("df_seguridad_alimentaria.csv"), " + ",
          tags$code("df_entrega_insumos.csv")
        ),
        
        div(
          class = "kpi-grid",
          kpi_box("s_kpi_registros", "Registros alimentarios", "fa-carrot",   PAL["bosque"]),
          kpi_box("s_kpi_insumos",   "Entregas de insumos",    "fa-box-open", PAL["tierra"]),
          kpi_box("s_kpi_tipos",     "Tipos de insumo",        "fa-list",     PAL["naranja"]),
          kpi_box("s_kpi_total",     "Unidades entregadas",    "fa-cubes",    PAL["azul"])
        ),
        
        fluidRow(
          column(
            7,
            card(
              card_header("📦 Insumos Entregados por Tipo (cantidad total)"),
              plotlyOutput("s_insumos_bar", height = "280px")
            )
          ),
          column(
            5,
            card(
              card_header("🥧 Composición de Insumos"),
              plotlyOutput("s_insumos_pie", height = "280px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header(tagList(
                "📊 Actividades Alimentarias por Tipo",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("s_actividades", height = "260px")
            )
          ),
          column(
            6,
            card(
              card_header(tagList(
                "🐟 Piscicultura vs Avicultura",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("s_prod_compare", height = "260px")
            )
          )
        ),
        
        card(
          card_header("📋 Tabla — Seguridad Alimentaria"),
          DTOutput("s_tabla_seg")
        ),
        card(
          card_header("📋 Tabla — Insumos"),
          DTOutput("s_tabla_ins")
        )
      )
    )
  ),
  
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-paw me-1"), "Jaguar"),
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 220,
        class = "filtros-panel",
        tags$h6("🎛 Filtros"),
        selectInput(
          "j_comunidad", "Comunidad",
          choices = c("Todas", COMUNIDADES), selected = "Todas"
        ),
        sliderInput(
          "j_anios", "Período",
          min = ANIO_MIN, max = ANIO_MAX,
          value = c(ANIO_MIN, ANIO_MAX), step = 1, sep = ""
        )
      ),
      
      div(
        class = "p-3",
        
        div(
          class = "info-box",
          tags$b("Evidencias de Jaguar"), " | Fuente: ",
          tags$code("Evidencias_jaguar.csv"),
          " — Registros de avistamientos, huellas y cámaras trampa"
        ),
        
        div(
          class = "kpi-grid",
          kpi_box("j_kpi_total",       "Total evidencias",   "fa-paw",         PAL["bosque"]),
          kpi_box("j_kpi_tipos",       "Tipos de evidencia", "fa-layer-group", PAL["medio"]),
          kpi_box("j_kpi_comunidades", "Comunidades",        "fa-map",         PAL["tierra"]),
          kpi_box("j_kpi_reciente",    "Último registro",    "fa-calendar",    PAL["azul"])
        ),
        
        fluidRow(
          column(
            5,
            card(
              card_header("🥧 Tipo de Evidencia"),
              plotlyOutput("j_tipo_pie", height = "280px")
            )
          ),
          column(
            7,
            card(
              card_header("📅 Evidencias por Mes"),
              plotlyOutput("j_serie", height = "280px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header(tagList(
                "📊 Evidencias por Comunidad",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("j_comunidad_bar", height = "260px")
            )
          ),
          column(
            6,
            card(
              card_header(tagList(
                "📆 Frecuencia Mensual Promedio",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("j_frecuencia", height = "260px")
            )
          )
        ),
        
        card(
          card_header("📋 Tabla de Evidencias"),
          DTOutput("j_tabla")
        )
      )
    )
  ),
  
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-handshake me-1"), "Socialización"),
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 220,
        class = "filtros-panel",
        tags$h6("🎛 Filtros"),
        selectInput(
          "so_comunidad", "Comunidad",
          choices = c("Todas", COMUNIDADES), selected = "Todas"
        ),
        sliderInput(
          "so_anios", "Período",
          min = ANIO_MIN, max = ANIO_MAX,
          value = c(ANIO_MIN, ANIO_MAX), step = 1, sep = ""
        )
      ),
      
      div(
        class = "p-3",
        
        div(
          class = "info-box",
          tags$b("Socialización de Acuerdos Comunitarios"), " | Fuente: ",
          tags$code("Socialización_acuerdos.csv")
        ),
        
        div(
          class = "kpi-grid",
          kpi_box("so_kpi_n",   "Socializaciones",  "fa-comments",   PAL["bosque"]),
          kpi_box("so_kpi_com", "Comunidades",      "fa-map-marker", PAL["tierra"]),
          kpi_box("so_kpi_mon", "Monitores activos","fa-user-check", PAL["azul"])
        ),
        
        fluidRow(
          column(
            8,
            card(
              card_header("📅 Socializaciones por Mes y Comunidad"),
              plotlyOutput("so_serie", height = "280px")
            )
          ),
          column(
            4,
            card(
              card_header("🥧 Por Comunidad"),
              plotlyOutput("so_pie", height = "280px")
            )
          )
        ),
        
        card(
          card_header(tagList(
            "📊 Temas Frecuentes en Socializaciones",
            tags$span("NUEVO", class = "badge-new ms-2")
          )),
          plotlyOutput("so_temas", height = "260px")
        ),
        
        card(
          card_header("📋 Tabla de Socializaciones"),
          DTOutput("so_tabla")
        )
      )
    )
  ),
  
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-chart-line me-1"), "Indicadores"),
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 220,
        class = "filtros-panel",
        tags$h6("🎛 Filtros"),
        selectInput(
          "i_comunidad", "Comunidad",
          choices = c("Todas", COMUNIDADES), selected = "Todas"
        ),
        sliderInput(
          "i_anios", "Período",
          min = ANIO_MIN, max = ANIO_MAX,
          value = c(ANIO_MIN, ANIO_MAX), step = 1, sep = ""
        ),
        hr(),
        div(
          class = "info-box",
          tags$b("Leyenda:"), br(),
          "✅ Meta cumplida", br(),
          "🟡 En proceso", br(),
          "🔴 Por debajo de meta"
        )
      ),
      
      div(
        class = "p-3",
        
        div(
          class = "info-box",
          "Indicadores calculados sobre los datos filtrados. Los marcados con ",
          tags$span("NUEVO", class = "badge-new"),
          " no estaban en los notebooks EDA originales."
        ),
        
        card(
          card_header("📊 Panel de Indicadores — Semáforo"),
          tableOutput("ind_tabla")
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header("🕸 Radar de Cumplimiento"),
              plotlyOutput("ind_radar", height = "380px")
            )
          ),
          column(
            6,
            card(
              card_header("📈 Evolución Anual de Indicadores Clave"),
              plotlyOutput("ind_evolucion", height = "380px")
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            card(
              card_header(tagList(
                "⚡ Índice de Actividad Compuesto (IAC)",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("ind_iac", height = "280px"),
              div(
                class = "p-2 text-muted small",
                "IAC = (Asambleas × 1 + Capacitaciones × 1.5 + Horas × 0.1) / Comunidades activas"
              )
            )
          ),
          column(
            6,
            card(
              card_header(tagList(
                "📉 Brecha de Equidad de Género por Comunidad",
                tags$span("NUEVO", class = "badge-new ms-2")
              )),
              plotlyOutput("ind_brecha_gen", height = "280px")
            )
          )
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# 4. SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Helpers de filtrado ────────────────────────────────────────────────────
  filtrar <- function(df, com_input, anios_input) {
    df <- df |> filter(Anio >= anios_input[1] & Anio <= anios_input[2])
    if (!is.null(com_input) && com_input != "Todas") {
      df <- df |> filter(str_detect(Comunidad, fixed(com_input, ignore_case = TRUE)))
    }
    df
  }

  # Sincronizar botón reset en el Resumen con todos los filtros
  observeEvent(input$btn_reset, {
    updateSelectInput(session, "f_comunidad",  selected = "Todas")
    updateSliderInput(session, "f_anios",      value    = c(ANIO_MIN, ANIO_MAX))
    updateSelectInput(session, "a_comunidad",  selected = "Todas")
    updateSliderInput(session, "a_anios",      value    = c(ANIO_MIN, ANIO_MAX))
    updateSelectInput(session, "c_comunidad",  selected = "Todas")
    updateSliderInput(session, "c_anios",      value    = c(ANIO_MIN, ANIO_MAX))
    updateSelectInput(session, "s_comunidad",  selected = "Todas")
    updateSliderInput(session, "s_anios",      value    = c(ANIO_MIN, ANIO_MAX))
    updateSelectInput(session, "j_comunidad",  selected = "Todas")
    updateSliderInput(session, "j_anios",      value    = c(ANIO_MIN, ANIO_MAX))
    updateSelectInput(session, "so_comunidad", selected = "Todas")
    updateSliderInput(session, "so_anios",     value    = c(ANIO_MIN, ANIO_MAX))
    updateSelectInput(session, "i_comunidad",  selected = "Todas")
    updateSliderInput(session, "i_anios",      value    = c(ANIO_MIN, ANIO_MAX))
  })

  # ── Datos reactivos globales (pestaña Resumen) ─────────────────────────────
  r_asm <- reactive({ filtrar(DATOS$asm, input$f_comunidad, input$f_anios) })
  r_cap <- reactive({ filtrar(DATOS$cap, input$f_comunidad, input$f_anios) })

  # ── Datos reactivos por pestaña ────────────────────────────────────────────
  a_asm  <- reactive({ filtrar(DATOS$asm, input$a_comunidad, input$a_anios) })
  c_cap  <- reactive({ filtrar(DATOS$cap, input$c_comunidad, input$c_anios) |>
                         filter(Tema %in% input$c_temas) })
  j_jag  <- reactive({
    df <- DATOS$jag
    if (nrow(df) == 0) return(df)
    fc <- names(df)[str_detect(str_to_lower(names(df)), "fecha|date")][1]
    if (!is.na(fc)) {
      df <- df |> mutate(Fecha_j = as.Date(.data[[fc]]),
                         Anio    = year(Fecha_j)) |>
        filter(Anio >= input$j_anios[1] & Anio <= input$j_anios[2])
    }
    df
  })
  so_soc <- reactive({
    df <- DATOS$soc
    if (nrow(df) == 0) return(df)
    fc <- names(df)[str_detect(str_to_lower(names(df)), "fecha|date")][1]
    cm <- names(df)[str_detect(str_to_lower(names(df)), "comunidad")][1]
    if (!is.na(fc)) {
      df <- df |> mutate(Fecha_s = as.Date(.data[[fc]]),
                         Anio    = year(Fecha_s),
                         YearMon = floor_date(Fecha_s, "month")) |>
        filter(Anio >= input$so_anios[1] & Anio <= input$so_anios[2])
    }
    if (!is.na(cm) && input$so_comunidad != "Todas") {
      df <- df |> filter(str_detect(.data[[cm]], fixed(input$so_comunidad, ignore_case = TRUE)))
    }
    df
  })
  i_asm  <- reactive({ filtrar(DATOS$asm, input$i_comunidad, input$i_anios) })
  i_cap  <- reactive({ filtrar(DATOS$cap, input$i_comunidad, input$i_anios) })

  # ════════════════════════════════════════════════════════════════════════════
  # RESUMEN EJECUTIVO
  # ════════════════════════════════════════════════════════════════════════════

  output$kpi_asm   <- renderText({ format(nrow(r_asm()), big.mark=",") })
  output$kpi_cap   <- renderText({ format(nrow(r_cap()), big.mark=",") })
  output$kpi_part  <- renderText({
    format(sum(r_asm()$Participantes, na.rm=TRUE) +
           sum(r_cap()$Participantes, na.rm=TRUE), big.mark=",")
  })
  output$kpi_horas <- renderText({ format(sum(r_cap()$Horas, na.rm=TRUE), big.mark=",") })
  output$kpi_com   <- renderText({
    length(unique(c(r_asm()$Comunidad, r_cap()$Comunidad)))
  })
  output$kpi_eqgen <- renderText({
    tot <- sum(r_asm()$Participantes, na.rm=TRUE)
    muj <- sum(r_asm()$Mujeres, na.rm=TRUE)
    if (tot > 0) paste0(round(100 * muj / tot, 1), "%") else "—"
  })

  output$res_serie <- renderPlotly({
    s1 <- r_asm() |> count(YearMon, name="n") |> mutate(Tipo="Asambleas")
    s2 <- r_cap() |> count(YearMon, name="n") |> mutate(Tipo="Capacitaciones")
    df <- bind_rows(s1, s2) |> filter(!is.na(YearMon))
    plot_ly(df, x=~YearMon, y=~n, color=~Tipo,
            colors=c("Asambleas"=PAL["bosque"],"Capacitaciones"=PAL["naranja"]),
            type="scatter", mode="lines+markers",
            line=list(width=2), marker=list(size=7),
            text=~paste0("<b>",Tipo,"</b><br>",format(YearMon,"%b %Y"),"<br>N=",n),
            hoverinfo="text") |>
      layout_base("Actividad Mensual",
                  xaxis=list(title="",rangeslider=list(visible=TRUE)),
                  yaxis=list(title="N° actividades"),
                  legend=list(orientation="h"),hovermode="x unified")
  })

  output$res_comunidad <- renderPlotly({
    df <- bind_rows(
      r_asm() |> count(Comunidad, name="n") |> mutate(Tipo="Asambleas"),
      r_cap() |> count(Comunidad, name="n") |> mutate(Tipo="Capacitaciones")
    ) |> filter(!is.na(Comunidad))
    plot_ly(df, x=~Comunidad, y=~n, color=~Tipo,
            colors=c("Asambleas"=PAL["bosque"],"Capacitaciones"=PAL["naranja"]),
            type="bar", hovertemplate="<b>%{x}</b><br>%{fullData.name}: %{y}<extra></extra>") |>
      layout_base("Por Comunidad", barmode="group",
                  xaxis=list(title=""), yaxis=list(title="N°"),
                  legend=list(orientation="h"))
  })

  output$res_anual <- renderPlotly({
    df <- bind_rows(
      r_asm() |> count(Anio, name="n") |> mutate(Tipo="Asambleas"),
      r_cap() |> count(Anio, name="n") |> mutate(Tipo="Capacitaciones")
    ) |> filter(!is.na(Anio))
    plot_ly(df, x=~factor(Anio), y=~n, color=~Tipo,
            colors=c("Asambleas"=PAL["bosque"],"Capacitaciones"=PAL["naranja"]),
            type="bar", hovertemplate="<b>%{x}</b><br>%{fullData.name}: %{y}<extra></extra>") |>
      layout_base("Por Año", barmode="stack",
                  xaxis=list(title="Año"), yaxis=list(title="N°"),
                  legend=list(orientation="h"))
  })

  output$res_heatmap <- renderPlotly({
    df <- bind_rows(
      r_asm() |> mutate(Tipo="Asm"),
      r_cap() |> mutate(Tipo="Cap")
    ) |> filter(!is.na(Comunidad) & !is.na(YearMon)) |>
      count(Mes=format(YearMon,"%Y-%m"), Comunidad) |>
      complete(Mes, Comunidad, fill=list(n=0))
    plot_ly(df, x=~Comunidad, y=~Mes, z=~n, type="heatmap",
            colorscale=list(c(0,"#ECFDF5"),c(.5,"#40916C"),c(1,"#1B4332")),
            hovertemplate="<b>%{x}</b><br>%{y}<br>Actividades: %{z}<extra></extra>") |>
      layout_base("Mapa de Calor",
                  xaxis=list(title=""), yaxis=list(title="",autorange="reversed"))
  })

  # ════════════════════════════════════════════════════════════════════════════
  # ASAMBLEAS
  # ════════════════════════════════════════════════════════════════════════════

  output$a_kpi_n    <- renderText({ format(nrow(a_asm()), big.mark=",") })
  output$a_kpi_ord  <- renderText({ sum(a_asm()$Tipo_asamblea == "Ordinaria", na.rm=TRUE) })
  output$a_kpi_ext  <- renderText({ sum(a_asm()$Tipo_asamblea == "Extraordinaria", na.rm=TRUE) })
  output$a_kpi_hom  <- renderText({ format(sum(a_asm()$Hombres, na.rm=TRUE), big.mark=",") })
  output$a_kpi_muj  <- renderText({ format(sum(a_asm()$Mujeres, na.rm=TRUE), big.mark=",") })
  output$a_kpi_prom <- renderText({
    if (nrow(a_asm()) > 0) round(mean(a_asm()$Participantes, na.rm=TRUE), 1) else "—"
  })

  output$a_serie <- renderPlotly({
    df <- a_asm() |> count(YearMon, Comunidad) |> filter(!is.na(YearMon))
    tipo <- if (!is.null(input$a_tipo_grafico)) input$a_tipo_grafico else "bar"
    p <- plot_ly(df, x=~YearMon, y=~n, color=~Comunidad,
                 colors=unname(PAL[c("bosque","tierra","azul","naranja")]),
                 type=if(tipo=="bar") "bar" else "scatter",
                 mode=if(tipo=="line") "lines+markers" else NULL,
                 line=if(tipo=="line") list(width=2.5) else NULL,
                 text=~paste0("<b>",Comunidad,"</b><br>",format(YearMon,"%b %Y"),"<br>N=",n),
                 hoverinfo="text")
    if (tipo == "bar") p <- p |> layout(barmode="stack")
    p |> layout_base("Asambleas por Mes",
                     xaxis=list(title="",rangeslider=list(visible=TRUE)),
                     yaxis=list(title="N° asambleas"), legend=list(orientation="h"))
  })

  output$a_tipo <- renderPlotly({
    df <- a_asm() |> count(Tipo_asamblea) |> filter(!is.na(Tipo_asamblea))
    plot_ly(df, labels=~Tipo_asamblea, values=~n, type="pie", hole=.4,
            marker=list(colors=unname(PAL[c("bosque","naranja","azul")])),
            textinfo="label+percent") |>
      layout_base("Tipo de Asamblea", showlegend=TRUE)
  })

  output$a_genero <- renderPlotly({
    df <- a_asm() |> filter(!is.na(Comunidad)) |>
      group_by(Comunidad) |>
      summarise(Hombres=sum(Hombres,na.rm=TRUE), Mujeres=sum(Mujeres,na.rm=TRUE), .groups="drop") |>
      pivot_longer(c(Hombres,Mujeres), names_to="Genero", values_to="N")
    plot_ly(df, x=~Comunidad, y=~N, color=~Genero,
            colors=c("Hombres"=PAL["azul"],"Mujeres"=PAL["t_claro"]),
            type="bar", hovertemplate="<b>%{x}</b><br>%{fullData.name}: %{y}<extra></extra>") |>
      layout_base("Participación por Género", barmode="group",
                  xaxis=list(title=""), yaxis=list(title="Personas"),
                  legend=list(orientation="h"))
  })

  output$a_equidad <- renderPlotly({
    df <- a_asm() |> filter(!is.na(Comunidad)) |>
      group_by(Comunidad) |>
      summarise(
        Hombres=sum(Hombres,na.rm=TRUE), Mujeres=sum(Mujeres,na.rm=TRUE), .groups="drop"
      ) |>
      mutate(Total=Hombres+Mujeres,
             Pct_Muj=round(100*Mujeres/pmax(Total,1),1))
    plot_ly(df, x=~Comunidad, y=~Pct_Muj, type="bar",
            marker=list(color=~Pct_Muj,
                        colorscale=list(c(0,"#EF4444"),c(.5,"#F59E0B"),c(1,"#10B981")),
                        cmin=0, cmax=100),
            text=~paste0(Pct_Muj,"%"), textposition="outside",
            hovertemplate="<b>%{x}</b><br>% Mujeres: %{y}%<extra></extra>") |>
      layout_base("% Mujeres en Asambleas",
                  xaxis=list(title=""), yaxis=list(title="%", range=c(0,110)),
                  shapes=list(list(type="line",y0=50,y1=50,
                                   x0=-0.5,x1=length(unique(df$Comunidad))-0.5,
                                   line=list(dash="dash",color=PAL["naranja"],width=2))))
  })

  output$a_monitor <- renderPlotly({
    df <- a_asm() |> filter(!is.na(Monitor) & Monitor != "") |>
      count(Monitor, sort=TRUE) |> slice_max(n, n=10)
    plot_ly(df, x=~n, y=~reorder(Monitor,n), type="bar", orientation="h",
            marker=list(color=PAL["medio"]),
            text=~n, textposition="outside",
            hovertemplate="<b>%{y}</b><br>Asambleas: %{x}<extra></extra>") |>
      layout_base("Top 10 Monitores", xaxis=list(title="N° asambleas"), yaxis=list(title=""))
  })

  output$a_regularidad <- renderPlotly({
    df <- a_asm() |> filter(!is.na(Comunidad) & !is.na(YearMon)) |>
      group_by(Comunidad) |>
      summarise(
        Meses_activos = n_distinct(YearMon),
        Total_meses   = as.numeric(
          interval(min(YearMon,na.rm=TRUE), max(YearMon,na.rm=TRUE)) %/% months(1)
        ) + 1,
        .groups="drop"
      ) |>
      mutate(IRA = round(100 * Meses_activos / pmax(Total_meses,1), 1))
    plot_ly(df, x=~Comunidad, y=~IRA, type="bar",
            marker=list(color=~IRA,
                        colorscale=list(c(0,"#FEE2E2"),c(.5,"#FEF3C7"),c(1,"#D1FAE5")),
                        cmin=0, cmax=100),
            text=~paste0(IRA,"%"), textposition="outside",
            hovertemplate="<b>%{x}</b><br>IRA: %{y}%<extra></extra>") |>
      layout_base("Índice de Regularidad de Asambleas (IRA)",
                  xaxis=list(title=""), yaxis=list(title="% meses con asamblea", range=c(0,115)),
                  shapes=list(list(type="line",y0=80,y1=80,
                                   x0=-0.5,x1=length(unique(df$Comunidad))-0.5,
                                   line=list(dash="dash",color=PAL["bosque"],width=2))))
  })

  output$a_tabla <- renderDT({
    a_asm() |>
      select(Fecha, Comunidad, Tipo_asamblea, Monitor, Hombres, Mujeres, Participantes, Decision_principal) |>
      arrange(desc(Fecha)) |>
      mutate(Fecha=format(Fecha,"%d/%m/%Y")) |>
      datatable(filter="top", extensions=c("Buttons","Scroller"),
                options=list(dom="Bfrtip", buttons=c("csv","excel"),
                             scrollY=320, scroller=TRUE,
                             language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
                class="compact stripe hover") |>
      formatStyle("Participantes", background=styleColorBar(c(0,200),"#B7E4C7"))
  })

  # ════════════════════════════════════════════════════════════════════════════
  # CAPACITACIONES
  # ════════════════════════════════════════════════════════════════════════════

  output$c_kpi_n     <- renderText({ format(nrow(c_cap()), big.mark=",") })
  output$c_kpi_horas <- renderText({ format(sum(c_cap()$Horas, na.rm=TRUE), big.mark=",") })
  output$c_kpi_part  <- renderText({ format(sum(c_cap()$Participantes, na.rm=TRUE), big.mark=",") })
  output$c_kpi_ic    <- renderText({
    tot_part <- sum(c_cap()$Participantes, na.rm=TRUE)
    tot_h    <- sum(c_cap()$Horas, na.rm=TRUE)
    if (tot_part > 0) round(tot_h / tot_part, 2) else "—"
  })
  output$c_kpi_temas <- renderText({ length(unique(c_cap()$Tema)) })
  output$c_kpi_ef    <- renderText({
    format(sum(c_cap()$Horas * c_cap()$Participantes, na.rm=TRUE), big.mark=",")
  })

  output$c_serie <- renderPlotly({
    df <- c_cap() |> count(YearMon, Comunidad) |> filter(!is.na(YearMon))
    plot_ly(df, x=~YearMon, y=~n, color=~Comunidad,
            colors=unname(PAL[c("bosque","tierra","azul","naranja")]),
            type="scatter", mode="lines+markers",
            line=list(width=2.5), marker=list(size=8),
            text=~paste0("<b>",Comunidad,"</b><br>",format(YearMon,"%b %Y"),"<br>N=",n),
            hoverinfo="text") |>
      layout_base("Sesiones por Mes",
                  xaxis=list(title="",rangeslider=list(visible=TRUE)),
                  yaxis=list(title="N° sesiones"), legend=list(orientation="h"),
                  hovermode="x unified")
  })

  output$c_temas_plot <- renderPlotly({
    df <- c_cap() |> count(Tema, sort=TRUE) |> filter(!is.na(Tema))
    plot_ly(df, x=~n, y=~reorder(Tema,n), type="bar", orientation="h",
            marker=list(color=PAL["bosque"]),
            text=~n, textposition="outside",
            hovertemplate="<b>%{y}</b><br>N: %{x}<extra></extra>") |>
      layout_base("Temas", xaxis=list(title=""), yaxis=list(title=""))
  })

  output$c_horas_acum <- renderPlotly({
    df <- c_cap() |> group_by(YearMon, Comunidad) |>
      summarise(Horas=sum(Horas,na.rm=TRUE), .groups="drop") |>
      filter(!is.na(YearMon)) |> arrange(YearMon) |>
      group_by(Comunidad) |> mutate(Horas_acum=cumsum(Horas))
    plot_ly(df, x=~YearMon, y=~Horas_acum, color=~Comunidad,
            colors=unname(PAL[c("bosque","tierra","azul","naranja")]),
            type="scatter", mode="lines", fill="tozeroy", alpha=0.4,
            line=list(width=2.5),
            hovertemplate="<b>%{fullData.name}</b><br>%{x|%b %Y}<br>Acum.: %{y} h<extra></extra>") |>
      layout_base("Horas Acumuladas", xaxis=list(title=""), yaxis=list(title="Horas"),
                  legend=list(orientation="h"))
  })

  output$c_esfuerzo <- renderPlotly({
    df <- c_cap() |> filter(Participantes>0 & Horas>0) |>
      mutate(Esfuerzo=Horas*Participantes) |>
      group_by(YearMon, Comunidad) |>
      summarise(E=sum(Esfuerzo,na.rm=TRUE), .groups="drop") |>
      filter(!is.na(YearMon))
    plot_ly(df, x=~YearMon, y=~E, color=~Comunidad,
            colors=unname(PAL[c("bosque","tierra","azul","naranja")]),
            type="bar",
            hovertemplate="<b>%{fullData.name}</b><br>%{x|%b %Y}<br>Horas-persona: %{y}<extra></extra>") |>
      layout_base("Esfuerzo (Horas × Personas)", barmode="stack",
                  xaxis=list(title="",rangeslider=list(visible=TRUE)),
                  yaxis=list(title="Horas-persona"), legend=list(orientation="h"))
  })

  output$c_intensidad <- renderPlotly({
    df <- c_cap() |> filter(!is.na(Comunidad)) |>
      group_by(Comunidad) |>
      summarise(IC=round(sum(Horas,na.rm=TRUE)/pmax(sum(Participantes,na.rm=TRUE),1),2),
                .groups="drop")
    plot_ly(df, x=~Comunidad, y=~IC, type="bar",
            marker=list(color=PAL["naranja"]),
            text=~paste0(IC," h/p"), textposition="outside",
            hovertemplate="<b>%{x}</b><br>IC: %{y} hrs/persona<extra></extra>") |>
      layout_base("Intensidad de Capacitación",
                  xaxis=list(title=""), yaxis=list(title="Horas / participante"),
                  shapes=list(list(type="line",y0=2,y1=2,x0=-0.5,
                                   x1=length(unique(df$Comunidad))-0.5,
                                   line=list(dash="dash",color=PAL["bosque"],width=2))))
  })

  output$c_genero <- renderPlotly({
    df <- c_cap() |> filter(!is.na(Comunidad)) |>
      group_by(Comunidad) |>
      summarise(H=sum(Hombres,na.rm=TRUE), M=sum(Mujeres,na.rm=TRUE), .groups="drop") |>
      mutate(Tot=H+M, PctM=round(100*M/pmax(Tot,1),1))
    plot_ly(df, x=~Comunidad, y=~PctM, type="bar",
            marker=list(color=PAL["t_claro"]),
            text=~paste0(PctM,"%"), textposition="outside",
            hovertemplate="<b>%{x}</b><br>% Mujeres: %{y}%<extra></extra>") |>
      layout_base("% Mujeres en Capacitaciones",
                  xaxis=list(title=""), yaxis=list(title="%", range=c(0,115)),
                  shapes=list(list(type="line",y0=50,y1=50,
                                   x0=-0.5,x1=length(unique(df$Comunidad))-0.5,
                                   line=list(dash="dash",color=PAL["naranja"],width=2))))
  })

  output$c_tabla <- renderDT({
    c_cap() |>
      select(Fecha, Comunidad, Monitor, Tema, Participantes, Horas, Capacidad_adquirida) |>
      arrange(desc(Fecha)) |>
      mutate(Fecha=format(Fecha,"%d/%m/%Y")) |>
      datatable(filter="top", extensions=c("Buttons","Scroller"),
                options=list(dom="Bfrtip", buttons=c("csv","excel"),
                             scrollY=320, scroller=TRUE,
                             language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
                class="compact stripe hover") |>
      formatStyle("Horas", background=styleColorBar(c(0,20),"#FFD166")) |>
      formatStyle("Participantes", background=styleColorBar(c(0,50),"#B7E4C7"))
  })

  # ════════════════════════════════════════════════════════════════════════════
  # SEGURIDAD ALIMENTARIA
  # ════════════════════════════════════════════════════════════════════════════

  output$s_kpi_registros <- renderText({ nrow(DATOS$seg) })
  output$s_kpi_insumos   <- renderText({ nrow(DATOS$ins) })
  output$s_kpi_tipos     <- renderText({ length(unique(DATOS$ins$Insumos_entregados)) })
  output$s_kpi_total     <- renderText({
    format(sum(DATOS$ins$N_insumos, na.rm=TRUE), big.mark=",")
  })

  output$s_insumos_bar <- renderPlotly({
    df <- DATOS$ins |> group_by(Insumos_entregados) |>
      summarise(Total=sum(N_insumos,na.rm=TRUE), N=n(), .groups="drop") |>
      arrange(desc(Total))
    plot_ly(df, x=~Insumos_entregados, y=~Total, type="bar",
            marker=list(color=PAL["tierra"]),
            text=~Total, textposition="outside",
            hovertemplate="<b>%{x}</b><br>Total: %{y}<br>Entregas: %{customdata}",
            customdata=~N) |>
      layout_base("Insumos por Tipo",
                  xaxis=list(title=""), yaxis=list(title="Unidades"))
  })

  output$s_insumos_pie <- renderPlotly({
    df <- DATOS$ins |> group_by(Insumos_entregados) |>
      summarise(Total=sum(N_insumos,na.rm=TRUE), .groups="drop")
    plot_ly(df, labels=~Insumos_entregados, values=~Total, type="pie", hole=.4,
            marker=list(colors=unname(PAL[c("tierra","bosque","azul","naranja","amarillo")])),
            textinfo="label+percent") |>
      layout_base("", showlegend=TRUE)
  })

  output$s_actividades <- renderPlotly({
    df <- DATOS$seg
    if (nrow(df) == 0) { return(plotly_empty()) }
    tipo_col <- names(df)[str_detect(str_to_lower(names(df)), "actividad|tipo|tipo_actividad")][1]
    if (is.na(tipo_col)) { return(plotly_empty()) }
    df2 <- df |> filter(!is.na(.data[[tipo_col]]) & .data[[tipo_col]] != "") |>
      count(.data[[tipo_col]], name="n") |> rename(Tipo=1) |> arrange(desc(n))
    plot_ly(df2, x=~n, y=~reorder(Tipo,n), type="bar", orientation="h",
            marker=list(color=PAL["medio"]),
            text=~n, textposition="outside",
            hovertemplate="<b>%{y}</b><br>N: %{x}<extra></extra>") |>
      layout_base("Actividades Alimentarias",
                  xaxis=list(title=""), yaxis=list(title=""))
  })

  output$s_prod_compare <- renderPlotly({
    df <- DATOS$seg
    if (nrow(df) == 0) { return(plotly_empty()) }
    tipo_col <- names(df)[str_detect(str_to_lower(names(df)), "actividad|tipo")][1]
    if (is.na(tipo_col)) { return(plotly_empty()) }
    df2 <- df |> filter(str_detect(str_to_lower(.data[[tipo_col]]), "piscic|avicul")) |>
      mutate(Prod=ifelse(str_detect(str_to_lower(.data[[tipo_col]]),"piscic"),"Piscicultura","Avicultura")) |>
      count(Prod)
    plot_ly(df2, labels=~Prod, values=~n, type="pie", hole=.5,
            marker=list(colors=c(PAL["bosque"],PAL["naranja"])),
            textinfo="label+value+percent") |>
      layout_base("Piscicultura vs Avicultura", showlegend=TRUE)
  })

  output$s_tabla_seg <- renderDT({
    datatable(DATOS$seg, filter="top",
              extensions=c("Buttons","Scroller"),
              options=list(dom="Bfrtip", buttons=c("csv","excel"),
                           scrollY=260, scroller=TRUE,
                           language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
              class="compact stripe hover")
  })

  output$s_tabla_ins <- renderDT({
    datatable(DATOS$ins, filter="top",
              extensions=c("Buttons","Scroller"),
              options=list(dom="Bfrtip", buttons=c("csv","excel"),
                           scrollY=260, scroller=TRUE,
                           language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
              class="compact stripe hover")
  })

  # ════════════════════════════════════════════════════════════════════════════
  # JAGUAR
  # ════════════════════════════════════════════════════════════════════════════

  output$j_kpi_total <- renderText({
    df <- j_jag(); if(nrow(df)==0) return("0"); nrow(df)
  })
  output$j_kpi_tipos <- renderText({
    df <- j_jag()
    tc <- names(df)[str_detect(str_to_lower(names(df)),"tipo|evidencia|type")][1]
    if (nrow(df)==0 || is.na(tc)) return("—")
    length(unique(df[[tc]]))
  })
  output$j_kpi_comunidades <- renderText({
    df <- j_jag()
    cc <- names(df)[str_detect(str_to_lower(names(df)),"comunidad")][1]
    if (nrow(df)==0 || is.na(cc)) return("—")
    length(unique(df[[cc]]))
  })
  output$j_kpi_reciente <- renderText({
    df <- j_jag()
    if (nrow(df)==0 || is.null(df$Fecha_j)) return("—")
    format(max(df$Fecha_j, na.rm=TRUE), "%d/%m/%Y")
  })

  output$j_tipo_pie <- renderPlotly({
    df <- j_jag(); if(nrow(df)==0) return(plotly_empty())
    tc <- names(df)[str_detect(str_to_lower(names(df)),"tipo|evidencia|type")][1]
    if (is.na(tc)) return(plotly_empty())
    df2 <- df |> filter(!is.na(.data[[tc]]) & .data[[tc]]!="") |>
      count(.data[[tc]], name="n") |> rename(Tipo=1)
    plot_ly(df2, labels=~Tipo, values=~n, type="pie", hole=.4,
            marker=list(colors=unname(PAL[c("bosque","tierra","azul","naranja","amarillo")])),
            textinfo="label+value") |>
      layout_base("", showlegend=TRUE)
  })

  output$j_serie <- renderPlotly({
    df <- j_jag(); if(nrow(df)==0 || is.null(df$Fecha_j)) return(plotly_empty())
    df2 <- df |> mutate(YearMon=floor_date(Fecha_j,"month")) |>
      count(YearMon, name="n") |> filter(!is.na(YearMon))
    plot_ly(df2, x=~YearMon, y=~n, type="bar",
            marker=list(color=PAL["bosque"]),
            hovertemplate="%{x|%b %Y}<br>Evidencias: %{y}<extra></extra>") |>
      layout_base("Evidencias por Mes",
                  xaxis=list(title=""), yaxis=list(title="N° evidencias"))
  })

  output$j_comunidad_bar <- renderPlotly({
    df <- j_jag(); if(nrow(df)==0) return(plotly_empty())
    cc <- names(df)[str_detect(str_to_lower(names(df)),"comunidad")][1]
    if (is.na(cc)) return(plotly_empty())
    df2 <- df |> filter(!is.na(.data[[cc]])) |>
      count(.data[[cc]], sort=TRUE) |> rename(Comunidad=1)
    plot_ly(df2, x=~Comunidad, y=~n, type="bar",
            marker=list(color=PAL["medio"]),
            text=~n, textposition="outside",
            hovertemplate="<b>%{x}</b><br>N: %{y}<extra></extra>") |>
      layout_base("Evidencias por Comunidad",
                  xaxis=list(title=""), yaxis=list(title="N°"))
  })

  output$j_frecuencia <- renderPlotly({
    df <- j_jag(); if(nrow(df)==0 || is.null(df$Fecha_j)) return(plotly_empty())
    df2 <- df |> mutate(Mes=month(Fecha_j, label=TRUE, abbr=TRUE)) |>
      count(Mes, name="n")
    plot_ly(df2, x=~Mes, y=~n, type="scatter", mode="lines+markers",
            line=list(color=PAL["bosque"], width=2.5),
            marker=list(size=9, color=PAL["bosque"]),
            hovertemplate="<b>%{x}</b><br>Promedio: %{y}<extra></extra>") |>
      layout_base("Estacionalidad — Evidencias por Mes del Año",
                  xaxis=list(title="Mes"), yaxis=list(title="N°"))
  })

  output$j_tabla <- renderDT({
    df <- j_jag()
    datatable(df, filter="top", extensions=c("Buttons","Scroller"),
              options=list(dom="Bfrtip", buttons=c("csv","excel"),
                           scrollY=320, scroller=TRUE,
                           language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
              class="compact stripe hover")
  })

  # ════════════════════════════════════════════════════════════════════════════
  # SOCIALIZACIÓN
  # ════════════════════════════════════════════════════════════════════════════

  output$so_kpi_n <- renderText({ nrow(so_soc()) })
  output$so_kpi_com <- renderText({
    df <- so_soc()
    cc <- names(df)[str_detect(str_to_lower(names(df)),"comunidad")][1]
    if (nrow(df)==0 || is.na(cc)) return("—")
    length(unique(df[[cc]]))
  })
  output$so_kpi_mon <- renderText({
    df <- so_soc()
    mc <- names(df)[str_detect(str_to_lower(names(df)),"monitor")][1]
    if (nrow(df)==0 || is.na(mc)) return("—")
    length(unique(df[[mc]]))
  })

  output$so_serie <- renderPlotly({
    df <- so_soc(); if(nrow(df)==0) return(plotly_empty())
    cc <- names(df)[str_detect(str_to_lower(names(df)),"comunidad")][1]
    if (is.na(cc) || is.null(df$YearMon)) return(plotly_empty())
    df2 <- df |> count(YearMon, .data[[cc]], name="n") |>
      rename(Comunidad=2) |> filter(!is.na(YearMon))
    plot_ly(df2, x=~YearMon, y=~n, color=~Comunidad,
            colors=unname(PAL[c("bosque","tierra","azul","naranja")]),
            type="bar",
            hovertemplate="<b>%{fullData.name}</b><br>%{x|%b %Y}<br>N: %{y}<extra></extra>") |>
      layout_base("Socializaciones por Mes", barmode="stack",
                  xaxis=list(title="",rangeslider=list(visible=TRUE)),
                  yaxis=list(title="N°"), legend=list(orientation="h"))
  })

  output$so_pie <- renderPlotly({
    df <- so_soc(); if(nrow(df)==0) return(plotly_empty())
    cc <- names(df)[str_detect(str_to_lower(names(df)),"comunidad")][1]
    if (is.na(cc)) return(plotly_empty())
    df2 <- df |> count(.data[[cc]], name="n") |> rename(Comunidad=1)
    plot_ly(df2, labels=~Comunidad, values=~n, type="pie", hole=.4,
            marker=list(colors=unname(PAL[c("bosque","tierra","azul","naranja")])),
            textinfo="label+value") |>
      layout_base("", showlegend=TRUE)
  })

  output$so_temas <- renderPlotly({
    df <- so_soc(); if(nrow(df)==0) return(plotly_empty())
    dc <- names(df)[str_detect(str_to_lower(names(df)),"decision|acuerdo|tema|descripcion")][1]
    if (is.na(dc)) return(plotly_empty())
    palabras <- df |> filter(!is.na(.data[[dc]]) & .data[[dc]]!="") |>
      pull(.data[[dc]]) |> str_to_lower() |>
      str_split("\\s+") |> unlist() |>
      table() |> sort(decreasing=TRUE)
    stopwords <- c("de","la","el","en","y","a","con","para","que","los","las",
                   "se","del","un","una","por","al","su","es","le","no")
    palabras <- palabras[!names(palabras) %in% stopwords & nchar(names(palabras)) > 3]
    df2 <- tibble(Palabra=names(palabras[1:min(15,length(palabras))]),
                  Freq=as.integer(palabras[1:min(15,length(palabras))]))
    plot_ly(df2, x=~Freq, y=~reorder(Palabra,Freq), type="bar", orientation="h",
            marker=list(color=PAL["medio"]),
            text=~Freq, textposition="outside",
            hovertemplate="<b>%{y}</b><br>Frecuencia: %{x}<extra></extra>") |>
      layout_base("Palabras Más Frecuentes en Acuerdos",
                  xaxis=list(title="Frecuencia"), yaxis=list(title=""))
  })

  output$so_tabla <- renderDT({
    df <- so_soc()
    datatable(df, filter="top", extensions=c("Buttons","Scroller"),
              options=list(dom="Bfrtip", buttons=c("csv","excel"),
                           scrollY=320, scroller=TRUE,
                           language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
              class="compact stripe hover")
  })

  # ════════════════════════════════════════════════════════════════════════════
  # INDICADORES
  # ════════════════════════════════════════════════════════════════════════════

  ind_data <- reactive({
    asm <- i_asm(); cap <- i_cap()

    # IRA — Índice de Regularidad de Asambleas
    IRA <- if (nrow(asm) > 0) {
      m_proy <- as.numeric(interval(min(asm$YearMon,na.rm=TRUE),
                                    max(asm$YearMon,na.rm=TRUE)) %/% months(1)) + 1
      m_con  <- n_distinct(asm$YearMon, na.rm=TRUE)
      round(100 * m_con / pmax(m_proy,1), 1)
    } else { NA_real_ }

    # IEGP — Equidad género asambleas
    tot_gen <- sum(asm$Participantes, na.rm=TRUE)
    IEGP <- if (tot_gen > 0) {
      round(100 * sum(asm$Mujeres,na.rm=TRUE) / tot_gen, 1)
    } else { NA_real_ }

    # IC — Intensidad capacitación
    tot_p <- sum(cap$Participantes, na.rm=TRUE)
    IC <- if (tot_p > 0) {
      round(sum(cap$Horas,na.rm=TRUE) / tot_p, 2)
    } else { NA_real_ }

    # DTC — Diversidad temática
    DTC <- length(unique(cap$Tema[!is.na(cap$Tema)]))

    # TCA — Tasa crecimiento actividades
    act_a <- bind_rows(
      asm |> count(Anio, name="n"),
      cap |> count(Anio, name="n")
    ) |> group_by(Anio) |> summarise(T=sum(n), .groups="drop") |> arrange(Anio)
    TCA <- if (nrow(act_a) >= 2) {
      round(100 * (act_a$T[nrow(act_a)] - act_a$T[1]) / pmax(act_a$T[1],1), 1)
    } else { NA_real_ }

    # IEGP_cap — Equidad género capacitaciones
    tot_cap_gen <- sum(cap$Participantes, na.rm=TRUE)
    IEGP_CAP <- if (tot_cap_gen > 0) {
      round(100 * sum(cap$Mujeres,na.rm=TRUE) / tot_cap_gen, 1)
    } else { NA_real_ }

    # EF — Esfuerzo formativo (horas-persona)
    EF <- sum(cap$Horas * cap$Participantes, na.rm=TRUE)

    # NMA — N° medio de asambleas por comunidad/mes
    NMA <- if (nrow(asm) > 0 && n_distinct(asm$Comunidad, na.rm=TRUE) > 0) {
      round(nrow(asm) / (n_distinct(asm$Comunidad,na.rm=TRUE) *
                          pmax(n_distinct(asm$YearMon,na.rm=TRUE),1)), 2)
    } else { NA_real_ }

    tibble(
      Indicador    = c("Regularidad Asambleas (IRA)",
                       "Equidad Género — Asambleas (IEGP)",
                       "Intensidad Capacitación (IC)",
                       "Diversidad Temática (DTC)",
                       "Tasa Crecimiento Actividades (TCA)",
                       "Equidad Género — Capacitaciones",
                       "Esfuerzo Formativo total (horas-persona)",
                       "Asambleas / comunidad / mes"),
      Nuevo        = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
      Valor_num    = c(IRA, IEGP, IC, DTC, TCA, IEGP_CAP, EF, NMA),
      Valor_txt    = c(
        if(!is.na(IRA))       paste0(IRA,"%")       else "—",
        if(!is.na(IEGP))      paste0(IEGP,"%")      else "—",
        if(!is.na(IC))        paste0(IC," h/p")     else "—",
        paste0(DTC," temas"),
        if(!is.na(TCA))       paste0(TCA,"%")       else "—",
        if(!is.na(IEGP_CAP))  paste0(IEGP_CAP,"%") else "—",
        format(EF, big.mark=","),
        if(!is.na(NMA))       as.character(NMA)     else "—"
      ),
      Meta         = c("≥ 80%","45–55%","≥ 2 h/p","≥ 5 temas","> 0%","45–55%","Máximo","≥ 1"),
      Estado       = c(
        if(!is.na(IRA))      ifelse(IRA>=80,"✅",ifelse(IRA>=60,"🟡","🔴")) else "⚪",
        if(!is.na(IEGP))     ifelse(IEGP>=45&IEGP<=55,"✅","🟡") else "⚪",
        if(!is.na(IC))       ifelse(IC>=2,"✅","🟡") else "⚪",
        ifelse(DTC>=5,"✅","🟡"),
        if(!is.na(TCA))      ifelse(TCA>0,"✅","🔴") else "⚪",
        if(!is.na(IEGP_CAP)) ifelse(IEGP_CAP>=45&IEGP_CAP<=55,"✅","🟡") else "⚪",
        ifelse(EF>0,"✅","🔴"),
        if(!is.na(NMA))      ifelse(NMA>=1,"✅","🟡") else "⚪"
      )
    )
  })

  output$ind_tabla <- renderTable({
    df <- ind_data() |>
      mutate(Nuevo = ifelse(Nuevo, "★ NUEVO", "")) |>
      select(Estado, Indicador, Nuevo, Valor_txt, Meta)
    df
  }, striped=TRUE, hover=TRUE, bordered=TRUE, spacing="s",
     sanitize.text.function=identity)

  output$ind_radar <- renderPlotly({
    df <- ind_data()
    # Normalizar a 0-100 los 5 primeros indicadores
    vals <- c(
      pmin(coalesce(df$Valor_num[1],0)/80,1)*100,
      pmin(abs(coalesce(df$Valor_num[2],0)-50)/5,1) |> (\(x)(1-x)*100)(),
      pmin(coalesce(df$Valor_num[3],0)/2,1)*100,
      pmin(coalesce(df$Valor_num[4],0)/5,1)*100,
      pmin(pmax(coalesce(df$Valor_num[5],0),0)/30,1)*100
    )
    etiq <- c("Regularidad<br>Asambleas","Equidad<br>Género","Intensidad<br>Cap.",
              "Diversidad<br>Temática","Crecimiento<br>Actividades")
    plot_ly(type="scatterpolar",
            r=c(vals,vals[1]), theta=c(etiq,etiq[1]),
            fill="toself",
            fillcolor="rgba(45,106,79,0.2)",
            line=list(color=PAL["bosque"],width=2.5),
            mode="lines+markers",
            marker=list(size=8,color=PAL["bosque"]),
            name="Cumplimiento") |>
      layout(polar=list(radialaxis=list(visible=TRUE,range=c(0,100))),
             paper_bgcolor="white", showlegend=FALSE,
             title=list(text="Cumplimiento de Indicadores (%)",
                        font=list(color=PAL["bosque"],size=14)))
  })

  output$ind_evolucion <- renderPlotly({
    asm <- i_asm(); cap <- i_cap()
    ev <- bind_rows(
      asm |> filter(!is.na(Anio)) |>
        group_by(Anio) |>
        summarise(Valor=n(), .groups="drop") |> mutate(Ind="Asambleas"),
      cap |> filter(!is.na(Anio)) |>
        group_by(Anio) |>
        summarise(Valor=n(), .groups="drop") |> mutate(Ind="Capacitaciones"),
      cap |> filter(!is.na(Anio)) |>
        group_by(Anio) |>
        summarise(Valor=sum(Horas,na.rm=TRUE), .groups="drop") |> mutate(Ind="Horas cap."),
      asm |> filter(!is.na(Anio)) |>
        group_by(Anio) |>
        summarise(Valor=round(100*sum(Mujeres,na.rm=TRUE)/pmax(sum(Participantes,na.rm=TRUE),1),1),
                  .groups="drop") |> mutate(Ind="% Mujeres (Asm)")
    )
    plot_ly(ev, x=~factor(Anio), y=~Valor, color=~Ind,
            colors=unname(PAL[c("bosque","naranja","azul","t_claro")]),
            type="scatter", mode="lines+markers",
            line=list(width=2.5), marker=list(size=9),
            hovertemplate="<b>%{fullData.name}</b><br>Año %{x}<br>Valor: %{y}<extra></extra>") |>
      layout(title=list(text="Evolución Anual",font=list(color=PAL["bosque"],size=14)),
             paper_bgcolor="white", plot_bgcolor="#FAFAFA",
             xaxis=list(title="Año"), yaxis=list(title=""),
             legend=list(orientation="h"))
  })

  output$ind_iac <- renderPlotly({
    asm <- i_asm(); cap <- i_cap()
    n_com <- length(unique(c(asm$Comunidad, cap$Comunidad)))
    iac_a <- bind_rows(
      asm |> filter(!is.na(Anio)) |> group_by(Anio) |>
        summarise(Asm=n(), .groups="drop"),
      cap |> filter(!is.na(Anio)) |> group_by(Anio) |>
        summarise(Cap=n(), H=sum(Horas,na.rm=TRUE), .groups="drop")
    ) |> group_by(Anio) |>
      summarise(Asm=sum(Asm,na.rm=TRUE), Cap=sum(Cap,na.rm=TRUE),
                H=sum(H,na.rm=TRUE), .groups="drop") |>
      mutate(IAC=round((Asm*1 + Cap*1.5 + H*0.1)/pmax(n_com,1),1))
    plot_ly(iac_a, x=~factor(Anio), y=~IAC, type="bar",
            marker=list(color=PAL["bosque"]),
            text=~IAC, textposition="outside",
            hovertemplate="Año %{x}<br>IAC: %{y}<extra></extra>") |>
      layout(title=list(text="Índice de Actividad Compuesto",font=list(color=PAL["bosque"],size=14)),
             paper_bgcolor="white", plot_bgcolor="#FAFAFA",
             xaxis=list(title="Año"), yaxis=list(title="IAC"))
  })

  output$ind_brecha_gen <- renderPlotly({
    asm <- i_asm()
    df <- asm |> filter(!is.na(Comunidad)) |>
      group_by(Comunidad) |>
      summarise(PctM=round(100*sum(Mujeres,na.rm=TRUE)/pmax(sum(Participantes,na.rm=TRUE),1),1),
                .groups="drop") |>
      mutate(Brecha=abs(PctM-50),
             Color=ifelse(PctM>=45&PctM<=55,"#10B981",ifelse(PctM>=35,"#F59E0B","#EF4444")))
    plot_ly(df, x=~Comunidad, y=~Brecha, type="bar",
            marker=list(color=~Color),
            text=~paste0(PctM,"% muj."), textposition="outside",
            hovertemplate="<b>%{x}</b><br>Brecha: %{y} pp<br>%{text}<extra></extra>") |>
      layout(title=list(text="Brecha de Equidad de Género (puntos porcentuales vs 50%)",
                        font=list(color=PAL["bosque"],size=13)),
             paper_bgcolor="white", plot_bgcolor="#FAFAFA",
             xaxis=list(title=""), yaxis=list(title="Puntos porcentuales vs paridad"),
             annotations=list(list(x=0,y=0,text="0 = paridad perfecta",
                                   showarrow=FALSE,
                                   font=list(color=PAL["gris"],size=11))))
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. LANZAR
# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
