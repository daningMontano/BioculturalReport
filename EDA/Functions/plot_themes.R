# =============================================================================
# R/logic/plot_themes.R
# Tema visual y paletas — Manual de Marca Fundación Pachamama (Lunalunares 2024)
# Exporta:
#   pachamama_theme()   → tema bslib para ui.R  (requiere bslib)
#   theme_pachamama()   → tema ggplot2 para gráficas
#   PAL, PAL_SEQ, PAL_DIV, PAL_CUAL, scale_fill_pacha*
# =============================================================================

# ── Paleta oficial ─────────────────────────────────────────────────────────
PAL <- list(
  hoja    = "#183d1e",   # verde oscuro — primario
  rio     = "#3c6d67",   # verde azulado — secundario
  cielo   = "#79c6c4",   # turquesa/menta
  arcilla = "#9caa7d",   # verde oliva
  ave     = "#dd9536",   # ocre/naranja
  semilla = "#f4e238",   # amarillo
  blanco  = "#ffffff",
  negro   = "#000000",
  fondo   = "#f7f5f0",   # blanco cálido (UI background)
  texto   = "#1a2e1c"
)

# Secuencias útiles para escalas
PAL_SEQ  <- c("#f7f5f0","#9caa7d","#79c6c4","#3c6d67","#183d1e")
PAL_DIV  <- c("#183d1e","#3c6d67","#79c6c4","#f7f5f0","#f4e238","#dd9536","#c0601a")
PAL_CUAL <- c("#183d1e","#dd9536","#79c6c4","#f4e238","#9caa7d","#3c6d67","#e88a3c","#5a9e7a")

# ── Tema ggplot2 base ──────────────────────────────────────────────────────
theme_pachamama <- function(base_size = 12, legend_pos = "bottom") {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Raleway") +
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = PAL$fondo, color = NA),
    panel.background = ggplot2::element_rect(fill = PAL$fondo, color = NA),
    panel.grid.major = ggplot2::element_line(color = paste0(PAL$hoja, "14"), linewidth = 0.4),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line        = ggplot2::element_line(color = paste0(PAL$hoja, "30"), linewidth = 0.5),
    axis.ticks       = ggplot2::element_line(color = paste0(PAL$hoja, "30")),
    axis.text        = ggplot2::element_text(color = PAL$rio,  size = ggplot2::rel(0.85)),
    axis.title       = ggplot2::element_text(color = PAL$hoja, face = "bold", size = ggplot2::rel(0.9)),
    plot.title       = ggplot2::element_text(color = PAL$hoja, face = "bold",
                                             size  = ggplot2::rel(1.15), margin = ggplot2::margin(b = 4)),
    plot.subtitle    = ggplot2::element_text(color = PAL$rio,  size = ggplot2::rel(0.88),
                                             margin = ggplot2::margin(b = 10)),
    plot.caption     = ggplot2::element_text(color = PAL$arcilla, size = ggplot2::rel(0.74), hjust = 0),
    legend.position  = legend_pos,
    legend.title     = ggplot2::element_text(color = PAL$hoja, face = "bold", size = ggplot2::rel(0.85)),
    legend.text      = ggplot2::element_text(color = PAL$rio,  size = ggplot2::rel(0.82)),
    strip.background = ggplot2::element_rect(fill = PAL$hoja, color = NA),
    strip.text       = ggplot2::element_text(color = "#fff", face = "bold", size = ggplot2::rel(0.85)),
    plot.margin      = ggplot2::margin(12, 16, 8, 12)
  )
}

# ── Tema bslib para la UI de Shiny ────────────────────────────────────────
#' Genera el tema bslib alineado al Manual de Marca Fundación Pachamama
#' Se usa en ui.R: page_navbar(theme = pachamama_theme())
pachamama_theme <- function() {
  bslib::bs_theme(
    version       = 5,
    bg            = "#f7f5f0",   # fondo cálido
    fg            = "#1a2e1c",   # texto oscuro
    primary       = "#183d1e",   # Hoja — verde oscuro
    secondary     = "#3c6d67",   # Río — verde azulado
    success       = "#3c6d67",
    info          = "#79c6c4",   # Cielo
    warning       = "#dd9536",   # Ave
    danger        = "#c0601a",
    base_font     = bslib::font_google("Raleway", wght = c(300, 400, 600, 700, 800)),
    heading_font  = bslib::font_google("Raleway", wght = c(700, 800)),
    font_scale    = 0.92,
    `navbar-bg`              = "#f7f5f0",
    `navbar-light-brand-color`       = "#183d1e",
    `navbar-light-active-color`      = "#183d1e",
    `navbar-light-color`             = "#3c6d67",
    `navbar-light-hover-color`       = "#183d1e",
    `card-border-radius`             = "10px",
    `card-box-shadow`                = "0 2px 10px rgba(24,61,30,0.08)",
    `btn-border-radius`              = "6px"
  )
}

# ── Escalas de color reutilizables ────────────────────────────────────────
scale_fill_pacha   <- function(...) ggplot2::scale_fill_manual(values = PAL_CUAL, ...)
scale_color_pacha  <- function(...) ggplot2::scale_color_manual(values = PAL_CUAL, ...)

scale_fill_pacha_seq <- function(reverse = FALSE, ...) {
  pal <- if (reverse) rev(PAL_SEQ) else PAL_SEQ
  ggplot2::scale_fill_gradientn(colors = pal, ...)
}
scale_fill_pacha_div <- function(...) {
  ggplot2::scale_fill_gradientn(colors = PAL_DIV, ...)
}
