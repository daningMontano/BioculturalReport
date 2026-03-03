# =============================================================================
# .Rprofile — Se ejecuta automáticamente al abrir el proyecto en RStudio
# =============================================================================

# Activar renv automáticamente
source("renv/activate.R")

# Opciones globales
options(
  shiny.port          = 3838,
  shiny.launch.browser = TRUE,
  shiny.autoreload    = TRUE,   # recarga automática al editar archivos
  warn                = 1,
  scipen               = 999,
  OutDec              = ".",
  repos               = c(CRAN = "https://cloud.r-project.org")
)

# Mensaje de bienvenida
if (interactive()) {
  packageStartupMessage(
    "\n🌿 Fundación Pachamama Dashboard\n",
    "   → Lanzar app:  shiny::runApp()\n",
    "   → Setup:       source('setup.R')\n"
  )
}
