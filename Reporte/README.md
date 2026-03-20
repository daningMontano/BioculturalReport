# 📊 Reporte Biocultural — Fundación Pachamama

## Estructura

```
Reporte/
├── app.R                     ← App Shiny interactiva ⭐ PRINCIPAL
├── reporte_biocultural.qmd   ← Reporte estático Quarto (paramétrico)
├── estilos.css               ← Estilos CSS corporativos
└── README.md                 ← Este archivo
```

El repositorio asume que `Data/` está un nivel arriba (`../Data/`):

```
bioculturalreport/
├── Data/
│   ├── Asambleas.csv
│   ├── Capacitaciones.csv
│   ├── df_entrega_insumos.csv
│   ├── df_seguridad_alimentaria.csv
│   ├── Evidencias_jaguar.csv
│   └── Socialización_acuerdos.csv
├── EDA/
└── Reporte/
    ├── app.R
    ├── reporte_biocultural.qmd
    ├── estilos.css
    └── README.md
```

---

## 🚀 Lanzar la App Shiny

```r
# Opción 1 — RStudio: abrir app.R y clic en "Run App"

# Opción 2 — consola R (estando dentro de Reporte/)
shiny::runApp("app.R", port = 3838, launch.browser = TRUE)

# Opción 3 — PowerShell
cd C:\PACHAMAMA\CONSERVACION\Aplicaciones\BioculturalReport\Reporte
Rscript -e "shiny::runApp('app.R', port=3838, launch.browser=TRUE)"
```

## Pestañas

| Pestaña | Contenido |
|---------|-----------|
| Resumen | KPIs globales, serie temporal, mapa de calor |
| Gobernanza | Asambleas: serie, género, tipo, monitores, IRA |
| Capacitaciones | Sesiones, horas, temas, esfuerzo, intensidad |
| Seg. Alimentaria | Insumos, piscicultura vs avicultura |
| Jaguar | Evidencias, estacionalidad, comunidades |
| Socialización | Acuerdos, palabras frecuentes |
| Indicadores | Semáforo 8 indicadores, radar, evolución, IAC, brecha género |

## Paquetes requeridos

shiny, bslib, bsicons, dplyr, tidyr, ggplot2, plotly, lubridate, stringr, forcats, scales, DT, glue, purrr
