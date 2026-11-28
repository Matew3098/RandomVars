# Paquetes necesarios para el proyecto

packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "readxl",
  "writexl",
  "plotly",
  "tseries"
)

# Instalar los paquetes que no estén instalados
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Cargar los paquetes
invisible(lapply(packages, library, character.only = TRUE))
