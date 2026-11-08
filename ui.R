library(shiny)
library(bslib)

ui <- navbarPage(
  title = "Simulación y Métodos Numéricos - EPN",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # =========================================================================
  # PESTAÑA 1: Generación de Números Pseudoaleatorios
  # =========================================================================
  tabPanel(
    "Números Pseudoaleatorios",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Parámetros del Generador"),
        selectInput(
          "gen_metodo",
          "Método:",
          choices = c(
            "Método Congruencial Lineal (MCL)" = "mcl",
            "Método de Lehmer" = "lehmer",
            "Cuadrados Medios" = "cuadrados"
          )
        ),
        numericInput(
          "gen_n",
          "Cantidad de números (N):",
          value = 1000,
          min = 10,
          step = 10
        ),
        numericInput("gen_semilla", "Semilla inicial (x0):", value = 4122),
        numericInput("gen_a", "Multiplicador (a):", value = 1664525),
        numericInput("gen_c", "Incremento (c):", value = 1013904223),
        numericInput("gen_m", "Módulo (m):", value = 2^32),
        hr(),
        actionButton(
          "gen_calcular",
          "Generar Secuencia",
          class = "btn-primary w-100"
        ),
        hr(),
        h4("Exportación"),
        downloadButton("dl_excel", "Descargar .xlsx", class = "w-100 mb-2"),
        downloadButton("dl_txt", "Descargar .txt / .csv", class = "w-100")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel(
            "Gráficos y Resumen",
            fluidRow(
              column(6, plotOutput("gen_plot_hist", height = "350px")),
              column(
                6,
                h4("Resumen Estadístico"),
                verbatimTextOutput("gen_resumen_stats")
              )
            )
          ),
          tabPanel("Tabla de Datos", br(), dataTableOutput("gen_tabla_datos"))
        )
      )
    )
  ),

  # =========================================================================
  # PESTAÑA 2: Aproximación de Integrales por el Método de Montecarlo
  # =========================================================================
  tabPanel(
    "Integración Montecarlo",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Configuración de la Integral"),
        textInput("mc_funcion", "Expresión f(x):", value = "exp(-x^2)"),
        numericInput("mc_a", "Límite inferior (a):", value = 0),
        numericInput("mc_b", "Límite superior (b):", value = 1),
        numericInput(
          "mc_N",
          "Número de puntos (N):",
          value = 5000,
          min = 100,
          step = 500
        ),
        actionButton(
          "mc_calcular",
          "Calcular Integral",
          class = "btn-primary w-100"
        )
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(6, plotOutput("mc_plot_geom", height = "350px")),
          column(6, plotOutput("mc_plot_conv", height = "350px"))
        ),
        hr(),
        h4("Resultados Numéricos"),
        verbatimTextOutput("mc_resultados")
      )
    )
  )
  # Agregar pestañas 3 al 5
)
