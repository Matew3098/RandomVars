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
        tags$head(
          tags$style(HTML(
            "
            #mc_resultados {
                font-size: 16px;
                font-family: Arial;
                line-height: 1.5;
                }
            "
          ))
        ),

        verbatimTextOutput("mc_resultados")
      )
    )
  ),

  # =========================================================================
  # PESTAÑA 3: Generación de Variables Aleatorias Discretas
  # =========================================================================
  tabPanel(
    "Variables Discretas",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Configuración Discreta"),
        selectInput(
          "disc_tipo",
          "Tipo de Distribución:",
          choices = c(
            "Binomial" = "binomial",
            "Poisson" = "poisson",
            "Geométrica" = "geometrica",
            "Personalizada (Transformación Inversa)" = "personalizada"
          )
        ),
        # Condicionales o campos generales
        sliderInput(
          "disc_n_sim",
          "Tamaño de muestra:",
          min = 100,
          max = 10000,
          value = 1000,
          step = 100
        ),
        conditionalPanel(
          condition = "input.disc_tipo == 'binomial'",
          numericInput("disc_binom_size", "Ensayos (n):", value = 10),
          sliderInput(
            "disc_binom_prob",
            "Probabilidad (p):",
            min = 0.01,
            max = 0.99,
            value = 0.5
          )
        ),
        conditionalPanel(
          condition = "input.disc_tipo == 'poisson'",
          numericInput("disc_pois_lambda", "Lambda (lambda):", value = 3.5)
        ),
        conditionalPanel(
          condition = "input.disc_tipo == 'geometrica'",
          sliderInput(
            "disc_geom_prob",
            "Probabilidad (p):",
            min = 0.01,
            max = 0.99,
            value = 0.4
          )
        ),
        conditionalPanel(
          condition = "input.disc_tipo == 'personalizada'",
          textAreaInput(
            "disc_vals",
            "Valores X (separados por coma):",
            value = "1, 2, 3, 4"
          ),
          textAreaInput(
            "disc_probs",
            "Probabilidades P(X=x) (suman 1):",
            value = "0.1, 0.3, 0.4, 0.2"
          )
        ),
        actionButton(
          "disc_calcular",
          "Simular Discreta",
          class = "btn-primary w-100"
        ),
        hr(),
        downloadButton("disc_dl_excel", "Exportar .xlsx", class = "w-100 mb-2"),
        downloadButton("disc_dl_txt", "Exportar .txt", class = "w-100")
      ),
      mainPanel(
        width = 9,
        plotOutput("disc_plot_bar", height = "400px"),
        hr(),
        h4("Resumen Descriptivo"),
        verbatimTextOutput("disc_resumen")
      )
    )
  ),

  # =========================================================================
  # PESTAÑA 4: Generación de Variables Aleatorias Continuas
  # =========================================================================
  tabPanel(
    "Variables Continuas",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Configuración Continua"),
        selectInput(
          "cont_tipo",
          "Tipo de Distribución:",
          choices = c(
            "Uniforme" = "uniforme",
            "Exponencial" = "exponencial",
            "Normal" = "normal",
            "Personalizada (Aceptación/Rechazo o Inv.)" = "personalizada"
          )
        ),
        numericInput(
          "cont_n_sim",
          "Tamaño de muestra:",
          value = 1000,
          min = 100
        ),
        conditionalPanel(
          condition = "input.cont_tipo == 'uniforme'",
          numericInput("cont_unif_min", "Mínimo (a):", value = 0),
          numericInput("cont_unif_max", "Máximo (b):", value = 1)
        ),
        conditionalPanel(
          condition = "input.cont_tipo == 'exponencial'",
          numericInput("cont_exp_rate", "Rate (lambda):", value = 1.0)
        ),
        conditionalPanel(
          condition = "input.cont_tipo == 'normal'",
          numericInput("cont_norm_mean", "Media (mu):", value = 0),
          numericInput(
            "cont_norm_sd",
            "Desviación Estándar (sigma):",
            value = 1
          )
        ),
        conditionalPanel(
          condition = "input.cont_tipo == 'personalizada'",
          textInput("cont_f_densidad", "Densidad f(x):", value = "2*x"),
          numericInput("cont_lim_inf", "Límite Inferior Soporte:", value = 0),
          numericInput("cont_lim_sup", "Límite Superior Soporte:", value = 1)
        ),
        actionButton(
          "cont_calcular",
          "Simular Continua",
          class = "btn-primary w-100"
        ),
        hr(),
        downloadButton("cont_dl_excel", "Exportar .xlsx", class = "w-100 mb-2"),
        downloadButton("cont_dl_txt", "Exportar .txt", class = "w-100")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(6, plotOutput("cont_plot_hist", height = "350px")),
          column(6, plotOutput("cont_plot_cdf", height = "350px"))
        )
      )
    )
  ),

  # =========================================================================
  # PESTAÑA 5: Pruebas de Bondad de Ajuste y Diagnóstico Estocástico
  # =========================================================================
  tabPanel(
    "Pruebas de Bondad y Diagnóstico",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Entrada de Datos"),
        radioButtons(
          "test_origen",
          "Origen de la muestra:",
          choices = c(
            "Usar datos de Pestaña 1" = "p1",
            "Subir archivo externo" = "archivo"
          )
        ),
        conditionalPanel(
          condition = "input.test_origen == 'archivo'",
          fileInput(
            "test_file",
            "Seleccionar archivo (.csv / .xlsx):",
            accept = c(".csv", ".xlsx", ".xls")
          )
        ),
        hr(),
        h4("Configuración de Pruebas"),
        sliderInput(
          "test_alpha",
          "Nivel de significancia (alfa):",
          min = 0.01,
          max = 0.10,
          value = 0.05,
          step = 0.01
        ),
        checkboxGroupInput(
          "test_pruebas",
          "Seleccionar Pruebas:",
          choices = c(
            "Prueba Kolmogorov-Smirnov (KS)" = "ks",
            "Prueba de Rachas (Runs Test)" = "rachas"
          ),
          selected = c("ks", "rachas")
        ),
        actionButton(
          "test_ejecutar",
          "Ejecutar Diagnóstico",
          class = "btn-primary w-100"
        )
      ),
      mainPanel(
        width = 9,
        h4("Resultados de las Pruebas Estadísticas"),
        tableOutput("test_tabla_resultados"),
        hr(),
        h4("Gráficos Diagnósticos"),
        fluidRow(
          column(6, plotOutput("test_plot_1", height = "300px")),
          column(6, plotOutput("test_plot_2", height = "300px"))
        )
      )
    )
  )
)
