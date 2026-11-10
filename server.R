library(shiny)
library(ggplot2)
library(nortest)
library(tseries)
library(openxlsx)
library(plotly)

mti <- function(n, probs, labels) {
  ord <- order(probs, decreasing = TRUE)
  probs_ord <- probs[ord]
  labels_ord <- labels[ord]

  F_x <- cumsum(probs_ord)

  U <- runif(n)

  pos <- findInterval(U, c(0, F_x), rightmost.closed = TRUE)

  return(labels_ord[pos])
}

server <- function(input, output, session) {
  # =========================================================================
  # FUNCIONES DE SOPORTE / ALGORITMOS (A IMPLEMENTAR)
  # =========================================================================

  # Pestaña 1: Algoritmos de generación de números pseudoaleatorios
  generar_mcl <- function(n, semilla, a, c, m) {
    # TODO: Implementar x_{n+1} = (a * x_n + c) mod m
    return(NULL)
  }

  generar_lehmer <- function(n, semilla, c) {
    # TODO: Implementar algoritmo de Lehmer según los pasos de la guía
    return(NULL)
  }

  generar_cuadrados_medios <- function(n, semilla) {
    # TODO: Implementar Método de los Cuadrados Medios (Middle-Square Method)
    return(NULL)
  }

  # Pestaña 2: Integración de Montecarlo
  integrar_montecarlo <- function(expr_str, a, b, N) {
    f <- tryCatch(
      {
        eval(parse(text = paste("function(x) {", expr_str, "}")))
      },
      error = function(e) {
        return(NULL)
      }
    )

    if (!is.null(f)) {
      a <- input$mc_a
      b <- input$mc_b
      N <- input$mc_N

      U <- runif(N)
      x <- a + (b - a) * U
      y <- tryCatch(f(x), error = function(e) NULL)

      if (is.null(y)) {
        return(list(
          valido = FALSE,
          mensaje = "La función ingresada no es válida. Verifica su expresión."
        ))
      }

      int_estimada <- mean(y) * (b - a)
      int_real <- tryCatch(
        integrate(f, lower = a, upper = b)$value,
        error = function(e) NA
      )
      evolucion <- (b - a) * cumsum(y) / (1:N)
    }

    return(list(
      valido = TRUE,
      mensaje = NULL,
      expr_str = expr_str,
      a = a,
      b = b,
      N = N,
      f = f,
      x_sim = x,
      y_sim = y,
      int_estimada = int_estimada,
      error_std = abs(int_estimada - int_real),
      ref = int_real,
      conv_estimates = evolucion
    ))
  }

  # Pestaña 3: Variables Aleatorias Discretas
  simular_discreta <- function(tipo, n_sim) {
    res <- switch(
      tipo,
      "binomial" = {
        rbinom(
          n = input$disc_n_sim,
          size = input$disc_binom_size,
          p = input$disc_binom_prob
        )
      },
      "poisson" = {
        rpois(n = input$disc_n_sim, lambda = input$disc_pois_lambda)
      },
      "geometrica" = {
        rgeom(n = input$disc_n_sim, prob = input$disc_geom_prob)
      },
      "personalizada" = {
        vals <- as.numeric(unlist(strsplit(input$disc_vals, ",")))
        probs <- as.numeric(unlist(strsplit(input$disc_probs, ",")))
        if (abs(sum(probs) - 1) < 1e-6) {
          mti(n = input$disc_n_sim, probs = probs, labels = vals)
        } else {
          stop("Las probabilidades ingresadas no suman 1.")
        }
      },
      stop("Tipo de distribución no reconocido")
    )
    return(res)
  }

  # Pestaña 4: Variables Aleatorias Continuas
  simular_continua <- function(tipo, n_sim, ...) {
    # TODO: Generar Uniforme, Exponencial, Normal o Personalizada (Inversa / Aceptación y Rechazo)
    return(NULL)
  }

  # Pestaña 5: Pruebas de Bondad de Ajuste y Diagnóstico
  ejecutar_pruebas_bondad <- function(datos, alpha, pruebas) {
    # TODO: Aplicar Prueba Kolmogorov-Smirnov (KS) y Prueba de Rachas (Runs Test)
    return(NULL)
  }

  # =========================================================================
  # REACTIVOS PRINCIPALES (EVENTOS Y ALMACENAMIENTO DE DATOS)
  # =========================================================================

  # Reactivo Pestaña 1: Números Pseudoaleatorios
  datos_pseudoaleatorios <- eventReactive(input$gen_calcular, {
    # req(input$gen_semilla, input$gen_n)
    # Lógica según input$gen_metodo ("mcl", "lehmer", "cuadrados")
    return(NULL)
  })

  # Reactivo Pestaña 2: Montecarlo
  datos_montecarlo <- eventReactive(input$mc_calcular, {
    req(input$mc_funcion, input$mc_a, input$mc_b, input$mc_N) # Verficicamos que tengamos desde el ui los datos necesarios para ejecutar ls simulación
    res <- integrar_montecarlo(
      # Con los datos validados ejecuto la expresión
      expr_str = input$mc_funcion,
      a = input$mc_a,
      b = input$mc_b,
      N = input$mc_N
    )
    validate(
      need(
        !is.null(res) && isTRUE(res$valido),
        if (is.null(res)) {
          "No se pudo ejecutar la simulación."
        } else {
          res$mensaje
        }
      )
    )
    return(res)
  })

  # Reactivo Pestaña 3: Variables Discretas
  datos_discretos <- eventReactive(input$disc_calcular, {
    req(input$disc_tipo, input$disc_n_sim)
    muestra <- simular_discreta(input$disc_tipo, input$disc_n_sim)
    validate(need(!is.null(muestra), "No se pudo ejecutar la simulación."))

    # Distribución teórica en el soporte observado
    x <- sort(unique(muestra))
    teorica <- switch(
      input$disc_tipo,
      "binomial" = dbinom(x, input$disc_binom_size, input$disc_binom_prob),
      "poisson" = dpois(x, input$disc_pois_lambda),
      "geometrica" = dgeom(x, input$disc_geom_prob),
      "personalizada" = {
        vals <- as.numeric(unlist(strsplit(input$disc_vals, ",")))
        probs <- as.numeric(unlist(strsplit(input$disc_probs, ",")))
        probs[match(x, vals)]
      }
    )

    return(list(
      tipo = input$disc_tipo,
      muestra = muestra,
      x = x,
      empirica = as.numeric(table(muestra)) / length(muestra),
      teorica = teorica
    ))
  })

  # Reactivo Pestaña 4: Variables Continuas
  datos_continuos <- eventReactive(input$cont_calcular, {
    # req(input$cont_tipo, input$cont_n_sim)
    return(NULL)
  })

  # Reactivo Pestaña 5: Carga o selección de muestra para Diagnóstico
  datos_diagnostico <- eventReactive(input$test_ejecutar, {
    # Si input$test_origen == "p1", tomar datos_pseudoaleatorios()
    # Si input$test_origen == "archivo", leer input$test_file$datapath
    return(NULL)
  })

  # =========================================================================
  # PESTAÑA 1: GENERACIÓN DE NÚMEROS PSEUDOALEATORIOS (OUTPUTS)
  # =========================================================================

  output$gen_plot_hist <- renderPlot({
    # datos <- datos_pseudoaleatorios()
    # req(datos)
    # TODO: Histograma interactivo ajustado a la densidad uniforme U(0,1)
  })

  output$gen_resumen_stats <- renderPrint({
    # datos <- datos_pseudoaleatorios()
    # req(datos)
    # TODO: Resumen estadístico (E[X] y V(X))
  })

  output$gen_tabla_datos <- renderDataTable({
    # datos <- datos_pseudoaleatorios()
    # req(datos)
    # TODO: Tabla de datos generados
  })

  # Exportación Pestaña 1
  output$dl_excel <- downloadHandler(
    filename = function() {
      paste0("pseudoaleatorios_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # TODO: Guardar datos en formato .xlsx
    }
  )

  output$dl_txt <- downloadHandler(
    filename = function() {
      paste0("pseudoaleatorios_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # TODO: Guardar datos en formato .txt / .csv
    }
  )

  # =========================================================================
  # PESTAÑA 2: APROXIMACIÓN DE INTEGRALES POR MONTECARLO (OUTPUTS)
  # =========================================================================

  output$mc_plot_geom <- renderPlot({
    res <- datos_montecarlo()
    req(res)
    plot(
      res$x_sim,
      res$y_sim,
      type = 'p',
      col = '#d7a87c',
      pch = 16,
      xlab = "X",
      ylab = "f(X)",
      main = "Simulación Monte Carlo"
    )
  })

  output$mc_plot_conv <- renderPlot({
    res <- datos_montecarlo()
    req(res)
    plot(
      1:res$N,
      res$conv_estimates,
      type = "l",
      col = '#4a646c',
      xlab = 'Numero de simulaciones',
      ylab = 'Acumulado de la integral',
      main = 'Evolución de la integral',
    )
    abline(h = res$ref, col = '#c0a7de')
  })

  output$mc_resultados <- renderPrint({
    res <- datos_montecarlo()
    req(res)
    cat('El valor aproximado es: ', res$int_estimada, '\n')
    cat('El error estandar es: ', res$error_std, '\n')
    cat('EL valor real es', res$ref, '\n')
  })

  # =========================================================================
  # PESTAÑA 3: GENERACIÓN DE VARIABLES ALEATORIAS DISCRETAS (OUTPUTS)
  # =========================================================================

  output$disc_plot_bar <- renderPlot({
    res <- datos_discretos()
    req(res, res$muestra)
    barplot(
      rbind(res$empirica, res$teorica),
      beside = TRUE,
      names.arg = res$x,
      col = c("#d7a87c", "#4a646c"),
      xlab = "x",
      ylab = "Probabilidad",
      legend.text = c("Empírica", "Teórica")
    )
  })

  output$disc_resumen <- renderPrint({
    res <- datos_discretos()
    req(res, res$muestra)
    cat('La media de los datos es: ', mean(res$muestra), '\n')
    cat('La varianza es: ', var(res$muestra), '\n')
    cat(
      'El error de probabilidad medio es: ',
      mean(abs(res$teorica - res$empirica)),
      '\n'
    )
  })

  output$disc_dl_excel <- downloadHandler(
    filename = function() {
      res <- datos_discretos()
      tipo_str <- if (!is.null(res$tipo)) res$tipo else "discreta"
      paste0("Datos_aleatorios_", tipo_str, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      res <- datos_discretos()
      req(res, res$muestra)
      df <- data.frame(simulacion = seq_along(res$muestra), valor = res$muestra)
      writexl::write_xlsx(df, path = file)
    }
  )

  output$disc_dl_txt <- downloadHandler(
    filename = function() {
      res <- datos_discretos()
      tipo_str <- if (!is.null(res$tipo)) res$tipo else "discreta"
      paste0("Datos_aleatorios_", tipo_str, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- datos_discretos()
      req(res, res$muestra)
      df <- data.frame(simulacion = seq_along(res$muestra), valor = res$muestra)
      write.csv(df, file = file, row.names = FALSE)
    }
  )

  # =========================================================================
  # PESTAÑA 4: GENERACIÓN DE VARIABLES ALEATORIAS CONTINUAS (OUTPUTS)
  # =========================================================================

  output$cont_plot_hist <- renderPlot({
    # res <- datos_continuos()
    # req(res)
    # TODO: Histograma con curva de densidad empírica frente a la teórica
  })

  output$cont_plot_cdf <- renderPlot({
    # res <- datos_continuos()
    # req(res)
    # TODO: CDF empírica vs. teórica
  })

  output$cont_dl_excel <- downloadHandler(
    filename = function() {
      paste0("variables_continuas_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # TODO: Descarga en .xlsx
    }
  )

  output$cont_dl_txt <- downloadHandler(
    filename = function() {
      paste0("variables_continuas_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # TODO: Descarga en .txt / .csv
    }
  )

  # =========================================================================
  # PESTAÑA 5: PRUEBAS DE BONDAD DE AJUSTE Y DIAGNÓSTICO (OUTPUTS)
  # =========================================================================

  output$test_tabla_resultados <- renderTable({
    # res <- datos_diagnostico()
    # req(res)
    # TODO: Tabla con estadístico de prueba, p-valor y decisión (alfa configurable)
  })

  output$test_plot_1 <- renderPlot({
    # res <- datos_diagnostico()
    # req(res)
    # TODO: Gráfico diagnóstico 1 (p. ej. P-P plot / Q-Q plot o ajuste de densidad)
  })

  output$test_plot_2 <- renderPlot({
    # res <- datos_diagnostico()
    # req(res)
    # TODO: Gráfico diagnóstico 2 (p. ej. dispersión temporal para independencia)
  })
}
