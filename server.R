library(shiny)
<<<<<<< HEAD
library(openxlsx)
library(plotly)
library(ggplot2)
=======
# Cargar librerías adicionales según se requieran:
# library(openpyxl)
# library(ggplot2)
# library(nortest)
# library(tseries)
mti <- function(n, probs, labels) {
  ord <- order(probs, decreasing = TRUE)
  probs_ord <- probs[ord]
  labels_ord <- labels[ord]

  F_x <- cumsum(probs_ord)

  U <- runif(n)

  pos <- findInterval(U, c(0, F_x), rightmost.closed = TRUE)

  return(labels_ord[pos])
}
>>>>>>> 739e3f21e11d691a94f7da865048d18ef3e859a5

# Definir la lógica del servidor requerida para Shiny
function(input, output, session) {
  # Aquí se colocan los cálculos, gráficos y elementos reactivos.
  # Por ejemplo, un texto de salida simple:

  output$texto_ejemplo <- renderText({
    paste("Escribiste:", input$mi_input)
  })
  
  # PESTAÑA 1: Generación de Números Pseudoaleatorios
  MCL <- function(a , c , m , x0 , N){
    res <- numeric(N+1)
    u <- numeric(N+1) # Contiene los datos normalizados
    res[1] <- x0
    for(i in 2:(N+1)){
      res[i] <- (a*res[i-1] + c) %% m
    }
    u <- res / m
    return(list(Res = res, U = u))
  }
<<<<<<< HEAD
  
  # Con el metodo de Lehmer pueden aparecer iterantes negativos!!
  MLehmer <- function(x0, c , N){
    n <- nchar(x0) # Longitud de x0
    k <- nchar(c)  # Longitud de c
    if(n < k){
      return(-1)
    }
    res <- numeric(N+1) # Contiene los xi
    u <- numeric(N+1) # Contiene los ui
    res[1] <- x0
    for(i in 2:(N+1))
    {
      num <- format(res[i-1]*c, scientific = FALSE) # Desactiva la notación científica y pone 0's a la izquierda
      dif <- n + k - nchar(num)
      num_izq <- as.numeric(substr(num, 1, k-dif))  # Substrae k valores de la izquierda
      num_der <- as.numeric(substr(num, k+1-dif, nchar(num))) # Substrae los valores de las cifras restantes
      res[i] <- num_der - num_izq
    }
    u <- res / (10^n)
    return(list(Res = res, U = u))
=======

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
>>>>>>> 739e3f21e11d691a94f7da865048d18ef3e859a5
  }
  
  # Con el metodo de los cuadrados medios se presenta un ciclo corto
  MCM <- function(x0,N){
    n <- nchar(x0)
    res <- numeric(N+1)
    u <- numeric(N+1)
    res[1] <- x0
    for(i in 2:(N+1)){
      cuad <- format(res[i-1]^2, scientific = FALSE)
      dif <- 2*n - nchar(cuad)
      poscentral <- floor(nchar(cuad)/2)
      res[i] <-as.numeric(substr(cuad, poscentral - n/2 +1 - ifelse(dif%%2 != 0, dif-1, dif ), poscentral + n/2 - ifelse(dif%%2 != 0, dif-1, dif)))
    }
    u <- res / (10^n)
    return(list(Res = res, U = u))
  }
  
  # Inputs: gen_metodo, gen_n, gen_semilla, gen_a, gen_c, gen_m
  datos_generados <- eventReactive(input$gen_calcular, {
    metodo <- input$gen_metodo
    N <- input$gen_n
    x0 <- input$gen_semilla
    if (metodo == "mcl") {
      resultado <- MCL(x0 = x0, a = input$gen_a, c = input$gen_c, m = input$gen_m,N = N)
    } else if (metodo == "lehmer") {
      resultado <- MLehmer(x0 = x0, c = input$gen_c, N = N)
    } else if (metodo == "cuadrados") {
      resultado <- MCM(x0 = x0, N = N)
    }
    return(resultado)
  })
<<<<<<< HEAD
  
=======

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

>>>>>>> 739e3f21e11d691a94f7da865048d18ef3e859a5
  output$gen_tabla_datos <- renderDataTable({
    resultado <- datos_generados()
    data.frame(
      Iteracion = 0:(length(resultado$U)-1),
      Valor_Res = resultado$Res,
      Valor_U = resultado$U
    )
  })
  
  output$gen_plot_hist <- renderPlotly({
    resultado <- datos_generados()$U[-1]
    df <- data.frame(valores = resultado)
    p <- ggplot(df, aes(x = valores)) +
      geom_histogram(aes(y = after_stat(density)), 
                     fill = "blue",color = "white",bins = 15,alpha = 0.8) +
      stat_function(fun = dunif, args = list(min = 0, max = 1), color = "red", linewidth = 1) +
      labs(title = 'Números Aleatorios Generados',x = 'Valores generados',y = "Probabilidad") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$gen_resumen_stats <- renderPrint({
    resultado <- datos_generados()
    x <- resultado$Res[-1]
    u <- resultado$U[-1]
    cat("VALORES GENERADOS\n")
    print(summary(x))
    cat("Esperanza:", mean(x), "\n")
    cat("Varianza:", var(x), "\n")
    
    cat("\nVALORES NORMALIZADOS\n")
    print(summary(u))
    cat("Esperanza:", mean(u), "\n")
    cat("Varianza:", var(u), "\n")
  })
  
  output$dl_excel <- downloadHandler(
    filename = function() {
      paste0("Num_Aleatorios", ".xlsx")
    },
    content = function(file) {
      resultado <- datos_generados()
      datos <- data.frame(
        Iteracion = 0:(length(resultado$U)-1),
        Valor_Res = resultado$Res,
        Valor_U = resultado$U
      )
      write.xlsx(datos, file, rowNames = FALSE)
    }
  )
  
  output$dl_txt <- downloadHandler(
    filename = function() {
      paste0("Num_Aleatorios", ".csv")
    },
    content = function(file) {
      resultado <- datos_generados()
      datos <- data.frame(
        Iteracion = 0:(length(resultado$U)-1),
        Valor_Res = resultado$Res,
        Valor_U = resultado$U
      )
      write.csv(datos, file, row.names = FALSE)
    }
  )
<<<<<<< HEAD
=======

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
>>>>>>> 739e3f21e11d691a94f7da865048d18ef3e859a5
}
