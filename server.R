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
  MCL <- function(a, c, m, x0, N) {
    res <- numeric(N + 1)
    u <- numeric(N + 1) # Contiene los datos normalizados
    res[1] <- x0
    for (i in 2:(N + 1)) {
      res[i] <- (a * res[i - 1] + c) %% m
    }
    u <- res / m
    return(list(Res = res, U = u))
  }

  MLehmer <- function(x0, c, N) {
    n <- nchar(x0) # Longitud de x0
    k <- nchar(c) # Longitud de c
    if (n < k) {
      return(-1)
    }
    res <- numeric(N + 1) # Contiene los xi
    u <- numeric(N + 1) # Contiene los ui
    res[1] <- x0
    for (i in 2:(N + 1)) {
      num <- format(res[i - 1] * c, scientific = FALSE) # Desactiva la notación científica y pone 0's a la izquierda
      dif <- n + k - nchar(num)
      num_izq <- as.numeric(substr(num, 1, k - dif)) # Substrae k valores de la izquierda
      num_der <- as.numeric(substr(num, k + 1 - dif, nchar(num))) # Substrae los valores de las cifras restantes
      res[i] <- num_der - num_izq
    }
    u <- res / (10^n)
    return(list(Res = res, U = u))
  }

  MCM <- function(x0, N) {
    n <- nchar(x0)
    res <- numeric(N + 1)
    u <- numeric(N + 1)
    res[1] <- x0
    for (i in 2:(N + 1)) {
      cuad <- format(res[i - 1]^2, scientific = FALSE)
      dif <- 2 * n - nchar(cuad)
      poscentral <- floor(nchar(cuad) / 2)
      res[i] <- as.numeric(substr(
        cuad,
        poscentral - n / 2 + 1 - ifelse(dif %% 2 != 0, dif - 1, dif),
        poscentral + n / 2 - ifelse(dif %% 2 != 0, dif - 1, dif)
      ))
    }
    u <- res / (10^n)
    return(list(Res = res, U = u))
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
  simular_continua <- function(tipo, n_sim) {
    # TODO: Generar Uniforme, Exponencial, Normal o Personalizada (Inversa / Aceptación y Rechazo)
    if (tipo == "uniforme") {
      a <- input$cont_unif_min
      b <- input$cont_unif_max
      muestra <- runif(n_sim, min = a, max = b)
      f_teorica <- function(x) dunif(x, min = a, max = b)
      F_teorica <- function(x) punif(x, min = a, max = b)
    } else if (tipo == "exponencial") {
      lambda <- input$cont_exp_rate
      muestra <- rexp(n_sim, rate = lambda)
      f_teorica <- function(x) dexp(x, rate = lambda)
      F_teorica <- function(x) pexp(x, rate = lambda)
    } else if (tipo == "normal") {
      mu <- input$cont_norm_mean
      sigma <- input$cont_norm_sd
      muestra <- rnorm(n_sim, mean = mu, sd = sigma)
      f_teorica <- function(x) dnorm(x, mean = mu, sd = sigma)
      F_teorica <- function(x) pnorm(x, mean = mu, sd = sigma)
    } else if (tipo == "personalizada") {
      a <- input$cont_lim_inf
      b <- input$cont_lim_sup
      expr_str <- input$cont_f_densidad

      # Transformación inversa numérica aproximada para f(x) en [a, b]
      U <- runif(n_sim)
      # Generamos una malla fina para evaluar la CDF acumulada numérica
      grid_x <- seq(a, b, length.out = 1000)
      f_eval <- tryCatch(
        {
          sapply(grid_x, function(x) eval(parse(text = expr_str)))
        },
        error = function(e) NULL
      )

      validate(need(
        !is.null(f_eval),
        "Error en la expresión de la densidad f(x). Verifica la sintaxis."
      ))

      # Normalizamos por si acaso el área no es exactamente 1 en el soporte dado
      dx <- grid_x[2] - grid_x[1]
      area <- sum(f_eval) * dx
      f_eval_norm <- f_eval / area

      # Construir CDF empírica numérica en la malla
      cdf_eval <- cumsum(f_eval_norm) * dx
      cdf_eval[length(cdf_eval)] <- 1 # Asegurar 1 al final

      # Método de Transformación Inversa por interpolación lineal
      muestra <- approx(x = cdf_eval, y = grid_x, xout = U, rule = 2)$y
      f_teorica <- function(x) {
        ifelse(x >= a & x <= b, eval(parse(text = expr_str)) / area, 0)
      }
      F_teorica <- function(x) {
        sapply(x, function(val) {
          if (val <= a) {
            return(0)
          }
          if (val >= b) {
            return(1)
          }
          sub_grid <- grid_x[grid_x <= val]
          if (length(sub_grid) == 0) {
            return(0)
          }
          sub_f <- f_eval_norm[1:length(sub_grid)]
          sum(sub_f) * dx
        })
      }
    }
    return(list(
      muestra = muestra,
      tipo = tipo,
      f_teorica = f_teorica,
      F_teorica = F_teorica
    ))
  }

  # Pestaña 5: Pruebas de Bondad de Ajuste y Diagnóstico
  ejecutar_pruebas_bondad <- function(
    datos,
    alpha = 0.05,
    pruebas = c("ks", "runs")
  ) {
    resultados <- list()

    # 1. Prueba de Kolmogorov-Smirnov (Uniformidad en [0, 1])
    if ("ks" %in% pruebas) {
      test_ks <- suppressWarnings(ks.test(datos, "punif", min = 0, max = 1))
      p_ks <- test_ks$p.value
      resultados[["ks"]] <- data.frame(
        Prueba = "Kolmogorov-Smirnov (Uniformidad)",
        Estadístico = round(as.numeric(test_ks$statistic), 5),
        P_Valor = round(p_ks, 5),
        Decisión = ifelse(
          p_ks > alpha,
          "No se rechaza H0 (Uniforme)",
          "Se rechaza H0"
        ),
        stringsAsFactors = FALSE
      )
    }

    # 2. Prueba de Rachas / Runs Test (Independencia)
    if ("runs" %in% pruebas) {
      dicotomica <- factor(datos > median(datos))
      if (nlevels(dicotomica) == 2) {
        test_runs <- tseries::runs.test(dicotomica)
        p_runs <- test_runs$p.value
        resultados[["runs"]] <- data.frame(
          Prueba = "Prueba de Rachas (Independencia)",
          Estadístico = round(as.numeric(test_runs$statistic), 5),
          P_Valor = round(p_runs, 5),
          Decisión = ifelse(
            p_runs > alpha,
            "No se rechaza H0 (Independiente)",
            "Se rechaza H0"
          ),
          stringsAsFactors = FALSE
        )
      } else {
        resultados[["runs"]] <- data.frame(
          Prueba = "Prueba de Rachas (Independencia)",
          Estadístico = NA,
          P_Valor = NA,
          Decisión = "Error: datos constantes (sin variación)",
          stringsAsFactors = FALSE
        )
      }
    }

    tabla_final <- if (length(resultados) > 0) {
      do.call(rbind, resultados)
    } else {
      data.frame(Mensaje = "Selecciona al menos una prueba estadística.")
    }
    rownames(tabla_final) <- NULL
    return(list(
      datos = datos,
      alpha = alpha,
      tabla = tabla_final
    ))
  }

  # =========================================================================
  # REACTIVOS PRINCIPALES (EVENTOS Y ALMACENAMIENTO DE DATOS)
  # =========================================================================

  # Reactivo Pestaña 1: Números Pseudoaleatorios
  datos_pseudoaleatorios <- eventReactive(input$gen_calcular, {
    metodo <- input$gen_metodo
    N <- input$gen_n
    if (metodo == "mcl") {
      resultado <- MCL(
        x0 = input$gen_semilla_m,
        a = input$gen_a,
        c = input$gen_c_m,
        m = input$gen_m,
        N = N
      )
    } else if (metodo == "lehmer") {
      resultado <- MLehmer(x0 = input$gen_semilla_l, c = input$gen_c_l, N = N)
    } else if (metodo == "cuadrados") {
      resultado <- MCM(x0 = input$gen_semilla_c, N = N)
    }
    return(resultado)
  })

  # Reactivo Pestaña 2: Montecarlo
  datos_montecarlo <- eventReactive(input$mc_calcular, {
    req(input$mc_funcion, input$mc_a, input$mc_b, input$mc_N)
    res <- integrar_montecarlo(
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
    req(input$cont_tipo, input$cont_n_sim)
    res <- simular_continua(input$cont_tipo, input$cont_n_sim)
    return(res)
  })

  # Reactivo Pestaña 5: Carga o selección de muestra para Diagnóstico
  datos_diagnostico <- eventReactive(input$test_ejecutar, {
    req(input$test_origen)
    u <- switch(
      input$test_origen,
      "p1" = {
        validate(need(
          input$gen_calcular > 0,
          "Primero genera los números en la Pestaña 1 (botón Calcular)."
        ))
        res_p1 <- datos_pseudoaleatorios()
        validate(need(
          is.list(res_p1),
          "Error en los parámetros del generador de la Pestaña 1 (ej. longitud de semilla < constante)."
        ))
        v <- as.numeric(res_p1$U[-1])
        v <- v[is.finite(v)]
        validate(need(
          all(v >= 0 & v < 1),
          "El generador produjo valores fuera de [0,1). Revisa los parámetros de la Pestaña 1."
        ))
        v
      },
      "archivo" = {
        req(input$test_file)
        ext <- tools::file_ext(input$test_file$name)
        df <- switch(
          tolower(ext),
          "csv" = read.csv(input$test_file$datapath),
          "txt" = read.table(input$test_file$datapath, header = TRUE),
          "xlsx" = openxlsx::read.xlsx(input$test_file$datapath),
          validate("Formato no soportado. Sube un archivo .csv, .txt o .xlsx")
        )
        as.numeric(na.omit(df[[1]]))
      }
    )

    validate(
      need(
        length(u) >= 10,
        "Se necesitan al menos 10 datos numéricos válidos para ejecutar las pruebas."
      )
    )

    alpha <- if (!is.null(input$test_alpha)) input$test_alpha else 0.05
    pruebas_sel <- if (!is.null(input$test_pruebas)) {
      input$test_pruebas
    } else {
      c("ks", "runs")
    }

    return(ejecutar_pruebas_bondad(
      datos = u,
      alpha = alpha,
      pruebas = pruebas_sel
    ))
  })

  # =========================================================================
  # PESTAÑA 1: GENERACIÓN DE NÚMEROS PSEUDOALEATORIOS (OUTPUTS)
  # =========================================================================

  output$gen_plot_hist <- renderPlotly({
    resultado <- datos_pseudoaleatorios()
    req(resultado)
    resultado <- resultado$U[-1]
    df <- data.frame(valores = resultado)
    p <- ggplot(df, aes(x = valores)) +
      geom_histogram(
        aes(y = after_stat(density)),
        fill = "#18bc9c",
        color = "white",
        bins = 15,
        alpha = 0.8
      ) +
      stat_function(
        fun = dunif,
        args = list(min = 0, max = 1),
        color = "#8e44ad",
        linewidth = 1
      ) +
      labs(
        title = 'Números Aleatorios Generados',
        x = 'Valores generados',
        y = "Densidad"
      ) +
      theme_minimal()
    ggplotly(p)
  })

  output$gen_resumen_stats <- renderPrint({
    resultado <- datos_pseudoaleatorios()
    req(resultado)
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

  output$gen_tabla_datos <- renderDataTable({
    resultado <- datos_pseudoaleatorios()
    req(resultado)
    data.frame(
      Iteracion = 0:(length(resultado$U) - 1),
      Valor_Res = resultado$Res,
      Valor_U = resultado$U
    )
  })

  # Exportación Pestaña 1
  output$dl_excel <- downloadHandler(
    filename = function() {
      paste0("Num_Aleatorios_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      resultado <- datos_pseudoaleatorios()
      datos <- data.frame(
        Iteracion = 0:(length(resultado$U) - 1),
        Valor_Res = resultado$Res,
        Valor_U = resultado$U
      )
      write.xlsx(datos, file, rowNames = FALSE)
    }
  )

  output$dl_txt <- downloadHandler(
    filename = function() {
      paste0("Num_Aleatorios_", Sys.Date(), ".csv")
    },
    content = function(file) {
      resultado <- datos_pseudoaleatorios()
      datos <- data.frame(
        Iteracion = 0:(length(resultado$U) - 1),
        Valor_Res = resultado$Res,
        Valor_U = resultado$U
      )
      write.csv(datos, file, row.names = FALSE)
    }
  )

  # =========================================================================
  # PESTAÑA 2: APROXIMACIÓN DE INTEGRALES POR MONTECARLO (OUTPUTS)
  # =========================================================================

  output$mc_plot_geom <- renderPlotly({
    res <- datos_montecarlo()
    req(res)
    df_pts <- data.frame(x = res$x_sim, y = res$y_sim)
    p <- ggplot(df_pts, aes(x = x, y = y)) +
      geom_point(color = "#18bc9c", alpha = 0.6, size = 1.8) +
      stat_function(fun = res$f, color = "#8e44ad", linewidth = 1) +
      labs(
        title = "Simulación Monte Carlo",
        x = "X",
        y = "f(X)"
      ) +
      theme_minimal()
    ggplotly(p)
  })

  output$mc_plot_conv <- renderPlotly({
    res <- datos_montecarlo()
    req(res)
    df_conv <- data.frame(sim = 1:res$N, est = res$conv_estimates)
    p <- ggplot(df_conv, aes(x = sim, y = est)) +
      geom_line(color = "#18bc9c", linewidth = 0.8) +
      labs(
        title = "Evolución de la integral",
        x = "Número de simulaciones",
        y = "Acumulado de la integral"
      ) +
      theme_minimal()
    if (!is.na(res$ref)) {
      p <- p +
        geom_hline(
          yintercept = res$ref,
          color = "#c0a7de",
          linetype = "dashed",
          linewidth = 1
        )
    }
    ggplotly(p)
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

  output$disc_plot_bar <- renderPlotly({
    res <- datos_discretos()
    req(res, res$muestra)
    df_bar <- rbind(
      data.frame(
        x = factor(res$x),
        Probabilidad = res$empirica,
        Tipo = "Empírica"
      ),
      data.frame(
        x = factor(res$x),
        Probabilidad = res$teorica,
        Tipo = "Teórica"
      )
    )

    p <- ggplot(df_bar, aes(x = x, y = Probabilidad, fill = Tipo)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(
        values = c("Empírica" = "#18bc9c", "Teórica" = "#8e44ad")
      ) +
      labs(
        title = "Distribución Empírica vs. Teórica",
        x = "x",
        y = "Probabilidad",
        fill = ""
      ) +
      theme_minimal()

    ggplotly(p)
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

  output$cont_plot_hist <- renderPlotly({
    res <- datos_continuos()
    req(res)
    df <- data.frame(valores = res$muestra)
    p <- ggplot(df, aes(x = valores)) +
      geom_histogram(
        aes(y = after_stat(density)),
        fill = "#18bc9c",
        color = "white",
        bins = 25,
        alpha = 0.7
      ) +
      stat_function(fun = res$f_teorica, color = "#8e44ad", linewidth = 1.2) +
      labs(
        title = "Histograma y Densidad Teórica",
        x = "x",
        y = "Densidad"
      ) +
      theme_minimal()
    ggplotly(p)
  })

  output$cont_plot_cdf <- renderPlotly({
    res <- datos_continuos()
    req(res)
    df <- data.frame(valores = res$muestra)
    p <- ggplot(df, aes(x = valores)) +
      stat_ecdf(geom = "step", color = "#18bc9c", linewidth = 1) +
      stat_function(
        fun = res$F_teorica,
        color = "#8e44ad",
        linewidth = 1.1,
        linetype = "dashed"
      ) +
      labs(
        title = "Función de Distribución Acumulada (CDF)",
        x = "x",
        y = "F(x)"
      ) +
      theme_minimal()
    ggplotly(p)
  })

  output$cont_dl_excel <- downloadHandler(
    filename = function() {
      res <- datos_continuos()
      tipo_str <- if (!is.null(res$tipo)) res$tipo else "continua"
      paste0("var_continua_", tipo_str, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      res <- datos_continuos()
      req(res)
      df <- data.frame(Simulacion = seq_along(res$muestra), Valor = res$muestra)
      openxlsx::write.xlsx(df, file, rowNames = FALSE)
    }
  )

  output$cont_dl_txt <- downloadHandler(
    filename = function() {
      res <- datos_continuos()
      tipo_str <- if (!is.null(res$tipo)) res$tipo else "continua"
      paste0("var_continua_", tipo_str, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- datos_continuos()
      req(res)
      df <- data.frame(Simulacion = seq_along(res$muestra), Valor = res$muestra)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # =========================================================================
  # PESTAÑA 5: PRUEBAS DE BONDAD DE AJUSTE Y DIAGNÓSTICO (OUTPUTS)
  # =========================================================================

  output$test_tabla_resultados <- renderTable(
    {
      res <- datos_diagnostico()
      req(res, res$tabla)
      res$tabla
    },
    digits = 4,
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  output$test_plot_1 <- renderPlotly({
    res <- datos_diagnostico()
    req(res, res$datos)
    n <- length(res$datos)
    df_qq <- data.frame(
      teoricos = qunif(ppoints(n), min = 0, max = 1),
      empiricos = sort(res$datos)
    )
    p <- ggplot(df_qq, aes(x = teoricos, y = empiricos)) +
      geom_point(color = "#18bc9c", alpha = 0.25, size = 1.8) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "#8e44ad",
        linetype = "dashed",
        linewidth = 1
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      labs(
        title = "Q-Q Plot Uniforme(0, 1)",
        x = "Cuantiles Teóricos U(0,1)",
        y = "Cuantiles Empíricos"
      ) +
      theme_minimal()
    ggplotly(p)
  })

  output$test_plot_2 <- renderPlotly({
    res <- datos_diagnostico()
    req(res, res$datos)
    df_seq <- data.frame(
      indice = seq_along(res$datos),
      valor = res$datos
    )
    p <- ggplot(df_seq, aes(x = indice, y = valor)) +
      geom_point(color = "#2c3e50", alpha = 0.25, size = 1) +
      scale_fill_gradient(low = "#e8f8f5", high = "#18bc9c") +
      geom_hline(
        yintercept = 0.5,
        color = "#8e44ad",
        linetype = "dashed",
        linewidth = 0.9
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(
        title = "Dispersión Secuencial (Densidad)",
        x = "Índice (i)",
        y = "Valor U_i",
        fill = "Frecuencia"
      ) +
      theme_minimal()
    ggplotly(p)
  })
}
