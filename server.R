library(shiny)
library(openxlsx)
library(plotly)
library(ggplot2)

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
}
