library(shiny)

# Definir la lógica del servidor requerida para Shiny
function(input, output, session) {
  # Aquí se colocan los cálculos, gráficos y elementos reactivos.
  # Por ejemplo, un texto de salida simple:

  output$texto_ejemplo <- renderText({
    paste("Escribiste:", input$mi_input)
  })
}
