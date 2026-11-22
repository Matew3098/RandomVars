Aplicación interactiva desarrollada en **R Shiny** para explorar métodos de simulación estocástica y métodos numéricos. Permite generar números pseudoaleatorios, simular variables aleatorias, aproximar integrales mediante Montecarlo y evaluar propiedades estadísticas de secuencias generadas.

## Funcionalidades

La aplicación cuenta con cinco módulos principales:

1. **Números pseudoaleatorios:** implementación de los métodos Congruencial Lineal, Lehmer y Cuadrados Medios, con visualizaciones y exportación de resultados.
2. **Integración Montecarlo:** aproximación de integrales definidas mediante simulación, gráficos de convergencia y estimación del error.
3. **Variables aleatorias discretas:** simulación de distribuciones Binomial, Poisson, Geométrica y distribuciones personalizadas.
4. **Variables aleatorias continuas:** simulación de distribuciones Uniforme, Exponencial, Normal y distribuciones personalizadas.
5. **Bondad de ajuste y diagnóstico:** aplicación de las pruebas Kolmogorov-Smirnov y Rachas para evaluar muestras y secuencias aleatorias.

## Ejecución local

1. Clonar el repositorio:

   ```bash
   git clone <URL_DEL_REPOSITORIO>
   ```

2. Abrir el proyecto en RStudio o acceder a su directorio:

   ```bash
   cd <NOMBRE_DEL_REPOSITORIO>
   ```

3. Instalar los paquetes necesarios (si aún no están instalados):

   ```r
   install.packages(c(
     "shiny",
     "bslib",
     "ggplot2",
     "DT",
     "readxl",
     "writexl"
   ))
   ```

4. Ejecutar la aplicación:

   ```r
   shiny::runApp()
   ```

## Autores

Cárdenas Mateo, Milena Palacios

**Asignatura:** Simulación  
**Semestre:** 2026-B
