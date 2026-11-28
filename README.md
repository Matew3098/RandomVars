# RandomVars

Aplicación interactiva desarrollada en **R Shiny** para explorar y experimentar con métodos de simulación estocástica y métodos numéricos.

La aplicación permite generar números pseudoaleatorios, simular variables aleatorias discretas y continuas, aproximar integrales mediante el método de Montecarlo y evaluar propiedades estadísticas de secuencias generadas.

## Funcionalidades

La aplicación cuenta con cinco módulos principales:

### 1. Números pseudoaleatorios

Implementación de diferentes métodos para la generación de números pseudoaleatorios:

- Método Congruencial Lineal.
- Método de Lehmer.
- Método de Cuadrados Medios.

Incluye visualizaciones de las secuencias generadas y opciones para exportar los resultados.

### 2. Integración Montecarlo

Aproximación numérica de integrales definidas mediante simulación de Montecarlo.

Incluye:
- Gráficos de convergencia.
- Estimación del error de aproximación.
- Opciones para trabajar con integrales impropias.

### 3. Variables aleatorias discretas

Simulación de variables aleatorias a partir de diferentes distribuciones:

- Binomial.
- Poisson.
- Geométrica.
- Distribuciones personalizadas.

### 4. Variables aleatorias continuas

Generación y simulación de variables aleatorias continuas mediante:

- Uniforme.
- Exponencial.
- Normal.
- Distribuciones personalizadas.

### 5. Bondad de ajuste y diagnóstico

Evaluación de las propiedades estadísticas de muestras y secuencias aleatorias mediante pruebas estadísticas:

- Prueba de Kolmogorov-Smirnov.
- Prueba de Rachas.

## Tecnologías y dependencias

El proyecto está desarrollado utilizando **R** y el framework **Shiny**.

Los principales paquetes utilizados son:

- `shiny`: desarrollo de aplicaciones web interactivas.
- `bslib`: personalización de interfaces.
- `ggplot2`: generación de gráficos estadísticos.
- `readxl`: lectura de archivos Excel.
- `writexl`: exportación de resultados a Excel.
- `plotly`: visualizaciones interactivas.
- `tseries`: herramientas para análisis de series temporales.

## Instalación y ejecución local

### 1. Clonar el repositorio

```bash
git clone https://github.com/Matew3098/RandomVars.git
```

### 2. Acceder al directorio del proyecto

```bash
cd RandomVars
```

### 3. Instalar las dependencias

Desde la consola de R o RStudio, ejecutar:

```r
install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "readxl",
  "writexl",
  "plotly",
  "tseries"
))
```

### 4. Ejecutar la aplicación

Ubicarse en el directorio raíz del proyecto y ejecutar:

```r
shiny::runApp()
```

La aplicación se iniciará en el navegador web predeterminado o en el visor de RStudio.

Imagen de funcionamiento del módulo Variables discretas:
<img width="1295" height="636" alt="image" src="https://github.com/user-attachments/assets/eeee62a9-ce94-4dde-922e-bc616cec9306" />

## Autores

- Mateo Cárdenas
- Milena Palacios

## Información académica

- **Asignatura:** Simulación.
- **Semestre:** 2026-B.

