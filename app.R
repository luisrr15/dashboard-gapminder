
# LIBRERIAS
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(gapminder)
library(DT)
library(leaflet)
# BASE DE DATOS EN ESPAÑOL
datos <- gapminder %>%
  
  rename(
    pais = country,
    continente = continent,
    año = year,
    esperanza_vida = lifeExp,
    poblacion = pop,
    pib_percapita = gdpPercap
  )

# INTERFAZ
ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(
    title = "Dashboard Econometrico Mundial"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(

  menuItem(
    "Inicio",
    tabName = "inicio",
    icon = icon("house")
  ),

  menuItem(
    "Tabla",
    tabName = "tabla",
    icon = icon("table")
  ),

  menuItem(
    "Graficos",
    tabName = "graficos",
    icon = icon("chart-line")
  ),

  menuItem(
    "Economia",
    tabName = "economia",
    icon = icon("chart-column")
  ),
  
  menuItem(
    "Regresion",
    tabName = "regresion",
    icon = icon("calculator")
  ),
  
  menuItem(
    "Comparaciones",
    tabName = "comparaciones",
    icon = icon("globe")
  ),
  
  menuItem(
    "Indicadores",
    tabName = "mapa",
    icon = icon("chart-simple")
  )

)
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # PESTAÑA INICIO
      tabItem(
        
        tabName = "inicio",
        
        fluidRow(
          
          valueBox(
            value = "142",
            subtitle = "Paises",
            icon = icon("globe"),
            color = "blue"
          ),
          
          valueBox(
            value = "5",
            subtitle = "Continentes",
            icon = icon("globe"),
            color = "green"
          ),
          
          valueBox(
            value = "1952 - 2007",
            subtitle = "Periodo",
            icon = icon("calendar"),
            color = "yellow"
          )
          
        ),
        
        h2("Analisis Econometrico Mundial"),
        
        p("Este dashboard interactivo presenta un analisis econometrico utilizando la base de datos Gapminder."),
        
        p("Se estudian variables economicas y sociales como el PIB per capita, la esperanza de vida y las diferencias entre paises y continentes."),
        
        tags$hr()
        
      ),
      
      # PESTAÑA TABLA
      tabItem(
        
        tabName = "tabla",
        
        h2("Base de datos"),
        
        p("En esta seccion se presenta la base de datos utilizada para el analisis econometrico."),
        
        p("La base Gapminder contiene informacion economica y social de distintos paises a traves del tiempo."),
        
        tags$hr(),
        
        DTOutput("tabla")
        
      ),
      
      # PESTAÑA GRAFICOS
      tabItem(
        
        tabName = "graficos",
        
        h2("Graficos Interactivos"),
        p("Los graficos permiten analizar la evolucion de variables economicas y sociales entre paises."),
        
        selectInput(
          "continente",
          "Seleccione continente:",
          choices = unique(datos$continente)
        ),
        
        selectInput(
          "pais",
          "Seleccione pais:",
          choices = unique(datos$pais)
        ),
        
        sliderInput(
          "años",
          "Seleccione rango de años:",
          min = min(datos$año),
          max = max(datos$año),
          value = c(min(datos$año), max(datos$año)),
          sep = ""
        ),
        
        plotlyOutput("grafico")
        
      ),
      
      # PESTAÑA ECONOMIA
      tabItem(
        
        tabName = "economia",
        
        h2("Analisis Economico Mundial"),
        
        p("En esta seccion se presentan indicadores economicos globales obtenidos de la base de datos Gapminder."),
        
        p("El analisis permite observar diferencias en el PIB per capita y la evolucion economica entre paises y continentes."),
        
        tags$hr(),
        
        plotlyOutput("grafico_economia", height = "600px")
        
      ),
      # PESTAÑA REGRESION
      tabItem(
        tabName = "regresion",
        
        h2("Regresión econométrica"),
        
        p("En esta sección se estima un modelo de regresión lineal simple para analizar la relación entre el PIB per cápita y la esperanza de vida."),
        
        p("El objetivo es identificar si existe una relación positiva entre el nivel de ingreso de un país y su nivel de bienestar medido a través de la esperanza de vida."),
        
        tags$hr(),
        
        h3("Modelo econométrico"),
        
        p("El modelo estimado es el siguiente:"),
        
        tags$pre("esperanza_vida = β0 + β1 * log(PIB_per_capita) + ε"),
        
        p("Donde β1 mide el efecto del ingreso sobre la esperanza de vida."),
        
        tags$hr(),
        
        h3("Resultados del modelo"),
        
        verbatimTextOutput("modelo_regresion"),
        
        tags$hr(),
        
        h3("Gráfico de regresión"),
        
        plotlyOutput("grafico_regresion", height = "500px"),
        
        tags$hr(),
        
        h3("Interpretación"),
        
        p("Si el coeficiente β1 es positivo y significativo, significa que los países con mayor PIB per cápita tienden a tener mayor esperanza de vida."),
        p("Esto sugiere una relación positiva entre desarrollo económico y bienestar social.")
      ),
      # PESTAÑA COMPARACIONES
      tabItem(
        
        tabName = "comparaciones",
        
        h2("Comparacion Economica por Continente"),
        
        p("Esta seccion compara indicadores economicos entre continentes para identificar diferencias en desarrollo y bienestar."),
        
        p("Las comparaciones permiten visualizar desigualdades economicas entre regiones del mundo."),
        
        tags$hr(),
        
        plotlyOutput("grafico_comparacion")
        
      ),
      # PESTAÑA INDICADORES
      tabItem(
        
        tabName = "mapa",
        
        h2("Indicadores Globales"),
        
        p("Esta seccion presenta indicadores economicos y sociales globales obtenidos de la base de datos Gapminder."),
        
        tags$hr(),
        
        fluidRow(
          
          valueBoxOutput("pib_promedio"),
          
          valueBoxOutput("vida_promedio")
          
        ),
        
        fluidRow(
          
          valueBoxOutput("pais_mayor_pib"),
          
          valueBoxOutput("total_paises")
          
        )
        
        
      )
      
    )
    
  )

)
# SERVIDOR
server <- function(input, output) {
  
  # TABLA
  output$tabla <- renderDT({
    
    datatable(datos)
    
  })
  
  # GRAFICO
  output$grafico <- renderPlotly({
    
    datos_filtrados <- datos %>%
      
      filter(
        
        continente == input$continente,
        
        pais == input$pais,
        
        año >= input$años[1],
        
        año <= input$años[2]
        
      )
    
    grafico <- ggplot(
      
      datos_filtrados,
      
      aes(
        x = año,
        y = esperanza_vida
      )
      
    ) +
      
      geom_line(
        color = "blue",
        size = 1.5
      ) +
      
      geom_point(
        size = 2
      ) +
      
      labs(
        title = paste(
          "Evolucion de Esperanza de Vida:",
          input$pais
        ),
        x = "Año",
        y = "Esperanza de Vida"
      )
    
    ggplotly(grafico)
    
  })
  
  # GRAFICO ECONOMICO
  output$grafico_economia <- renderPlotly({
    
    promedio <- datos %>%
      
      group_by(año) %>%
      
      summarise(
        pib_promedio = mean(pib_percapita)
      )
    
    grafico <- ggplot(
      promedio,
      aes(
        x = año,
        y = pib_promedio
      )
    ) +
      
      geom_line(size = 1.5, color = "blue") +
      
      labs(
        title = "Evolucion del PIB Per Capita Mundial",
        x = "Año",
        y = "PIB Per Capita"
      )
    
    ggplotly(grafico)
    
  })
  # REGRESION
  output$modelo_regresion <- renderPrint({
    
    modelo <- lm(
      esperanza_vida ~ pib_percapita,
      data = datos
    )
    
    summary(modelo)
    
  })
  
  # GRAFICO REGRESION
  output$grafico_regresion <- renderPlotly({
    
    grafico <- ggplot(
      datos,
      aes(
        x = pib_percapita,
        y = esperanza_vida
      )
    ) +
      
      geom_point(color = "darkgreen") +
      
      geom_smooth(
        method = "lm",
        color = "red"
      ) +
      
      labs(
        title = "Relacion entre PIB y Esperanza de Vida",
        x = "PIB Per Capita",
        y = "Esperanza de Vida"
      )
    
    ggplotly(grafico)
    
  })
  
  # COMPARACION ENTRE CONTINENTES
  output$grafico_comparacion <- renderPlotly({
    
    comparacion <- datos %>%
      
      group_by(continente) %>%
      
      summarise(
        pib_promedio = mean(pib_percapita)
      )
    
    grafico <- ggplot(
      comparacion,
      aes(
        x = continente,
        y = pib_promedio,
        fill = continente
      )
    ) +
      
      geom_bar(stat = "identity") +
      
      labs(
        title = "PIB Per Capita Promedio por Continente",
        x = "Continente",
        y = "PIB Per Capita"
      )
    
    ggplotly(grafico)
    
  })
  # PIB PROMEDIO
  output$pib_promedio <- renderValueBox({
    
    valueBox(
      paste0("$", round(mean(datos$pib_percapita), 2)),
      "PIB Per Capita Promedio",
      icon = icon("dollar-sign"),
      color = "green"
    )
    
  })
  
  # ESPERANZA DE VIDA
  output$vida_promedio <- renderValueBox({
    
    valueBox(
      round(mean(datos$esperanza_vida), 1),
      "Esperanza de Vida Promedio",
      icon = icon("heart"),
      color = "blue"
    )
    
  })
  
  # PAIS MAYOR PIB
  output$pais_mayor_pib <- renderValueBox({
    
    pais_top <- datos$pais[which.max(datos$pib_percapita)]
    
    valueBox(
      pais_top,
      "Pais con Mayor PIB",
      icon = icon("globe"),
      color = "yellow"
    )
    
  })
  
  # TOTAL PAISES
  output$total_paises <- renderValueBox({
    
    valueBox(
      length(unique(datos$pais)),
      "Total de Paises",
      icon = icon("flag"),
      color = "red"
    )
    
  })
}
# EJECUTAR APP
shinyApp(ui = ui, server = server)