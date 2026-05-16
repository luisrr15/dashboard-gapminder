
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
    tabName = "inicio"
  ),

  menuItem(
    "Tabla",
    tabName = "tabla"
  ),

  menuItem(
    "Graficos",
    tabName = "graficos"
  ),

  menuItem(
    "Economia",
    tabName = "economia"
  ),
  
  menuItem(
    "Regresion",
    tabName = "regresion"
  ),
  
  menuItem(
    "Comparaciones",
    tabName = "comparaciones"
  ),
  
  menuItem(
    "Mapa",
    tabName = "mapa"
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
        
        p("Proyecto interactivo de analisis economico utilizando la base de datos Gapminder")
        
      ),
      
      # PESTAÑA TABLA
      tabItem(
        
        tabName = "tabla",
        
        DTOutput("tabla")
        
      ),
      
      # PESTAÑA GRAFICOS
      tabItem(
        
        tabName = "graficos",
        
        h2("Graficos Interactivos"),
        
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
        
        plotlyOutput("grafico_economia")
        
      ),
      # PESTAÑA REGRESION
      tabItem(
        
        tabName = "regresion",
        
        h2("Modelo de Regresion"),
        
        verbatimTextOutput("modelo"),
        
        plotlyOutput("grafico_regresion")
        
      ),
      # PESTAÑA COMPARACIONES
      tabItem(
        
        tabName = "comparaciones",
        
        h2("Comparacion Economica por Continente"),
        
        plotlyOutput("grafico_comparacion")
        
      ),
      # PESTAÑA MAPA
      tabItem(
        
        tabName = "mapa",
        
        h2("Mapa Mundial Interactivo"),
        
        leafletOutput("mapa")
        
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
  output$modelo <- renderPrint({
    
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
  # MAPA
  output$mapa <- renderLeaflet({
    
    leaflet() %>%
      
      addTiles()
    
  })
}
# EJECUTAR APP
shinyApp(ui = ui, server = server)