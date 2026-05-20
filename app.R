
# LIBRERIAS
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(gapminder)
library(DT)

# BASE DE DATOS EN ESPAÑOL
datos <- gapminder %>%
  rename(
    pais           = country,
    continente     = continent,
    año            = year,
    esperanza_vida = lifeExp,
    poblacion      = pop,
    pib_percapita  = gdpPercap
  )

# CSS ESTÉTICO
estilo <- tags$style(HTML("
  .content-wrapper, .right-side { background-color: #F4F6F9; }
  .main-sidebar { background-color: #FFFFFF; border-right: 1px solid #DDE3EC; }
  .sidebar-menu > li > a { color: #4A5568; font-size: 14px; }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover {
    color: #1E3A8A;
    background-color: #EFF6FF;
    border-left: 3px solid #1E3A8A;
  }
  .sidebar-menu > li > a .fa { color: #3B82F6; }
  .main-header .navbar,
  .main-header .logo { background-color: #1E3A8A !important; }
  .box {
    border-top: 3px solid #1E3A8A;
    border-radius: 6px;
    box-shadow: 0 1px 6px rgba(0,0,0,0.07);
  }
  /* Recuadro de bienvenida */
  .bienvenida {
    background-color: #EFF6FF;
    border-left: 4px solid #1E3A8A;
    border-radius: 6px;
    padding: 16px 20px;
    margin-bottom: 20px;
    color: #1E3A8A;
    font-size: 15px;
    line-height: 1.7;
  }
"))

# INTERFAZ
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Dashboard Econometrico Mundial"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Inicio",        tabName = "inicio",       icon = icon("house")),
      menuItem("Tabla",         tabName = "tabla",        icon = icon("table")),
      menuItem("Graficos",      tabName = "graficos",     icon = icon("chart-line")),
      menuItem("Economia",      tabName = "economia",     icon = icon("chart-column")),
      menuItem("Regresion",     tabName = "regresion",    icon = icon("calculator")),
      menuItem("Comparaciones", tabName = "comparaciones",icon = icon("globe")),
      menuItem("Indicadores",   tabName = "mapa",         icon = icon("chart-simple"))
      
    )
    
  ),
  
  dashboardBody(
    
    estilo,
    
    tabItems(
      
      # ── PESTAÑA INICIO ────────────────────────────────────────────
      tabItem(
        tabName = "inicio",
        
        fluidRow(
          valueBox("142",         "Países analizados",       icon = icon("globe"),    color = "blue"),
          valueBox("5",           "Continentes",             icon = icon("earth-americas"), color = "navy"),
          valueBox("1952 - 2007", "Período de análisis",     icon = icon("calendar"), color = "teal")
        ),
        
        # Recuadro de bienvenida
        tags$div(class = "bienvenida",
                 tags$b("¿Qué es este dashboard?"), tags$br(),
                 "Este panel interactivo permite explorar datos económicos y sociales de 142 países
           entre 1952 y 2007, usando la base de datos Gapminder.", tags$br(), tags$br(),
                 tags$b("¿Cómo usarlo?"), tags$br(),
                 tags$b("→ Tabla: "), "filtra y explora los datos por año y continente.", tags$br(),
                 tags$b("→ Gráficos: "), "selecciona un continente y un país para ver su evolución.", tags$br(),
                 tags$b("→ Economía: "), "observa el PIB mundial filtrando por continente.", tags$br(),
                 tags$b("→ Regresión: "), "analiza la relación entre PIB y esperanza de vida.", tags$br(),
                 tags$b("→ Comparaciones: "), "compara continentes eligiendo el año.", tags$br(),
                 tags$b("→ Indicadores: "), "revisa los KPIs globales para cada año."
        ),
        
        tags$hr(),
        
        p("Variables incluidas: PIB per cápita (USD), esperanza de vida (años) y población total.")
        
      ),
      
      # ── PESTAÑA TABLA ─────────────────────────────────────────────
      tabItem(
        tabName = "tabla",
        
        h2("Base de datos"),
        p("Filtra la tabla por año y continente para explorar los registros."),
        tags$hr(),
        
        # MEJORA: filtros encima de la tabla
        fluidRow(
          column(4,
                 selectInput("tabla_año", "Filtrar por año:",
                             choices = c("Todos" = "todos", sort(unique(datos$año))),
                             selected = "todos"
                 )
          ),
          column(4,
                 selectInput("tabla_continente", "Filtrar por continente:",
                             choices = c("Todos" = "todos", as.character(sort(unique(datos$continente)))),
                             selected = "todos"
                 )
          )
        ),
        
        DTOutput("tabla")
        
      ),
      
      # ── PESTAÑA GRAFICOS ──────────────────────────────────────────
      tabItem(
        tabName = "graficos",
        
        h2("Evolución por País"),
        p("Selecciona un continente y luego un país para ver cómo evolucionó su esperanza de vida."),
        tags$hr(),
        
        fluidRow(
          column(4,
                 selectInput("continente", "Seleccione continente:",
                             choices = sort(unique(as.character(datos$continente)))
                 )
          ),
          column(4,
                 # MEJORA: el listado de países se actualiza según el continente
                 selectInput("pais", "Seleccione país:", choices = NULL)
          ),
          column(4,
                 sliderInput("años", "Rango de años:",
                             min = min(datos$año), max = max(datos$año),
                             value = c(min(datos$año), max(datos$año)),
                             sep = "", step = 5
                 )
          )
        ),
        
        plotlyOutput("grafico", height = "420px")
        
      ),
      
      # ── PESTAÑA ECONOMIA ──────────────────────────────────────────
      tabItem(
        tabName = "economia",
        
        h2("Análisis Económico Mundial"),
        p("Evolución del PIB per cápita promedio a lo largo del tiempo."),
        tags$hr(),
        
        # MEJORA: filtro por continente
        fluidRow(
          column(4,
                 selectInput("eco_continente", "Filtrar por continente:",
                             choices = c("Mundial (todos)" = "todos",
                                         as.character(sort(unique(datos$continente)))),
                             selected = "todos"
                 )
          )
        ),
        
        plotlyOutput("grafico_economia", height = "500px")
        
      ),
      
      # ── PESTAÑA REGRESION ─────────────────────────────────────────
      tabItem(
        tabName = "regresion",
        
        h2("Regresión Econométrica"),
        p("Se estima un modelo de regresión lineal simple para analizar la relación entre el PIB per cápita y la esperanza de vida."),
        tags$hr(),
        
        h3("Modelo econométrico"),
        p("El modelo estimado es el siguiente:"),
        tags$pre("esperanza_vida = β0 + β1 * PIB_per_capita + ε"),
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
      
      # ── PESTAÑA COMPARACIONES ─────────────────────────────────────
      tabItem(
        tabName = "comparaciones",
        
        h2("Comparación Económica por Continente"),
        p("PIB per cápita promedio por continente. Cambia el año para ver cómo evolucionaron las diferencias."),
        tags$hr(),
        
        # MEJORA: selector de año para que el gráfico sea interactivo
        fluidRow(
          column(4,
                 selectInput("comp_año", "Seleccionar año:",
                             choices  = sort(unique(datos$año)),
                             selected = 2007
                 )
          )
        ),
        
        plotlyOutput("grafico_comparacion", height = "450px")
        
      ),
      
      # ── PESTAÑA INDICADORES ───────────────────────────────────────
      tabItem(
        tabName = "mapa",
        
        h2("Indicadores Globales"),
        p("Mueve el slider para ver cómo cambian los indicadores en cada año."),
        tags$hr(),
        
        # MEJORA: slider de año para que los valueBox se actualicen
        fluidRow(
          column(6,
                 sliderInput("ind_año", "Seleccionar año:",
                             min = min(datos$año), max = max(datos$año),
                             value = 2007, step = 5, sep = "",
                             animate = animationOptions(interval = 1200, loop = FALSE)
                 )
          )
        ),
        
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
server <- function(input, output, session) {
  
  # ── TABLA con filtros ──────────────────────────────────────────────────────
  output$tabla <- renderDT({
    
    df <- datos
    
    if (input$tabla_año != "todos") {
      df <- df %>% filter(año == as.integer(input$tabla_año))
    }
    
    if (input$tabla_continente != "todos") {
      df <- df %>% filter(as.character(continente) == input$tabla_continente)
    }
    
    datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
    
  })
  
  # ── Actualizar países según continente elegido ─────────────────────────────
  observeEvent(input$continente, {
    paises_filtrados <- datos %>%
      filter(as.character(continente) == input$continente) %>%
      pull(pais) %>% as.character() %>% sort() %>% unique()
    
    updateSelectInput(session, "pais", choices = paises_filtrados)
  })
  
  # ── GRAFICO evolución por país ─────────────────────────────────────────────
  output$grafico <- renderPlotly({
    
    datos_filtrados <- datos %>%
      filter(
        as.character(continente) == input$continente,
        as.character(pais) == input$pais,
        año >= input$años[1],
        año <= input$años[2]
      )
    
    grafico <- ggplot(datos_filtrados, aes(x = año, y = esperanza_vida)) +
      geom_line(color = "#1E3A8A", linewidth = 1.5) +
      geom_point(color = "#1E3A8A", size = 3) +
      labs(
        title    = paste("Esperanza de vida —", input$pais),
        subtitle = paste("Continente:", input$continente),
        x        = "Año",
        y        = "Esperanza de vida (años)"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(grafico)
    
  })
  
  # ── GRAFICO ECONOMICO con filtro de continente ─────────────────────────────
  output$grafico_economia <- renderPlotly({
    
    df <- datos
    
    if (input$eco_continente != "todos") {
      df <- df %>% filter(as.character(continente) == input$eco_continente)
    }
    
    promedio <- df %>%
      group_by(año) %>%
      summarise(pib_promedio = mean(pib_percapita))
    
    titulo <- if (input$eco_continente == "todos") {
      "Evolución del PIB per cápita — Mundial"
    } else {
      paste("Evolución del PIB per cápita —", input$eco_continente)
    }
    
    grafico <- ggplot(promedio, aes(x = año, y = pib_promedio)) +
      geom_line(linewidth = 1.5, color = "#1E3A8A") +
      geom_point(color = "#1E3A8A", size = 3) +
      labs(
        title = titulo,
        x     = "Año",
        y     = "PIB per cápita promedio (USD)"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(grafico)
    
  })
  
  # ── REGRESION ──────────────────────────────────────────────────────────────
  output$modelo_regresion <- renderPrint({
    modelo <- lm(esperanza_vida ~ pib_percapita, data = datos)
    summary(modelo)
  })
  
  output$grafico_regresion <- renderPlotly({
    
    grafico <- ggplot(datos, aes(x = pib_percapita, y = esperanza_vida)) +
      geom_point(color = "#3B82F6", alpha = 0.4, size = 2) +
      geom_smooth(method = "lm", color = "#1E3A8A", se = TRUE) +
      labs(
        title    = "Relación entre PIB per cápita y Esperanza de vida",
        subtitle = "Cada punto es un país en un año determinado",
        x        = "PIB per cápita (USD)",
        y        = "Esperanza de vida (años)"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(grafico)
    
  })
  
  # ── COMPARACIONES por año ──────────────────────────────────────────────────
  output$grafico_comparacion <- renderPlotly({
    
    comparacion <- datos %>%
      filter(año == as.integer(input$comp_año)) %>%
      group_by(continente) %>%
      summarise(pib_promedio = mean(pib_percapita)) %>%
      arrange(desc(pib_promedio))
    
    grafico <- ggplot(
      comparacion,
      aes(x = reorder(continente, pib_promedio), y = pib_promedio, fill = continente)
    ) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(values = c(
        "Africa"   = "#3B82F6",
        "Americas" = "#1E3A8A",
        "Asia"     = "#0EA5E9",
        "Europe"   = "#6366F1",
        "Oceania"  = "#8B5CF6"
      )) +
      labs(
        title    = paste("PIB per cápita promedio por continente —", input$comp_año),
        subtitle = "Ordenado de menor a mayor",
        x        = "Continente",
        y        = "PIB per cápita promedio (USD)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(grafico)
    
  })
  
  # ── INDICADORES filtrados por año ──────────────────────────────────────────
  datos_año <- reactive({
    datos %>% filter(año == input$ind_año)
  })
  
  output$pib_promedio <- renderValueBox({
    valueBox(
      paste0("$", round(mean(datos_año()$pib_percapita), 0)),
      paste("PIB per cápita promedio —", input$ind_año),
      icon  = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$vida_promedio <- renderValueBox({
    valueBox(
      paste0(round(mean(datos_año()$esperanza_vida), 1), " años"),
      paste("Esperanza de vida promedio —", input$ind_año),
      icon  = icon("heart"),
      color = "blue"
    )
  })
  
  output$pais_mayor_pib <- renderValueBox({
    pais_top <- datos_año()$pais[which.max(datos_año()$pib_percapita)]
    valueBox(
      pais_top,
      paste("País con mayor PIB —", input$ind_año),
      icon  = icon("globe"),
      color = "yellow"
    )
  })
  
  output$total_paises <- renderValueBox({
    valueBox(
      length(unique(datos_año()$pais)),
      paste("Países con datos en", input$ind_año),
      icon  = icon("flag"),
      color = "red"
    )
  })
  
}

# EJECUTAR APP
shinyApp(ui = ui, server = server)
