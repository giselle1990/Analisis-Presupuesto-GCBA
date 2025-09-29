# ===============================================
# Dashboard Presupuesto GCBA - app.R
# ===============================================

# Instalar paquetes si no están
paquetes <- c("shiny", "shinydashboard", "tidyverse", "DT")
instalar <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if (length(instalar)) install.packages(instalar)

# Cargar librerías
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# ============================
# Formateadores ES (escala larga)
# ============================
money_es <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    av <- abs(v)
    if (av >= 1e12) {
      val <- v/1e12
      paste0("$ ", format(round(val, 1), big.mark=".", decimal.mark=","), " billones")
    } else if (av >= 1e9) {
      val <- v/1e9
      paste0("$ ", format(round(val, 1), big.mark=".", decimal.mark=","), " miles de millones")
    } else if (av >= 1e6) {
      val <- v/1e6
      paste0("$ ", format(round(val, 1), big.mark=".", decimal.mark=","), " millones")
    } else {
      paste0("$ ", format(round(v, 0), big.mark=".", decimal.mark=","))
    }
  })
}
money_es_single <- function(x) money_es(x)[1]

# ============================
# Cargar y procesar datos CSV
# ============================
presupuesto <- read_csv("presupuesto.csv", show_col_types = FALSE) %>%
  rename(
    anio = Año,
    area = AREA,
    programa = PROG,
    unidad_ejecutora = UE,
    presupuesto = `Suma de Devengado`,
    descripcion = Descripción
  ) %>%
  mutate(
    presupuesto = as.numeric(gsub("[^0-9\\.]", "", presupuesto))
  )

presupuesto_inflacion <- read_csv("presupuesto_inflacion.csv", show_col_types = FALSE) %>%
  rename(
    anio = AÑO,
    indicador_real = `Multiplicador Real`
  ) %>%
  mutate(indicador_real = as.numeric(indicador_real))

# Agregar columna de presupuesto real
presupuesto <- presupuesto %>%
  left_join(presupuesto_inflacion, by = "anio") %>%
  mutate(presupuesto_real = presupuesto * indicador_real)

# ============================
# Función auxiliar para Tab 5
# ============================
resumen_area <- function(datos, area_sel) {
  datos_area <- datos %>% filter(area == area_sel, anio %in% c(2022, 2023, 2024))
  
  comparacion <- datos_area %>%
    group_by(descripcion, anio) %>%
    summarise(total = sum(presupuesto_real, na.rm=TRUE), .groups="drop") %>%
    pivot_wider(names_from = anio, values_from = total, values_fill = 0) %>%
    mutate(var = .data[["2024"]] - .data[["2022"]])
  
  top3 <- comparacion %>% arrange(desc(`2024`)) %>% slice(1:3)
  bottom3 <- comparacion %>% arrange(var) %>% slice(1:3)
  
  list(top3 = top3, bottom3 = bottom3)
}

# ============================
# UI
# ============================
ui <- dashboardPage(
  title = "Dashboard Presupuesto GCBA",
  skin = "green",
  
  dashboardHeader(title = "Análisis Presupuestario"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio", tabName = "inicio", icon = icon("play-circle")),
      menuItem("Presupuesto", tabName = "montos", icon = icon("chart-bar")),
      menuItem("Evolución", tabName = "evolucion", icon = icon("line-chart")),
      menuItem("Datos", tabName = "datos", icon = icon("table")),
      menuItem("Detalle de Área", tabName = "detalle_area", icon = icon("layer-group")),
      menuItem("Análisis 2022-2024", tabName = "kpi", icon = icon("chart-line"))
    ),
    selected = "inicio"
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .btn-inicio {
          display: block;
          margin: auto;
          margin-top: 25vh;
          font-size: 28px;
          font-weight: bold;
          padding: 20px 40px;
          border-radius: 12px;
          box-shadow: 0px 8px 15px rgba(0,0,0,0.3);
          background-color: #28a745;
          color: white;
          transition: all 0.3s ease 0s;
        }
        .btn-inicio:hover {
          background-color: #218838;
          transform: translateY(-3px);
          box-shadow: 0px 12px 20px rgba(0,0,0,0.35);
        }
      "))
    ),
    
    tabItems(
      # TAB 0: Inicio
      tabItem(tabName = "inicio",
              fluidRow(
                column(12,
                       h2("Análisis Presupuestario del GCBA"),
                       box(width=12, status="success", solidHeader=TRUE,
                           h4("¿Cuáles fueron los programas con menos financiación en 2022-2024?")
                       ),
                       box(width=12, status="success", solidHeader=TRUE,
                           h4("¿Cuáles fueron los de mayor financiación en 2022-2024?")
                       ),
                       box(width=12, status="success", solidHeader=TRUE,
                           h4("¿Se modificaron competencias en la Jefatura de Gobierno?")
                       ),
                       box(width=12, status="success", solidHeader=TRUE,
                           h4("¿Qué tendencias se observan en el periodo analizado?")
                       ),
                       br(),
                       actionButton("go_presupuesto", "Ingresar al Dashboard", class="btn-inicio")
                )
              )
      ),
      
      # TAB 1: Presupuesto
      tabItem(tabName = "montos",
              fluidRow(
                column(
                  width = 6,
                  selectInput("anio", "Seleccionar Año:",
                              choices = sort(unique(presupuesto$anio)),
                              selected = max(presupuesto$anio)
                  )
                )
              ),
              uiOutput("vb_areas"),
              fluidRow(
                box(
                  title = "Programas con Más Presupuesto 🎉 (Global)",
                  width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("graf_top5")
                )
              )
      ),
      
      # TAB 2: Evolución
      tabItem(tabName = "evolucion",
              fluidRow(
                box(
                  title = "Presupuesto Nominal vs Real",
                  width = 12, status = "info", solidHeader = TRUE,
                  plotOutput("graf_evolucion")
                )
              )
      ),
      
      # TAB 3: Datos
      tabItem(tabName = "datos",
              box(
                title = "Presupuesto Detallado", width = 12,
                status = "warning", solidHeader = TRUE,
                DTOutput("tabla_presupuesto")
              )
      ),
      
      # TAB 4: Detalle de Área
      tabItem(tabName = "detalle_area",
              h3(textOutput("titulo_area")),
              fluidRow(
                box(title = "Programas con Más Presupuesto 🎉", width = 6,
                    plotOutput("graf_top5_area")),
                box(title = "Programas con Menos Presupuesto 😡", width = 6,
                    plotOutput("graf_bottom5_area"))
              )
      ),
      
      # TAB 5: Análisis comparado 2022-2024
      tabItem(tabName = "kpi",
              fluidRow(
                column(6,
                       selectInput("area_kpi", "Seleccionar Área:",
                                   choices = unique(presupuesto$area),
                                   selected = unique(presupuesto$area)[1]
                       )
                )
              ),
              fluidRow(
                box(title="Top 3 Programas con Mayor Presupuesto Real (2024)", width=6,
                    plotOutput("graf_top_kpi")),
                box(title="Top 3 Programas con Mayor Caída/Eliminados (2022-2024)", width=6,
                    plotOutput("graf_bottom_kpi"))
              ),
              fluidRow(
                box(title="Interpretación Automática", width=12, status="success",
                    textOutput("texto_kpi"))
              )
      )
    )
  )
)

# ============================
# SERVER
# ============================
server <- function(input, output, session) {
  
  # Botón inicio
  observeEvent(input$go_presupuesto, {
    updateTabItems(session, "tabs", "montos")
  })
  
  # Filtro por año
  datos_filtrados <- reactive({
    presupuesto %>% filter(anio == input$anio)
  })
  
  # Área seleccionada
  area_seleccionada <- reactiveVal(NULL)
  
  # ValueBoxes dinámicos
  output$vb_areas <- renderUI({
    datos_area <- datos_filtrados() %>%
      group_by(area) %>%
      summarise(total = sum(presupuesto, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice(1:5)
    
    fluidRow(
      lapply(seq_len(nrow(datos_area)), function(i) {
        box_color <- c("orange", "green", "blue", "red", "purple")[i]
        monto_fmt <- money_es_single(datos_area$total[i])
        
        tarjeta <- valueBox(
          value = monto_fmt,
          subtitle = datos_area$area[i],
          icon = icon("building"),
          color = box_color,
          width = 12
        )
        
        column(
          width = 4,
          actionLink(
            inputId = paste0("area_", i),
            label = tarjeta,
            style = "width:100%; text-align:left;"
          )
        )
      })
    )
  })
  
  # Observadores para ir al detalle
  observe({
    datos_area <- datos_filtrados() %>%
      group_by(area) %>%
      summarise(total = sum(presupuesto, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice(1:5)
    
    for (i in seq_len(nrow(datos_area))) {
      local({
        idx <- i
        area_name <- datos_area$area[idx]
        observeEvent(input[[paste0("area_", idx)]], {
          area_seleccionada(area_name)
          updateTabItems(session, "tabs", "detalle_area")
        })
      })
    }
  })
  
  # Título detalle
  output$titulo_area <- renderText({
    req(area_seleccionada())
    paste("Detalle del Área:", area_seleccionada(), "-", input$anio)
  })
  
  # Gráficos detalle área
  output$graf_top5_area <- renderPlot({
    req(area_seleccionada())
    datos <- datos_filtrados() %>% filter(area == area_seleccionada())
    top5 <- datos %>% arrange(desc(presupuesto)) %>% slice(1:5)
    
    ggplot(top5, aes(x = reorder(descripcion, presupuesto), y = presupuesto)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(labels = money_es) +
      labs(x = "Programa", y = "Presupuesto")
  })
  
  output$graf_bottom5_area <- renderPlot({
    req(area_seleccionada())
    datos <- datos_filtrados() %>% filter(area == area_seleccionada())
    bottom5 <- datos %>% arrange(presupuesto) %>% slice(1:5)
    
    ggplot(bottom5, aes(x = reorder(descripcion, presupuesto), y = presupuesto)) +
      geom_col(fill = "tomato") +
      coord_flip() +
      scale_y_continuous(labels = money_es) +
      labs(x = "Programa", y = "Presupuesto")
  })
  
  # Gráfico global
  output$graf_top5 <- renderPlot({
    top5 <- datos_filtrados() %>% arrange(desc(presupuesto)) %>% slice(1:5)
    ggplot(top5, aes(x = reorder(descripcion, presupuesto), y = presupuesto)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(labels = money_es) +
      labs(title = paste("Programas con Más Presupuesto 🎉 -", input$anio),
           x = "Programa", y = "Presupuesto")
  })
  
  # Evolución nominal vs real
  output$graf_evolucion <- renderPlot({
    datos_nominal <- presupuesto %>%
      group_by(anio) %>%
      summarise(total_nominal = sum(presupuesto, na.rm=TRUE), .groups="drop")
    
    datos_plot <- datos_nominal %>%
      left_join(presupuesto_inflacion, by="anio") %>%
      mutate(total_real = total_nominal * indicador_real)
    
    ggplot(datos_plot, aes(x=anio)) +
      geom_line(aes(y=total_nominal, color="Nominal"), linewidth=1.2) +
      geom_line(aes(y=total_real, color="Real"), linewidth=1.2) +
      scale_y_continuous(labels=money_es) +
      labs(title="Evolución del Presupuesto",
           y="Monto ($ escala larga)", x="Año", color="Tipo") +
      theme_minimal()
  })
  
  # Tabla
  output$tabla_presupuesto <- renderDT({
    datatable(presupuesto)
  })
  
  # ================= TAB 5: Análisis 2022-2024 =================
  output$graf_top_kpi <- renderPlot({
    datos <- resumen_area(presupuesto, input$area_kpi)$top3
    ggplot(datos, aes(x=reorder(descripcion, `2024`), y=`2024`)) +
      geom_col(fill="darkgreen") +
      coord_flip() +
      scale_y_continuous(labels = money_es) +
      labs(x="", y="Presupuesto Real 2024")
  })
  
  output$graf_bottom_kpi <- renderPlot({
    datos <- resumen_area(presupuesto, input$area_kpi)$bottom3
    ggplot(datos, aes(x=reorder(descripcion, var), y=var)) +
      geom_col(fill="darkred") +
      coord_flip() +
      scale_y_continuous(labels = money_es) +
      labs(x="", y="Variación Real 2022-2024")
  })
  
  # Texto interpretativo automático
  output$texto_kpi <- renderText({
    datos_top <- resumen_area(presupuesto, input$area_kpi)$top3
    datos_bottom <- resumen_area(presupuesto, input$area_kpi)$bottom3
    
    top_nombres <- paste(datos_top$descripcion, collapse = ", ")
    bottom_nombres <- paste(datos_bottom$descripcion, collapse = ", ")
    
    paste0("En el área ", input$area_kpi, 
           " los programas con mayor presupuesto real en 2024 fueron: ", top_nombres, 
           ". En cambio, los programas que sufrieron mayor caída o eliminación fueron: ", bottom_nombres, ".")
  })
}

# ============================
# Lanzar app
# ============================
shinyApp(ui, server)
