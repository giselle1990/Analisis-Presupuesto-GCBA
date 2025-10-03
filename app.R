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
      paste0("$ ", format(round(v, 0), big.mark=".", decimal.mark=",")) }
  })
}
money_es_single <- function(x) money_es(x)[1]

# ============================
# Cargar y procesar datos CSV
# ============================
# Presupuesto general
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

# Inflación / deflactor
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

# Datos Jefatura
jefatura_data <- read_csv("data.csv", show_col_types = FALSE) %>%
  rename(
    descripcion = Descripción,
    anio = Año,
    presupuesto = `Devengado Real`
  ) %>%
  mutate(
    anio = as.integer(anio),
    presupuesto = as.numeric(gsub("[^0-9]", "", presupuesto))
  )

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
  skin = "green",
  
  dashboardHeader(title = "Dashboard Presupuesto GCBA"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio", tabName = "inicio", icon = icon("play-circle")),
      menuItem("Presupuesto", tabName = "montos", icon = icon("chart-bar")),
      menuItem("Evolución", tabName = "evolucion", icon = icon("line-chart")),
      menuItem("Datos", tabName = "datos", icon = icon("table")),
      menuItem("Detalle de Área", tabName = "detalle_area", icon = icon("layer-group")),
      menuItem("Análisis 2022-2024", tabName = "kpi", icon = icon("chart-line")),
      menuItem("Jefatura de Gobierno", tabName = "jefatura", icon = icon("landmark")),
      menuItem("Políticas Públicas", tabName = "politicas", icon = icon("image")) 
    ),
    selected = "inicio"
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .small-box {
        border: 2px solid black !important;   /* borde negro */
        box-shadow: 5px 5px 15px rgba(0,0,0,0.4);  /* relieve/sombra */
        border-radius: 12px !important;       /* bordes más suaves */
      }
      .small-box h3 {
        font-weight: bold;
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
                           h4("¿Cuáles fueron los programas con menos financiación en 2022-2024?")),
                       box(width=12, status="success", solidHeader=TRUE,
                           h4("¿Cuáles fueron los de mayor financiación en 2022-2024?")),
                       box(width=12, status="success", solidHeader=TRUE,
                           h4("¿Se modificaron competencias en la Jefatura de Gobierno?")),
                       box(width=12, status="success", solidHeader=TRUE,
                           h4("¿Qué tendencias se observan en el periodo analizado?")),
                       br(),
                       div(style = "text-align:center; margin-top:20px;",
                           actionButton("go_presupuesto", "Ingresar al Dashboard",
                                        style = "font-size:28px; font-weight:bold; 
                                             font-family:'DIN', Arial, Helvetica, sans-serif;
                                             color:black;
                                             background-color:#8FBC8F;
                                             border:none; border-radius:12px;
                                             padding:20px 40px;"))
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
                                   selected = unique(presupuesto$area)[1])
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
                    htmlOutput("texto_kpi"))   # 👈 cambio aquí
              )
      ),
      
      
      # TAB 6: Jefatura
      tabItem(tabName = "jefatura",
              fluidRow(
                column(12, align = "center",
                       selectInput("anio_jef", "Seleccionar Año:",
                                   choices = sort(unique(jefatura_data$anio)),
                                   selected = min(jefatura_data$anio),
                                   width = "50%")
                )
              ),
              br(),
              h3("Programas de Jefatura de Gobierno"),
              uiOutput("resumen_jefatura"),
              br(),
              box(title = "Análisis", width = 12, status = "info", solidHeader = TRUE,
                  HTML("
              <p style='text-align:justify; font-size:16px; line-height:1.6;'>
              El análisis de la evolución del presupuesto asignado a la 
              <strong>Jefatura de Gobierno de la Ciudad de Buenos Aires</strong> en el período 
              <strong>2022–2024</strong> evidencia un marcado <strong>crecimiento en la cantidad de competencias 
              y programas bajo su órbita</strong>, acompañado de un incremento en los montos que se le asignan año tras año. 
              Este fenómeno no resulta neutral desde el punto de vista institucional: lejos de tratarse de una mera 
              reorganización administrativa, puede interpretarse como una estrategia orientada a 
              <strong>concentrar funciones y recursos en un área que, según la propia Constitución de la Ciudad y la Ley N.º 70</strong>, 
              no se encuentra sometida al mismo nivel de control externo que las demás jurisdicciones. 

              <br><br>

              Dicho marco normativo establece que el <strong>control del gasto público debe ser ejercido por los organismos 
              de control autónomos de la Ciudad</strong>, con el objetivo de garantizar transparencia, eficiencia y legalidad en la gestión estatal. 
              Sin embargo, la incorporación progresiva de nuevas competencias a la Jefatura de Gobierno termina funcionando, en los hechos, 
              como una <em>zona de excepción al control</em>, pues al desplazarse programas y partidas presupuestarias hacia esta área 
              se reduce el alcance real de la auditoría sobre la ejecución de los recursos públicos. 

              <br><br>

              A mi criterio, este crecimiento presupuestario y funcional en la Jefatura no sólo refleja una tendencia a la 
              <strong>centralización política</strong>, sino también una forma de 
              <strong>esquivar deliberadamente los mecanismos de fiscalización externa</strong> que la Constitución porteña había diseñado 
              justamente para asegurar un uso responsable y transparente del dinero de la ciudadanía. 
              </p>
            ")
              )
      ),
      
      
      # TAB 7: Políticas Públicas
      tabItem(tabName = "politicas",
              h2("Importancia de Políticas Públicas Basadas en Datos"),
              fluidRow(
                box(title = "Filmina", width = 12, status = "primary", solidHeader = TRUE,
                    imageOutput("img_politicas", height = "600px"))
              ),
              br(),
              div(style = "text-align:center; margin-top:20px;",
                  actionButton("volver_inicio_politicas", "Volver al Dashboard Principal",
                               style = "font-size:20px; font-weight:bold; 
                                font-family:'DIN', Arial, Helvetica, sans-serif;
                                color:black;
                                background-color:#8FBC8F;
                                border:none; border-radius:12px;
                                padding:10px 25px;"))
      )
      
    ) # <- cierre de tabItems
  )   # <- cierre de dashboardBody
)     # <- cierre de dashboardPage

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
      geom_col(fill = "#90EE90") +  # verde claro
      geom_text(aes(label = scales::comma(presupuesto, big.mark=".", decimal.mark=",")), 
                position = position_stack(vjust = 0.5), color = "black", size = 3) +
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
      geom_text(aes(label = scales::comma(presupuesto, big.mark=".", decimal.mark=",")), 
                position = position_stack(vjust = 0.5), color = "black", size = 3) +
      coord_flip() +
      scale_y_continuous(labels = money_es) +
      labs(x = "Programa", y = "Presupuesto")
  })
  
  # Gráfico global
  output$graf_top5 <- renderPlot({
    top5 <- datos_filtrados() %>% arrange(desc(presupuesto)) %>% slice(1:5)
    
    ggplot(top5, aes(x = reorder(descripcion, presupuesto), y = presupuesto)) +
      geom_col(fill = "#90EE90") +  # verde claro
      geom_text(aes(label = scales::comma(presupuesto, big.mark=".", decimal.mark=",")), 
                position = position_stack(vjust = 0.5), color = "black", size = 3) +
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
  # Interpretación Automática con HTML
  output$texto_kpi <- renderUI({
    datos_top <- resumen_area(presupuesto, input$area_kpi)$top3
    datos_bottom <- resumen_area(presupuesto, input$area_kpi)$bottom3
    
    top_nombres <- paste(datos_top$descripcion, collapse = ", ")
    bottom_nombres <- paste(datos_bottom$descripcion, collapse = ", ")
    
    HTML(paste0(
      "<p style='font-size:15px; line-height:1.6; text-align:justify;'>
    El análisis del área <strong>", input$area_kpi, "</strong> en el período 2022–2024 
    muestra un <strong>crecimiento diferencial en ciertos programas</strong>. 
    En 2024, los programas más financiados fueron: <span style='color:darkgreen;'>", top_nombres, "</span>. 
    Este comportamiento refleja una priorización de políticas y recursos en dichos sectores. 
    <br><br>
    En contraposición, se observa una <strong>caída presupuestaria significativa</strong> o incluso 
    la eliminación de programas como: <span style='color:darkred;'>", bottom_nombres, "</span>. 
    Este retroceso puede responder a cambios de gestión, redefinición de prioridades 
    o estrategias de reasignación del gasto.
    <br><br>
    En conjunto, la evolución presupuestaria evidencia tanto las <em>apuestas políticas</em> 
    del gobierno como las áreas relegadas en el período analizado, lo que permite 
    comprender mejor la orientación del gasto público en la Ciudad.</p>"
    ))
  })
  
  # ================= TAB 6: Jefatura =================
  output$resumen_jefatura <- renderUI({
    anio_sel <- input$anio_jef
    
    prev <- jefatura_data %>%
      filter(anio < anio_sel) %>%
      distinct(descripcion)
    
    actuales <- jefatura_data %>% filter(anio == anio_sel)
    
    nuevas <- anti_join(actuales, prev, by = "descripcion")
    
    if (anio_sel == min(jefatura_data$anio)) {
      box(
        title = paste("Programas existentes en", anio_sel),
        width = 12, status = "success", solidHeader = TRUE,
        HTML(paste0("<ul>", paste0("<li>", actuales$descripcion, "</li>", collapse = ""), "</ul>"))
      )
    } else {
      box(
        title = paste("Programas sumados en", anio_sel),
        width = 12, status = "primary", solidHeader = TRUE,
        if (nrow(nuevas) > 0) {
          HTML(paste0("<ul>", paste0("<li>", nuevas$descripcion, "</li>", collapse = ""), "</ul>"))
        } else {
          p("No se sumaron programas nuevos en este año.")
        }
      )
    }
  })
  
  # ================= TAB 7: Políticas Públicas =================
  output$img_politicas <- renderImage({
    list(
      src = "C.png",  
      contentType = 'image/png',
      width = "100%",
      height = "100%",
      alt = "Importancia de Políticas Públicas Basadas en Datos"
    )
  }, deleteFile = FALSE)
  observeEvent(input$volver_inicio_politicas, {
    updateTabItems(session, "tabs", "montos")
  })
}
# Lanzar app
# ============================
shinyApp(ui, server)
