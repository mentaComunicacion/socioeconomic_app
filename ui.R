library(leaflet)
library(rhandsontable)

# Choices for drop-downs
vars <- c(
  "Indice Socioeconomico" = "scr_mnt",
  "Indice (secundario)" = "scr_m_2",
  "Personas" = "totlpbl",
  "Hogares" = "hogares")


navbarPage(title = div(
           div(
            id = "img-id",
            img(src = "http://mentacomunicacion.com.ar/wp-content/uploads/2018/04/logo_menta_web_06.png",
                height = 36, width = 100)
           ),
           "SOCIOECONOMIC INDEX", id = "title"),
           id="navbar",
           position = "fixed-top",
           windowTitle = "Socioeco - Menta", 
           collapsible = TRUE,
           tabPanel("Interactive Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        bootstrapPage(fixedPanel(id = "controls", class = "collapse in",
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = "auto", height = "auto",
                                      h3("Control Dashboard"),
                                      HTML('<button data-toggle="collapse" data-target="#demo"></button>'),
                                      hr(),
                                      tags$div(id = 'demo',  class="collapse in",
                                      selectInput("color", "Color", vars),
                                      selectInput("provincia", "Provinces", provincias, selected = NULL),
                                      conditionalPanel("input.provincia", 
                                                       selectInput("departamento", "Districts", c("Departamentos"=""),
                                                                   selected = NULL)),
                                      numericInput("minScoreMap", "Min. Score", min = 0, max = 5, value = 0),
                                      numericInput("maxScoreMap", "Max. Score", min = 0, max = 5, value = 5)
                        ))
                        ),
                        
                        tags$div(id="cite",
                                 'Developed by ', tags$em('Menta Comunicación SRL'), ' (All rights reserved).'
                        )
                    )
           ),
           
           tabPanel("About",
                    icon("crosshair"),
                    h2("Key Features"),
                    br(),
                    h3("Variables"),
                    h4("The index was built at radius level (the littlest unit of the Argentina National Census) according to a set of variables: "),
                    h4(tags$ul(
                      tags$p("1 % of hhs with worker householder;"),
                      tags$p("2) % of hhs with at least one Unmet Basic Needs (NBI);"),
                      tags$p("3) % of women;"),
                      tags$p("4) % of hhs with householder who has at least a high school degree;"),
                      tags$p("5) % of households con calidad de materiales deficiente;"),
                      tags$p("6) Mean age;"),
                      tags$p("7) Median age."))),
                    br(),
                    h3("Methodology"),
                    h4("The information was analized using a Principal Component Analysis (PCA), 
                    a dimensionality-reduction method used to obtain as much information as possible from a large dataset
                    by transforming existing variables into latent variables. Two indexes were obtained to observe the  
                    distinctive characteristics of the census radius population."),
                    br(),
                    h3("Primary and Secondary Dimensions"),
                    h4("The Primary Dimension of the Socioeconomic Index detects with a high value the radius with: low % of Hhs with Unmet Basic Needs and viviendas con materiales deficientes,
                       high % of Hhs with householder who has at least a high school degree, high figures of mean age and high % of women. Also with smaller households."),
                    h4("The primary dimension "
                    "La primera dimensión del Análisis de Componentes Principales, esto es, el índice principal, 
                    explica  casi la mitad de la variabilidad total (47%) del conjunto de datos y describe,
                       en una sola variable, las características socioeconómicas de la población del radio."),
                    tags$img(src = "pca_dim1.png", align = "center", height = "480px", width = "640px"),
                  br(),
                  br(),
                  h4("La segunda dimensión, que explica alrededor de un 20% de la variablidad total, contempla principalmente las características
                       de los radios con bajo porcentaje de hogares con jefe ocupado:"),
                    tags$img(src = "pca_dim2.png", align = "center",  height = "480px", width = "640px"),
                    hr()
                    ),
           tabPanel("Indicadores",
                    icon("crosshair"),
                    h2("Atributos por radio"),
                    fluidRow(
                      column(3,
                             selectInput("prov", "Provincia", c("Provincias"="", provincias),
                                         selected = 'Mendoza')
                      ),
                      column(3,
                             conditionalPanel("input.prov",
                                              selectInput("deptos", "Departamento", c("Departamentos"="", departamentos), 
                                                          selected = "Capital", multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(2,
                             numericInput("minScore", "Mín. índice", min=0, max=5, value=0)
                      ),
                      column(2,
                             numericInput("maxScore", "Máx. índice", min=0, max=5, value=5)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           )
        )