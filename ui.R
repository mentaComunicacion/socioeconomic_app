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
                                                       selectInput("departamento", "Departamento", c("Departamentos"=""),
                                                                   selected = NULL)),
                                      numericInput("minScoreMap", "Mín. Índice", min = 0, max = 5, value = 0),
                                      numericInput("maxScoreMap", "Máx. Índice", min = 0, max = 5, value = 5)
                        ))
                        ),
                        
                        tags$div(id="cite",
                                 'Desarrollado por ', tags$em('Menta Comunicación SRL'), ' (todos los derechos reservados).'
                        )
                    )
           ),
           
           tabPanel("¿Qué es esto?",
                    icon("crosshair"),
                    h2("Características generales"),
                    br(),
                    h3("Variables contempladas"),
                    h4("Para construir el índice se analizó, a nivel de radio (la unidad más pequeña del Censo 
de Población, Hogares y Viviendas 2010), un conjunto de variables:"),
                    h4(tags$ul(
                      tags$p("1 Porcentaje de hogares con jefe ocupado;"),
                      tags$p("2) Porcentaje de hogares con al menos una Necesidad Básica Insatisfecha (NBI);"),
                      tags$p("3) Porcentaje de mujeres en el radio;"),
                      tags$p("4) Porcentaje de hogares con jefe con secundario completo;"),
                      tags$p("5) Porcentaje de viviendas con calidad de materiales deficiente;"),
                      tags$p("6) Edad promedio del radio;"),
                      tags$p("7) Edad mediana del radio."))),
                    br(),
                    h3("Metodología de construcción"),
                    h4("En base a esta información se realizó un Análisis de Componentes Principales, 
                      una técnica estadística que consiste en obtener la mayor cantidad de información posible de un 
                      conjunto de datos construyendo variables latentes a partir de las variables existentes. Esto 
                      permitió obtener dos índices, uno principal y otro secundario, donde se pueden apreciar 
                      características distintivas de la población de cada radio."),
                    br(),
                    h3("Dimensión principal y secundaria"),

                    h4("A continuación podemos observar cómo este Índice socioeconómico en su dimensión principal
                      identifica con un valor elevado a aquellos radios con bajo porcentaje de hogares con NBI y de viviendas 
                      con materiales deficientes, alto porcentaje de hogares con jefatura con secundario completo, 
                      alta edad promedio y alto porcentaje de mujeres. Esto también se condice con hogares de tamaño 
                  promedio más pequeño."),
                    h4("La primera dimensión del Análisis de Componentes Principales, esto es, el índice principal, 
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