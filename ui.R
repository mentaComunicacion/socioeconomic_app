library(leaflet)
library(rhandsontable)

# Choices for drop-downs
vars <- c(
  "Socioeconomic Index" = "scr_mnt",
  "Index (secondary)" = "scr_m_2",
  "Total Population" = "totlpbl",
  "Households" = "hogares")


navbarPage(title = div(
           div(
            id = "img-id",
            tags$a(img(src = "http://mentacomunicacion.com.ar/wp-content/uploads/2018/04/logo_menta_web_06.png",
                       height = 36, width = 100),href="https://mentacomunicacion.com.ar/bigdata/",target = '_blank')),
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
                                 'Developed by ', tags$em('Menta Comunicación SRL')
                        )
                    )
           ),
           
           tabPanel("Attributes",
                    icon("crosshair"),
                    h2("Attributes per radius"),
                    fluidRow(
                      column(3,
                             selectInput("prov", "Province", c("Provincias"="", provincias),
                                         selected = 'Mendoza')
                      ),
                      column(3,
                             conditionalPanel("input.prov",
                                              selectInput("deptos", "District", c("Departamentos"="", departamentos), 
                                                          selected = "Capital", multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(2,
                             numericInput("minScore", "Min. score", min=0, max=5, value=0)
                      ),
                      column(2,
                             numericInput("maxScore", "Max. score", min=0, max=5, value=5)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
           tabPanel("About",
                    icon("crosshair"),
                    h2("Key Features"),
                    br(),
                    h3("Variables"),
                    h4("The index was built at radius level (the littlest unit of the Argentina National Census) according to a set of variables: "),
                    h4(tags$ul(
                      tags$p("1 % of hhs with employed householder;"),
                      tags$p("2) % of hhs with at least one Unmet Basic Needs (NBI);"),
                      tags$p("3) % of women;"),
                      tags$p("4) % of hhs with householder who has at least a high school degree;"),
                      tags$p("5) % of hhs with poor housing conditions;"),
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
                    h4("The Primary Dimension of the Socioeconomic Index detects high figures in radius with: low % of hhs with Unmet Basic Needs (NBI) and with poor housing conditions,
                       high % of hhs with householder who has at least a high school degree, high figures of mean age and high % of women. Also with smaller households."),
                    h4("The Primary Dimension of the PCA explains almost half of the total variability (47%) of the data set and it describes in only one variable the socioeconomic characteristics 
                       of the radius population."),
                    tags$img(src = "pca_dim1.png", align = "center", height = "480px", width = "800px"),
                    br(),
                    br(),
                    h4("The Secondary Dimension explains almost 20% of the total variability and it mainly describes the characteristics of radius 
                     with low % of hhs with employed householder:"),
                    tags$img(src = "pca_dim2.png", align = "center",  height = "480px", width = "800px"),
                    hr()
           )
           
        )