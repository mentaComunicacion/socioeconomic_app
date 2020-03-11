#Cargo las librerías
paquetes <- list("tidyverse", "ggrepel", "lattice","scales", "RColorBrewer", 
                 "leaflet")

for (i in seq_along(paquetes)) {
  if (!require(paquetes[[i]], character.only = TRUE)) 
  {install.packages(paquetes[[i]])}
  require(paquetes[[i]], character.only = TRUE)
}

set.seed(100)

options(scipen = 999)

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Creo el mapa
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -66, lat = -35, zoom = 5)
  })
  
  # Devuelve los radios que cumplen con el criterio
   zipsInBounds <- reactive({
     filter(resultados, 
            provincia == input$provincia,
            depto == input$departamento,
            scr_mnt >= input$minScoreMap,
            scr_mnt <= input$maxScoreMap
     ) %>% 
    ungroup()
   })
  
  filtered_data <- reactive({
      filter(resultados, 
             provincia == input$provincia,
             depto == input$departamento,
             scr_mnt >= input$minScoreMap,
             scr_mnt <= input$maxScoreMap
      )
    
  })
  
# Asigno los colores y las leyendas de los intervalos de colores

  observe({
    
    if (nrow(filtered_data())==0)
      return(NULL)
    
    colorBy <- input$color

    if (colorBy == "scr_mnt") {
      colorData <- filtered_data()$scr_mnt
      pal <- colorBin("magma", colorData, pretty = T, reverse = T)
    } 
    else if (colorBy == "scr_m_2") {
      colorData <- filtered_data()$scr_m_2
      pal <- colorBin("magma", colorData, pretty = T, reverse = T)
    }
    
    else if (colorBy == "totlpbl") {
      colorData <- filtered_data()$totlpbl
      pal <- colorBin("Blues", colorData, bins = 5)
    }
    else if (colorBy == "hogares") {
      colorData <- filtered_data()$hogares
      pal <- colorBin("Greens", colorData, bins = 5)
    }
    
    bbox <- as.list(st_bbox(filtered_data()))
    lng1 <- bbox$xmin
    lat1 <- bbox$ymin
    lng2 <- bbox$xmax
    lat2 <- bbox$ymax

   leafletProxy("map", 
                data = filtered_data())  %>%
     clearShapes() %>%
     flyToBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>% 
     addPolygons(weight = 1.5, opacity = 0.3, color = "grey", stroke = TRUE,
                 fillColor = ~ pal(colorData), fillOpacity = 0.5, 
                 layerId = ~ link) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title = "Intervals",
                 layerId="colorLegend")
  })
  
  # Muestra el popup con los datos de cada radio
  showZipcodePopup <- function(link_elegido, lat, lng) {
    selectedZip <- filter(zipsInBounds(), link == link_elegido)
    content <- as.character(tagList(
      tags$h4("Índice: ", selectedZip$scr_mnt),
      tags$strong(sprintf("%s, %s",
                               selectedZip$depto, selectedZip$provincia
      )), tags$br(),
      sprintf("Fracción: %s", selectedZip$idfrac), tags$br(),
      sprintf("Radio: %s", selectedZip$idradio), tags$br(),
      sprintf("Personas: %s", selectedZip$totlpbl)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = link_elegido)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })

  ## Inputs ##################################################
  
  observe({
    provincia <- 
      if (is.null(input$provincia)) character(0) else {
        resultados %>%
          `$`('provincia') %>%
          unique() %>%
          sort()
      }
    stillSelected <- isolate(input$provincia[input$provincia %in% provincia])
    updateSelectInput(session, "provincia", choices = provincia,
                      selected = stillSelected)
  })
    
  observe({
    departamento <- 
      if (is.null(input$provincia)) character(0) else {
      resultados %>%
          filter(provincia == input$provincia) %>%
          `$`('depto') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$departamento[input$departamento %in% departamento])
    updateSelectInput(session, "departamento", choices = departamento,
                      selected = stillSelected)
  })
  
  
  ## Data Explorer ###########################################

  observe({
    prov <- 
      if (is.null(input$prov)) character(0) else {
        resultados %>%
          `$`('provincia') %>%
          unique() %>%
          sort()
      }
    stillSelected <- isolate(input$prov[input$prov %in% prov])
    updateSelectInput(session, "prov", choices = prov,
                      selected = stillSelected)
  })
    
  observe({
    deptos <- if (is.null(input$prov)) character(0) else {
      filter(cleantable, provincia %in% input$prov) %>%
        `$`('depto') %>%
        unique() %>%
        sort()
    }
     stillSelected <- isolate(input$deptos[input$deptos %in% deptos])
     updateSelectInput(session, "deptos", choices = deptos,
                       selected = stillSelected)
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        scr_mnt >= input$minScore,
        scr_mnt <= input$maxScore,
        provincia %in% input$prov,
        depto %in% input$deptos) %>%
    select(provincia, depto, idfrac, idradio, scr_mnt, totlpbl, hogares)
    
    names(df) <- c('Provincia', 'Departamento', 'Fracción', 'Radio', 'Índice', 'Personas', 'Hogares')
    
    action <- DT::dataTableAjax(session, df) 
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE) %>% 
      DT::formatStyle(1, backgroundColor = 'white', target = 'row')
  })
  
  # Atención: esta llave cierra la función  
  
###--- PCA plots
  
  output$pca_dim1 <- renderImage({"www/pca_dim1.png"})
  
  output$pca_dim2 <- renderImage({"www/pca_dim2.png"})
  
  
}