library(shiny)
library(shinyMobile)
library(leaflet)
library(tidyverse)
library(sf)

#----------
# Districts 
#----------

distr_in_area <- readRDS("distr_in_area.RDS")

districts <- as.vector(sort(distr_in_area$Name.x))

# Districts in neighbour cities bordering Helsinki
Espoo_Vantaa <- c("Otaniemi", "Westend", "Ruukinranta", "Mäkkylä",
                  "Lintulaakso", "Uusmäki", "Ylästö", "Tikkurila",
                  "Pakkala", "Koivuhaka", "Viertola", "Kuninkaala",
                  "Vaarala", "Länsimäki")

districts <- districts[!districts %in% Espoo_Vantaa]

# Helsinki land area for the 'Where are we?' map 
hki <- distr_in_area %>% 
  filter(!Name.x %in% c("Ulkosaaret", "Miessaari", "Aluemeri",
                        "Länsisaaret", "Itäsaaret", Espoo_Vantaa))

#------------------------
# Park roads and trees
#------------------------

roads_in_distr_in_area <- readRDS("roads_in_distr_in_area.RDS")
trees_in_distr_in_area <- readRDS("trees_in_distr_in_area.RDS")

#---------------------
# Protected buildings
#---------------------

prot_build_in_distr_in_area <- readRDS("prot_build_in_distr_in_area.RDS")

#-------------------
# City bike stations
#-------------------

bikestations_in_distr_in_area <- readRDS("bikestations_in_distr_in_area.RDS")

#-----------------
# shinyMobile app
#-----------------

shiny::shinyApp(
  
  ui = f7Page(
    
    waiter::use_waiter(), 
    
    title = "Walking and biking in Helsinki", 
    preloader = FALSE, 
    allowPWA = FALSE,
    
    options = list(
      theme = c("auto"),
      dark = TRUE,
      filled = FALSE,
      touch = list(
        tapHold = TRUE,
        tapHoldDelay = 750,
        iosTouchRipple = FALSE
      ),
      iosTranslucentBars = FALSE,
      navbar = list(
        iosCenterTitle = TRUE,
        hideNavOnPageScroll = TRUE
      ),
      toolbar = list(hideNavOnPageScroll = FALSE),
      pullToRefresh = FALSE
    ),
    
    f7TabLayout(
      navbar = f7Navbar(
        subNavbar = NULL,
        title = "Walk and bike in Helsinki",
        hairline = TRUE,
        shadow = TRUE,
        bigger = FALSE,
        transparent = FALSE,
        leftPanel = TRUE,
        rightPanel = FALSE
      ),
      
      panels = tagList(
        f7Panel(
          side = "left",
          id = "leftpanel",
          theme = c("light"),
          effect = "cover",
          resizable = TRUE,
          
          f7BlockTitle(title = "District", size = 'medium'),
          
          f7Card(
            f7SmartSelect(inputId = "target",
                          label = "District",
                          choices = districts,
                          selected = "Kluuvi",
                          openIn = "popup")
            ),
          
          f7Card(
            textOutput(outputId = "note")
          )
        )
      ),
 
           
      f7Tabs(animated=TRUE, id="tabs", style = c("toolbar"),
            
             f7Tab(
               tabName = "Map",
               icon = f7Icon("map"),
               active = TRUE,
               
               f7Row(
                 f7Col(
                   f7Card(
                     leafletOutput(outputId = "dist")
                   )
                 )
               )
             ),
             
             f7Tab(
               tabName = "Where are we?",
               icon = f7Icon("location"),
               active = FALSE,
               
               f7Row(
                 f7Col(
                   f7Card(
                     leafletOutput(outputId = "bigdist")
                   )
                 )
               )
             ),
             
             f7Tab(
               tabName = "Park roads",
               icon = f7Icon("speedometer"),
               active = FALSE,
               
               f7Row(
                 f7Col(
                   f7Card(
                     plotOutput(outputId = "hist")
                   )
                 )
               )
             ),
             
             f7Tab(
               tabName = "About",
               icon = f7Icon("question_circle"),
               active = FALSE,

               f7Row(
                 f7Col(
                   f7Card(
                     f7Text(inputId = "aboutdata", label = "All data via Helsinki Region Infoshare, and Helsinki Region Transport", value = " "),
                     f7List(
                       f7ListItem(
                         f7Link(label = "Metropolitan area in districts", href = "https://hri.fi/data/en_GB/dataset/paakaupunkiseudun-aluejakokartat")
                       ),
                       f7ListItem(
                         f7Link(label = "City of Helsinki road map", href = "https://hri.fi/data/en_GB/dataset/helsingin-liikennevaylat")
                       ),
                       f7ListItem(
                         f7Link(label = "Buildings and areas protected by detailed plans of the City of Helsinki", href = "https://hri.fi/data/en_GB/dataset/asemakaavoilla-suojellut-rakennukset-ja-alueet-helsingissa")
                       ),
                       f7ListItem(
                         f7Link(label = "Urban tree database of the City of Helsinki", href = "https://hri.fi/data/en_GB/dataset/helsingin-kaupungin-puurekisteri")
                       ),
                       f7ListItem(
                         f7Link(label = "Helsinki Region Transport’s (HSL) city bicycle stations", href = "https://hri.fi/data/en_GB/dataset/hsl-n-kaupunkipyoraasemat")
                       ),
                       f7ListItem(
                         f7Link(label = "Helsinki Region Transport’s (HSL) Digitransit Platform", href = "https://digitransit.fi/en/developers/apis/1-routing-api/bicycling/")
                       )
                       )
                     ),
                   f7Card(
                     f7Text(inputId = "aboutother", label = "Code and blog", value = " "),
                     f7List(
                       f7ListItem(
                         f7Link(label = "R code of this app", href="https://github.com/tts/walkhki")
                       ),
                       f7ListItem(
                         f7Link(label = "Blog post", href = "http://tuijasonkkila.fi/blog/2021/05/walks-and-biking-in-helsinki/")
                       )
                     )
                   )
                 )
               )
             )
      )
    )
    ),


  server = function(input, output, session) {
    
    waiter::waiter_show(
      color = "#1a291f",
      html = waiter::spin_cube_grid()
    )

    District <- reactive({
      distr_in_area %>%
        filter(Name.x == input$target)
    })
    
    Roads <- reactive({
      roads_in_distr_in_area %>% 
        filter(Name.x == input$target)
    })
    
    Trees <- reactive({
      trees_in_distr_in_area %>% 
        filter(Name.x == input$target)
    })
    
    Buildings <- reactive({
      prot_build_in_distr_in_area %>% 
        filter(Name.x == input$target)
    })
    
    Stations <- reactive({
      bikestations_in_distr_in_area %>% 
        filter(Name.x == input$target)
    })
    
    waiter::waiter_hide() 
    
    
    output$dist <- renderLeaflet({
      
     m <- leaflet(sf::st_zm(District())) %>%
       addTiles() %>% 
       addPolygons(color = "steelblue2") %>% 
       addLayersControl(
         overlayGroups = c("Buildings", "Park roads", "Stations", "Trees"),
         options = layersControlOptions(collapsed = FALSE)
       )
      
      if(nrow(Roads()) > 0) {
        m <- m %>%
          addPolylines(data = sf::st_zm(Roads()), color = "sienna", group = "Park roads")
      }
      
      if(nrow(Trees()) > 0) {
        m <- m %>%
          addCircles(data = sf::st_zm(Trees()), color = "springgreen3", label = Trees()$nimi, group = "Trees")
      }
      
      if(nrow(Buildings()) > 0) {
        m <- m %>%
          addPolygons(data = sf::st_zm(Buildings()), color = "darkorange", 
                      label = paste0(Buildings()$osoite, " (", Buildings()$laji, ")"),
                      group = "Buildings")
      }
      
      if(nrow(Stations()) > 0) {
        m <- m %>% 
          addCircleMarkers(data = sf::st_zm(Stations()), color = "yellow", weight = 3, opacity = 0.6, group = "Stations")
      }
        
        m

      
    }
    )
    
    
    output$bigdist <- renderLeaflet({
      
      m2 <- leaflet(sf::st_zm(hki)) %>%
        addTiles() %>%
        addPolygons(fillColor = "white",
                    fillOpacity = 0.3,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    label = hki$Name.x)
      
      m2 <- m2 %>%
        addPolygons(data = sf::st_zm(District()), 
                    color = "red", 
                    label = District()$Name.x)
      
      
    })
    
    
    output$hist <- renderPlot({
      
      if(nrow(Roads()) > 0) {
        p <- hist(Roads()$length_n,
                  breaks = 20,
                  main = paste0("Park road lengths in ", input$target),
                  xlab = "m",
                  ylab = "Count",
                  las = 1)
        p
        
      }
      
    })
    
    
    output$note <- renderText({
      "Click a yellow bike station to find out the number of available bikes. Note that there are a few new stations opening up in summer 2021 with no info yet."
      })
    
         
    observeEvent(input$dist_marker_click, {
      
      click <- input$dist_marker_click
      
      if(is.null(click)){
        return()
      } else {
        
        lo <- formatC(click$lat, digits = 5, format = "f")
        la <- formatC(click$lng, digits = 5, format = "f")
        
        bike_station_clicked <- Stations() %>%
          mutate(X = formatC(sf::st_coordinates(.)[,1], digits = 5, format = "f"),
                 Y = formatC(sf::st_coordinates(.)[,2], digits = 5, format = "f")) %>%
          filter(X == la & Y == lo) %>%
          select(ID) %>%
          sf::st_drop_geometry(.) %>%
          as.character()
        
        res <- httr::POST(url = "https://api.digitransit.fi/routing/v1/routers/hsl/index/graphql",
                          body = paste0('{bikeRentalStation(id:"', bike_station_clicked, '"){name bikesAvailable}}'),
                          httr::add_headers(.headers = c("accept" = "application/json",
                                                         "Content-Type" = "application/graphql")))
        
        json <- jsonlite::fromJSON(httr::content(res, as = "text"))
        
        text <- paste0("Bikes available at ", json$data$bikeRentalStation$name, ": ",json$data$bikeRentalStation$bikesAvailable)
        
        dist <- leafletProxy("dist")
        
        dist %>% clearPopups() %>%
          addPopups(click$lng, click$lat, text)
        
      }
      
    })
    
  }
  
)

