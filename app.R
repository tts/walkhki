library(shiny)
library(shinyMobile)
library(leaflet)
library(tidyverse)
library(sf)
library(shinyjs)

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

# Helsinki land area for the 'Where are we' map 
hki <- distr_in_area %>% 
  filter(!Name.x %in% c("Ulkosaaret", "Miessaari", "Aluemeri",
                        "Länsisaaret", "Itäsaaret", Espoo_Vantaa))

#------------------------
# Park roads and trees
#------------------------

roads_in_distr_in_area <- readRDS("roads_in_distr_in_area_latest.RDS")
trees_in_distr_in_area <- readRDS("trees_in_distr_in_area_latest.RDS")

#---------------------
# Protected buildings
#---------------------

prot_build_in_distr_in_area <- readRDS("prot_build_in_distr_in_area_latest.RDS")

#-------------------
# City bike stations
#-------------------

bikestations_in_distr_in_area <- readRDS("bikestations_in_distr_in_area_latest.RDS")

#-------------------------
# Bike accidents 2016-2020
#-------------------------

b_acc <- readRDS("b_acc.RDS")

#-----------------
# shinyMobile app
#-----------------

# https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/
# https://github.com/AugustT/shiny_geolocation

jsCode <- 'shinyjs.geoloc = function() {

        var options = {
          enableHighAccuracy: true,
          timeout: 5000,
          maximumAge: 0
        };
                
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
        
        function onError (err) {
            Shiny.onInputChange("geolocation", false);
        }
        
        function onSuccess (position) {
        
            setTimeout(function () {
                var coords = position.coords;
                console.log(coords.latitude + ", " + coords.longitude);
                Shiny.onInputChange("geolocation", true);
                Shiny.onInputChange("lat", coords.latitude);
                Shiny.onInputChange("accuracy", coords.accuracy);
                Shiny.onInputChange("long", coords.longitude);
            }, 5)
        
        }
};
'


shiny::shinyApp(
  
  
  ui = f7Page(
    
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("geoloc")),
    
    tags$head(
      tags$style(HTML(":root {--f7-theme-color: #d1ae20}")),
      tags$style(HTML('#geoloc{background-color: #d1ae20}')),
      # https://stackoverflow.com/a/64789171
      tags$style(HTML(".shiny-notification {position:fixed;top: 30%;left: 30%;right: 40%;padding: 1px;text-align: center;background-color: #d1ae20}"))),
    
    title = "Walk and bike (but watch out)", 
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
        title = "Walk and bike (but watch out)",
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
            htmlOutput(outputId = "note")
          )
        )
      ),
      
      
      f7Tabs(animated=FALSE, id="tabs", style = c("toolbar"),
             
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
               tabName = "WhereAreWe",
               icon = f7Icon("location"),
               active = FALSE,
               
               f7Row(
                 f7Col(
                   f7Card(
                     actionButton(inputId = "geoloc", label = "Show my location", 
                                  width = "30%", icon = icon("map-marker-alt"),
                                  onClick ="shinyjs.geoloc()")
                   ),
                   f7Card(
                     leafletOutput(outputId = "bigdist")
                   )
                 )
               )
             ),
             
             f7Tab(
               tabName = "BikeAccidents",
               icon = f7Icon("exclamationmark_triangle"),
               active = FALSE,
               
               f7Row(
                 f7Col(
                   f7Card(
                     leafletOutput(outputId = "b_a")
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
                     f7Text(inputId = "aboutdata", label = "Data by Helsinki Region Infoshare, Helsinki Region Transport, and Statistics Finland", value = " "),
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
                       ),
                       f7ListItem(
                         f7Link(label = "Road traffic accidents 2016-2020", href = "https://www.paikkatietohakemisto.fi/geonetwork/srv/eng/catalog.search#/metadata/de71e0a1-4516-4d50-bd54-e384e5174546")
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
    
    pal <- colorFactor(
      palette = c("black", "purple", "red"),
      domain = b_acc$severity
    )
    
    # The name of the district clicked in the "Where are we" map
    r <- reactiveValues(
      d = NULL)
    
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
    
    Accidents <- reactive({
      b_acc %>% 
        filter(Name.x == input$target)
    })
    
    output$dist <- renderLeaflet({
      
      withProgress(message = 'Loading...', detail = '', {
        
        m <- leaflet(sf::st_zm(District())) %>%
          addProviderTiles(
            "OpenStreetMap",
            group = "OpenStreetMap"
          ) %>%
          addProviderTiles(
            "Stamen.Toner",
            group = "Stamen.Toner"
          ) %>%
          addPolygons(color = "steelblue2") %>% 
          addLayersControl(
            baseGroups = c(
              "OpenStreetMap", "Stamen.Toner"
            ),
            overlayGroups = c("Bike accidents", "Bike stations", 
                              "Buildings", "Park roads", "Trees")
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
            addCircleMarkers(data = sf::st_zm(Stations()), color = "yellow", weight = 3, opacity = 0.6, group = "Bike stations")
        }
        
        if(nrow(Accidents()) > 0) {
          m <- m %>%
            addCircleMarkers(data = sf::st_zm(Accidents()), 
                             color = ~pal(severity), 
                             radius = 8, weight = 3, opacity = 0.6,
                       label = paste0(Accidents()$lkmpp, " bike(s) in accident ", 
                                      Accidents()$kkonn, "/", Accidents()$vvonn, 
                                      " at ", Accidents()$kello, ". Outcome: ", Accidents()$severity), 
                       group = "Bike accidents")
        }
        
        m
        
      })
      
    })
    
    
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
                    color = "black", 
                    label = District()$Name.x)
      
      
    })
    
  
    output$b_a <- renderLeaflet({
      
      withProgress(message = 'Loading...', detail = '', {
        
        m_b <- leaflet(sf::st_zm(hki)) %>%
          addProviderTiles(
            "OpenStreetMap",
            group = "OpenStreetMap"
          ) %>%
          addProviderTiles(
            "Stamen.Toner",
            group = "Stamen.Toner"
          ) %>%
          addCircleMarkers(data = sf::st_zm(b_acc),
                           color = ~pal(severity), 
                           radius = 3, weight = 3, opacity = 0.6,
                           label = paste0(b_acc$lkmpp, 
                                          " bike(s) in accident ", 
                                          b_acc$kkonn, "/", b_acc$vvonn, 
                                          " at ", b_acc$kello, 
                                          ". Outcome: ", b_acc$severity)
                           ) %>% 
          addLayersControl(
            baseGroups = c(
              "OpenStreetMap", "Stamen.Toner"
            )
          ) %>% 
          addLegend(pal = pal, values = ~b_acc$severity, opacity = 0.6,
                    title = "Outcome")
        
        m_b
      
      })
      
    })
    
    
    output$note <- renderText({
     paste0("Click a <font color=\"yellow\">yellow</font> bike station to find out the number of available Helsinki city bikes there. ", 
            "<br/><br/>",
            "Circles in other colors show those traffic accidents in 2016-2020 which involved one or more bikes. See label for more info.",
            "<br/><br/>",
            "The default detail map is about the district of Kluuvi. Choose another district from the dropdown list above, or by clicking an area in the tab <i>Where are we</i>")
    })
  
    
    # Bike station click
    observeEvent( input$dist_marker_click, {
      
      click <- input$dist_marker_click
      
      # Exclude other markers
      req(click$group == "Bike stations")

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
                                                       "Content-Type" = "application/graphql",
                                                       "digitransit-subscription-key" = "[your key here")))
      
      json <- jsonlite::fromJSON(httr::content(res, as = "text"))
      
      text <- paste0("Bikes available at ", json$data$bikeRentalStation$name, ": ",json$data$bikeRentalStation$bikesAvailable)
      
      dist <- leafletProxy("dist")
      
      dist %>% clearPopups() %>%
        addPopups(click$lng, click$lat, text)
      
    })
    
    # When the action button is clicked
    observeEvent( input$geoloc, {
      js$geoloc()
    })
    
    
    # When the "Where are we" map is clicked
    observeEvent( input$bigdist_shape_click, {
      
      dclick <- input$bigdist_shape_click
      
      req(dclick)
      
      la <- formatC(dclick$lat, digits = 5, format = "f")
      lo <- formatC(dclick$lng, digits = 5, format = "f")
      
      thisloc <- data.frame(long = lo, lat = la, stringsAsFactors = FALSE)
      thisloc_sf <- sf::st_as_sf(thisloc, coords = c("long","lat"), crs = 4326)
      
      clicked_district_int <- sf::st_intersects(thisloc_sf, hki)
      clicked_district_name <- hki$Name.x[unlist(clicked_district_int)]
      
      this_district <- hki %>%
        filter(Name.x == clicked_district_name)
      
      r$d <- this_district$Name.x
      
    })
    
    # Update the maps (and the histogram if in use) when the 'Where are we' map is clicked,
    # or when a new district is chosen from the list
    observeEvent( r$d, {
      req(r$d != input$target)
      
      updateF7SmartSelect(
        inputId = "target",
        label = "District",
        choices = districts,
        selected = r$d,
        openIn = "popup"
      )
      
    })
    
    # Add a marker showing location. 
    # Note that this never fires if the map is not yet rendered.
    # If needed, use e.g. the map event input$MAPID_center. It provides the coordinates 
    # of the center of the currently *visible* map, which means that the map _is_ rendered. 
    observe({
      
      req(input$lat, input$dist_center, input$bigdist_center)
      
      leafletProxy("dist") %>%
        addMarkers(lat = as.numeric(input$lat), lng = as.numeric(input$long), label = "I am here", layerId = "Me")
      
      leafletProxy("bigdist") %>%
        addMarkers(lat = as.numeric(input$lat), lng = as.numeric(input$long), label = "I am here", layerId = "Me")
      
    })
    
    # Delete the 'I am here' marker by clicking it 
    observe({
      leafletProxy("dist") %>%
        removeMarker(input$dist_marker_click$id)
      
      leafletProxy("bigdist") %>%
        removeMarker(input$bigdist_marker_click$id)
    })
    
  }
  
)

