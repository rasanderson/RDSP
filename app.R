# Libraries needed for app
library(leaflet)
library(mapview)
library(shiny)
library(shinydashboard)
library(sf)
library(rgdal)
library(raster)
library(dplyr)

source("setup.R")
#source("road_casualties.R")

ui <- dashboardPage(title = "Newcastle University Rural Data Science PlatForm" ,
  dashboardHeader(title="Rural Data Science Platform"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="home", icon=icon("home")),
      menuItem("Biodiversity", tabName = "nbn", icon = icon("bug")),
      menuItem("Farming", tabName = "AgCensus", icon = icon("grain", lib="glyphicon")),
      menuItem("Hydrology", tabName = "Hydrology", icon = icon("tint")),
      menuItem("Health", tabName = "Campy", icon = icon("ambulance")),
      menuItem("Traffic", tabName = "Traffic", icon = icon("car"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", # UI Home ####
              h1("Newcastle University: Rural Data Science Platform (beta)"),
              img(src="cheviots.png"),
              h2("Introduction"),
              h3("Welcome to the Rural Data Science Platform (RDSP). This website give access
                to physical, environmental, socio-economic and medical data for
                North East England. Navigate through the website using the menu 
                bars on the left."),
              hr(),
              h3("This website has been developed part of a pilot study to explore the potential for a
                Rural Data Science Platform. It will provide a repository for data, collected from
                national, regional, local and University sources. It will also integrate with 
                real-time models, to allow end-users to explore the potential impacts of different
                scenarios on the environmental, socio-economics, and human health in rural areas.")
      ),
      tabItem(tabName = "nbn", # UI Biodiversity ####
              h1("National Biodiversity Atlas"),
              h3("Here you can access selected species distribution data from the
                 National Biodiversity Atlas (previously the National Biodiversity
                 Network)."),
              tags$br(),
              h3("Note: Use the drop-down menu on the left to select taxa of
                 interest, and click on points to obtain details about species,
                 year of collection and recorder (where available)."),
              fluidPage(
                # sidebarLayout(
                #   sidebarPanel(
                fluidRow(
                  column(6,
                    h3("The National Biodiversity Atlas is used to collect `citizen science` data
                       on species-distributions throughout the UK. Users will eventually be able
                       to use the Rural Data Science Platform to overlay species records with environmental
                       data, plot changes over time, etc. The beta version displays data for 
                       one group of spiders, insects, birds and mammals, but it will eventually 
                       provide access to records for other groups."),
                    selectInput(inputId = "nbn_select_major", h2("Group of species"),
                                choices = c(#"Birds" = "birds",
                                            "Mammals" = "mammals",
                                            "Insects" = "insects",
                                            "Spiders (Araneae)" = "spiders")),
                    selectInput(inputId = "nbn_select_order", h2("Order"),
                                choices = NULL),
                    selectInput(inputId = "nbn_select_family", h2("Family"), 
                                choices = NULL) 
                  ),
                  # mainPanel(
                  column(6, 
                    leafletOutput("nbn_map")
                  )
                )
              )
      ),
      tabItem(tabName = "AgCensus", # UI AgCensus ####
              h1("Agricultural census data"),
              h3("Summary data from the Agricultural census, 2km resolution"),
              fluidPage(
                sidebarPanel(
                  h3("Agricultural Census data are collected by Defra on an annual basis. The full
                    dataset includes information on number of all farms, those in Least Favoured 
                    Areas (LFAs), area farmed, grass, cattle, sheep, pigs, poultry, numbers of
                    farmers and numbers of self-employed. It is planned that this part of the RO 
                    will eventually contain more of these data, updated as they become available.")
                ),
                mainPanel(
                  leafletOutput("census_map")
                )
              )
      ),
      tabItem(tabName = "Hydrology", # UI Hydrology ####
              h1("Hydrology and river networks"),
              h3("Information on sub-catchments for the River Tyne"),
              fluidPage(
                sidebarPanel(
                  h3("A wide range of hydrologically relevant data is collected at Newcastle University.
                    This page shows the main river networks and subcatchments within the catchment of
                    the River Tyne. The page will be integrated with temperature and rainfall data
                    from the Meteorological Office, that will be fed into hydrological models such
                    as TOPMODEL and SHE to predict rates of water flow, flood risk etc.")
                ),
                mainPanel(
                  leafletOutput("hydrology_map")
                )
              )
      ),
      tabItem(tabName = "Campy", # UI Campylobacter maps ####
              h1("Regional risk of Campylobacter"),
              h3("These maps show the predicted spatial risk of contamination
                 in fields based on simulated rainfall, cattle movements,
                 cropping patterns and the disease characteristics"),
              fluidPage(
                fluidRow(
                  column(3,
                         h3("Outputs from a model developed by Shirley, Rushton et al. to simulate
                         Campylobacter risk in relation to temperature, rainfall, pasture growth
                         and husbandry of cattle and sheep"),
                         sliderInput("campy_slider", "Day of year",
                                     min = 10, max = 360,
                                     value = 10, step = 10
                         )
                  ),
                  column(3,
                           h3("Model schema"),
                           img(src="campy_model.png", height='424px', width='572px')
                  ),
                  column(3, offset = 2,
                         imageOutput("campy_map")
                  )
                )
              )
      ),
      tabItem(tabName = "Traffic", # UI road casualties ####
              h1("Road Traffic Accidents (RTA)"),
              h3("Information on the severity and distribution of RTAs, both in space and time.
                  Understanding RTAs is essential, given their severe social costs, and the RO will
                  help to identify potential accident blackspots, relative to the volume of traffic,
                  road speed limits, class of roads, and types of vehicles involved."),
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    h3("Click on the symbols to display information about the types of vehicle
                       involved in the RTA."),
                    selectInput(inputId = "RTA_severity_sel", h2("Severity"), 
                                choices = c("Serious", "Fatal")) #,
                  ),
                  mainPanel(
                    h3("Distribution map"),
                    leafletOutput("RTA_map") #,
                    # h3("Changes over time"),
                    # plotOutput("RTA_plot")
                  )
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
    # Server Biodiversity ####
    nbn_order_choices <- eventReactive(input$nbn_select_major, {
      if(input$nbn_select_major == "mammals"){
        nbn_mammals_order_lst <- as.list(sort(as.character(unique(nbn_mammals$order))))
        names(nbn_mammals_order_lst) <- sort(as.character(unique(nbn_mammals$order)))
      } else if (input$nbn_select_major == "spiders"){
        nbn_araneae_order_lst <- as.list(sort(as.character(unique(nbn_araneae$order))))
        names(nbn_araneae_order_lst) <- sort(as.character(unique(nbn_araneae$order)))
      } else {
        nbn_insecta_order_lst <- as.list(sort(as.character(unique(nbn_insecta$order))))
        names(nbn_insecta_order_lst) <- sort(as.character(unique(nbn_insecta$order)))
      }
    })
    
    observeEvent(input$nbn_select_major, {
      selected_order <- input$nbn_select_order
      if(selected_order == "Araneae"){
        families <- dplyr::filter(nbn_araneae, order==selected_order)
        nbn_family_lst <- as.list(sort(as.character(unique(families$family))))
        names(nbn_family_lst) <- sort(as.character(unique(families$family)))
        reduced_nbn <- dplyr::filter(nbn_araneae, order==selected_order) 
      } else if (input$nbn_select_major == "mammals") {
        families <- dplyr::filter(nbn_mammals, order==selected_order)
        nbn_family_lst <- as.list(sort(as.character(unique(families$family))))
        names(nbn_family_lst) <- sort(as.character(unique(families$family)))
        reduced_nbn <- dplyr::filter(nbn_mammals, order==selected_order) 
      } else {
        families <- dplyr::filter(nbn_insecta, order==selected_order)
        nbn_family_lst <- as.list(sort(as.character(unique(families$family))))
        names(nbn_family_lst) <- sort(as.character(unique(families$family)))
        reduced_nbn <- dplyr::filter(nbn_insecta, order==selected_order) 
      }
      updateSelectInput(session, inputId = "nbn_select_order", choices = nbn_order_choices())
    })
    
    observeEvent(input$nbn_select_order, {
      selected_order <- input$nbn_select_order
      if(selected_order == "Araneae"){
        families <- dplyr::filter(nbn_araneae, order==selected_order)
        nbn_family_lst <- as.list(sort(as.character(unique(families$family))))
        names(nbn_family_lst) <- sort(as.character(unique(families$family)))
        reduced_nbn <- dplyr::filter(nbn_araneae, order==selected_order)
      } else if (input$nbn_select_major == "mammals"){
        families <- dplyr::filter(nbn_mammals, order==selected_order)
        nbn_family_lst <- as.list(sort(as.character(unique(families$family))))
        names(nbn_family_lst) <- sort(as.character(unique(families$family)))
        reduced_nbn <- dplyr::filter(nbn_mammals, order==selected_order)
      } else {
        families <- dplyr::filter(nbn_insecta, order==selected_order)
        nbn_family_lst <- as.list(sort(as.character(unique(families$family))))
        names(nbn_family_lst) <- sort(as.character(unique(families$family)))
        reduced_nbn <- dplyr::filter(nbn_insecta, order==selected_order)
      }
      updateSelectInput(session, inputId = "nbn_select_family", choices = nbn_family_lst)
    })

    output$nbn_map  <- renderLeaflet({
      nbn_subset_plot <- reactive({
        if(input$nbn_select_major == "spiders"){
          return(dplyr::filter(nbn_araneae, order==input$nbn_select_order, family==input$nbn_select_family))
        } else if (input$nbn_select_major == "mammals") {
          return(dplyr::filter(nbn_mammals, order==input$nbn_select_order, family==input$nbn_select_family))
        } else {
          return(dplyr::filter(nbn_insecta, order==input$nbn_select_order, family==input$nbn_select_family))
        }
      })
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10, zoomDelta=0.05, zoomSnap=0.05)) %>%
        addTiles(group = "OSM (default)") %>%
        addCircleMarkers(lng = nbn_subset_plot()$lng, lat = nbn_subset_plot()$lat,
                         popup = paste("<b>Taxonomy:</b>", nbn_subset_plot()$scntfcN,"<br/>",
                                       "<b>Common name:</b>", nbn_subset_plot()$commnNm, "<br/>",
                                       "<b>Year:</b> ",  nbn_subset_plot()$strtDtY, "<br/>",
                                       "<b>Recorder:</b> ", nbn_subset_plot()$dtPrvdr)) %>%
        fitBounds(lng1 = as.numeric(st_bbox(ro_region)[1]),
                  lat1 = as.numeric(st_bbox(ro_region)[2]),
                  lng2 = as.numeric(st_bbox(ro_region)[3]),
                  lat2 = as.numeric(st_bbox(ro_region)[4]))
    })
    

  output$census_map <- renderLeaflet({ # Server AgCensus ####
   paltcow <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                        values(agcensus$tot_cattle), na.color = "transparent")
    palbeef <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                        values(agcensus$beef), na.color = "transparent")
    paldairy <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                         values(agcensus$dairy), na.color = "transparent")
    palpigs <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                        values(agcensus$pigs), na.color = "transparent")
    palpoultry <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                           values(agcensus$poultry), na.color = "transparent")
    palsheep <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                         values(agcensus$sheep), na.color = "transparent")
    
    leaflet(options = leafletOptions(minZoom = 7.25, zoomDelta=0.1, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addRasterImage(agcensus$tot_cattle,  colors=paltcow,   group="Cattle", opacity=0.6) %>% 
      addRasterImage(agcensus$beef, colors=palbeef, group="Beef", opacity=0.6) %>%
      addRasterImage(agcensus$dairy, colors=paldairy, group="Dairy", opacity=0.6) %>%
      addRasterImage(agcensus$pigs, colors=palpigs, group="Pigs", opacity=0.6) %>%
      addRasterImage(agcensus$poultry, colors=palpoultry, group="Poultry", opacity=0.6) %>%
      addRasterImage(agcensus$sheep, colors=palsheep, group="Sheep", opacity=0.6) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("Cattle", "Beef", "Dairy", "Pigs", "Poultry", "Sheep"),
        position = "bottomleft", 
        options = layersControlOptions(collapsed = FALSE))  %>%
      addLegend(pal=paltcow,  values=values(agcensus$tot_cattle), title="Tot Cattle/km", group="Cattle") %>%
      addLegend(pal=palbeef, values=values(agcensus$beef), title="Beef cattle/km", group="Beef") %>%
      addLegend(pal=paldairy, values=values(agcensus$dairy), title="Dairy/km", group="Dairy") %>%
      addLegend(pal=palpigs, values=values(agcensus$pigs), title="Pigs/km", group="Pigs") %>%
      addLegend(pal=palpoultry, values=values(agcensus$poultry), title="Poultry/km", group="Poultry") %>%
      addLegend(pal=palsheep, values=values(agcensus$sheep), title="Sheep/km", group="Sheep") %>%
      setView(lng = -2, lat = 54, zoom=4) %>% 
      setMaxBounds(lng1 = as.numeric(st_bbox(ro_region)[1]),
                   lat1 = as.numeric(st_bbox(ro_region)[2]),
                   lng2 = as.numeric(st_bbox(ro_region)[3]),
                   lat2 = as.numeric(st_bbox(ro_region)[4])) %>%
      hideGroup(c("Beef","Dairy", "Pigs", "Poultry", "Sheep"))
    
 })

  output$hydrology_map <- renderLeaflet({ # Server hydrology ####
    leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "Relief") %>% 
      addFeatures(topmod_30catch_sf_ll,
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  group="Tyne subcatchments") %>%
      addFeatures(tyne_rivers_ll, group="River network") %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite", "Relief"),
        overlayGroups = c("Tyne subcatchments", "River network"),
        options = layersControlOptions(collapsed = FALSE))  %>%
      addMarkers(lng=-1.6178, lat=54.9783, popup="Newcastle upon Tyne") %>% 
      setView(lng = -2, lat = 55, zoom = 8.6)  %>%
      setMaxBounds(lng1 = -1.5,
                   lat1 = 54.5,
                   lng2 = -2.5,
                   lat2 = 55.5 ) %>% 
    hideGroup("Tyne subcatchments")
  })
  
  output$campy_map <- renderImage({ # Server Campy maps ####
    day_no <- input$campy_slider
    campy_file <- paste0("www/campy risk_", day_no, "_v3.png")
    list(src = campy_file, height=600, width=450)
  }, deleteFile = FALSE
  )
  
  output$RTA_map <- renderLeaflet({ # Server RTA ####
    RTA_severity_plot <- reactive(
     # if(input$RTA_severity_sel=="All records"){
        RTA_ll
      #} else {
      #  dplyr::filter(RTA_ll, accident_severity==input$RTA_severity_sel)
      #}
    )
    
    leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>% 
      addCircleMarkers(lng = st_coordinates(RTA_severity_plot())[,1],
                       lat = st_coordinates(RTA_severity_plot())[,2],
                       color = RTA_severity_plot()$symbol_color,
                       popup = paste("<b>Vehicle:</b> ", RTA_severity_plot()$vehicle_type,"<br/>",
                                     "<b>Date:</b> ",  RTA_severity_plot()$date, "<br/>",
                                     #"<b>Time:</b> ", stringr::str_sub(hms::as.hms(RTA_severity_plot()$time), 1, 5), "<br/>",
                                     "<b>Severity:</b> ", RTA_severity_plot()$accident_severity)) %>% 
      
      setView(lng = -2, lat = 55, zoom = 8.6)  %>%
      setMaxBounds(lng1 = -3,
                   lat1 = 54.5,
                   lng2 = -1.5,
                   lat2 = 57 )
  })
  
  # output$RTA_plot <- renderPlot({
  #   RTA_fig <- reactive(
  #     if(input$RTA_severity_sel == "All records"){
  #        RTA_daily_counts
  #     }else{
  #       dplyr::filter(RTA_daily_counts, `Casualty Severity`==input$RTA_severity_sel)
  #     }
  #   )
  #   #plot(RTA_fig()$day, RTA_fig()$daily_RTA)
  #   ggplot(RTA_fig(), aes(x=day, y=daily_RTA)) +
  #     geom_smooth() +
  #     ylab("Number of RTA per day") +
  #     xlab("Date")
  # })
}

shinyApp(ui, server)

