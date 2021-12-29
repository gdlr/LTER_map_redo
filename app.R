#########
# Libraries
#########

library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(here)
library(sf)
library(plotly)
library(shinyalert)
library(htmltools)
library(mapview)
library(fs)
library(systemfonts)

source('global.R', local = TRUE)

#################
##             ##
##      UI     ##
##             ##
#################


ui <- fluidPage(
  # Set up popup
                ### Left Side (map)
    fluidRow(class = "map",
             column(6, leafletOutput("mymap", height = '100vh'),
                    offset = 0,
                    style = 'padding: 0px; height: 100vh',
                    ),
             column(6, 
                    fluidRow(class = "row1",
                             uiOutput("siteImage"),
                             absolutePanel(class = "panel",
                                           width = "80%",
                               fluidRow(class = "text",
                                      htmlOutput("siteName")),
                                      br(),
                                      actionButton("button", "Click for a full description of the site."),
                                      br(),
                                      br(),
                                      align = "center"
                               )
                    ),
                    fluidRow(class = "plotly",
                             plotlyOutput("yearPlot"),
                             align = "center"
                             ),
        ),
        fixedPanel(class = "oldsites",
                   actionButton("oldSites", label = "Display old LTER sites"),
                   left = 50, top = 10,
                   align = "left"),
        fixedPanel(class = "download",
                   actionButton("Download", label = a(href="LTERmap.png", "Download Map", download=NA, target="_blank")),
                   left = 50, top = 45,
                   align = "left"),
    
    tags$head(tags$style(HTML("
      .row1{display: flex;
            align-items: center;
            justify-content: center}
      .plotly{}
      .oldsites{}"
    ))),
    tags$head(tags$style("
      .row1{height:50vh}
      .plotly{height: 50vh}"
    ),
    tags$head(tags$style(HTML(".leaflet-container { background: #FFFFFF}
                               .panel {background-color: rgba(255,255,255,.85)}"))
    ),
    tags$head(tags$style(
        HTML('#siteName {text-align: center}')
    )),
    tags$style(HTML('#button {font-size: large; background-color:#f9fef}')),
    tags$style(HTML('#oldsites {font-size: large; background-color:#f9fef}')),
    
    tags$head(tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'
                               )
                          )
              )
    
    
    )
)
)

#################
##             ##
##    SERVER   ##
##             ##
#################


server <- function(input, output, session) {
  
  message("started server")
  # Reactive val that switches between true/false after a click
  whichplot <- reactiveVal(TRUE)
    
  ##-------------##
  ## Leaflet Map ##
  ##-------------##
          ##-------------##
          ## Current Map ##
          ##-------------##
          message("made current map")
          
          current_map <-  leaflet() %>%
                      addProviderTiles(providers$Esri.WorldTerrain,
                                       options = providerTileOptions(minZoom = 2)) %>% 
                  
                  addMarkers(data = current_extra[1,], label = ~(Name), layerId = ~Code, popup =~text, icon = Admin) %>% # NCO
                  addMarkers(data = current_extra[2,], label = ~(Name), layerId = ~Code, popup =~text, icon = MarineNeg) %>% # CCE
                  addMarkers(data = current_extra[3,], label = ~(Name), layerId = ~Code, popup =~text, icon = `mixed_landscape`) %>% # CDR
                  addMarkers(data = current_extra[4,], label = ~(Name), layerId = ~Code, popup =~text, icon = Forest) %>% # HRB
                  addMarkers(data = current_extra[5,], label = ~(Name), layerId = ~Code, popup =~text, icon = `mixed_landscape_neg`) %>% # JRN
                  addMarkers(data = current_extra[6,], label = ~(Name), layerId = ~Code, popup =~text, icon = Marine) %>% # NES
                  addMarkers(data = current_extra[7,], label = ~(Name), layerId = ~Code, popup =~text, icon = Coastal) %>% # PIE
                  addMarkers(data = current_extra[8,], label = ~(Name), layerId = ~Code, popup =~text, icon = Coastalneg) %>% #VCR
                  
                  addCircleMarkers(data = current, fillColor = ~palette(Type),
                                   color = "black",
                                   weight = 1, radius = 6, stroke = TRUE, fillOpacity = 1,
                                   label = ~(Name),
                                   layerId = ~Code,
                                   popup = ~text
                  ) %>% 
                  
                  addEasyButton(easyButton(id = "Reset", icon = "fa-undo", title = "Reset",
                    onClick=JS("function(btn, map){ map.setZoom(2); map.setView([-110, -20])}"))) %>% 
                  
                  # addEasyButton(easyButton(id = "download", icon = "fa-download", title = "Download Map",
                  #                          onClick=JS("function(btn, map) {
                  #                 Shiny.onInputChange('download', 'clicked', {priority: 'event'});}"))) %>% 
                  
                  setMaxBounds(lng1 = -230, lat1 = -90, lng2=10, lat2=90)
          
          
          ##---------------------##
          ##  Download on click  ##
          ##---------------------##
            # Handled via a href to the actual file (genius, thanks internet)
          
          # observeEvent(input$download, {
          #   output$downloadData<<-downloadHandler(filename = "LTERMap.png",
          #                                         content =  mapshot(
          #                                           which_map(), filename = "LTERMap.png", cliprect = "viewport",
          #                                           zoom = 3
          #                                         ),
          #                                         )
          #   jsinject <- "setTimeout(function(){window.open($('#downloadData').attr('href'))}, 100);"
          #   session$sendCustomMessage(type = 'jsCode', list(value = jsinject))    
          # })
              
          ##---------------##
          ## Zoom on click ##
          ##---------------##
          observe({ ### Zoom when the marker is clicked ###
            message("map marker click observed")
                  click <- input$mymap_marker_click
                  zoom <- 4
                  if(is.null(click))
                      return()
          
                  leafletProxy('mymap') %>%
                      setView(click$lng, click$lat, zoom = 4)
              })
          
          ###------------------###
          #   Old Sites Switch   #
          ###------------------###
          # Whichplot is just a TRUE/FALSE switch where TRUE indicates it's on the current site
          which_map <- reactive({
            if (whichplot()) {
              current_map
            } else {
              old_map
            }
          })
          
          ###---------------###
          #   Old Sites Map   #
          ###---------------###
          old_map <-  leaflet() %>%
            addProviderTiles(providers$Esri.WorldTerrain,
                             options = providerTileOptions(minZoom = 2)) %>% 
            addCircleMarkers(data = inactive_sites,
                             fillColor = ~palette(Type),
                             color = "black",
                             weight = 1, radius = 6, stroke = TRUE, fillOpacity = 1,
                             label = ~(Name),
                             layerId = ~Code,
                             popup = ~text) %>% 
            addEasyButton(easyButton(id = "Reset", icon = "fa-undo", title = "Reset",
                                     onClick=JS("function(btn, map){ map.setZoom(2); map.setView([-110, -20])}"))) %>% 
            
            addEasyButton(easyButton(id = "download", icon = "fa-download", title = "Download Map",
                                     onClick=JS("function(btn, map) {
                                  Shiny.onInputChange('download', 'clicked', {priority: 'event'});}"))) %>% 
            
            setMaxBounds(lng1 = -230, lat1 = -90, lng2=10, lat2=90)
          
          output$mymap <- renderLeaflet({which_map()})
    
##-------------------##
#   Timeline Plot:   ##
##-------------------##
        ##-----      -----##
        ##  Initial Plot  ##
        ##-----      -----##
        current_plot <- ggplotly(
                ggplot(data = current_sites) +
                  geom_point(aes(x = Established, y = Code, color = Type)) +
                  geom_point(aes(x = `End date`, y = Code, color = Type)) +
                  geom_segment(aes(x = Established, xend = `End date`, y = Code, yend = Code, color= Type),
                               size = 1.2) +
        
                  geom_vline(aes(xintercept = current_year), color = "grey30", linetype = "dashed") +
        
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_color_manual(values = colors) +
                  labs(title = "LTER Timeline", x = NULL, y = NULL, legend = NULL) +
                  theme(text = element_text(size=8)) +
                  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020))
              ) %>%
                layout(legend = list(orientation = "h",   # show entries horizontally
                                     xanchor = "center",
                                     x = 0.4,
                                     y=-0.1,
                                     tracegroupgap = 0))  %>% # use center of legend as anchor"
                layout(title = "LTER Timeline", font = list(size = 16)) %>% 
                 config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "toggleSpikelines", "zoomIn2d", "zoomOut2d", "hoverCompareCartesian", "hoverClosestCartesian", "autoScale2d", "resetScale2d"),
                        displaylogo = FALSE,
                        displayModeBar = TRUE)

        ##-----      -----##
        ##    Old Plot    ##
        ##-----      _____##
        old_plot <- ggplotly(
                  ggplot(data = map_coords) +
                    geom_point(aes(x = Established, y = Code, color = Type)) +
                    geom_point(aes(x = `End date`, y = Code, color = Type)) +
                    geom_segment(aes(x = Established, xend = `End date`, y = Code, yend = Code, color= Type),
                                 size = 1.2) +
                    
                    geom_vline(aes(xintercept = current_year), color = "grey30", linetype = "dashed") +
                    
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    scale_color_manual(values = colors) +
                    labs(title = "LTER Timeline", x = NULL, y = NULL, legend = NULL) +
                    theme(text = element_text(size=8)) +
                    scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020))
                ) %>%
                  layout(legend = list(orientation = "h",   # show entries horizontally
                                       xanchor = "center",
                                       x = 0.4,
                                       y=-0.1,
                                       tracegroupgap = 0)) %>% 
                  layout(title = "LTER Timeline", font = list(size = 16)) %>% 
                        config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "toggleSpikelines", "zoomIn2d", "zoomOut2d", "hoverCompareCartesian", "hoverClosestCartesian", "autoScale2d", "resetScale2d"),
                               displaylogo = FALSE,
                               displayModeBar = TRUE)# "use center of legend as anchor"

        ##-----                  -----##
        ##    Reactive plot Switch    ##
        ##-----                  _____##
              # This switches the reactive value after a click on oldSites
              observeEvent(input$oldSites, {
                whichplot(!whichplot())
              })
              
              which_graph <- reactive({
                if (whichplot()) {
                  current_plot
                } else {
                  old_plot
                }
              })
              
              ##----------##
              ##  Render  ##
              ##----------##
              output$yearPlot <- renderPlotly({which_graph()})

    
        ##-----                    ------##
        ##  Highlight current site plot  ##
        ##-----                    ------##
          # Observe event based on the map marker click
          observe({
              click<-input$mymap_marker_click
              if(is.null(click))
                  return()
              
              ## -----------------------##
              ## Reactive plotly plot:  ##
              ## -----------------------##
              message("started making highlight plot")
                           output$yearPlot <- renderPlotly({
                             ggplotly(
                               ggplot() +
                                 geom_vline(aes(xintercept = current_year), color = "grey60", linetype = "dashed") +
          
                                 geom_point(data = map_coords, aes(x = Established, y = Code), color = "grey90") +
                                 geom_point(data = map_coords, aes(x = `End date`, y = Code), color = "grey90") +
                                 geom_segment(data = map_coords, aes(x = Established, xend = `End date`, y = Code, yend = Code), color= "grey90",
                                              size = 1.2) +
          
                                 geom_point(data = filtered_df(), aes(x = Established, y = Code, color = Type)) +
                                 geom_point(data = filtered_df(), aes(x = `End date`, y = Code, color = Type)) +
                                 geom_segment(data = filtered_df(), aes(x = Established, xend = `End date`, y = Code, yend = Code, color= Type),
                                              size = 2) +
          
                                 geom_vline(aes(xintercept = current_year), color = "grey60", linetype = "dashed") +
          
                                 geom_text(data = filtered_df(), aes(x = Established, y = Code, label = Established), position = position_nudge(y = -2, x = 1)) +
                                 geom_text(data = filtered_df(), aes(x = `End date`, y = Code, label = `End date`), position = position_nudge(y = 1.5, x = -1)) +
          
                                 theme_minimal() +
                                 scale_color_manual(values = colors) +
                                 labs(title = paste(filtered_df()$Code, "Timeline"), x = NULL, y = NULL, legend = NULL) +
                                 scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
                                 scale_y_discrete(expand = c(0.1, 0.1), labels = NULL)
                             ) %>%
                               partial_bundle() %>%
          
                               layout(legend = list(orientation = "h",   # show entries horizontally
                                                    xanchor = "center",
                                                    x = 0.4,
                                                    y=-0.1)) %>% # use center of legend as anchor"
                               layout(title = paste(filtered_df()$Code, "Timeline"), font = list(size = 16)) %>% 
                               config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "toggleSpikelines", "zoomIn2d", "zoomOut2d", "hoverCompareCartesian", "hoverClosestCartesian", "autoScale2d", "resetScale2d"),
                                      displaylogo = FALSE,
                                      displayModeBar = TRUE)
                           }) 
          
          })
    
    ######        ####
    #  Sidebar Text  #
    ######        ####
          which_text <- reactive({
            if (whichplot()) {
              current_text
            } else {
              old_text
            }
          })
          
          
          
          # Set up reactive values:
          message("text reactive values")
          current_text <- reactiveValues(siteName = "<h1>Interactive Map of LTER Sites</h1>
                                                     <br>
                                                     <h3>This map displays all 28 active LTER sites. Click on each site for more information!</h3>",
                                         siteImage = "",
                                         siteInfo = "")
          
          old_text <- reactiveValues(siteName = paste("<h1>", "Archived LTER Sites", "</h1>", "<br>", "<h3>", "These LTER sites have been phased out of the LTER Network, though many sites continue to support other research initiaves and some still manage to contribute to current LTER science.", "</h3>"),
                                     siteImage = "",
                                     siteInfo = "")
          # 
          # # Observe event for the click:
          #
          observe({
              click<-input$mymap_marker_click$id
              if(is.null(click))
                     return()
              
              message("current text updates")
            current_text$siteName <- paste("<h1>", filtered_df()$Name, "</h1>")
            current_text$siteImage <- img(src = paste(filtered_df()$Code, ".jpg", sep = ""), width = "100%")
            current_text$siteInfo <- paste("<h4>", filtered_df()$Name, "<$h4>",
                              "<h4>", "Ecosystem Type:", filtered_df()$Type, "</h4>",
                              "<h4>", "Current Grant:", filtered_df()$'current-grant', "</h4><h4>",
                              "<a href=\"", filtered_df()$Website, "\">", "Website Link</a>", "</h4><h4>",
                              "<a href=\"", filtered_df()$Data, "\">", "Link to Data</a>", "</h4><h4>",
                              "<a href=\"", filtered_df()$Publications, "\">", "Link to Publications</a>", "</h4><h4>")
            
            
            message("old text updates")
            old_text$siteName <- paste("<h1>", filtered_df()$Name, "</h1>")
            old_text$siteImage <- img(src = paste(filtered_df()$Code, ".jpg", sep = ""), width = "100%")
            old_text$siteInfo <- paste("<h4>", filtered_df()$Name, "</h4>",
                                           "<h4>", "Ecosystem Type:", filtered_df()$Type, "</h4>",
                                           "<h4>", "Current Grant:", filtered_df()$'current-grant', "</h4><h4>",
                                           "<a href=\"", filtered_df()$Website, "\">", "Website Link</a>", "</h4><h4>",
                                           "<a href=\"", filtered_df()$Data, "\">", "Link to Data</a>", "</h4><h4>",
                                           "<a href=\"", filtered_df()$Publications, "\">", "Link to Publications</a>", "</h4><h4>")
          })
          
          ### Default text for the sidebar:
          output$siteName <- renderText(which_text()$siteName)
          output$siteImage <- renderUI(which_text()$siteImage)
          output$siteInfo <- renderText(which_text()$siteInfo)


    ######        #####
    #    Click ID     #
    ######        #####
    click_id <- reactive({
      input$mymap_marker_click$id
    })
    
            ######          #####
            #    Filtered DF    #
            ######          #####
            filtered_df <- reactive({
              map_coords %>% 
                dplyr::filter(Code %in% click_id())
            })
    
    ######              #####
    #    Site info modal    #
    ######              #####
          # Create default text when nothing is selected
    observeEvent(input$button, {
      if(is.null(input$mymap_marker_click$id)){
        shinyalert(
          "Click a site to see more information"
        )
      } else {
      shinyalert(
        title = paste("<h2>", filtered_df()$Name, "Description", "</h2>"),
        text = paste("<h4>", filtered_df()$Description, "</h4>", "<hr>", 
                     "<h3>", 
                     "Current Grant:", "<a href=\"", filtered_df()$grant_link,"\"", " target=\"_blank\"", "\">", filtered_df()$`current-grant`,"</a>", "|", 
                     "<a href=\"", filtered_df()$Website, "\">", "Website Link</a>", "|", 
                     "<a href=\"", filtered_df()$Data, "\">", "Link to Data</a>", "|", 
                     "<a href=\"", filtered_df()$Publications, "\">", "Link to Publications</a>",
                     "</h3>"
                     ),
        html = TRUE,
        size = "l")
      message("shinyalert activated")
      }
    })
    
      ######              #####
      #    Old Sites modal    #
      ######              #####
          
          # Create variable action button label based on T/F switch
          which_label <- reactive({
            if (whichplot()) {
              "Display old LTER sites"
            } else {
              "Display current LTER sites"
             }
          })
          
          # Update action button based on oldSites click
          observeEvent(input$oldSites, {
            updateActionButton(session, "oldSites", label = which_label())
          })
          
          
          # Restore default text based on oldSites click
          observeEvent(input$oldSites, {
              old_text$siteName = paste("<h1>", "Archived LTER Sites", "</h1>", "<br>", "<h3>", "These LTER sites have been phased out of the LTER Network, though many sites continue to support other research initiaves and some still manage to contribute to current LTER science.", "</h3>")
              old_text$siteImage = ""
              old_text$siteInfo = ""
              
              current_text$siteName = "<h1>Interactive Map of LTER Sites</h1>
                                        <br>
                                        <h3>This map displays all 28 active LTER sites. Click on each site for more information!</h3>"
              current_text$siteImage = ""
              current_text$siteInfo = ""
          })
          
}

#############
#-------------
#############
shinyApp(ui, server)
