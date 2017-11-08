   #
   # This is a Shiny web application. You can run the application by clicking
   # the 'Run App' button above.
   #
   # Find out more about building applications with Shiny here:
   #
   #    http://shiny.rstudio.com/
   #
   
   library(shiny)
   library(leaflet)
   library(DT)
   library(ggplot2)
   library(reshape2)

   theme(plot.title = element_text(hjust = 0.5))
   theme_update(plot.title = element_text(hjust = 0.5))
   
   capitals_brews <- read.csv("capitals_brews.csv", stringsAsFactors = FALSE)
   BrewsCoords <- read.csv("brewscoords.csv",stringsAsFactors = FALSE)
   TidyBeers <- read.csv("TidyBeers.csv",stringsAsFactors = FALSE)
   TidyBreweries <- read.csv("TidyBreweries.csv",stringsAsFactors = FALSE)
   BrewsAndBreweries <- read.csv("BrewsAndBreweries.csv",stringsAsFactors = FALSE)
   
   server <- function(input, output, session) {
     
     observe({
       updateTextInput(session, "mytext", value=input$myslider)
       })
   
     output$mymap <- renderLeaflet({
       leaflet(options = leafletOptions(zoomControl = FALSE, draggable = FALSE)) %>%
         addTiles(options = providerTileOptions(minZoom =5, maxZoom =5)) %>%
         setView(-96.38,39.1, zoom=5) %>%
         addMarkers(data = capitals_brews, layerId= ~ State, lng = ~ long, lat = ~ lat, popup = paste("<b>",capitals_brews$StateLong, "</b>","<br>", "Breweries: ", capitals_brews$BreweryCount, "<br>", "Median IBU: ",capitals_brews$IBU, "<br>",
                                                                                                      "Median ABV: ", capitals_brews$ABV))
     })

     output$scatterplot <- renderPlot({
       ggplot(BrewsCoords, aes(x=ABV, y=IBU)) + geom_point(size=2) + geom_abline(intercept=-34.1, slope = 1282.0)+
       ggtitle("Correlation of ABV and IBU")
     })
     
     output$scatterplotlvl <- renderPlot({
       ggplot(data = BrewsAndBreweries, aes(x=ABV, y=IBU, color = ABVlvl))+
         geom_point(size=2)+
         geom_abline(intercept=-34.1, slope = 1282.0)+
         ggtitle("Correlation of ABV and IBU by ABV level")
     })
     
     BeerFacts <- capitals_brews[,c(2,7:8)]
     
     # For easier side-by-side comparison, we multiply ABV by 807 to approximate the same range of values
     BeerFactsABV <- BeerFacts
     BeerFacts$ABV <- BeerFacts$ABV * 807
     
     # Then we melt these facts to get a long table with ABV and IBU as variables, and their values in the Value column
     
     BeerFacts.long <- melt(BeerFacts)
     BeerFacts.long <- BeerFacts.long[order(BeerFacts.long$State),]
     
     # The following plot shows side-by-side median IBU and ABV data per state
     
     output$combinedbar <- renderPlot({
       ggplot(BeerFacts.long,aes(x=State,y=value,fill=factor(variable)))+
         geom_bar(stat="identity",position="dodge", width=0.8)+
         scale_fill_discrete(name="Measurement",
                             breaks=c(0,1),
                             labels=c("ABV","IBU"))+
         xlab("State")+ylab("Level")+
         ggtitle("Median ABV and Median IBU Per State")+
         scale_fill_discrete(breaks=c("ABV","IBU"))+
         theme(legend.title=element_blank())
     })
     
     # This bar plot shows median ABV data per state
     
     output$ABVbar <- renderPlot({
       ggplot(BeerFactsABV,aes(State,ABV))+
         geom_col(fill="#45415E")+
         coord_cartesian(ylim=c(0.03,0.075))+
         xlab("State")+ylab("ABV")+
         ggtitle("Median ABV Per State")
     })
     # This bar plot shows median IBU data per state (South Dakota == 0)
     
     output$IBUbar <- renderPlot({
       ggplot(BeerFacts,aes(State,IBU))+
         geom_col(fill="#91B3BC")+
         coord_cartesian(ylim=c(0,63))+
         xlab("State")+ylab("IBU")+
         ggtitle("Median IBU Per State")
     })
     
     output$Ounces <- renderPlot({
       ggplot(data=TidyBeers, aes(TidyBeers$Ounces))+
         geom_histogram(binwidth=2,col="red",aes(fill=..count..),alpha=.8)+
         scale_x_continuous(breaks=c(6,8,10,12,14,16,18,20,22,24,26,38,30,32,34))+
         xlab("Fluid Ounces")+ylab("Number of Beers")+
         ggtitle("Volume of Beers in Fluid Ounces")
     })
     
     observeEvent(input$mymap_marker_click, {
       click <- input$mymap_marker_click
       
       if(is.null(click))
         return()   
       
       #pulls lat from shiny click event
       lat <- click$lat
       
       coords <- as.data.frame(cbind(lat))
      
      # BrewNumber <- nrow(BrewsCoords[which(BrewsCoords$lat==coords$lat),])
       
       BrewsCoords_table <- reactive({
         stateinfo <- BrewsCoords[which(BrewsCoords$lat==coords$lat),]
         if(nrow(BrewsCoords[which(BrewsCoords$lat==coords$lat),]) >= input$myslider) {
           stateinfo[1:input$myslider,c(1:6)]
         } else {
             stateinfo[1:nrow(BrewsCoords[which(BrewsCoords$lat==coords$lat),]),c(1:6)]
           }
       })
       
       
       output$table <- DT::renderDataTable(BrewsCoords_table(), rownames=FALSE,
                                           options = list(paging=FALSE,
                                                          order = list(list(1,'asc')),
                                                          initComplete = JS(
                                                            "function(settings, json) {",
                                                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                            "}")))
                                        
       
     })
     
   }
   
   ui <- fluidPage(
     titlePanel("Stars, Stripes, and Beer Presentation"),
     br(),
     h3("Welcome! Please select a tab below."),
     br(),
     mainPanel(
       br(),
       tabsetPanel(
         tabPanel("Beer Info Per State",
                  br(),
                  h4("Click on a state to display information about that state's beers. More information is available in the table below."),
                  h4("Drag the map to locate Hawaii and/or Alaska."),
                  br(),
                  br(),
                  leafletOutput("mymap",height="760px",width="1370px"),
                  br(),
                  sliderInput("myslider", "Select the number of beers to display (will not exceed maximum available)", min=0, max=50, value=5, step=5, width='600px'),
                  DT::dataTableOutput("table")
         ),
         tabPanel("Graphics",
                  br(),
                  em(h4("Please see the following plots which illustrate correlation between ABV and IBU.")),
                  br(),
                  plotOutput("scatterplot"),
                  br(),
                  plotOutput("scatterplotlvl"),
                  br(),
                  plotOutput("combinedbar"),
                  br(),
                  plotOutput("ABVbar"),
                  br(),
                  plotOutput("IBUbar"),
                  br(),
                  em(h4("The following plot illustrates the distribution of the volume of each beer in fluid ounces.")),
                  br(),
                  plotOutput("Ounces")
                  ),
         tabPanel("Presentation",
                  br(),
                  includeMarkdown("StarsStripesAndBeerFinal.md")),
         tabPanel("Codebook",
                  br(),
                  includeMarkdown("Codebook.md"))     
        
       )
     )  
   )    
   
   
   shinyApp(ui = ui, server = server)
