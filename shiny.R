# Replication file for: "German Automotive Industry Dashboard"
# RPubs-link: https://rpubs.com/mstefan-rpubs/cars
# (c) Martin Stefan, July 2020

rm(list = ls())

# packages
library(sp)
library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)

# import data 
shape <- readRDS("data/shapefile.rds") # shape file
regis <- readRDS("data/registrations.RDS") # registration data
brandnames <- dimnames(regis)[[1]]

# compute percentages
regis_pct <- regis
sums1 <-  apply(regis,c(2,3),sum)[,"total"]
sums2 <-  apply(regis,c(2,3),sum)[,"new"]
for(i in 1:dim(regis)[[1]]) regis_pct[i,,"total"] <- regis[i,,"total"] / sums1 * 100
for(i in 1:dim(regis)[[1]]) regis_pct[i,,"new"] <- regis[i,,"new"] / sums2 * 100
rm(regis,sums1,sums2,i)

# add percentages data to shape file
shape1 <- shape
shape2 <- shape
for(i in dimnames(regis_pct)[[1]]) shape1@data[,i] <- regis_pct[i,1:16,"total"]
for(i in dimnames(regis_pct)[[1]]) shape2@data[,i] <- regis_pct[i,1:16,"new"]
rm(shape,i)


ui <- dashboardPage(
  
  # layout
  skin = "red",
  
  # header
  dashboardHeader(
    titleWidth = 250,
    title = "Car brands in Germany"
  ),
  
  # sidebar
  dashboardSidebar(
    width = 250,
    radioButtons("make","Brand:",brandnames),
    radioButtons("stat","Statistic:",c("New registrations 2019"="total",
                                       "Total registrations"="new"))
  ),
  
  # body
  dashboardBody(
    
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    
    box(title = "Regional Distribution",
        leafletOutput("map")),
    
    box(title = "Market share total",
        plotOutput("pie")),

    box(title = "Highest market shares",
        plotOutput("bar"))
        
  )
)



server <- function(input, output, session){
  
  # get inputs
  stat <- reactive(input$stat)
  make <- reactive(input$make)

  # create outputs
  observe({
    
    # select correct data for map
    if(stat()=="total") shape <- shape1
    if(stat()=="new") shape <- shape2
    
    # draw map
    map <- leaflet(shape) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd",shape@data[,make()])(shape@data[,make()]),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE))
    
    # send map to output
    output$map <- renderLeaflet(map)
    
    
    
    # draw pie chart
    x <- regis_pct[,"total",stat()]
    y <- sum(x) - x[make()]
    x <- c(x[make()],y)
    names(x) <- c(make(),"Others")

    # send pie chart to output
    output$pie <- renderPlot({
      par(mar = rep(0,4))
      pie(x,
          clockwise = T,
          init.angle = 90,
          labels = paste(names(x),": ",round(x,2),"%",sep=""),
          col = c("tomato","snow2"))
    })
    
    
    # bar chart
    z <- regis_pct[make(),1:16,stat()]
    output$bar <- renderPlot({
      barplot(sort(z, decreasing = T)[1:5],
              ylab="Market share in %", xlab="") 
    })
    
  })
  
}

# run shiny
shinyApp(ui, server, options=list(launch.browser=T))
