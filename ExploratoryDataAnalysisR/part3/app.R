
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Lab 1 - Part 3 - Data Comparison"),
  
  theme = shinytheme("spacelab"),
  
  # Sidebar with a slider input for number of bins 
  fixedRow(
  sidebarPanel(selectInput("variable", "Charts option:", 
                           c("CDC" = "cdc",
                             "Twitter Map" = "twitter",
                             "CDC Vs Twitter" = "cdcVsTwitter"))
  )),
  
  # Show a plot of the generated distribution
  fixedRow(
    plotlyOutput("FluPlot")
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$variable,{
  
  if(input$variable == "cdc"){
    output$FluPlot <- renderPlotly({
      HeatMap  <-read.csv("C:/Users/aarti/OneDrive/Documents/Semester 2/DIC/Lab1/DICLAB1/WeeklyCDCHeatMap.csv", header=T)
      plot_geo(HeatMap, locationmode = 'USA-states') %>%
        add_trace(
          z = ~HeatMap$LEVEL, locations = ~HeatMap$STATE,
          color = ~HeatMap$LEVEL, colors = c("#00c200","#00ff00","#8cf700","#baf700","#e0f500","#f7df00","#fcb100","#fc8200","#fa4f00","#cc0000")) %>%
        colorbar(title = "ILI Activity Level") %>%
        layout(
          geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
          )
        )
    })
  }
  
  if(input$variable == "twitter"){
    
    output$FluPlot <- renderPlotly({
      HeatMap1  <-read.csv("C:/Users/aarti/OneDrive/Documents/Semester 2/DIC/Lab1/DICLAB1/stateCount.csv", header=T)
      plot_geo(HeatMap1, locationmode = 'USA-states') %>%
        add_trace(
          z = ~HeatMap1$LEVEL, locations = ~HeatMap1$Var1,
          color = ~HeatMap1$Freq, colors = c("#00c200","#00ff00","#8cf700","#baf700","#e0f500","#f7df00","#fcb100","#fc8200","#fa4f00","#cc0000") ) %>%
        colorbar(title = "Tweet Frequency") %>%
        layout(
          title = 'Explanation',
          geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
          )
        )
    })
  }
  
  if(input$variable == "cdcVsTwitter"){
    output$FluPlot <- renderPlotly({
      HeatMapC  <-read.csv("C:/Users/aarti/OneDrive/Documents/Semester 2/DIC/Lab1/DICLAB1/WeeklyCDCHeatMap.csv", header=T)
      p1 <- plot_geo(HeatMapC, locationmode = 'USA-states') %>%
        add_trace(
          z = ~HeatMapC$LEVEL, locations = ~HeatMapC$STATE,
          color = ~HeatMapC$LEVEL, colors = c("#00c200","#00ff00","#8cf700","#baf700","#e0f500","#f7df00","#fcb100","#fc8200","#fa4f00","#cc0000")) %>%
        colorbar(title = "CDC Activity Level") %>%
        layout(
          geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
          )
        )
      
      HeatMapT  <-read.csv("C:/Users/aarti/OneDrive/Documents/Semester 2/DIC/Lab1/DICLAB1/stateCount.csv", header=T)
      p2 <- plot_geo(HeatMapT, locationmode = 'USA-states') %>%
        add_trace(
          z = ~HeatMapT$LEVEL, locations = ~HeatMapT$Var1,
          color = ~HeatMapT$Freq, colors = c("#00c200","#00ff00","#8cf700","#baf700","#e0f500","#f7df00","#fcb100","#fc8200","#fa4f00","#cc0000") ) %>%
        colorbar(title = "Twitter Activity Level") %>%
        layout(
          title = 'CDC Data vs Tweet Data',
          geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
          )
        )
      
      subplot(p1,p2)
    })
  }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

