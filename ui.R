library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Time Series Visualization Tool"),

  fluidRow(
    column(2, actionButton("left", "Rotate Left")),
    column(2, actionButton("right", "Rotate Right")),
    column(2, numericInput("demands", "Number of Demands",min = 1, max = 20, step = 1L, value = 1  )),
    column(2, numericInput("bias", "Change Threshold", min = 0, max = .9, step = .1, value = .3)),
    column(3, numericInput("fence", "Periods to Consider", min = 1, max = 20, step = 1L, value = 20))
  ),
  fluidRow(
    tableOutput("table")
  ), 
  fluidRow(
    plotOutput("tsPlot")    
  ),
  fluidRow(
    tableOutput("SPC")
  ),
  fluidRow(
    column(1),
    column(11, 
           sliderInput("animation", "Looping Animation:",
                       min = 1, max = 20,
                       value = 1, step = 1,
                       animate =
                         animationOptions(interval = 600, loop = TRUE))    # actionButton("Reset", "Reset")
    )),
  fluidRow(
    column(1),
    column(11, 
           p(HTML("<b><big>Instructions</b></big>")),
           p(HTML("<b>Number of Demands:</b> Generates a random but static period of historical demand, with this number of 'hits' over 20 periods.")),
           p(HTML("<b>Change Threshold:</b> Rudimentary statiscal process control.  Lower table will display 'TRUE' if forecast change after update exceeds this threshold.")),
           p(HTML("<b>Periods to Consider:</b> Determines  the amount of data visible to the forecasting algorithm.  Data used annotated by TRUE/FALSE in the 'Used' row of the upper table.")),
           p(HTML("<b>Rotate Left:</b> Move the time series one position to the left, with wrapping (far left value becomes far right value.")),
           p(HTML("<b>Rotate Right:</b> Move the time series one position to the right, with wrapping (far right value becomes far left value.")),
           p(HTML("\n")),
           p(HTML("<b><big>Upper Table:</big></b> Table output of demand data used to construct the forecast and plot.")),
           p(HTML("<b><big>Plot:</big></b> Red line represents the historical time series data.  Black line is overall average demand over entire series")),
           p(HTML("&nbsp;&nbsp;&nbsp; Blue bar represents a simple exponential smoothing forecast using from 1 to 20 periods of past data")),
           p(HTML("<b><big>Lower Table:</big></b> Shows the changing forecast values when parameters are changed or the time series rotated.")),
           p(HTML("&nbsp;&nbsp;&nbsp; <b>Legacy Forecast:</b> The previous forecast.")),
           p(HTML("&nbsp;&nbsp;&nbsp; <b>Forecast:</b> The most recent forecast.")),
           p(HTML("&nbsp;&nbsp;&nbsp; <b>Change Greater than Threshold:</b> TRUE if proporition change between legacy forecast and forecast is greater than Change Threashold."))
    ))
))
