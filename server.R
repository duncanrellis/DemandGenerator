library(shiny)
library(dplyr)
library(forecast)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  getProcessChange <- function() {
    legacyForecast <- values$legacyForecast
    
    if (values$forecast != 0) {
      # browser()
      SPC <- input$bias
      if (abs(legacyForecast - values$forecast) / legacyForecast > SPC) {
        # Forecast has changed
        return (TRUE)
      } else {
        return (FALSE)
        # not sufficient to change
      }
    } else {
      return (FALSE)
    }
  }
  
  values <- reactiveValues()
  values$tsData <- rep(0, 20)
  values$forecast <- .0001
  values$legacyForecast <- .0001
  values$Violate <- FALSE
  
  observeEvent(input$fence, {
    values$legacyForecast <- values$forecast
    values$forecast <<- 
      forecast::ses(values$tsData[(length(values$tsData) - input$fence + 1): length(values$tsData)],
                    alpha = .2)$mean[1]  
  })
  observeEvent(input$demands, {
    tsData <- rep(0, 20)
    tsData[sample(1:20, input$demands)] <- 1
    values$tsData <- tsData
    values$forecast <<- 
      forecast::ses(values$tsData[(length(values$tsData) - input$fence + 1): length(values$tsData)],
                    alpha = .2)$mean[1]  
    ## One time reset
    values$legacyForecast <- values$forecast
  })

  output$SPC <- renderTable({
    observe(input$bias)
    tibble("Change Greater than Threshold" = getProcessChange(), 
               "Legacy Forecast" = round(values$legacyForecast, 3),
               "Forecast" = round(values$forecast, 3))
  })
  
  output$table <- renderTable({
    used <- rep(FALSE, 20 - input$fence)
    used <- c(used, rep(TRUE, 20 - length(used)))
    t(data.frame(Period = 1:20,
                 Demands = as.integer(values$tsData),
                 Used = as.character(used)))
  }, colnames = FALSE, rownames = TRUE)

  observeEvent(input$left, {
    values$legacyForecast <- values$forecast
    values$tsData <<- c(values$tsData[-1], values$tsData[1])
    values$forecast <<- forecast::ses(values$tsData[(length(values$tsData) - input$fence + 1): length(values$tsData)],
                                      alpha = .2)$mean[1]
  })
  observeEvent(input$right, {
    values$legacyForecast <- values$forecast
    values$tsData <<- c(values$tsData[length(tsData)], values$tsData[1:(length(values$tsData) -1)])
    values$forecast <<- forecast::ses(values$tsData[(length(values$tsData) - input$fence + 1): length(values$tsData)],
                                      alpha = .2)$mean[1]
  })
  observeEvent(input$animation, {
    ## It's the same as a right button push
    values$legacyForecast <- values$forecast
    values$tsData <<- c(values$tsData[-1], values$tsData[1])
    values$forecast <<- forecast::ses(values$tsData[(length(values$tsData) - input$fence + 1): length(values$tsData)],
                                      alpha = .2)$mean[1]
  })
  output$tsPlot <- renderPlot({
    fcast <- values$forecast
    plotDf <- data.frame(Period = 1:20,
                 Demands = as.integer(values$tsData),
                 Type = "Historical",
                 stringsAsFactors = FALSE)
  
    plotDf <- bind_rows(plotDf,
                        data.frame(Period = 21,
                                   Demands = fcast,
                                   Type = "Forecast",
                                   stringsAsFactors = FALSE))

    ## Plot params
    axisFont <- 22
    textFont <- 20
    
    ggplot(plotDf %>% filter(Type != "Forecast"), aes(x = Period, y = Demands, fill = Type)) + 
      geom_line(lwd = 2, color = "dark red") +
      geom_hline(yintercept = mean(values$tsData), lty = 2, lwd = 1.5) +
      geom_bar(data = plotDf %>% filter(Type == "Forecast"), stat = "identity",
               fill = "dark blue", alpha = .8, color = "black") +
      theme(
        axis.title.y  = element_text(size = axisFont),
        axis.title.x = element_text(size = axisFont),
        axis.text = element_text(size = textFont)
      )
    })  
})
