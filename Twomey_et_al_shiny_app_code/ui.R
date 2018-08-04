library(shiny)
library(ggplot2)

fluidPage(

  ##  Application title
  titlePanel("Explore Spectra"),

  sidebarLayout(
    
      ## setup
      position = "left",
    
      ## Crystal and gap parameters
    sidebarPanel(
      sliderInput("crystal_width_mean", "Crystal Width Mean:"
        , min = 4.25, max = 4.75, value = 4.50, step = 0.25)
    , sliderInput("crystal_width_sd", "Crystal Width SD:"
        , min = 0.2, max = 0.6, value = 0.4, step = 0.20)
    , sliderInput("gap_width_mean", "Gap Width Mean:"
        , min = 4.25, max = 4.75, value = 4.50, step = 0.25)
    , sliderInput("gap_width_sd", "Gap Width SD:"
        , min = 0.2, max = 0.6, value = 0.4, step = 0.20)
      
      ## Pigment parameters
    , radioButtons("Pigment", "Pigment:"
      , c("canary xanthophyll A"    = "A"
        , "drosopterin"             = "D"
        , "both"                    = "Both"
        , "none"                    = "None")
      , selected = "None"
      )
    )
       
     # Main ggplot only
    , mainPanel(plotOutput(outputId = "spec_plot"))
  )
)
