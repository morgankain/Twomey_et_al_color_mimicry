library(shiny)
library(ggplot2)

fluidPage(

  ##  Application title
  titlePanel("Explore Spectra"),

  sidebarLayout(
    
      ## setup
      position = "left",
    
      ## Platelet and gap parameters
    sidebarPanel(
      sliderInput("platelet_thickness_mean", "Platelet Thickness Mean:"
        , min = 4.25, max = 4.75, value = 4.50, step = 0.25)
      
    , helpText("Average thickness of the stacks of iridophore platelets")
      
    , sliderInput("platelet_thickness_sd", "Platelet Thickness SD:"
        , min = 0.2, max = 0.6, value = 0.4, step = 0.20)
      
    , helpText("Standard deviation in the thickness of iridophore platelets
      among stacks")
      
    , sliderInput("cytoplasm_gap_thickness_mean", "Cytoplasm Gap Thickness Mean:"
        , min = 4.25, max = 4.75, value = 4.50, step = 0.25)
      
    , helpText("Average thickness of the cytoplasm gaps between iridophore platelets among stacks")
      
    , sliderInput("cytoplasm_gap_thickness_sd", "Cytoplasm Gap Thickness SD:"
        , min = 0.2, max = 0.6, value = 0.4, step = 0.20)
      
    , helpText("Standard deviation in the thickness of the cytoplasm gaps between iridophore 
      platelets among stacks")
      
      ## Pigment parameters
    , radioButtons("Pigment", "Pigments:"
      , c("canary xanthophyll A"    = "A"
        , "drosopterin"             = "D"
        , "both"                    = "Both"
        , "none"                    = "None")
      , selected = "None"
      )
      
    , helpText("Skin pigments that are also present")
      
    )
       
     # Main ggplot only
    , mainPanel(plotOutput(outputId = "spec_plot")
    , img(src = 'param_figure.png', align = "right", width = "750"))
  )
)
