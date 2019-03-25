library(shiny)
library(ggplot2)
library(png)

function(input, output) {
  
  sliderValues <- reactive({
    
    ## Compose data frame
    data.frame(
      Name = c(
        "Platelet Thickness Mean"
      , "Platelet Thickness SD"
      , "Gap Width Mean"
      , "Gap Width SD"
      , "Pigment"
        ),
      Value = as.character(c(
        input$'platelet_thickness_mean'
      , input$'platelet_thickness_sd'
      , input$'cytoplasm_gap_thickness_mean'
      , input$'cytoplasm_gap_thickness_sd'
      , input$'Pigment'
        )), 
      stringsAsFactors = FALSE)
  }) 

  ## ggplot theme
theme_set(theme_bw())
theme_update(
    axis.text.x = element_text(size = 22)
  , axis.text.y = element_text(size = 22)
  , axis.title.x = element_text(size = 22)
  , axis.title.y = element_text(size = 22)
  , legend.title = element_text(size = 14)
  , panel.grid.major = element_blank()
  , panel.grid.minor = element_blank()
  , strip.background = element_blank()
  , legend.key.size = unit(.25, "cm")
  , legend.key = element_rect(fill = "white")
  , panel.border = element_rect(colour = "black", fill = NA, size = 1)
  , strip.text.x = element_text(size = 16, colour = "black", face = "bold"))

  ## Load necessary data (prerun spectra and colors)
figs1dat    <- readRDS("supp_out.rds")
rgb_pigment <- suppressWarnings(read.csv("rgb_pigment.csv"))
rgb_pigment_user    <- suppressWarnings(read.csv("rgb_pigment_user_def.csv"))
pigment_reflectance <- suppressWarnings(read.csv("pigment_reflectance.csv"))
pigment_reflectance_user <- suppressWarnings(read.csv("pigment_reflectance_user_def.csv"))

  ## needed spectra adjustment for including pigments
adjust_spec <- function (spectra, pigment_spectra, pigment_opt) {
  
  ## Unpack pigment choice
  if (pigment_opt == "A") {
    A <- 1; D <- 0
  } else if (pigment_opt == "D") {
    A <- 0; D <- 1  
  } else if (pigment_opt == "Both") {
    A <- 1; D <- 1     
  } else if (pigment_opt == "None") {
    A <- 0; D <- 0     
  }
  
  ## Calculations to transform wavelengths to visible colors
  pigment_adjust <- pigment_spectra$A * ifelse(A == 1, 0.5, 0) + pigment_spectra$D * ifelse(D == 1, 0.5, 0)
  pigment_adjust <- 10 ^ (- pigment_adjust)
  spectra_adjust1 <- spectra$spectra * (0.95 / 100)
  spectra_adjust2 <- 1 - spectra_adjust1
  ## Whole formula given here for completeness despite *0
  final_spec <- (
   ( pigment_adjust ^ 2 ) * ( spectra_adjust1 ) + 
   ( ( pigment_adjust ^ 2 ) * ( spectra_adjust2 ^ 2 ) * ( 1 ^ 2 ) * 0 ) + 
    0.050
  ) * 100
  
  spectra$spectra <- final_spec
  
  return(spectra)

}

  ## Subset data according to user choices
spec_dat <- reactive({
  
  ## spec without adjusting with pigment data
unweighted_spec <-  with(supp_fig
    , subset(supp_fig
      , platelet_thickness_mean      == input$'platelet_thickness_mean'      &
        platelet_thickness_sd        == input$'platelet_thickness_sd'        & 
        cytoplasm_gap_thickness_mean == input$'cytoplasm_gap_thickness_mean' &
        cytoplasm_gap_thickness_sd   == input$'cytoplasm_gap_thickness_sd'))
  
  ## spec after adjusting with pigment data
  adjust_spec(
  spectra         = unweighted_spec
, pigment_spectra = if(input$'User_pig' == "No") {pigment_reflectance} else {pigment_reflectance_user}
, pigment_opt     = input$'Pigment')
  
})

  ## Determine color based on spectra chosen by the user
color_dat <- reactive({
  
  which_spec <- with(supp_fig
    , subset(supp_fig
      , platelet_thickness_mean      == input$'platelet_thickness_mean' &
        platelet_thickness_sd        == input$'platelet_thickness_sd' & 
        cytoplasm_gap_thickness_mean == input$'cytoplasm_gap_thickness_mean' &
        cytoplasm_gap_thickness_sd == input$'cytoplasm_gap_thickness_sd'))
  
  ## Retrieve pigment data
  if (input$'User_pig' == "No") {
  which_color <- rgb_pigment[rgb_pigment$unique_spec == which_spec$unique_spec[1], ]
  } else {
  which_color <- rgb_pigment_user[rgb_pigment_user$unique_spec == which_spec$unique_spec[1], ]
  }
  if (nrow(which_color) > 0) {
  which_color <- subset(which_color, Pigment == input$'Pigment') 
  with(which_color, rgb(R, G, B, maxColorValue = 255))
  } else {
  rgb(255, 255, 255, maxColorValue = 255)
  }
  
})

  ## render the ggplot based on the spectra chosen by the user, with a background color given by the calculation
  output$spec_plot <- renderPlot({

backcol <- color_dat()
    
gg1 <- ggplot(spec_dat(), aes(wavelength, spectra)) + geom_line(lwd = 4, colour = "black") + 
      xlab("Wavelength") + ylab("Reflectance") +
      scale_y_continuous(limits = c(0, 65)) +
       theme(panel.background = element_rect(fill = backcol
         , colour = "lightblue"
         , size = 0.5
         , linetype = "solid"))

  ## If no user defined pigments are given, print that this option was ignored on the plot
  if (backcol == "#FFFFFF") {
  gg1 + geom_text(aes(label = "No user defined pigments given, 
no pigment was used to generate this plot"), x = 750, y = 60, size = 8)
  } else {
  gg1
  }
    
    })
  
}
