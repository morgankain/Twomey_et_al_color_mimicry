#################################################
##### Top level script for fitting spectra  #####
#################################################

#######
### Objective
#######

## The main objective of these scripts is to fit reflectance spectra to an observed spectrum using
## Maia et al's (2009) multilayer model:

## Maia, R., Caetano, J. V. O., Bao, S. N., & Macedo, R. H. (2009). 
## Iridescent structural colour production in male blue-black grassquit feather barbules: the role of keratin and melanin. 
## Journal of the Royal Society Interface, 6(suppl_2), S203-S211.

## This model has been modified to incorporate variance parameters
## allowing for the production of broadband reflectance spectra, and can be fit over a custom wavelength
## range to allow fitting to occur only at wavelengths outside the influence of skin pigments.

#######
### Caveats, Assumptions, Use 
#######

## First, and most important, while these scripts return a "fitted spectra" they should be used
## *in conjunction* with a researcher's understanding of their organism: the fitting procedure requires
## the inclusion of many user-defined choices. Specifically, a researcher should be able to answer these
## questions before attempting this method: (1) Is the skin structure appropriate for this kind of model?
## This is not always obvious; for example certain frogs, such as Bombina and Oophaga histrionica, seem 
## to lack iridophores even in brightly colored areas. (2) What are the refractive indices and approximate 
## sizes of the materials of the multilayer? And (3) what pigments are present in the skin and what are 
## their absorbance ranges? For example if drosopterin is present in the skin, this means the fitting range 
## should be wavelengths longer than where these pigments absorb light (e.g., > 600 nm in the case of drosopterin).

## Second, we point out that these scripts are set up for a relatively narrow range of problems.
## Data on crystal widths is needed. For the current project these data were obtained from TEM images.
## It is possible to allow both crystal widths and gap widths to be fit as free parameters but several
## combinations of these parameters can produce similar spectra. A more sophisticated fitting proceedure will be
## needed for a problem in which all parameters are allowde to vary; currently, parameter estimates will likely not
## correspond well to reality. 

## Third, we acknowledge that the current brute force search algorithm is not a very rigorous method for reducing
## RSS. While it will find the minimal SS without bias, it will do so in an inefficient way.  

#######
### Required Set up
#######

## First, csv files are needed

## A csv file containing measured reflectance spectra named Spectra.csv

## Reflectance spectra must be included in the project directory as a csv 
## file with the following columns with the exact (i.e. case sensitive) names:
## 1) Sample  -- A number id for the spectra (corresponds to which_spec parameter in parameters.R)
## 2) Color   -- A name id for the spectra
## 3) wl      -- wavelength
## 4) spectra -- measured reflectance spectrum

## A csv file containing measured crystal width data named Widths.csv
## Note that the values are the cross-sectional widths (i.e. the vertical "height" of a crystal) in 
## nanometers

## Crystal width data must be included in the project directory as a csv file with the following columns
## with the exact (i.e. case sensitive) names:
## Note: this is true only if spec_gen == FALSE, that is, you do not desire to simulate a spectra from
## defined parameters (see parameters.R)
## 1) Sample  -- A number id for the spectra that must correspond to the Sample number in the spectra data csv
## 2) Color   -- A name id for the spectra that must correspond to the Sample number in the spectra data csv
## 3) Width   -- The width of each measured crystal

## Example csv files are provided in the .zip

## Second, parameters must be adjusted manually in parameters.R. No other scripts other than that file
## and this file need to be opened to run this fitting procedure

#######
### Code
#######

## For general cleanliness code is separated into different R scripts and run with source("script.R)

### Packages, functions, setup
source("packages.R")
source("ggplot_theme.R")
source("functions.R")

### Data
Spec_dat  <- read.csv("Spectra.csv")
Width_dat <- read.csv("Widths.csv")

### Fit lognormal distributions to crystal data
source("empirical_fits.R")

### Parameters. Note: Load this script to adjust these parameters manually *first*
source("parameters.R")

### Gather parameters into a list for storage and clean the working directory
source("package_parameters.R")

### simulate a spectra from the chosen parameter values | spec_gen == "TRUE"
if (param_list$spec_gen == TRUE) {
  system.time({ source("spec_generate.R") })
}

### Fit. Contains a series of logicals to determine which fitting procedure is needed
## track time needed for fit
system.time({ source("fitting.R")} )

### plot the results
#source("fitted_spec_ggplot.R")
