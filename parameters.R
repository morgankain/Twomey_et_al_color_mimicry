#################################################
##### Set the parameters for the simulation #####
#################################################

##' @param multi_fit               -- Fit the same spectra multiple times across cores? 
##'                                       multi_fit == FALSE : Fits a single spectrum a single time on one core
##'                                       multi_fit == TRUE  : Fits a single spectrum multiple times across cores
##' @param multi_fit_num_cores     -- Given multi_fit == TRUE: How many cores would you like to use? 
##'                                       Default is 1 less than the number of cores detected on the machine
##' @param spec_gen                -- Simulate a spectrum (TRUE) or use an empirically measured spectrum (FALSE). spec_gen = TRUE is useful for generating spectra from specified (rather than fitted) parameters.
##' @param genspec_nstacks         -- # of crystal stacks to use in simulated spectra. A "stack" is defined as a vertical transect through a set of crystals. 
##' @param genspec_ncrys           -- # of crystals per stack to use in simulated spectra
##' @param genspec_crys_dat        -- lognormal mean and variance for crystal widths in simulated spectra
##' @param genspec_gap_dat         -- lognormal mean and variance for gap widths in simulated spectra
##' @param fit_simulated_spectra   -- Given spec_gen == TRUE: Recover parameters used to simulated spectra (TRUE) or just simulate spectra and plot it (FALSE)
##' @param which_spec              -- Names spectra in csv file of spectra data
##' @param expected_gap_mean_range -- Given spec_gen == FALSE, establishes the search range for the mean parameter for the crystal gap widths
##' @param expected_gap_sd_range   -- Given spec_gen == FALSE, establishes the search range for the sd parameter for the crystal gap widths
##'                                       Because of the relative naivety of the fitting procedure, an appropriately chosen window for these two
##'                                       parameters should have the following criteria:
##'                                       1) Be wide enough to contain the true value
##'                                       2) Be narrow enough to run in an appropriate time period
##' @param fit_type                -- Use either 'GAP' for a two parameter fit (crystal mean gap width and sd in gap widths) or 'GAP_NUM' for a three
##'                                       Parameter fit that includes those from GAP and the number of crystals in each stack as well. 
##'                                       Note: 'GAP_NUM' is very slow and in our experience has not generated better matched spectra
##' @param ss_type                 -- Type of sums of squares to be used. 'RAW' is an unscaled ss
##'                                       while 'SCALED' scales the spectra to the relative change, thus focusing on curvature
##' @param ss_match                -- 'TRUE' is used when the absolute 'height' difference (percent reflectance) between fit and measured spectra is not of importance. 
##'                                       Used only if ss_type == 'RAW'
##'                                       Here the focus is strictly on the curvature of the spectra, in a slightly different way than ss_type == SCALED
##'                                       (the fitted spectra is shifted to meet the 'height').
##'                                       Note, this should give a very similar answer to ss_type == SCALED
##'                                       FALSE is used to include the 'height' of the spectra in the fit
##' @param full_wl_range           -- Use the full nm range of the spectra for the fit. This would only be used for organisms with no skin pigments. 
##' @param wl_range                -- Given wl_range == FALSE: choose the portion of the spectra to use to calculate ss (FALSE is the default)
##'                                       Identification of an appropriate range is paramount to a successful fitting run
##' @param crystal_count           -- User defined average number of crystals per stack. This is equivalent to the stacks parameter from the Maia script.
##' @param nstacks                 -- Number of stacks in fitting procedure
##'                                       In general, as the nstacks increases the spectra will become smoother
##'                                       Until we understand the data better, crystal counts are considered Poisson distributed
##'                                       It is likely there may be more/less overdispersion in crystal counts than is modeled with a Poisson
##' @param debugfit                -- Run a *much* coarser parameter search to search for bugs or to receive a
##'                                       very quick and dirty fit. This option should never be set to TRUE for best fits
##' @param ag0                     -- Parameter of transfer matrix function:
##'                                       angle of light in radians (default is 0.7854, which corresponds to 45 degrees)
##'                                       Should be adjusted for each problem and depending on how reflectance spectra were measured.
##' @param n0                      -- Parameter of transfer matrix function:
##'                                       Refractive index of air (set to 1)    
##' @param n1                      -- Parameter of transfer matrix function:
##'                                       First material in stack (e.g. cytoplasm). Default set to 1.33
##' @param n2                      -- Parameter of transfer matrix function:
##'                                       Second material in stack (e.g. guanine).  Default set to 1.83

#########
#### Fitting parameters 
#########

multi_fit                 <- FALSE
spec_gen                  <- TRUE

  if (spec_gen == TRUE) {
genspec_nstacks           <- 5000
genspec_ncrys             <- 10
genspec_crys_dat          <- c(4.25, 0.4)
genspec_gap_dat           <- c(4.75, 0.4)
fit_simulated_spectra     <- FALSE
  }

which_spec                <- 1

  if (multi_fit == TRUE) {
multi_fit_num_cores       <- detectCores() - 1
  }

expected_gap_mean_range   <- c(4.2, 5.4)
expected_gap_sd_range     <- c(0.10, 0.70)

fit_type                  <- "GAP"
#fit_type                 <- "GAP_NUM"
ss_type                   <- "RAW"
#ss_type                  <- "SCALED"
ss_match                  <- TRUE
full_wl_range             <- FALSE

  if (full_wl_range == "FALSE") {
wl_range                  <- c(550, 850)
  } else {
wl_range                  <- c(min(Spec_dat$wl), max(Spec_dat$wl))    
  }

  if (fit_type == "GAP") {
crystal_count             <- 10
  } else {
crystal_count             <- NULL
  }

nstacks                   <- 10
debugfit                  <- FALSE


#########
#### Transfer matrix parameters
#########

ag0                      <- 0          
n0                       <- 1             
n1                       <- 1.34
n2                       <- 1.83                
