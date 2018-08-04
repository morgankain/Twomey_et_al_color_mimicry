## Not using facet_wrap because of y-axis scales and a bit lazy to manually fix... grid.arrange is fine...

## quick change of parameter for logicals (troubles with NA for this step...)
fit_check <- is.na(param_list$fitting) 
if (fit_check == FALSE) {
  if (param_list$fitting == TRUE) {
    fit_check <- TRUE
  } else {
    fit_check <- FALSE
  }
}

if (param_list$spec_gen == FALSE & fit_check == TRUE) {

  if (param_list$multi_fit == FALSE) {

gg_samp_spectra_compare1 <- ggplot(
  spec_return_final[[4]][spec_return_final[[4]]$method == "Empirical", ],
  aes(wavelength, spectrum)) + geom_line(lwd = 1)

gg_samp_spectra_compare1 <- gg_samp_spectra_compare1 + 
  geom_vline(xintercept = param_list$wl_range[1], lwd = 1, linetype = "dashed", colour = "blue") + 
  geom_vline(xintercept = param_list$wl_range[2], lwd = 1, linetype = "dashed", colour = "black")
 
gg_samp_spectra_compare1 <- gg_samp_spectra_compare1 +
  geom_line(data = spec_return_final[[4]][spec_return_final[[4]]$method != "Empirical", ],
  aes(wavelength, spectrum), colour = "red") 

gg_samp_spectra_compare2 <- ggplot(
    spec_return_final[[4]][spec_return_final[[4]]$method == "Empirical", ],
  aes(wavelength, spectrum)) + geom_line(lwd = 1, colour = "black") + 
    geom_line(data = spec_return_final[[4]][spec_return_final[[4]]$method != "Empirical", ],
     aes(wavelength
      , spectrum)
      , lwd = 1, colour = "blue") + 
  geom_vline(xintercept = param_list$wl_range[1], lwd = 1, linetype = "dashed", colour = "blue") + 
  geom_vline(xintercept = param_list$wl_range[2], lwd = 1, linetype = "dashed", colour = "black") +
    scale_x_continuous(limits = c(param_list$wl_range[1], param_list$wl_range[2]))

grid.arrange(gg_samp_spectra_compare1, gg_samp_spectra_compare2, ncol = 1)

 } else {
   
## Can modify plotting to print out a list of ss, but that material can be accessed 
## in spec_return_final[[3]] (see legend on plot for matching color to ss)
  
## set up empty data frame for plotting
spec_return_final_for_plot <- data.frame(
  wavelength = numeric(0)
, spectrum   = numeric(0)
, sample     = numeric(0)
, method     = numeric(0)
)

for (i in 1:length(spec_return_final)) {
  spec_return_final_for_plot <- rbind(
    spec_return_final_for_plot
  , spec_return_final[[i]][[4]]
  )
}

## add column for fit
spec_return_final_for_plot$run_num <- rep(seq(1, length(spec_return_final)), each = length(param_list$wl) * 2)

gg_samp_spectra_compare1 <- ggplot(
  spec_return_final_for_plot[spec_return_final_for_plot$method == "Empirical"
    & spec_return_final_for_plot$run_num == 1, ]
, aes(wavelength, spectrum)) + geom_line(lwd = 1)

gg_samp_spectra_compare1 <- gg_samp_spectra_compare1 + 
  geom_vline(xintercept = param_list$wl_range[1], lwd = 1, linetype = "dashed", colour = "blue") + 
  geom_vline(xintercept = param_list$wl_range[2], lwd = 1, linetype = "dashed", colour = "black")
 
gg_samp_spectra_compare1 <- gg_samp_spectra_compare1 +
  geom_line(data = spec_return_final_for_plot[spec_return_final_for_plot$method != "Empirical", ],
  aes(wavelength, spectrum, colour = as.factor(run_num)), lwd = 1)

gg_samp_spectra_compare2 <- ggplot(
    spec_return_final_for_plot[spec_return_final_for_plot$method == "Empirical"
       & spec_return_final_for_plot$run_num == 1, ],
  aes(wavelength, spectrum)) + geom_line(lwd = 1, colour = "black") + 
    geom_line(data = spec_return_final_for_plot[spec_return_final_for_plot$method != "Empirical", ],
     aes(wavelength, spectrum, colour = as.factor(run_num)), lwd = 1) + 
  geom_vline(xintercept = param_list$wl_range[1], lwd = 1, linetype = "dashed", colour = "blue") + 
  geom_vline(xintercept = param_list$wl_range[2], lwd = 1, linetype = "dashed", colour = "black") +
    scale_x_continuous(limits = c(param_list$wl_range[1], param_list$wl_range[2]))

grid.arrange(gg_samp_spectra_compare1, gg_samp_spectra_compare2, ncol = 1)
  
}
  } else {
  
 gg_samp_spectra_compare1 <- ggplot(Spec_dat[Spec_dat$Color == "generated", ], aes(wl, spectra)) + 
   geom_line(lwd = 1)
 grid.arrange(gg_samp_spectra_compare1) 
 
}