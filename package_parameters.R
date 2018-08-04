######################################################
##### Package all of the parameters into a list  #####
######################################################

param_list <- list(
  ag0                     = ag0
, n0                      = n0
, n1                      = n1
, n2                      = n2
, multi_fit               = multi_fit
, spec_gen                = spec_gen
, genspec_nstacks         = ifelse(exists("genspec_nstacks"), genspec_nstacks, NA)
, genspec_ncrys           = ifelse(exists("genspec_ncrys"), genspec_ncrys, NA)
, genspec_crys_dat        = ifelse(rep(exists("genspec_crys_dat"), 2), genspec_crys_dat, NA)
, genspec_gap_dat         = ifelse(rep(exists("genspec_gap_dat"), 2), genspec_gap_dat, NA)
, fit_simulated_spectra   = ifelse(exists("fit_simulated_spectra"), fit_simulated_spectra, NA)
, fitting                 = ifelse(exists("fit_simulated_spectra"), fit_simulated_spectra, NA)
, which_spec              = which_spec
, multi_fit_num_cores     = ifelse(exists("multi_fit_num_cores"), multi_fit_num_cores, NA)
, expected_gap_mean_range = expected_gap_mean_range
, expected_gap_sd_range   = expected_gap_sd_range
, crystal_width_mean      = dist_fits$crystal_width_mean
, crystal_width_sd        = dist_fits$crystal_width_sd
, fit_type                = fit_type
, ss_type                 = ss_type
, ss_match                = ss_match
, full_wl_range           = full_wl_range
, wl_range                = wl_range
, wl                      = seq(min(Spec_dat$wl), max(Spec_dat$wl)) 
, crystal_spectra_emp     = Spec_dat
, crystal_count           = crystal_count
, nstacks                 = nstacks
, debugfit                = debugfit
)
  
suppressWarnings(rm(ag0, n0, n1, n2, multi_fit, spec_gen, genspec_nstacks, genspec_ncrys
  , genspec_crys_dat, genspec_gap_dat, fit_simulated_spectra, which_spec, multi_fit_num_cores
  , expected_gap_mean_range, expected_gap_sd_range, fit_type, ss_type, ss_match, full_wl_range
  , wl_range, crystal_count, nstacks, debugfit))