######################################################
##### Find and run the correct fitting function  #####
######################################################

  if (param_list$spec_gen == TRUE & param_list$fit_simulated_spectra == FALSE) {
  
    ## just plot
    source("fitted_spec_ggplot.R")
  
  } else {
      
    if (param_list$multi_fit == FALSE) {
      
      if (param_list$fit_type == "GAP") {
     
    ## fit   
    spec_return_final <- with(param_list, 
      frog_spectra_fit_GAP(
          ag0                        = ag0
        , n0                         = n0
        , n1                         = n1
        , n2                         = n2                                
        , crystal_number_mean        = crystal_count                           
        , crystal_width_mean         = crystal_width_mean 
        , crystal_width_sd           = crystal_width_sd         
        , gap_width_mean             = genspec_gap_dat[1]
        , gap_width_sd               = genspec_gap_dat[2]           
        , expected_gap_mean_range    = expected_gap_mean_range
        , expected_gap_sd_range      = expected_gap_sd_range
        , nstacks                    = nstacks
        , which_spec                 = which_spec
        , fitting                    = fitting
        , spec_gen                   = spec_gen
        , multi_fit                  = multi_fit
        , full_wl_range              = full_wl_range
        , wl_range                   = wl_range
        , wl                         = wl
        , crystal_spectra_emp        = crystal_spectra_emp            
        , debugfit                   = debugfit                            
        , ss_type                    = ss_type
        , ss_match                   = ss_match
      )
    )
    
      } else if (param_list$fit_type == "GAP_NUM") {
        
   ## fit   
    spec_return_final <- with(param_list, 
      frog_spectra_fit_GAP_NUM(
        ag0                          = ag0
        , n0                         = n0
        , n1                         = n1
        , n2                         = n2                                
        , crystal_number_mean        = NULL                           
        , crystal_width_mean         = crystal_width_mean 
        , crystal_width_sd           = crystal_width_sd         
        , gap_width_mean             = genspec_gap_dat[1]
        , gap_width_sd               = genspec_gap_dat[2]           
        , expected_gap_mean_range    = expected_gap_mean_range
        , expected_gap_sd_range      = expected_gap_sd_range
        , nstacks                    = nstacks
        , which_spec                 = which_spec
        , fitting                    = fitting
        , spec_gen                   = spec_gen
        , multi_fit                  = multi_fit
        , full_wl_range              = full_wl_range
        , wl_range                   = wl_range
        , wl                         = wl
        , crystal_spectra_emp        = crystal_spectra_emp            
        , debugfit                   = debugfit                            
        , ss_type                    = ss_type
        , ss_match                   = ss_match
      )
    )        
        
      }

## Jumping through some hoops here to set up the parameters in an appropraite way to send
## them to each core. To the best of our knowledge this is a reasonable enough way to
## address the need for the setup to be different depending on wheter only 1 spectra is 
## fit or multiple spectra are fit
    
    } else if (param_list$multi_fit == TRUE) {
  
    which_spec_multi <- vector("list", param_list$multi_fit_num_cores)
    which_spec <- rep(param_list$which_spec, length(which_spec_multi))
    
    for (i in 1:length(which_spec_multi)) {
      which_spec_multi[[i]] <- which_spec[i]
    }
    
      if (param_list$fit_type == "GAP") {
    
    ## fit
      ## results returned as a list of lists
      ## Maybe a way to use do.call to still allow for the use of with....
      ## Doing it this way is annoying
    spec_return_final <- mclapply(which_spec_multi
      , frog_spectra_fit_GAP
      , ag0                          = param_list$ag0
        , n0                         = param_list$n0
        , n1                         = param_list$n1
        , n2                         = param_list$n2                                
        , crystal_number_mean        = param_list$crystal_count                           
        , crystal_width_mean         = param_list$crystal_width_mean 
        , crystal_width_sd           = param_list$crystal_width_sd         
        , gap_width_mean             = param_list$genspec_gap_dat[1]
        , gap_width_sd               = param_list$genspec_gap_dat[2]           
        , expected_gap_mean_range    = param_list$expected_gap_mean_range
        , expected_gap_sd_range      = param_list$expected_gap_sd_range
        , nstacks                    = param_list$nstacks
        , fitting                    = param_list$fitting
        , spec_gen                   = param_list$spec_gen
        , multi_fit                  = param_list$multi_fit
        , full_wl_range              = param_list$full_wl_range
        , wl_range                   = param_list$wl_range
        , wl                         = param_list$wl
        , crystal_spectra_emp        = param_list$crystal_spectra_emp           
        , debugfit                   = param_list$debugfit                            
        , ss_type                    = param_list$ss_type
        , ss_match                   = param_list$ss_match
        , mc.cores                   = param_list$multi_fit_num_cores
      )
    
      } else if (param_list$fit_type == "GAP_NUM") {
        
    ## fit
      ## results returned as a list of lists
      ## Maybe a way to use do.call to still allow for the use of with....
      ## Doing it this way is annoying
    spec_return_final <- mclapply(which_spec_multi
      , frog_spectra_fit_GAP
      , ag0                          = param_list$ag0
        , n0                         = param_list$n0
        , n1                         = param_list$n1
        , n2                         = param_list$n2                                
        , crystal_number_mean        = NULL                          
        , crystal_width_mean         = param_list$crystal_width_mean 
        , crystal_width_sd           = param_list$crystal_width_sd         
        , gap_width_mean             = param_list$genspec_gap_dat[1]
        , gap_width_sd               = param_list$genspec_gap_dat[2]           
        , expected_gap_mean_range    = param_list$expected_gap_mean_range
        , expected_gap_sd_range      = param_list$expected_gap_sd_range
        , nstacks                    = param_list$nstacks
        , fitting                    = param_list$fitting
        , spec_gen                   = param_list$spec_gen
        , multi_fit                  = param_list$multi_fit
        , full_wl_range              = param_list$full_wl_range
        , wl_range                   = param_list$wl_range
        , wl                         = param_list$wl
        , crystal_spectra_emp        = param_list$crystal_spectra_emp           
        , debugfit                   = param_list$debugfit                            
        , ss_type                    = param_list$ss_type
        , ss_match                   = param_list$ss_match
        , mc.cores                   = param_list$multi_fit_num_cores
      )
          
      }
    
    }
    
  }