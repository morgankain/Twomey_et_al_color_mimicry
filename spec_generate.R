########################################################################################
##### Create a spectra from a chosen distribution of crystal and gap distributions #####
########################################################################################

## add a new distribution to the same data frame as the empirical spectra
dist_fits <- rbind(dist_fits, 
  data.frame(
    sample             = max(dist_fits) + 1
  , crystal_width_mean = param_list$genspec_crys_dat[1]
  , crystal_width_sd   = param_list$genspec_crys_dat[2]
  )
)

nstacks <- param_list$genspec_nstacks
avg_out <- matrix(nrow = length(param_list$wl), ncol = nstacks)

rand_sites <- matrix(nrow = nstacks, ncol = 3)
colnames(rand_sites) <- c("cw", "gw", "nc")

### Slightly cheaty way of obtaining a "truncated Poisson". 
  ### Allow all #s except for 0s and 1s for # of crystals to avoid breakage
    ## (very rare event, so this method should be ok) 
for (p in 1:nstacks) {
temp_num <- rpois(1, param_list$genspec_ncrys)
  while (temp_num < 2) {
temp_num <- rpois(1, param_list$genspec_ncrys)
  }

### Build the given stacks (number = param_list$nstacks)
  rand_sites[p, 1] <- with(param_list, 
    rlnorm(1, 
    meanlog = genspec_crys_dat[1], 
    sdlog = genspec_crys_dat[2]))
  rand_sites[p, 2] <- with(param_list, 
    rlnorm(1, 
    meanlog = genspec_gap_dat[1], 
    sdlog = genspec_gap_dat[2]))
  rand_sites[p, 3] <- temp_num
  
}

### Fit for the given stacks 
 for (o in 1:nstacks) {
   
## multilayer_adj returns from 250:1000
   trans_mat_out <- data.frame(
     with(param_list
       , multilayer_adj(
                ag0 = ag0
                , n0 = n0
                , n1 = n1
                , n2 = n2
                , d1 = rand_sites[o, 1]
                , d2 = rand_sites[o, 2]
                , stacks = rand_sites[o, 3])
              , stack_num = o)
   )
   
## only return the wavelengths given in the data
   avg_out[ , o] <- trans_mat_out[match(param_list$wl, trans_mat_out$wavelength), ]$model
   
 }

### Average the spectra
new_gen_spec <- with(param_list, data.frame(wavelength = wl, spectrum = rowMeans(avg_out)))

Spec_dat <- rbind(
  Spec_dat, 
  data.frame(
    Sample = rep(nrow(dist_fits), length(param_list$wl)), 
    Color = rep("generated", length(param_list$wl)),
    wl = new_gen_spec$wavelength,
    spectra = new_gen_spec$spectrum
    )
  )

### Adjust the spectrum to fit to this simulated spectrum
param_list$which_spec <- nrow(dist_fits)
param_list$crystal_spectra_emp <- Spec_dat
