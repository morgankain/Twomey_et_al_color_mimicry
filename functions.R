###############################
##### Required functions  #####
###############################

## Internal 'guts' of the whole procedure. The transfer matrix script 
## (written by Rafael Maia: http://rafaelmaia.net/)
## Maia, R., Caetano, J. V. O., Bao, S. N., & Macedo, R. H. (2009). 
## Iridescent structural colour production in male blue-black grassquit feather barbules: the role of keratin and melanin. 
## Journal of the Royal Society Interface, 6 (suppl_2), S203-S211.
multilayer_adj <- function(ag0, n0, n1, n2, d1, d2, stacks) {          ## Number of stacks in this region of the frog?

ms<-NULL
mp<-NULL

for (wl in 250:1000)
{

n3<-n1

#COMPLEX ANGLES OF INCIDENCES (SNELL'S LAW)
ag1<- asin(n0*sin(ag0)/n1)
ag2<- asin(n0*sin(ag0)/n2)
ag3<- asin(n0*sin(ag0)/n3)

#INTERFACE MATRIXES
#air-first material
r1p<-(n0*cos(ag0)-n1*cos(ag1))/(n0*cos(ag0)+n1*cos(ag1))
I1p<- matrix(c(1,r1p,r1p,1),nrow=2,ncol=2)
r1s<-(n1*cos(ag0)-n0*cos(ag1))/(n1*cos(ag0)+n0*cos(ag1))
I1s<- matrix(c(1,r1s,r1s,1),nrow=2,ncol=2)

#first-second material
r2p<-(n1*cos(ag1)-n2*cos(ag2))/(n1*cos(ag1)+n2*cos(ag2))
I2p<- matrix(c(1,r2p,r2p,1),nrow=2,ncol=2)
r2s<-(n2*cos(ag1)-n1*cos(ag2))/(n2*cos(ag1)+n1*cos(ag2))
I2s<- matrix(c(1,r2s,r2s,1),nrow=2,ncol=2)

#second-first material 
r3p<-(n2*cos(ag2)-n3*cos(ag3))/(n2*cos(ag2)+n3*cos(ag3))
I3p<- matrix(c(1,r3p,r3p,1),nrow=2,ncol=2)
r3s<-(n3*cos(ag2)-n2*cos(ag3))/(n3*cos(ag2)+n2*cos(ag3))
I3s<- matrix(c(1,r3s,r3s,1),nrow=2,ncol=2)

#TRANSFER MATRIXES
#first material
b1<- (2*pi*d1*n1*cos(ag1))/wl
L1<- matrix(c(exp(b1*1i),0,0,exp(b1*-1i)),nrow=2,ncol=2)

#second material
b2<- (2*pi*d2*n2*cos(ag2))/wl
L2<- matrix(c(exp(b2*1i),0,0,exp(b2*-1i)),nrow=2,ncol=2)

#TOTAL SCATTERING MATRIXES
SCATs<-function(x)
{
layers<-L1%*%I2s

if(x==1){ layers<-layers } else{
	for(i in 2:x)
	{layers<-layers%*%L2%*%I3s%*%L1%*%I2s
			}}
I1s%*%layers
}
SCAT1s<-SCATs(stacks)

SCATp<-function(x)
{
layerp<-L1%*%I2p

if(x==1){ layerp<-layerp } else{
	for(i in 2:x)
	{layerp<-layerp%*%L2%*%I3p%*%L1%*%I2p
			}}
I1p%*%layerp
}
SCAT1p<-SCATp(stacks)

#AMPLITUDE REFLECTIVITY
refs<-SCAT1s[2,1]/SCAT1s[1,1]

refp<-SCAT1p[2,1]/SCAT1p[1,1]

#FINAL REFLECTANCE
REFs<-refs%*%Conj(refs)

REFp<-refp%*%Conj(refp)

#MODEL
ms<-c(ms,REFs*100)

mp<-c(mp,REFp*100)

}
model.s<-as.numeric(ms)

model.p<-as.numeric(mp)

model<-(model.s+model.p)/2


#DATA TABLE
wavelength<-250:1000
mfilm<-data.frame(wavelength,model)

#GRAPHIC AND RESULT
mfilm
}

## Fitting function, currently relying on a "brute force" approach to the parameter space exploration.
## Fitting occurs in three steps, what we call a "successive refine" approach.
## A coarse search is undertaken in step 1. The outcome with the lowest SS is chosen as the center for the
## next search, which searches a much smaller, "finer" area. This same procedue is conducted a final time
## to a precision of .01 for mean and .005 for sd. This precision can be adjusted by changing the guts of
## the function. Adjusting the precision is not currently a top-level user-defined parameter.

frog_spectra_fit_GAP <- function(
  ag0, n0, n1, n2                                     ## transfer matrix parameters
  , crystal_number_mean                               ## either given or fit
  , crystal_width_mean, crystal_width_sd              ## from empirical data
  , gap_width_mean, gap_width_sd                      ## parameters for simulated spectra only (to be fit if gen_spec == FALSE)
  , expected_gap_mean_range, expected_gap_sd_range    ## search space
  , nstacks, which_spec, fitting, spec_gen, multi_fit ## other fitting procedure info
  , full_wl_range, wl_range, wl, crystal_spectra_emp  ## wavelength range and wl from empirical data
  , debugfit                                          ## short or complete fit
  , ss_type, ss_match                                 ## sums of squares options
  ) {
  
for (step_count in 1:3) {

if (step_count == 1) {
  
  ## First pass, most coarse grained. Choose some bounds at the very edges of possible parameter space
mean_opts <- seq(expected_gap_mean_range[1], expected_gap_mean_range[2]
  , by = ifelse(debugfit == FALSE, 0.2, 0.4))
sd_opts <- seq(expected_gap_sd_range[1], expected_gap_sd_range[2]
  , by = ifelse(debugfit == FALSE, 0.05, 0.15))

} else if (step_count == 2) {
  
  ## Pick what the optimum was and move on either side equal to the grain of the previous simulation
mean_opts <- seq(
  mean_opts[zz_opt] - 0.2, mean_opts[zz_opt] + 0.2
, by = ifelse(debugfit == FALSE, 0.05, 0.1))
sd_opts <- seq(
  sd_opts[xx_opt] - 0.05, sd_opts[xx_opt] + 0.05
, by = ifelse(debugfit == FALSE, 0.02, 0.05))

} else if (step_count == 3) {
  
 ## Pick what the optimum was and move on either side equal to the grain of the previous simulation
mean_opts <- seq(
  mean_opts[zz_opt] - 0.05, mean_opts[zz_opt] + 0.05
, by = ifelse(debugfit == FALSE, 0.02, 0.05))
sd_opts <- seq(
  sd_opts[xx_opt] - 0.02, sd_opts[xx_opt] + 0.02
, by = ifelse(debugfit == FALSE, 0.005, 0.02)) 

}

## matrix to hold sums of squares
sum_square_out <- matrix(nrow = length(mean_opts), ncol = length(sd_opts))

## total number of sims for this step
total <- length(mean_opts) * length(sd_opts) 

## progress bar only working if fitting on one core
# reset progress bar and a line break
  if (exists("pb")) {
rm(pb); print("")
  } 
 if (multi_fit == FALSE) {
pb <- txtProgressBar(min = 0, max = total, width = total)
  }
cc <- 1
print(paste(total, "sims in step", step_count, sep = " "))
## loop over mean and sd opts
for (zz in 1:length(mean_opts)) {
  for (xx in 1:length(sd_opts)) {
    
spec_return_trial <-
  stack_fitting_func(
  ag0                   = ag0
, n0                    = n0
, n1                    = n1
, n2                    = n2
, crystal_number_mean   = crystal_number_mean
, crystal_width_mean    = crystal_width_mean
, crystal_width_sd      = crystal_width_sd
, gap_width             = c(mean_opts[zz], sd_opts[xx])
, nstacks               = nstacks
, wl                    = wl
, which_spec            = which_spec
, fitting               = fitting
, full_wl_range         = full_wl_range
, wl_range              = wl_range
, crystal_spectra_emp   = crystal_spectra_emp
, spec_gen              = spec_gen
, avg_out               = matrix(nrow = length(wl), ncol = nstacks)
, ss_type               = ss_type
, ss_match              = ss_match)

sum_square_out[zz, xx] <- spec_return_trial

cc <- cc + 1
if (exists("pb")) {
  setTxtProgressBar(pb, cc)
} else {
  print(paste(round(cc/total*100, 0), "% done, step 1 of 4"))
}
  
  }
}

## Slightly convoluted but effective enough way of returning the optimal values of 
  ## both xx (mean) and zz (sd) that minimized the sum of squares
xx_opt <- which(colSums(sum_square_out == min(sum_square_out)) == 1)
zz_opt <- which(rowSums(sum_square_out == min(sum_square_out)) == 1)

}

## lots of stacks for smooth spectra for plotting with found parameters
final_run_nstacks <- 1000
print("")
print("Fit with 1000 stacks for smooth spectra in step 4")

spec_return_final <-
  stack_fitting_func(
  ag0                   = ag0
, n0                    = n0
, n1                    = n1
, n2                    = n2
, crystal_number_mean   = crystal_number_mean
, crystal_width_mean    = crystal_width_mean
, crystal_width_sd      = crystal_width_sd
, gap_width             = c(mean_opts[zz_opt], sd_opts[xx_opt]) ## Last run use the finest level mean and sd parameters found
, nstacks               = final_run_nstacks
, wl                    = wl
, which_spec            = which_spec
, fitting               = FALSE                                 ## This time return the actual fitted spectra
, full_wl_range         = full_wl_range
, wl_range              = wl_range
, crystal_spectra_emp   = crystal_spectra_emp
, spec_gen              = spec_gen
, avg_out               = matrix(nrow = length(wl), ncol = final_run_nstacks)
, ss_type               = ss_type
, ss_match              = ss_match)

### parameter_fits
spec_return_final[[3]] <- c(gap_width_mean = mean_opts[zz_opt], gap_width_sd = sd_opts[xx_opt])

### data frame for plotting
spec_return_final[[4]] <- 
  data.frame(
    wavelength  = rep(wl, 2)
  , spectrum    = c(
      crystal_spectra_emp[crystal_spectra_emp$Sample == which_spec, ]$spectra   ## pull out the empirical spectra
    , spec_return_final[[1]]$spectrum),                                         ## fitted spectra
    sample      = rep(which_spec, length(rep(wl, 2)))
  , method      = rep(c("Empirical", ifelse(full_wl_range == "TRUE", "300-1000", paste(wl_range[1], wl_range[2], sep = "-")))
    , each = length(spec_return_final[[1]]$spectrum)))

return(spec_return_final)
  
}    

frog_spectra_fit_GAP_NUM <- function(
  ag0, n0, n1, n2                                     ## transfer matrix parameters
  , crystal_number_mean                               ## either given or fit
  , crystal_width_mean, crystal_width_sd              ## from empirical data
  , gap_width_mean, gap_width_sd                      ## parameters for simulated spectra only (to be fit if gen_spec == FALSE)
  , expected_gap_mean_range, expected_gap_sd_range    ## search space
  , nstacks, which_spec, fitting, spec_gen, multi_fit ## other fitting procedure info
  , full_wl_range, wl_range, wl, crystal_spectra_emp  ## wavelength range and wl from empirical data
  , debugfit                                          ## short or complete fit
  , ss_type, ss_match                                 ## sums of squares options
  ) {
  
for (step_count in 1:3) {

if (step_count == 1) {
  
  ## First pass, most coarse grained. Choose some bounds at the very edges of possible parameter space
mean_opts <- seq(expected_gap_mean_range[1], expected_gap_mean_range[2]
  , by = ifelse(debugfit == FALSE, 0.2, 0.4))
sd_opts <- seq(expected_gap_sd_range[1], expected_gap_sd_range[2]
  , by = ifelse(debugfit == FALSE, 0.05, 0.15))
crys_num_opt <- seq(3, 19
  , by = ifelse(debugfit == FALSE, 4, 8))

} else if (step_count == 2) {
  
  ## Pick what the optimum was and move on either side equal to the grain of the previous simulation
mean_opts <- seq(
  mean_opts[zz_opt] - 0.2, mean_opts[zz_opt] + 0.2
, by = ifelse(debugfit == FALSE, 0.05, 0.1))
sd_opts <- seq(
  sd_opts[xx_opt] - 0.05, sd_opts[xx_opt] + 0.05
, by = ifelse(debugfit == FALSE, 0.02, 0.05))
crys_num_opt <- seq(crys_num_opt[yy_opt] - 2, crys_num_opt[yy_opt] + 2
  , by = ifelse(debugfit == FALSE, 1, 2))
crys_num_opt <- crys_num_opt[which(crys_num_opt > 1)]

} else if (step_count == 3) {
  
 ## Pick what the optimum was and move on either side equal to the grain of the previous simulation
mean_opts <- seq(
  mean_opts[zz_opt] - 0.05, mean_opts[zz_opt] + 0.05
, by = ifelse(debugfit == FALSE, 0.02, 0.05))
sd_opts <- seq(
  sd_opts[xx_opt] - 0.02, sd_opts[xx_opt] + 0.02
, by = ifelse(debugfit == FALSE, 0.005, 0.02)) 
crys_num_opt <- seq(crys_num_opt[yy_opt] - 1, crys_num_opt[yy_opt] + 1
  , by = 1)
crys_num_opt <- crys_num_opt[which(crys_num_opt > 1)]

}
  
sum_square_out <- matrix(nrow = length(mean_opts), ncol = length(sd_opts))
sum_square_out_crys <- vector("list", length(crys_num_opt))

## total number of sims for this step
total <- length(mean_opts) * length(sd_opts) * length(crys_num_opt)

## progress bar only working if fitting on one core
# reset progress bar and a line break
  if (exists("pb")) {
rm(pb); print("")
  } 
  if (multi_fit == FALSE) {
pb <- txtProgressBar(min = 0, max = total, width = total)
  }
cc <- 1
print(paste(total, "sims in step", step_count, sep = " "))
## loop over mean, sd, and crys num opts
for (yy in 1:length(crys_num_opt)) {
  for (zz in 1:length(mean_opts)) {
    for (xx in 1:length(sd_opts)) {
        
spec_return_trial <-
  stack_fitting_func(
  ag0                   = ag0
, n0                    = n0
, n1                    = n1
, n2                    = n2
, crystal_number_mean   = crys_num_opt[yy]
, crystal_width_mean    = crystal_width_mean
, crystal_width_sd      = crystal_width_sd
, gap_width             = c(mean_opts[zz], sd_opts[xx])
, nstacks               = nstacks
, wl                    = wl
, which_spec            = which_spec
, fitting               = fitting
, full_wl_range         = full_wl_range
, wl_range              = wl_range
, crystal_spectra_emp   = crystal_spectra_emp
, spec_gen              = spec_gen
, avg_out               = matrix(nrow = length(wl), ncol = nstacks)
, ss_type               = ss_type
, ss_match              = ss_match)

sum_square_out[zz, xx] <- spec_return_trial

cc <- cc + 1
if (exists("pb")) {
  setTxtProgressBar(pb, cc)
} else {
  print(paste(round(cc/total*100, 0), "% done, step 1 of 4"))
}
  
      }
    }

sum_square_out_crys[[yy]] <- sum_square_out
sum_square_out <- matrix(nrow = length(mean_opts), ncol = length(sd_opts))

}

## Slightly convoluted but effective enough way of returning the optimal values of 
  ## both xx (mean) and zz (sd) that minimized the sum of squares
sum_square_temp <- numeric(length(sum_square_out_crys))

for (i in 1:length(sum_square_out_crys)) {
  
  zz_temp <- which(rowSums(sum_square_out_crys[[i]] == min(sum_square_out_crys[[i]])) == 1)
  xx_temp <- which(colSums(sum_square_out_crys[[i]] == min(sum_square_out_crys[[i]])) == 1)

sum_square_temp[i] <- sum_square_out_crys[[i]][zz_temp, xx_temp]

}

yy_opt <- which(sum_square_temp == min(sum_square_temp))
temp_mat <- matrix(unlist(sum_square_out_crys[yy_opt]), nrow = length(mean_opts))

xx_opt <- which(colSums(temp_mat == min(temp_mat)) == 1)
zz_opt <- which(rowSums(temp_mat == min(temp_mat)) == 1)

}

## lots of stacks for smooth spectra for plotting with found parameters
final_run_nstacks <- 1000
print("")
print("Fit with 1000 stacks for smooth spectra in step 4")

spec_return_final <-
  stack_fitting_func(
  ag0                   = ag0
, n0                    = n0
, n1                    = n1
, n2                    = n2
, crystal_number_mean   = crys_num_opt[yy_opt]
, crystal_width_mean    = crystal_width_mean
, crystal_width_sd      = crystal_width_sd
, gap_width             = c(mean_opts[zz_opt], sd_opts[xx_opt]) ## Last run use the finest level mean and sd parameters found
, nstacks               = final_run_nstacks
, wl                    = wl
, which_spec            = which_spec
, fitting               = FALSE                                 ## This time return the actual fitted spectra
, full_wl_range         = full_wl_range
, wl_range              = wl_range
, crystal_spectra_emp   = crystal_spectra_emp
, spec_gen              = spec_gen
, avg_out               = matrix(nrow = length(wl), ncol = final_run_nstacks)
, ss_type               = ss_type
, ss_match              = ss_match)

### parameter_fits
spec_return_final[[3]] <- c(
  gap_width_mean = mean_opts[zz_temp], 
  gap_width_sd = sd_opts[xx_temp],
  crystal_number_mean = crys_num_opt[yy_opt]
  )

### data frame for plotting
spec_return_final[[4]] <- 
  data.frame(
    wavelength  = rep(wl, 2)
  , spectrum    = c(
      crystal_spectra_emp[crystal_spectra_emp$Sample == which_spec, ]$spectra   ## pull out the empirical spectra
    , spec_return_final[[1]]$spectrum),                                         ## fitted spectra
    sample      = rep(which_spec, length(rep(wl, 2)))
  , method      = rep(c("Empirical", ifelse(full_wl_range == "TRUE", "300-1000", paste(wl_range[1], wl_range[2], sep = "-")))
    , each = length(spec_return_final[[1]]$spectrum)))

return(spec_return_final)

} 

## outer wrapper to run the matrix transfer function and compare simulated spectra 
## from matrix transfer function with empirically measured spectra
stack_fitting_func <- function (
  ag0
  , n0
  , n1
  , n2
  , crystal_number_mean
  , crystal_width_mean
  , crystal_width_sd
  , gap_width
  , nstacks
  , wl
  , which_spec
  , fitting
  , full_wl_range
  , wl_range
  , crystal_spectra_emp
  , spec_gen
  , avg_out
  , ss_type
  , ss_match) {
  
rand_sites <- matrix(nrow = nstacks, ncol = 3)
colnames(rand_sites) <- c("cw", "gw", "nc")

## Slightly hacky way of obtaining a truncated Poisson distribution to allow all #s except for 0s and 1s 
## for # of crystals to avoid breakage (in general a very rare event)
for (p in 1:nstacks) {
temp_num <- rpois(1, crystal_number_mean)
  while (temp_num < 2) {
temp_num <- rpois(1, crystal_number_mean)
  }

### Build the given stacks (number = param_list$nstacks)
  rand_sites[p, 1] <- rlnorm(1, 
    meanlog = crystal_width_mean, 
    sdlog = crystal_width_sd)
  rand_sites[p, 2] <- rlnorm(1, 
    meanlog = gap_width[1], 
    sdlog = gap_width[2])
  rand_sites[p, 3] <- temp_num
  
}

### Fit for the given stacks 
 for (o in 1:nstacks) {
   
## multilayer_adj returns from 250:1000
   trans_mat_out <- data.frame(
              multilayer_adj(
                ag0 = ag0
                , n0 = n0
                , n1 = n1
                , n2 = n2
                , d1 = rand_sites[o, 1]
                , d2 = rand_sites[o, 2]
                , stacks = rand_sites[o, 3])
              , stack_num = o)
   
## only return the wavelengths given in the data
   avg_out[ , o] <- trans_mat_out[match(wl, trans_mat_out$wavelength), ]$model
 }

### Average the spectra from nstacks
 spec_out <- data.frame(wavelength = wl, spectrum = rowMeans(avg_out))
 
### Spectra for comparison 
 temp_samp <- crystal_spectra_emp[crystal_spectra_emp$Sample == which_spec, ]
 
### Fit
 if (ss_type == "SCALED") {
 ## oriented in terms of the proportional change relative to itself to avoid 
  ## absolute values (height of spectra)
   ## Also, multiplied by 1000 simply for fitting purposes (round errors, visualizations etc.)
    ## Wont affect results   
   ## Data must have appropriate column titles or this step will run into an error
   
   ## simulated
 spec_out <- transform(spec_out, scaled_spectra = (spectrum / sum(spectrum)) * 1000)
   ## data
 temp_samp <- transform(temp_samp, scaled_spectra = (spectra / sum(spectra)) * 1000)

 if (full_wl_range == TRUE) {
   least_squares_out <- (spec_out$scaled_spectra - temp_samp$scaled_spectra)^2
 } else if (full_wl_range == FALSE) {
   temp_range <- match(seq(wl_range[1], wl_range[2], by = 1), spec_out$wavelength)
   least_squares_out <- (spec_out[temp_range, ]$scaled_spectra - temp_samp[temp_range, ]$scaled_spectra)^2
 }
 
 ### or fit with raw ss
 } else if (ss_type == "RAW") {
   
 ## if match to the curvature is desired, shift curve
   if (ss_match == TRUE) {
       spec_out$spectrum <- spec_out$spectrum / (max(spec_out$spectrum) / max(temp_samp$spectra))
   }
   
 if (full_wl_range == TRUE) {
   least_squares_out <- (spec_out$spectrum - temp_samp$spectra)^2
 } else if (full_wl_range == FALSE) {
   temp_range <- match(seq(wl_range[1], wl_range[2], by = 1), spec_out$wavelength)
   least_squares_out <- (spec_out[temp_range, ]$spectrum - temp_samp[temp_range, ]$spectra)^2
 }
   
 }

 if (is.na(fitting)) {
   sum(least_squares_out)
 } else {
   if (fitting == TRUE) {
   sum(least_squares_out) 
   } else {
   return(list(spec_out, sum(least_squares_out)))
   }
 }
}