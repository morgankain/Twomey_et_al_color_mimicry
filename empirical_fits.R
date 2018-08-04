########################################################
### Fit some distributions to empirical crystal data ###
########################################################

uniq_samps <- unique(Width_dat$Sample)
dist_fits <- data.frame(
  sample             = numeric(length(uniq_samps))
, crystal_width_mean = numeric(length(uniq_samps))
, crystal_width_sd   = numeric(length(uniq_samps))
)

for (i in seq_along(uniq_samps)) {
  dist_fits[i, ]$sample <- uniq_samps[i]
  dist_fits[i, c(2, 3)] <- unlist(fitdistr(Width_dat[Width_dat$Sample == uniq_samps[i], 3], "logNormal"))[1:2]
}