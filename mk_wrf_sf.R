# the WRF outpuf for snowfall seems to have some issues. This script will
#   estimate snowfall based on precipitation and temperature

#-- Functions -----------------------------------------------------------------
# make wrf filenames
mk_wrf_fns <- function(gcm_period, var = "t2") {
  gcm_str <- if(gcm_period[1]) "GFDL-CM3" else "NCAR-CCSM4"
  if(gcm_period[2]) {
    tf_str <- "historical"
    years <- 1979:2005
  } else {
    tf_str <- "rcp85"
    years <- 2006:2100
  }
  paste0(
    "../raw_data/WRF/", var, "_hourly_wrf_", 
    gcm_str, "_", tf_str, "_", years, ".nc"
  )
}

# kelvin to Farenheit 
K_to_F <- function(K) {
  (K - 273.15) * (9/5) + 32
}

# calculate snowfall from df of temp/precip
calc_sf <- function(df) {
  df %>%
    mutate(date = as.Date(ymd_hms(rownames(df)))) %>%
    group_by(date) %>%
    summarise(sf = sum(pcpt[t2 <= 32]) * 7)
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(dplyr)
library(ncdf4)
library(snaptools)
library(lubridate)

nome_coords <- ak_coords[1, ]

gcm_periods_lst <- list(
  c(TRUE, TRUE),
  c(TRUE, FALSE),
  c(FALSE, TRUE),
  c(FALSE, FALSE)
)

t2_fns <- lapply(gcm_periods_lst, mk_wrf_fns)
pcpt_fns <- lapply(gcm_periods_lst, mk_wrf_fns, "pcpt")

wrf_t2_K <- lapply(t2_fns, wrf_get, nome_coords)
wrf_t2 <- lapply(wrf_t2_K, K_to_F)

wrf_pcpt <- lapply(pcpt_fns, wrf_get, nome_coords)

wrf_lst <- wrf_t2
for(i in 1:4){
  wrf_lst[[i]]$pcpt <- wrf_pcpt[[i]]$pcpt
}

sf_lst <- lapply(wrf_lst, calc_sf)

fn <- "../Nome_Mets_aux/data/nome_gcm_sf.Rds"
saveRDS(sf_lst, fn)

#------------------------------------------------------------------------------
