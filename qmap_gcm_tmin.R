# Quantile map the historical and future GCM temperature output

# generate filepaths for gcm tmin output
# take bool vec for determining gcm and period
mk_paths <- function(gcm_period) {
  gcm_str <- if(gcm_period[1]) "GFDL-CM3" else "NCAR-CCSM4"
  if(gcm_period[2]) {
    tf_str <- "historical"
    years <- 1979:2005
  } else {
    tf_str <- "rcp85"
    years <- 2006:2100
  }
  paste0(
    "../data-raw/WRF/daily/", file.path(gcm_str, tf_str), 
    "t2min/t2min_daily_wrf_", gcm_str, "_", tf_str, "_", years, ".nc"
  )
}

# kelvin to Farenheit 
K_to_F <- function(K) {
  (K - 273.15) * (9/5) + 32
}

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

nc_fns <- lapply(gcm_periods_lst, mk_paths)
wrf_t2min_K <- lapply(nc_fns, wrf_get, nome_coords)
wrf_t2min <- lapply(wrf_t2min_K, K_to_F)

fn <- "data/era5_tmin_adj.Rds"
era5_tmin_adj <- readRDS(fn) %>%
  filter(date <= "2005-12-31")

source("helpers.R")

# perform quantile mapping
# list to store results
tmin_lst <- list()
# CM3
cm3h_adj_lst <- qMap(
  era5_tmin_adj$tmin_adj, wrf_t2min[[1]]$t2min, 
  ret.deltas = TRUE
)
tmin_lst$cm3h <- cm3h_adj_lst$df
tmin_lst$cm3f <- qMap(
  sim = wrf_t2min[[2]]$t2min, 
  use.deltas = cm3h_adj_lst$deltas
)
# lists for ecdf creation
cm3h_ecdf_lst <- list(
  obs = era5_tmin_adj$tmin_adj,
  sim = tmin_lst$cm3h$sim, 
  sim_adj = tmin_lst$cm3h$sim_adj
)
cm3f_ecdf_lst <- list(
  obs = tmin_lst$cm3h$sim_adj,
  sim = tmin_lst$cm3f$sim, 
  sim_adj = tmin_lst$cm3f$sim_adj
)

# CCSM4
ccsm4h_adj_lst <- qMap(
  # missing 2005-12-31 in WRF output, adjust accordingly
  era5_tmin_adj[1:(dim(era5_tmin_adj)[1] - 1), ]$tmin_adj, 
  wrf_t2min[[3]]$t2min, 
  ret.deltas = TRUE
)
tmin_lst$ccsm4h <- ccsm4h_adj_lst$df
tmin_lst$ccsm4f <- qMap(
  sim = wrf_t2min[[4]]$t2min, 
  use.deltas = ccsm4h_adj_lst$deltas
)
# lists for ecdfs
ccsm4h_ecdf_lst <- list(
  obs = era5_tmin_adj$tmin_adj,
  sim = tmin_lst$ccsm4h$sim, 
  sim_adj = tmin_lst$ccsm4h$sim_adj
)
ccsm4f_ecdf_lst <- list(
  obs = tmin_lst$ccsm4h$sim_adj,
  sim = tmin_lst$ccsm4f$sim, 
  sim_adj = tmin_lst$ccsm4f$sim_adj
)

# add dates both models
tmin_lst$cm3h$date <- as.Date(rownames(wrf_t2min[[1]]))
tmin_lst$cm3f$date <- as.Date(rownames(wrf_t2min[[2]]))
tmin_lst$ccsm4h$date <- as.Date(rownames(wrf_t2min[[3]]))
tmin_lst$ccsm4f$date <- as.Date(rownames(wrf_t2min[[4]]))

# plot/save ECDFs
p1 <- ggECDF_compare(
  cm3h_ecdf_lst, 
  p_title = "Nome: CM3 -> ERA5"
)
p2 <- ggECDF_compare(
  cm3f_ecdf_lst, 
  p_title = "Nome: CM3F to Adj CM3H"
)
p3 <- ggECDF_compare(
  ccsm4h_ecdf_lst, 
  p_title = "Nome: CCSM4 -> ERA5"
)
p4 <- ggECDF_compare(
  ccsm4f_ecdf_lst, 
  p_title = "Nome: CCSM4F -> Adj CCSM4H"
)

# save ecdfs
ecdf_fns <- c(
  "figs/qmap/cm3_era5_tmin_ecdfs.png",
  "figs/qmap/cm3f_cm3h_tmin_ecdfs.png",
  "figs/qmap/ccsm4_era5_tmin_ecdfs.png",
  "figs/qmap/ccsm4f_ccsm4h_tmin_ecdfs.png"
)
ggsave(ecdf_fns[1], p1, width = 7, height = 4.5)
ggsave(ecdf_fns[2], p2, width = 7, height = 4.5)
ggsave(ecdf_fns[3], p3, width = 7, height = 4.5)
ggsave(ecdf_fns[4], p4, width = 7, height = 4.5)

# save adjusted data

fn <- "data/gcm_t2min_adj.Rds"
saveRDS(tmin_lst, fn)

#------------------------------------------------------------------------------
