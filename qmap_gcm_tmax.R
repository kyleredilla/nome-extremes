# Quantile map the historical and future GCM max temperature data

# generate filepaths for gcm tmin output
# take bool vec for determining gcm and period
mk_paths <- function(gcm_period, wrf_dir) {
  gcm_str <- if(gcm_period[1]) "GFDL-CM3" else "NCAR-CCSM4"
  if(gcm_period[2]) {
    tf_str <- "historical"
    years <- 1979:2005
  } else {
    tf_str <- "rcp85"
    years <- 2006:2100
  }
  file.path(
    wrf_dir, gcm_str, tf_str, 
    paste0("t2max/t2max_daily_wrf_", gcm_str, "_", tf_str, "_", years, ".nc")
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

wrf_dir <- Sys.getenv("WRF_DIR")

nc_fns <- lapply(gcm_periods_lst, mk_paths, wrf_dir)
wrf_t2max_K <- lapply(nc_fns, wrf_get, nome_coords)
wrf_t2max <- lapply(wrf_t2max_K, K_to_F)

data_dir <- Sys.getenv("DATA_DIR")
era_fp <- file.path(data_dir, "era5_tmax_adj.Rds")
era5_tmax_adj <- readRDS(era_fp) %>%
  filter(date <= "2005-12-31")

source("helpers.R")

# perform quantile mapping
# list to store results
tmax_lst <- list()
# CM3
cm3h_adj_lst <- qMap(
  era5_tmax_adj$tmax_adj, wrf_t2max[[1]]$t2max, 
  ret.deltas = TRUE
)
tmax_lst$cm3h <- cm3h_adj_lst$df
tmax_lst$cm3f <- qMap(
  sim = wrf_t2max[[2]]$t2max, 
  use.deltas = cm3h_adj_lst$deltas
)
# lists for ecdf creation
cm3h_ecdf_lst <- list(
  obs = era5_tmax_adj$tmax_adj,
  sim = tmax_lst$cm3h$sim, 
  sim_adj = tmax_lst$cm3h$sim_adj
)
cm3f_ecdf_lst <- list(
  obs = tmax_lst$cm3h$sim_adj,
  sim = tmax_lst$cm3f$sim, 
  sim_adj = tmax_lst$cm3f$sim_adj
)

# CCSM4
ccsm4h_adj_lst <- qMap(
  # missing 2005-12-31 in WRF output, adjust accordingly
  era5_tmax_adj[1:(dim(era5_tmax_adj)[1] - 1), ]$tmax_adj, 
  wrf_t2max[[3]]$t2max, 
  ret.deltas = TRUE
)
tmax_lst$ccsm4h <- ccsm4h_adj_lst$df
tmax_lst$ccsm4f <- qMap(
  sim = wrf_t2max[[4]]$t2max, 
  use.deltas = ccsm4h_adj_lst$deltas
)
# lists for ecdfs
ccsm4h_ecdf_lst <- list(
  obs = era5_tmax_adj$tmax_adj,
  sim = tmax_lst$ccsm4h$sim, 
  sim_adj = tmax_lst$ccsm4h$sim_adj
)
ccsm4f_ecdf_lst <- list(
  obs = tmax_lst$ccsm4h$sim_adj,
  sim = tmax_lst$ccsm4f$sim, 
  sim_adj = tmax_lst$ccsm4f$sim_adj
)

# add dates both models
tmax_lst$cm3h$date <- as.Date(rownames(wrf_t2max[[1]]))
tmax_lst$cm3f$date <- as.Date(rownames(wrf_t2max[[2]]))
tmax_lst$ccsm4h$date <- as.Date(rownames(wrf_t2max[[3]]))
tmax_lst$ccsm4f$date <- as.Date(rownames(wrf_t2max[[4]]))

# plot/save ECDFs
p1 <- ggECDF_compare(
  cm3h_ecdf_lst, 
  p_title = "Nome t2max: CM3 -> ERA5",
  xmin = -40, var = bquote(T[max]), xmax_adj = 20
)
p2 <- ggECDF_compare(
  cm3f_ecdf_lst, 
  p_title = "Nome t2max: CM3F to Adj CM3H",
  xmin = -40, var = bquote(T[max]), xmax_adj = 20
)
p3 <- ggECDF_compare(
  ccsm4h_ecdf_lst, 
  p_title = "Nome t2max: CCSM4 -> ERA5",
  xmin = -40, var = bquote(T[max]), xmax_adj = 20
)
p4 <- ggECDF_compare(
  ccsm4f_ecdf_lst, 
  p_title = "Nome t2max: CCSM4F -> Adj CCSM4H",
  xmin = -40, var = bquote(T[max]), xmax_adj = 20
)

# save ecdfs
qmap_dir <- file.path(Sys.getenv("FILES_DIR"), "qmap")
dir.create(qmap_dir, showWarnings = FALSE)

ecdf_fns <- c(
  file.path(qmap_dir, "cm3_era5_tmax_ecdfs.png"),
  file.path(qmap_dir, "cm3f_cm3h_tmax_ecdfs.png"),
  file.path(qmap_dir, "ccsm4_era5_tmax_ecdfs.png"),
  file.path(qmap_dir, "ccsm4f_ccsm4h_tmax_ecdfs.png")
)
ggsave(ecdf_fns[1], p1, width = 7, height = 4.5)
ggsave(ecdf_fns[2], p2, width = 7, height = 4.5)
ggsave(ecdf_fns[3], p3, width = 7, height = 4.5)
ggsave(ecdf_fns[4], p4, width = 7, height = 4.5)

# save adjusted data

fn <- file.path(data_dir, "gcm_t2max_adj.Rds")
saveRDS(tmax_lst, fn)

#------------------------------------------------------------------------------
