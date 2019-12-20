# Quantile map the historical and future GCM snowfall output

#-- Main ----------------------------------------------------------------------
library(dplyr)
library(lubridate)

fn <- "../Nome_Mets_aux/data/era5_sf_adj.Rds"
era5_sf_adj <- readRDS(fn) %>%
  filter(date <= "2005-12-31")

wrf_sf <- readRDS("../Nome_Mets_aux/data/nome_gcm_sf.Rds")

source("helpers.R")

# perform quantile mapping
# list to store results
sf_lst <- list()
# CM3
cm3h_adj_lst <- qMap(
  era5_sf_adj$sf_adj, wrf_sf[[1]]$sf, 
  ret.deltas = TRUE
)
sf_lst$cm3h <- cm3h_adj_lst$df
sf_lst$cm3f <- qMap(
  sim = wrf_sf[[2]]$sf, 
  use.deltas = cm3h_adj_lst$deltas
)
# lists for ecdf creation
cm3h_ecdf_lst <- list(
  obs = era5_sf_adj$sf_adj,
  sim = sf_lst$cm3h$sim, 
  sim_adj = sf_lst$cm3h$sim_adj
)
cm3f_ecdf_lst <- list(
  obs = sf_lst$cm3h$sim_adj,
  sim = sf_lst$cm3f$sim, 
  sim_adj = sf_lst$cm3f$sim_adj
)

# CCSM4
ccsm4h_adj_lst <- qMap(
  # missing 2005-12-31 in CCSM4 WRF output, adjust accordingly
  era5_sf_adj[1:(dim(era5_sf_adj)[1] - 1), ]$sf_adj, 
  wrf_sf[[3]]$sf, 
  ret.deltas = TRUE
)
sf_lst$ccsm4h <- ccsm4h_adj_lst$df
sf_lst$ccsm4f <- qMap(
  sim = wrf_sf[[4]]$sf, 
  use.deltas = ccsm4h_adj_lst$deltas
)
# lists for ecdfs
ccsm4h_ecdf_lst <- list(
  obs = era5_sf_adj$sf_adj,
  sim = sf_lst$ccsm4h$sim, 
  sim_adj = sf_lst$ccsm4h$sim_adj
)
ccsm4f_ecdf_lst <- list(
  obs = sf_lst$ccsm4h$sim_adj,
  sim = sf_lst$ccsm4f$sim, 
  sim_adj = sf_lst$ccsm4f$sim_adj
)

# add dates both models
sf_lst$cm3h$date <- wrf_sf[[1]]$date
sf_lst$cm3f$date <- wrf_sf[[2]]$date
sf_lst$ccsm4h$date <- wrf_sf[[3]]$date
sf_lst$ccsm4f$date <- wrf_sf[[4]]$date

# plot/save ECDFs
p1 <- ggECDF_compare(
  cm3h_ecdf_lst, 
  p_title = "Nome Snow: CM3 -> ERA5",
  xmin = 0, var = "sf", xmax_adj = 0
)
p2 <- ggECDF_compare(
  cm3f_ecdf_lst, 
  p_title = "Nome Snow: CM3F to Adj CM3H",
  xmin = 0, var = "sf", xmax_adj = 0
)
p3 <- ggECDF_compare(
  ccsm4h_ecdf_lst, 
  p_title = "Nome Snow: CCSM4 -> ERA5",
  xmin = 0, var = "sf", xmax_adj = 0
)
p4 <- ggECDF_compare(
  ccsm4f_ecdf_lst, 
  p_title = "Nome Snow: CCSM4F -> Adj CCSM4H",
  xmin = 0, var = "sf", xmax_adj = 0
)

# save ecdfs
ecdf_fns <- c(
  "../Nome_Mets_aux/figures/qmap/cm3_era5_sf_ecdfs.png",
  "../Nome_Mets_aux/figures/qmap/cm3f_cm3h_sf_ecdfs.png",
  "../Nome_Mets_aux/figures/qmap/ccsm4_era5_sf_ecdfs.png",
  "../Nome_Mets_aux/figures/qmap/ccsm4f_ccsm4h_sf_ecdfs.png"
)
ggsave(ecdf_fns[1], p1, width = 7, height = 4.5)
ggsave(ecdf_fns[2], p2, width = 7, height = 4.5)
ggsave(ecdf_fns[3], p3, width = 7, height = 4.5)
ggsave(ecdf_fns[4], p4, width = 7, height = 4.5)

# save adjusted data
fn <- "../Nome_Mets_aux/data/gcm_sf_adj.Rds"
saveRDS(sf_lst, fn)

#------------------------------------------------------------------------------
