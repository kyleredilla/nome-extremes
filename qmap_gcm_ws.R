# Quantile map the historical and future GCM wind speed output

#-- Main ----------------------------------------------------------------------
library(dplyr)
library(lubridate)

fn <- "data/era5_ws_adj.Rds"
era5_ws_adj <- readRDS(fn) %>%
  filter(ts < ymd("2006-01-01"))

wrf_wdws <- readRDS("data/wrf_wdws.Rds")

source("helpers.R")

# perform quantile mapping
# list to store results
ws_lst <- list()
# CM3
cm3h_adj_lst <- qMap(
  era5_ws_adj$ws_adj, wrf_wdws[[1]]$ws, 
  ret.deltas = TRUE
)
ws_lst$cm3h <- cm3h_adj_lst$df
ws_lst$cm3f <- qMap(
  sim = wrf_wdws[[2]]$ws, 
  use.deltas = cm3h_adj_lst$deltas
)
# lists for ecdf creation
cm3h_ecdf_lst <- list(
  obs = era5_ws_adj$ws_adj,
  sim = ws_lst$cm3h$sim, 
  sim_adj = ws_lst$cm3h$sim_adj
)
cm3f_ecdf_lst <- list(
  obs = ws_lst$cm3h$sim_adj,
  sim = ws_lst$cm3f$sim, 
  sim_adj = ws_lst$cm3f$sim_adj
)

# CCSM4
ccsm4h_adj_lst <- qMap(
  era5_ws_adj$ws_adj, 
  wrf_wdws[[3]]$ws, 
  ret.deltas = TRUE
)
ws_lst$ccsm4h <- ccsm4h_adj_lst$df
ws_lst$ccsm4f <- qMap(
  sim = wrf_wdws[[4]]$ws, 
  use.deltas = ccsm4h_adj_lst$deltas
)
# lists for ecdfs
ccsm4h_ecdf_lst <- list(
  obs = era5_ws_adj$ws_adj,
  sim = ws_lst$ccsm4h$sim, 
  sim_adj = ws_lst$ccsm4h$sim_adj
)
ccsm4f_ecdf_lst <- list(
  obs = ws_lst$ccsm4h$sim_adj,
  sim = ws_lst$ccsm4f$sim, 
  sim_adj = ws_lst$ccsm4f$sim_adj
)

# add dates both models
ws_lst$cm3h$ts <- wrf_wdws[[1]]$ts
ws_lst$cm3f$ts <- wrf_wdws[[2]]$ts
ws_lst$ccsm4h$ts <- wrf_wdws[[3]]$ts
ws_lst$ccsm4f$ts <- wrf_wdws[[4]]$ts

# plot/save ECDFs
p1 <- ggECDF_compare(
  cm3h_ecdf_lst, 
  p_title = "Nome WS: CM3 -> ERA5",
  xmin = 0, var = "ws", xmax_adj = 10
)
p2 <- ggECDF_compare(
  cm3f_ecdf_lst, 
  p_title = "Nome WS: CM3F to Adj CM3H",
  xmin = 0, var = "ws", xmax_adj = 10
)
p3 <- ggECDF_compare(
  ccsm4h_ecdf_lst, 
  p_title = "Nome WS: CCSM4 -> ERA5",
  xmin = 0, var = "ws", xmax_adj = 10
)
p4 <- ggECDF_compare(
  ccsm4f_ecdf_lst, 
  p_title = "Nome WS: CCSM4F -> Adj CCSM4H",
  xmin = 0, var = "ws", xmax_adj = 10
)

# save ecdfs
ecdf_fns <- c(
  "figs/qmap/cm3_era5_ws_ecdfs.png",
  "figs/qmap/cm3f_cm3h_ws_ecdfs.png",
  "figs/qmap/ccsm4_era5_ws_ecdfs.png",
  "figs/qmap/ccsm4f_ccsm4h_ws_ecdfs.png"
)
ggsave(ecdf_fns[1], p1, width = 7, height = 4.5)
ggsave(ecdf_fns[2], p2, width = 7, height = 4.5)
ggsave(ecdf_fns[3], p3, width = 7, height = 4.5)
ggsave(ecdf_fns[4], p4, width = 7, height = 4.5)

# save adjusted data
fn <- "data/gcm_ws_adj.Rds"
saveRDS(ws_lst, fn)

#------------------------------------------------------------------------------
