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
# CM3
cm3h_adj_lst <- qMap(
  era5_sf_adj$sf_adj, wrf_sf[[1]]$sf, 
  ret.deltas = TRUE
)
cm3f_adj <- qMap(
  sim = wrf_sf[[2]]$sf, 
  use.deltas = cm3h_adj_lst$deltas
)
cm3h_adj <- cm3h_adj_lst$df
cm3f_adj_lst <- list(
  obs = cm3h_adj_lst$df$sim_adj,
  sim = cm3f_adj$sim, 
  sim_adj = cm3f_adj$sim_adj
)

# CCSM4
ccsm4h_adj_lst <- qMap(
  # missing 2005-12-31 in CCSM4 WRF output, adjust accordingly
  era5_sf_adj[1:(dim(era5_sf_adj)[1] - 1), ]$sf_adj, 
  wrf_sf[[3]]$sf, 
  ret.deltas = TRUE
)
ccsm4f_adj <- qMap(
  sim = wrf_sf[[4]]$sf, 
  use.deltas = ccsm4h_adj_lst$deltas
)
ccsm4h_adj <- ccsm4h_adj_lst$df
ccsm4f_adj_lst <- list(
  obs = ccsm4h_adj_lst$df$sim_adj,
  sim = ccsm4f_adj$sim, 
  sim_adj = ccsm4f_adj$sim_adj
)

# plot/save ECDFs
p1 <- ggECDF_compare(
  cm3h_adj_lst$df, 
  p_title = "Nome Snow: CM3 -> ERA5"
)
p2 <- ggECDF_compare(
  cm3f_adj_lst, 
  p_title = "Nome Snow: CM3F to Adj CM3H"
)
p3 <- ggECDF_compare(
  ccsm4h_adj_lst$df, 
  p_title = "Nome Snow: CCSM4 -> ERA5"
)
p4 <- ggECDF_compare(
  ccsm4f_adj_lst, 
  p_title = "Nome Snow: CCSM4F -> Adj CCSM4H"
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
adj_fns <- c(
  "../Nome_Mets_aux/data/cm3h_sf_adj.Rds",
  "../Nome_Mets_aux/data/cm3f_sf_adj.Rds",
  "../Nome_Mets_aux/data/ccsm4h_sf_adj.Rds",
  "../Nome_Mets_aux/data/ccsm4f_sf_adj.Rds"
)

saveRDS(cm3h_adj, adj_fns[1])
saveRDS(cm3f_adj, adj_fns[2])
saveRDS(ccsm4h_adj, adj_fns[3])
saveRDS(ccsm4f_adj, adj_fns[4])

#------------------------------------------------------------------------------
