# quantile map the snowfall data

#-- Functions -----------------------------------------------------------------
# read hourly observed wind speeds
get_nome_ws <- function(fn, thr = 30) {
  df <- read.csv(fn) %>%
    rename(ts = valid) %>%
    select(ts, sped) %>%
    mutate(ts = ymd_hms(paste0(ts, ":00")))
  df <- df[-(which(duplicated(df$ts))), ]
  df$sped[df$sped == "M"] <- NA
  df <- df %>%
    mutate(sped = as.numeric(as.character(sped))) %>%
    filter(sped < 80)
  # remove spikes
  dg <- which(diff(df$sped) > thr)
  dl <- which(diff(df$sped) < -thr)
  dts <- as.numeric(
    difftime(df$ts[-1], df$ts[-(length(df$ts))], units = "hours")
  )
  dg_ts <- dts[dg[which((dl - 1) %in% dg)]] 
  dl_ts <- dts[dg[which((dl - 1) %in% dg)] + 1] 
  ri <- dg[dg_ts & dl_ts <= 2] + 1
  df[-ri, ]
}

#------------------------------------------------------------------------------

#-- Quantile Map ERA-Interim data ---------------------------------------------
#library(data.table)
library(dplyr)
library(lubridate)

source("helpers.R")

fn1 <- "../raw_data/IEM/ASOS/PAOM_wind_19800101-20200101.txt"
nome_ws <- get_nome_ws(fn1)

fn2 <- "../Nome_Mets_aux/data/era5_ws.Rds"
era5_ws <- readRDS(fn2) %>% 
  select(ts, ws) %>%
  filter(ts >= ymd("1980-01-01"))

era5_ws_adj <- qMap(nome_ws$sped, era5_ws$ws)
era5_ws$ws_adj <- era5_ws_adj$sim_adj

ecdf_lst <- list(
  obs = nome_ws$sped, 
  sim = era5_ws$ws,
  sim_adj = era5_ws$ws_adj
)

# ECDFs
p <- ggECDF_compare(ecdf_lst, xmin = 0, var = "ws", xmax_adj = 0)
# save validation
fn <- "../Nome_Mets_aux/figures/qmap/era5_ws_ecdfs.png"
ggsave(fn, p, width = 7, height = 4.5)

# save adjusted ERA5 output
fn <- "../Nome_Mets_aux/data/era5_ws_adj.Rds"
saveRDS(era5_ws, fn, compress = FALSE)

#------------------------------------------------------------------------------