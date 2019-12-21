# quantile map the snowfall data

#-- Functions -----------------------------------------------------------------
# read hourly observed wind speeds
get_nome_ws <- function(fn) {
  # hourly wind speed data 
  vars <- c("valid", "sped")
  nome <- fread(fn, select = vars)
  begin <- ymd("1979-01-01")
  end <- ymd("2018-12-31")
  nome[sped == "M", sped := NA]
  nome[, ':=' (valid = paste0(valid, ":00"),
               sped = as.numeric(sped))]
  nome[, valid := ymd_hms(valid)]
  nome <- nome[valid >= begin & valid <= end, sped]
}

# get ERA5 data
get_era5_ws <- function(fn) {
  readRDS(fn) %>%
    ungroup() %>%
    filter(ij == "1,2")
}

#------------------------------------------------------------------------------

#-- Quantile Map ERA-Interim data ---------------------------------------------
library(data.table)
library(lubridate)
library(dplyr)

source("helpers.R")

fn1 <- "../raw_data/IEM/ASOS/PAOM_wind_19790101-20181231.txt"
nome_ws <- get_nome_ws(fn1)

fn2 <- "../Nome_Mets_aux/data/era5_ws.Rds"
era5_ws <- get_era5_ws(fn2)

era5_ws_adj <- qMap(nome_ws, era5_ws$ws)
era5_ws$ws_adj <- era5_ws_adj$sim_adj

ecdf_lst <- list(
  obs = nome_ws, 
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
saveRDS(era5_ws, fn)

#------------------------------------------------------------------------------