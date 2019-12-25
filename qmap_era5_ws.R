# quantile map the snowfall data

#-- Functions -----------------------------------------------------------------
# read hourly observed wind speeds
get_nome_ws <- function(fn) {
  # spike detection algo
  # 1. calculate differences between each ws in ts
  # 2. calculate corresponding time differences as separate vector
  # 3. determine position(s) i of ws diffs x where x[i] > 30 and x[i + 1] > 30
  # 4. if corresponding time changes at i and i + 1 are < 4 hours, 
  #   add i + 1 to rm_vec
  # 5. remove values
  # hourly wind speed data 
  vars <- c("valid", "sped")
  nome <- fread(fn, select = vars)
  begin <- ymd("1979-01-01")
  end <- ymd("2018-12-31")
  nome[sped == "M", sped := NA]
  nome[, ':=' (valid = paste0(valid, ":00"),
               sped = as.numeric(sped))]
  nome[, valid := ymd_hms(valid)]
  nome <- nome[valid >= begin & valid <= end & sped < 80, ]
  wsl <- list(ws = nome[, sped], ts = nome[, valid])
  wsl <- lapply(wsl, function(x) abs(diff(x)))
  i1 <- which(wsl$ws > 30)
  i2 <- i1[(i1 + 1) %in% i1]
  i_rm <- i2[wsl$ts[i2] < (4 * 3600) & wsl$ts[i2 + 1] < (4 * 3600)] + 1
  nome[-i_rm, sped]
}

#------------------------------------------------------------------------------

#-- Quantile Map ERA-Interim data ---------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)

source("helpers.R")

fn1 <- "../raw_data/IEM/ASOS/PAOM_wind_19790101-20181231.txt"
nome_ws <- get_nome_ws(fn1)

fn2 <- "../Nome_Mets_aux/data/era5_ws.Rds"
era5_ws <- readRDS(fn2) %>% select(ts, ws)

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