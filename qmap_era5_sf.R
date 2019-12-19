# quantile map the snowfall data

#-- Functions -----------------------------------------------------------------
# read and prep daily nome data
get_nome_sf <- function(fn) {
  # Daily data 
  # only need these vars
  vars <- c("DATE", "SNOW", "SNOW_ATTRIBUTES")
  # better rnames
  bnames <- c("date", "sf", "sf_attr")
  nome <- fread(fn, select = vars, col.names = bnames)
  
  # convert to correct type and units (m and C) and 
  #   subset to matching time frame
  begin <- ymd("1979-01-01")
  end <- ymd("2018-12-31")
  nome[, ':=' (date = ymd(date),
               year = year(date),
               decade = year(date) - year(date) %% 10,
               sf = as.numeric(sf) / 1000)]
  nome[, ym := format(date, "%Y-%m")]
  nome <- nome[date >= begin & date <= end, sf]
}

# get ERA5 data
get_era5_sf <- function(fn) {
  readRDS(fn) %>%
    ungroup() %>%
    filter(ij == "1,2") %>%
    select(date, sf) %>%
    mutate(sf = sf * 7)
}

#------------------------------------------------------------------------------

#-- Quantile Map ERA-Interim data ---------------------------------------------
library(data.table)
library(lubridate)
library(dplyr)

source("helpers.R")

fn1 <- "../raw_data/GHCND/Nome.csv"
nome_sf <- get_nome_sf(fn1)

fn2 <- "../Nome_Mets_aux/data/era5.Rds"
era5_sf <- get_era5_sf(fn2)

era5_sf_adj <- qMap(nome_sf, era5_sf$sf)
era5_sf$sf_adj <- era5_sf_adj$sim_adj

ecdf_lst <- list(
  obs = nome_sf, 
  sim = era5_sf$sf,
  sim_adj = era5_sf$sf_adj
)

# ECDFs
p <- ggECDF_compare(ecdf_lst, xmin = 0, var = "sf", xmax_adj = 0)
# save validation
fn <- "../Nome_Mets_aux/figures/qmap/era5_sf_ecdfs.png"
ggsave(fn, p, width = 7, height = 4.5)

# save adjusted ERA5 output
fn <- "../Nome_Mets_aux/data/era5_sf_adj.Rds"
saveRDS(era5_sf, fn)

#------------------------------------------------------------------------------