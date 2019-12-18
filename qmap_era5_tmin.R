# quantile map the minimum temp data

#-- Functions -----------------------------------------------------------------
# convert C to F
C_to_F <- function(deg_C) {
  deg_C * (9/5) + 32
}

# read and prep daily nome data
get_nome_tmin <- function() {
  # Daily data 
  # only need these vars
  vars <- c("DATE", "TMIN", "TMIN_ATTRIBUTES")
  # better rnames
  bnames <- c("date", "tmin", "tmin_attr")
  fn <- "../raw_data/GHCND/Nome.csv"
  nome <- fread(fn, select = vars, col.names = bnames)
  
  # convert to correct type and units (m and C) and 
  #   subset to matching time frame
  begin <- ymd("1979-01-01")
  end <- ymd("2018-12-31")
  nome[, ':=' (date = ymd(date),
               year = year(date),
               decade = year(date) - year(date) %% 10,
               tmin = C_to_F(as.numeric(tmin)/10))]
  nome[, ym := format(date, "%Y-%m")]
  nome <- nome[date >= begin & date <= end, tmin]
}

# get ERA5 data
get_era5_tmin <- function() {
  fn <- "../Nome_Mets_aux/data/era5.Rds"
  readRDS(fn) %>%
    ungroup() %>%
    filter(ij == "1,2") %>%
    select(date, tmin) %>%
    mutate(tmin = C_to_F(tmin))
}

#------------------------------------------------------------------------------

#-- Quantile Map ERA-Interim data ---------------------------------------------
library(data.table)
library(lubridate)
library(dplyr)

source("helpers.R")

nome <- get_nome_tmin()
era5_tmin <- get_era5_tmin()
era5_tmin_adj <- qMap(nome, era5_tmin$tmin)
era5_tmin$tmin_adj <- era5_tmin_adj$sim_adj

results <- list(obs = nome, sim = era5_tmin$tmin, sim_adj = era5_tmin$tmin_adj)
  
# ECDFs
p <- ggECDF_compare(results)
# save validation
fn <- "../Nome_Mets_aux/figures/qmap/era5_tmin_ecdfs.png"
ggsave(fn, p, width = 7, height = 4.5)

# save adjusted ERA5 output
fn <- "../Nome_Mets_aux/data/ERA5_tmin_adj.Rds"
saveRDS(era5_tmin, fn)

#------------------------------------------------------------------------------
