# quantile map the max temp data

#-- Functions -----------------------------------------------------------------
# convert C to F
C_to_F <- function(deg_C) {
  deg_C * (9/5) + 32
}

# read and prep daily nome data
get_nome_tmax <- function(fn) {
  # Daily data 
  # only need these vars
  vars <- c("DATE", "TMAX")
  # better rnames
  bnames <- c("date", "tmax")
  nome <- fread(fn, select = vars, col.names = bnames)
  # convert to correct type and units (m and C) and 
  #   subset to matching time frame
  begin <- ymd("1979-01-01")
  end <- ymd("2018-12-31")
  # tmax units in tenths of degrees
  nome[, ':=' (date = ymd(date),
               tmax = as.numeric(tmax))]
  nome[date >= begin & date <= end, tmax]
}

# get ERA5 data
get_era5_tmax <- function(fn) {
  readRDS(fn) %>%
    ungroup() %>%
    filter(ij == "1,2") %>%
    select(date, tmax) %>%
    mutate(tmax = C_to_F(tmax))
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(data.table)
library(lubridate)
library(dplyr)

source("helpers.R")
# observed data
data_dir <- Sys.getenv("DATA_DIR")
obs_fp <- file.path(data_dir, "ghcnd_nome_19800101-20191231.csv")
nome_tmax <- get_nome_tmax(obs_fp)
# ERA5 data
era_fp <- file.path(data_dir, "era5.Rds")
era5_tmax <- get_era5_tmax(era_fp)

era5_tmax_adj <- qMap(nome_tmax, era5_tmax$tmax)
era5_tmax$tmax_adj <- era5_tmax_adj$sim_adj

ecdf_lst <- list(
  obs = nome_tmax, 
  sim = era5_tmax$tmax,
  sim_adj = era5_tmax$tmax_adj
)

# ECDFs
p <- ggECDF_compare(
  ecdf_lst, p_title = "Nome T2max ERA5 -> Observed", 
  xmin = -40, var = bquote(T[max]), xmax_adj = 20
)
# save validation
qmap_dir <- file.path(Sys.getenv("FILES_DIR"), "qmap")
dir.create(qmap_dir, showWarnings = FALSE)
qmap_fp <- file.path(qmap_dir, "era5_tmax_ecdfs.png")
ggsave(qmap_fp, p, width = 7, height = 4.5)

# save adjusted ERA5 output
era_out_fp <- file.path(data_dir, "era5_tmax_adj.Rds")
saveRDS(era5_tmax, era_out_fp)

#------------------------------------------------------------------------------
