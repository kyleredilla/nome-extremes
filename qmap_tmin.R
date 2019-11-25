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
  fn <- "F:/raw_data/Nome_Mets/Nome_daily.csv"
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

# get era5_data
get_era5_tmin <- function() {
  fn <- "data/era5.Rds"
  readRDS(fn) %>%
    ungroup() %>%
    filter(ij == "1,2") %>%
    select(tmin) %>%
    unlist() %>% unname() %>%
    C_to_F()
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(data.table)
library(lubridate)
library(dplyr)

source("helpers.R")

results <- get_nome_tmin() %>%
  qMap(get_era5_tmin())

# ECDFs
p <- ggECDF_compare(results)
# save
fn <- "figures/qmap/tmin_ecdfs.png"
ggsave(fn, p, width = 7, height = 4.5)

#------------------------------------------------------------------------------
