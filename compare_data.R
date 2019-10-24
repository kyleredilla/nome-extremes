# compare data from four different cells around Nome to see if 
#   one is clearly more representative than the others

# create paths to data files
mk_ncfns <- function(var) {
  suffix <- c("1979-1990", "1991-2002", "2003-2014", "2015-2018")
  paste0("data/ERA5_", var, "_Nome_quad_", suffix, ".nc")
}

# extract data from netcdfs
extract_nc <- function(nc_fn) {
  nc <- nc_open(nc_fn)
  arr <- ncvar_get(nc)
  ts <- ncvar_get(nc, varid = "time")
  nc_close(nc)
  dimnames(arr)[[3]] <- ts
  arr
}

# bind arrays and create data.frame
# two modes: sum (e.g. snowfall), avg (e.g. snow depth), 
make_df <- function(arr_lst, varname, fun = sum, conv = FALSE) {
  arr <- abind(arr_lst, along = 3)
  orig <- ymd_hms("1900-01-01 00:00:0")
  tvec <- hours(dimnames(arr)[[3]]) + orig
  
  # make data frame
  # split into each point and combine
  # add i,j grid cell identifier
  ij_df <- expand.grid(1:3, 1:3)
  df <- data.frame(var = unlist(asplit(arr, c(1, 2))),
                   ts = rep(tvec, 9), 
                   ij = rep(paste(ij_df[, 1], ij_df[, 2], sep = ","), 
                            each = length(tvec)))
  # summarise data by date
  df <- df %>% 
    mutate(date = date(ts)) %>%
    group_by(date, ij) %>%
    summarise(var = fun(var, na.rm = TRUE))
  
  # convert from Kelvin to Celsius if temp var
  if (conv == TRUE) {
    df <- df %>% mutate(var = var - 273.15)
  } 
  
  # rename variable
  names(df)[3] <- varname
  df
}

# compute distance between ERA5 i,j time series for all variables
# (and each var separately?)
# args will be df and DT, one for ERA5 (sim) and one for Nome (obs)
compute_dist <- function(sim, obs) {
  # drop unnecessary columns
  # tavg missing in 9933 cases for Nome daily. Ignoring from distance calc
  vars <- c("date", "sf", "sd", "tmin", "tmax")
  obs <- obs[, ..vars]
  # remove NAs and make sure same dates are removed from sim, drop dates
  obs <- na.omit(obs)
  valid_dates <- obs$date
  sim <- sim %>% filter(date %in% valid_dates)
  sim$date <- NULL
  obs[, date := NULL]
  # unique cells to calculate distance over
  cells <- levels(sim$ij)
  # compute distance for each cell
  obs <- as.matrix(obs)
  cell_dist <- function(cell, sim, Y) {
    X <- sim %>% 
      ungroup() %>%
      filter(ij == cell) %>% 
      select(-ij) %>% 
      as.matrix()
    sqrt(sum((X - Y)^2))
  }
  
  sapply(cells, cell_dist, sim, obs)
}

# take vector of dists with "i,j" coordinate names and return
#   df with lon/lat and dists
get_coords <- function(dists) {
  # get coords from an .nc file
  nc <- nc_open("data/ERA5_2m_temperature_Nome_quad_1979-1990.nc")
  xc <- nc$dim$longitude$vals
  yc <- nc$dim$latitude$vals
  nc_close(nc)
  # create df
  ijs <- names(dists)
  yci <- as.numeric(substr(ijs, 1, 1))
  xcj <- as.numeric(substr(ijs, 3, 3))
  data.frame(lon = xc[xcj], lat = yc[yci], dist = dists)
}


library(ncdf4)
library(data.table)
library(abind)
library(lubridate)
library(dplyr)
library(ggplot2)

# vars: snowfall, snow depth, min temp, max temp, avg temp
sf <- lapply(mk_ncfns("snowfall"), extract_nc) %>% 
  make_df("sf")
sd <- lapply(mk_ncfns("snow_depth"), extract_nc) %>% 
  make_df("sd", fun = mean)
tmin <- lapply(mk_ncfns("2m_temperature"), extract_nc) %>% 
  make_df("tmin", min, TRUE)
tmax <- lapply(mk_ncfns("2m_temperature"), extract_nc) %>% 
  make_df("tmax", max, TRUE)
tavg <- lapply(mk_ncfns("2m_temperature"), extract_nc) %>% 
  make_df("tavg", mean, TRUE)

# ignoring tavg in dist calc, missing 9933 observations in
#   selected time window
era5 <- cbind(sf, sd, tmin, tmax) %>% 
  select(date, ij, sf, sd, tmin, tmax)

# Daily data 
# only need these vars
vars <- c("DATE", 
          "SNOW", "SNOW_ATTRIBUTES", "SNWD", "SNWD_ATTRIBUTES", 
          "TMIN", "TMIN_ATTRIBUTES", "TMAX", "TMAX_ATTRIBUTES", 
          "TAVG", "TAVG_ATTRIBUTES")
# better rnames
bnames <- c("date", 
            "sf", "sf_attr", "sd", "sd_attr", 
            "tmin", "tmin_attr", "tmax", "tmax_attr", 
            "tavg", "tavg_attr")
nome <- fread("data/Nome_daily.csv", select = vars,
              col.names = bnames)

# convert to correct type and units (m and C) and 
#   subset to matching time frame
begin <- ymd("1979-01-01")
end <- ymd("2018-12-31")
nome[, ':=' (date = ymd(date),
             sf = as.numeric(sf)/1000,
             sd = as.numeric(sd)/1000,
             tmin = as.numeric(tmin)/10,
             tmax = as.numeric(tmax)/10,
             tavg = as.numeric(tavg)/10)]
nome <- nome[date >= begin & date <= end, ]

# compare distance between each grid point 
dists <- compute_dist(era5, nome)

dist_df <- get_coords(dists)

saveRDS(dist_df, "data/dist_results.Rds")
