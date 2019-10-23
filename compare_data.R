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
make_df <- function(arr_lst, sum_fun = sum) {
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
  df %>% 
    mutate(date = date(ts)) %>%
    group_by(date, ij) %>%
    summarise(sum_var = sum_fun(var))
}

library(ncdf4)
library(data.table)
library(abind)
library(lubridate)
library(dplyr)
library(ggplot2)

# snowfall
sf <- lapply(mk_ncfns("snowfall"), extract_nc) %>% make_df()
sd <- lapply(mk_ncfns("snow_depth"), extract_nc) %>% make_df()
tmin <- lapply(mk_ncfns("2k_temperature"), extract_nc) %>% make_df()
tmax
tavg

sf_mo <- sf_df %>% 
  ungroup() %>%
  mutate(date = format(date, "%Y-%m")) %>%
  group_by(date, ij) %>%
  summarise(sum_var = sum(sum_var)) %>%
  ungroup() %>%
  mutate(date = ymd(paste0(date, "-01")))


ggplot(sf_mo, aes(date, sum_var, color = ij)) + 
  geom_line(size = 1) + 
  scale_x_date(limits = c("1970-01-01", "1972-01-01"))

# Daily data 
# only need these vars
vars <- c("DATE", 
          "SNOW", "SNOW_ATTRIBUTES", "SNWD", "SNWD_ATTRIBUTES", 
          "TMAX", "TMAX_ATTRIBUTES", "TMIN", "TMIN_ATTRIBUTES", 
          "TAVG", "TAVG_ATTRIBUTES")
# better rnames
bnames <- c("date", 
            "sf", "sf_attr", "sd", "sd_attr", 
            "tmax", "tmax_attr", "tmin", "tmin_attr", 
            "tavg", "tavg_attr")
daily <- fread("data/Nome_daily.csv", select = vars,
               col.names = bnames)

# convert to correct type and units (m and C) and 
#   subset to matching time frame
begin <- ymd("1979-01-01")
end <- ymd("2018-12-31")
daily[, ':=' (date = ymd(date),
              sf = as.numeric(sf)/1000,
              sd = as.numeric(sd)/1000,
              tmax = as.numeric(tmax)/10,
              tmin = as.numeric(tmin)/10,
              tavg = as.numeric(tavg)/10)]
daily <- daily[date >= begin & date <= end, ]


# compare distance between each grid point 

