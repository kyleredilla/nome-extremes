# Convert ERA5 10u and 10v wind components to direction and speed
# Only convert grid cell directly north of Nome cell

#-- Setup ---------------------------------------------------------------------
mk_ncfns <- function() {
  suffix <- c("1979-1982", "1983-1986", "1987-1990", "1991-1994",
              "1995-1998", "1999-2002", "2003-2006", "2007-2010",
              "2011-2014", "2015-2018")
  paste0("../raw_data/ERA5/ERA5_10u_10v_Nome_sector_", suffix, ".nc")
}

# extract data from netcdfs
# specify ij of 9-cell grid around nome 
#   (remember: grid is transposed from reality)
extract_nc <- function(nc_fn, ij = c(2, 1)) {
  nc <- nc_open(nc_fn)
  u10 <- ncvar_get(nc, varid = "u10", start = c(ij, 1), count = c(1, 1, -1))
  v10 <- ncvar_get(nc, varid = "v10", start = c(ij, 1), count = c(1, 1, -1))
  ts <- ncvar_get(nc, varid = "time")
  nc_close(nc)
  uv <- as.data.frame(cbind(u10, v10))
  uv$ts <- as.POSIXct(ts * 3600, origin = "1900-01-01", tz = "UTC")
  uv
}

extr_conv <- function(nc_fn) {
  extract_nc(nc_fn) %>%
    uv2wdws
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(ncdf4)
library(magrittr)

source("helpers.R")

nc_fns <- mk_ncfns()

era5_ws <- dplyr::bind_rows(lapply(nc_fns, extr_conv))

saveRDS(era5_ws, "../Nome_Mets_aux/data/era5_ws.Rds")

#------------------------------------------------------------------------------
