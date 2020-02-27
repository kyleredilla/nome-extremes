# Convert ERA5 10u and 10v wind components to direction and speed
# Only convert grid cell directly north of Nome cell

#-- Setup ---------------------------------------------------------------------
mk_ncfns <- function() {
  suffix <- c("1979-1982", "1983-1986", "1987-1990", "1991-1994",
              "1995-1998", "1999-2002", "2003-2006", "2007-2010",
              "2011-2014", "2015-2018", "2019")
  paste0("../data-raw/ERA5/ERA5_10u_10v_Nome_sector_", suffix, ".nc")
}

# extract data from netcdfs
# specify ij of 9-cell grid around nome 
#   (remember: grid is transposed from reality)
extract_nc <- function(nc_fn, ij = c(2, 1)) {
  # some reason 2019 file has two extra variables that have complementary NAs
  # just added conditions to handle this single case
  sv <- c(ij, 1)
  sv2 <- c(ij, 7297)
  cv <- c(1, 1, -1)
  cv1 <- c(1, 1, 7296)
  nc <- nc_open(nc_fn)
  if(grepl("2019", nc_fn)) {
    u10_1 <- ncvar_get(nc, varid = "u10_0001", start = sv, count = cv1)
    v10_1 <- ncvar_get(nc, varid = "v10_0001", start = sv, count = cv1)
    u10_2 <- ncvar_get(nc, varid = "u10_0005", start = sv2, count = cv)
    v10_2 <- ncvar_get(nc, varid = "v10_0005", start = sv2, count = cv)
    u10 <- c(u10_1, u10_2)
    v10 <- c(v10_1, v10_2)
  } else {
    u10 <- ncvar_get(nc, varid = "u10", start = sv, count = cv)
    v10 <- ncvar_get(nc, varid = "v10", start = sv, count = cv)
  }
  ts <- ncvar_get(nc, varid = "time")
  nc_close(nc)
  uv <- as.data.frame(cbind(u10, v10))
  uv$ts <- as.POSIXct(ts * 3600, origin = "1900-01-01", tz = "UTC")
  uv
}

extr_conv <- function(nc_fn) {
  uv2wdws(extract_nc(nc_fn))
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(ncdf4)

source("helpers.R")

nc_fns <- mk_ncfns()

era5_ws <- dplyr::bind_rows(lapply(nc_fns, extr_conv))

saveRDS(era5_ws, "data/era5_ws.Rds", compress = FALSE)

#------------------------------------------------------------------------------
