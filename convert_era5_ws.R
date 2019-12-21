# Convert ERA5 10u and 10v wind components to direction and speed
# Only convert grid cell directly north of Nome cell

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
  arr <- ncvar_get(nc, start = c(ij, 1), count = c(1, 1, -1))
  ts <- ncvar_get(nc, varid = "time")
  nc_close(nc)
  dimnames(arr)[[3]] <- ts
  arr
}

library(parallel)

mclapply()