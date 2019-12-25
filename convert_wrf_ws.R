# Convert WRF u10 and v10 wind components to direction and speed

#-- Setup ---------------------------------------------------------------------
# generate boolean list for filename args
mk_args <- function() {
  tf <- c(TRUE, FALSE)
  split(cbind(rep(tf, each = 2), rep(tf, 2)), 1:4)
}

# generate filepaths for gcm tmin output
# take bool vec for determining component, gcm, and period
# args[1] TRUE = "u10", FALSE = "v10"
# args[2] TRUE = "GFDL-CM3", FALSE = "NCAR-CCSM4"
# args[3] TRUE = "historical", FALSE = "rcp85"
mk_paths <- function(args, u10 = TRUE) {
  var_str <- if(u10) "u10" else "v10"
  gcm_str <- if(args[1]) "GFDL-CM3" else "NCAR-CCSM4"
  if(args[2]) {
    tf_str <- "historical"
    years <- 1979:2005
  } else {
    tf_str <- "rcp85"
    years <- 2006:2100
  }
  paste0(
    "../raw_data/WRF/", var_str, "_hourly_wrf_", 
    gcm_str, "_", tf_str, "_", years, ".nc"
  )
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
suppressMessages({
  library(snaptools)
  library(lubridate)
})

source("helpers.R")

nome_coords <- ak_coords[1, ]

args_lst <- mk_args()
u10_fns <- lapply(args_lst, mk_paths)
v10_fns <- lapply(args_lst, mk_paths, u10 = FALSE)

# mclapply fails on first run with wrf_get for some reason
try(wrf_fail <- wrf_get(u10_fns[[1]], nome_coords, use_par = TRUE, cores = 64))
cat("extracting u10 component...\n")
t1 <- system.time(
  wrf_u10 <- lapply(u10_fns, wrf_get, nome_coords, use_par = TRUE, cores = 64)
)
cat(paste0("u10 component extracted, ", t1[3], " seconds\n"))
cat("extracting v10 component...\n")
t1 <- system.time(
  wrf_v10 <- lapply(v10_fns, wrf_get, nome_coords, use_par = TRUE, cores = 64)
)
cat(paste0("v10 component extracted, ", t1[3], " seconds\n"))

wrf_uv <- list()
for(i in 1:4) {
  wrf_uv[[i]] <- data.frame(
    u10 = wrf_u10[[i]],
    v10 = wrf_v10[[i]],
    ts = ymd_hms(rownames(wrf_u10[[i]]))
  )
}

cat("converting wind components...\n")
t1 <- system.time(
  wrf_wdws <- lapply(wrf_uv, uv2wdws)
)
cat(paste0("wind components converted, ", t1[3], " seconds\n"))

fn_out <- "../Nome_Mets_aux/data/wrf_wdws.Rds"
saveRDS(wrf_wdws, fn_out)
cat(paste0("winds saved as ", fn_out, "\n"))

#------------------------------------------------------------------------------
