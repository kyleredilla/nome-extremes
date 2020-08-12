# preprocess extreme snow, temp, and wind events for barplot creation

#-- Setup ---------------------------------------------------------------------
# get observed daily data, returns list of dfs
get_nome_daily <- function(fn) {
  # inch:meter
  im <- 0.0254
  df <- read.csv(fn) %>%
    mutate(
      date = ymd(DATE),
      month = month(date),
      SNOW = SNOW * im,
    ) %>%
    filter(date > "1990-01-01") %>%
    rename(sf = SNOW, tmin = TMIN, tmax = TMAX)
  
  list(
    tmin = df %>% select(date, tmin),
    tmax = df %>% 
      filter(month == 1 | month == 2) %>%
      select(date, tmax),
    sf = df %>% select(date, sf)
  )
}

# get observed and adjusted wind speed data, filter spikes
get_nome_ws <- function(fn, thr = 30) {
  df <- read.csv(fn) %>%
    rename(ts = valid) %>%
    select(ts, sped) %>%
    mutate(ts = ymd_hms(paste0(ts, ":00"))) %>%
    filter(ts >= ymd("1990-01-01"))
  
  df <- df[-(which(duplicated(df$ts))), ]
  df$sped[df$sped == "M"] <- NA
  df <- df %>%
    mutate(sped = as.numeric(as.character(sped))) %>%
    filter(sped < 80)
  # remove spikes
  dg <- which(diff(df$sped) > thr)
  dl <- which(diff(df$sped) < -thr)
  dts <- as.numeric(
    difftime(df$ts[-1], df$ts[-(length(df$ts))], units = "hours")
  )
  dg_ts <- dts[dg[which((dl - 1) %in% dg)]] 
  dl_ts <- dts[dg[which((dl - 1) %in% dg)] + 1] 
  ri <- dg[dg_ts & dl_ts <= 2] + 1
  df[-ri, ]
}

# make df from results of both GCMs and ERA
mk_df <- function(cm3, ccsm4, era5, obs) {
  mods <- c("CM3", "CCSM4", "ERA5", "Observed")
  obs <- obs %>%
    mutate(mod = factor("Observed", levels = mods)) %>%
    rename(sim_adj = names(.)[2])
  era5 <- era5 %>%
    mutate(mod = factor("ERA5", levels = mods)) %>%
    select(-c(names(.)[2]))
  cm3 <- cm3 %>%
    mutate(mod = factor("CM3", levels = mods))
  ccsm4 %>%
    mutate(mod = factor("CCSM4", levels = mods)) %>%
    bind_rows(cm3) %>%
    filter(
      # funkiness because we have "ts" and "date" columns in the 
      #   different time series data.frames
      {if("ts" %in% names(.)) ts else date} >= "2020-01-01" &
        {if("ts" %in% names(.)) ts else date} <= "2099-12-31"
    ) %>%
    select(names(.)[3], names(.)[2], names(.)[4]) %>%
    bind_rows(era5, obs) %>%
    filter({if("ts" %in% names(.)) ts else date} >= "1980-01-01") %>%
    mutate(
      decade = factor(
        floor(
          year({if("ts" %in% names(.)) ts else date})/10
        ) * 10
      )
    )
}

# count obs surpassing threshold (for tmin and sf)
# era arg is flag for whether to count from ERA (TRUE) or Observed (FALSE)
count_thr <- function(df, 
                      thr = -30, 
                      leq = TRUE, 
                      # era = TRUE, 
                      varname) {
  # new_lvls = if(era) c("CM3/CCSM4", "ERA5") else c("CM3/CCSM4", "Observed")
  new_lvls = c("CM3/CCSM4", "ERA5", "Observed")
  df <- df %>% 
    group_by(mod, decade) %>%
    # group_by(mod, year) %>%
    summarise(count = sum(
      {if(leq) sim_adj <= thr else sim_adj >= thr}, na.rm = TRUE)
    ) %>%
    ungroup
  # historical, either ERA or Observed
  hs <- df %>% 
    #filter(mod == {if(era) "ERA5" else "Observed"}) %>%
    filter(mod == "ERA5" | mod == "Observed") %>%
    mutate(mod = factor(mod, levels = new_lvls),
           minc = 0, maxc = 0) %>%
    rename(avc = count) %>%
    select(decade, avc, minc, maxc, mod)
  # select(year, avc, minc, maxc, mod)
  df %>%
    filter(mod == "CM3" | mod == "CCSM4") %>%
    group_by(decade) %>%
    #group_by(year) %>%
    summarise(
      avc = mean(count), 
      minc = min(count), 
      maxc = max(count)
    ) %>%
    mutate(mod = factor("CM3/CCSM4", levels = new_lvls)) %>%
    bind_rows(hs) %>%
    mutate(varname = varname)
}

# count high wind events
# era arg is flag for whether to count from ERA (TRUE) or Observed (FALSE)
# assumes df is sorted by model and time
# count_hwe <- function(df, thr = 30, d = 10, era = TRUE, varname) {
count_hwe <- function(df, thr = 30, d = 10, varname) {
  # new_lvls = if(era) c("CM3/CCSM4", "ERA5") else c("CM3/CCSM4", "Observed")
  new_lvls = c("CM3/CCSM4", "ERA5", "Observed")
  hws <- df %>% filter(sim_adj >= thr)
  dts <- as.numeric(
    difftime(hws$ts[-1], hws$ts[-(length(hws$ts))], units = "hours")
  )
  hws$hwe <- c(0, cumsum(dts > 2)) + 1
  hws <- bind_rows(
    lapply(unique(hws$hwe), function(hwe) {
      hwe_ts <- hws$ts[hws$hwe == hwe]
      decade <- hws$decade[hws$hwe == hwe][1]
      # year <- hws$year[hws$hwe == hwe][1]
      mod <- hws$mod[hws$hwe == hwe][1]
      dur <- as.numeric(
        difftime(hwe_ts[length(hwe_ts)], hwe_ts[1], units = "hours")
      )
      data.frame(ts = hwe_ts[1], hwe = hwe, hrs = dur, decade = decade, mod = mod)
      # data.frame(ts = hwe_ts[1], hwe = hwe, hrs = dur, year = year, mod = mod)
    })
  )
  
  hws <- hws %>%
    filter(hrs > d) %>%
    group_by(mod, decade) %>%
    #group_by(mod, year) %>%
    summarise(count = n()) %>%
    ungroup
  # historical, either ERA or Observed 
  hs <- hws %>% 
    # filter(mod == {if(era) "ERA5" else "Observed"}) %>%
    filter(mod == "ERA5" | mod == "Observed") %>%
    mutate(mod = factor(mod, levels = new_lvls),
           minc = 0, maxc = 0) %>%
    rename(avc = count) %>%
    select(decade, avc, minc, maxc, mod)
  # select(year, avc, minc, maxc, mod)
  hws %>%
    filter(mod == "CM3" | mod == "CCSM4") %>%
    group_by(decade) %>%
    # group_by(year) %>%
    summarise(
      avc = mean(count), 
      minc = min(count), 
      maxc = max(count)
    ) %>%
    mutate(mod = factor("CM3/CCSM4", levels = new_lvls)) %>%
    bind_rows(hs) %>%
    mutate(varname = varname)
}

# make barplot df
# mk_bar_df <- function(tmin_df, sf_df, ws_df, era = TRUE) {
mk_bar_df <- function(tmin_df, tmax_df, sf_df, ws_df) {
  vn1 <- "Days where $T_{minimum} \\leq -34.4$ °C"
  vn2 <- "Days where $T_{max} \\geq 0$ °C"
  vn3 <- "Days where snowfall $\\geq 20$ cm"
  vn4 <- "High-speed wind events"
  vn_levels <- c(vn1, vn2, vn3, vn4)
  bind_rows(
    # count_thr(tmin_df, era = era, varname = vn1),
    count_thr(tmin_df, varname = vn1),
    count_thr(tmax_df, thr = 32, leq = FALSE, varname = vn2),
    # count_thr(
    #   sf_df, thr = 0.2, leq = FALSE, era = era, varname = vn2
    # ),
    count_thr(
      sf_df, thr = 0.2, leq = FALSE, varname = vn3
    ),
    # count_hwe(ws_df, era = era, varname = vn3)
    count_hwe(ws_df, varname = vn4)
  ) %>%
    mutate(varname = factor(varname, levels = vn_levels))
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(tidyverse)
library(lubridate)

data_dir <- Sys.getenv("DATA_DIR")
gcm_tmin <- readRDS(file.path(data_dir, "gcm_t2min_adj.Rds"))
gcm_tmax <- readRDS(file.path(data_dir, "gcm_t2max_adj.Rds"))
gcm_sf <- readRDS(file.path(data_dir, "gcm_sf_adj.Rds"))
gcm_ws <- readRDS(file.path(data_dir, "gcm_ws_adj.Rds")) 

# filter tmax data to Jan/Feb
gcm_tmax <- lapply(gcm_tmax, function(df) {
  df %>%
    mutate(month = month(date)) %>%
    filter(month == 1 | month == 2) %>%
    select(sim, sim_adj, date)
})

# ignore data from prior to 1990 (and other than Jan/Feb for Tmax)
era_tmin_adj <- readRDS(file.path(data_dir, "era5_tmin_adj.Rds")) %>%
  filter(date >= "1990-01-01") %>%
  rename(sim_adj = tmin_adj)
era_tmax_adj <- readRDS(file.path(data_dir, "era5_tmax_adj.Rds")) %>%
  mutate(month = month(date)) %>%
  filter(date >= "1990-01-01" & (month == 1 | month == 2)) %>%
  rename(sim_adj = tmax_adj)
era_sf_adj <- readRDS(file.path(data_dir, "era5_sf_adj.Rds")) %>%
  filter(date >= "1990-01-01") %>%
  rename(sim_adj = sf_adj)
era_ws_adj <- readRDS(file.path(data_dir, "era5_ws_adj.Rds")) %>%
  filter(ts >= ymd("1990-01-01")) %>%
  rename(sim_adj = ws_adj)

# daily observations
ghcnd_fp <- file.path(data_dir, "ghcnd_nome_19800101-20191231.csv")
obs_lst <- get_nome_daily(ghcnd_fp)

# hourly observations (wind)
asos_dir <- Sys.getenv("ASOS_DIR")
asos_fp <- file.path(asos_dir, "PAOM_wind_19800101-20200101.txt")
obs_lst$ws <- get_nome_ws(asos_fp)

tmin_df <- mk_df(gcm_tmin[[2]], gcm_tmin[[4]], era_tmin_adj, obs_lst$tmin)
tmax_df <- mk_df(gcm_tmax[[2]], gcm_tmax[[4]], era_tmax_adj, obs_lst$tmax)
sf_df <- mk_df(gcm_sf[[2]], gcm_sf[[4]], era_sf_adj, obs_lst$sf)
ws_df <- mk_df(gcm_ws[[2]], gcm_ws[[4]], era_ws_adj, obs_lst$ws)

# save data.frame ready for barplot
bar_df <- mk_bar_df(tmin_df, tmax_df, sf_df, ws_df)
saveRDS(bar_df, file.path(data_dir, "extremes_bar_df.Rds"))

#------------------------------------------------------------------------------
