# make figures for use in manuscript

# figure 1 - counts of extremes for three meteorological variables with ERA 
# figure 2 - counts of extremes for three meteorological variables with obs 

#-- Setup ---------------------------------------------------------------------
# get observed daily data, returns list of dfs
get_nome_daily <- function(fn) {
  im <- 0.0254
  df <- read.csv(fn) %>%
    mutate(date = ymd(DATE),
           SNOW = SNOW * im,
           SNWD = SNWD * im) %>%
    rename(sf = SNOW, sd = SNWD, tmin = TMIN)
  
  list(
    tmin = df %>% select(date, tmin),
    sf = df %>% select(date, sf),
    sd = df %>% select(date, sd)
  )
}

# get observed and adjusted wind speed data, filter spikes
get_nome_ws <- function(fn, thr = 30) {
  df <- read.csv(fn) %>%
    rename(ts = valid) %>%
    select(ts, sped) %>%
    mutate(ts = ymd_hms(paste0(ts, ":00")))
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
                      era = TRUE, 
                      varname) {
  new_lvls = if(era) c("CM3/CCSM4", "ERA5") else c("CM3/CCSM4", "Observed")
  df <- df %>% 
    group_by(mod, decade) %>%
    summarise(count = sum(
      {if(leq) sim_adj <= thr else sim_adj >= thr}, na.rm = TRUE)
    ) %>%
    ungroup
  # historical, either ERA or Observed
  hs <- df %>% 
    filter(mod == {if(era) "ERA5" else "Observed"}) %>%
    mutate(mod = factor(mod, levels = new_lvls),
           minc = 0, maxc = 0) %>%
    rename(avc = count) %>%
    select(decade, avc, minc, maxc, mod)
  df %>%
    filter(mod == "CM3" | mod == "CCSM4") %>%
    group_by(decade) %>%
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
# presumes df is sorted by model and time
count_hwe <- function(df, thr = 30, d = 10, era = TRUE, varname) {
  new_lvls = if(era) c("CM3/CCSM4", "ERA5") else c("CM3/CCSM4", "Observed")
  hws <- df %>% filter(sim_adj >= thr)
  dts <- as.numeric(
    difftime(hws$ts[-1], hws$ts[-(length(hws$ts))], units = "hours")
  )
  hws$hwe <- c(0, cumsum(dts > 2)) + 1
  hws <- bind_rows(
    lapply(unique(hws$hwe), function(hwe) {
      hwe_ts <- hws$ts[hws$hwe == hwe]
      decade <- hws$decade[hws$hwe == hwe][1]
      mod <- hws$mod[hws$hwe == hwe][1]
      dur <- as.numeric(
        difftime(hwe_ts[length(hwe_ts)], hwe_ts[1], units = "hours")
      )
      data.frame(ts = hwe_ts[1], hwe = hwe, hrs = dur, decade = decade, mod = mod)
    })
  )
 
  hws <- hws %>%
    filter(hrs > d) %>%
    group_by(mod, decade) %>%
    summarise(count = n()) %>%
    ungroup
  # historical, either ERA or Observed 
  hs <- hws %>% 
    filter(mod == {if(era) "ERA5" else "Observed"}) %>%
    mutate(mod = factor(mod, levels = new_lvls),
           minc = 0, maxc = 0) %>%
    rename(avc = count) %>%
    select(decade, avc, minc, maxc, mod)
  hws %>%
    filter(mod == "CM3" | mod == "CCSM4") %>%
    group_by(decade) %>%
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
mk_bar_df <- function(tmin_df, sf_df, ws_df, era = TRUE) {
  vn1 <- "Days where $T_{minimum} \\leq -30$ °F"
  vn2 <- "Days where snowfall $\\geq 20$ cm"
  vn3 <- "High wind events (wind speed $\\geq 30$ mph for 10h)"
  vn_levels <- c(vn1, vn2, vn3)
  bind_rows(
    count_thr(tmin_df, era = era, varname = vn1),
    count_thr(
      sf_df, thr = 0.2, leq = FALSE, era = era, varname = vn2
    ),
    count_hwe(ws_df, era = era, varname = vn3)
  ) %>%
    mutate(varname = factor(varname, levels = vn_levels))
}

# make barplot
mk_barplot <- function(df) {
  cp <- if_else(levels(df$mod)[2] == "ERA5", 1, 4)
  cols <- c("darkolivegreen3", ggsci::pal_jco("default")(4)[cp])
  mk_tex <- function(string) latex2exp::TeX(string)
  
  ggplot(df, aes(x = decade, y = avc)) + 
    geom_bar(aes(fill = mod), stat = "identity") + 
    scale_fill_manual(values = cols) +  
    geom_errorbar(
      aes(ymin = minc, ymax = maxc, alpha = if_else(minc == maxc, 0, 1)),
      width = 0.2
    ) +
    geom_label(
      aes(label = avc, group = mod),
      vjust = -0.25, alpha = 1,
      label.padding = unit(0.1, "lines"), 
      label.size = NA
    ) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .2))) + 
    ylab("Count") + xlab("Decade") + labs(fill = "Source") +
    theme_classic() + 
    facet_wrap(~as.factor(varname), nrow = 3, scales = "free_y",
               labeller = as_labeller(mk_tex, default = label_parsed)) + 
    theme(strip.background = element_blank(),
          strip.text = element_text("serif", color = "black", size = 11,
                                    margin = margin(b = 0, t = 0)),
          panel.border = element_rect(color = "black", fill = NA),
          axis.text = element_text("serif", color = "black"),
          axis.title = element_text(family = "serif"),
          legend.text = element_text("serif"),
          legend.title = element_text("serif")) + 
    guides(alpha = FALSE)
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)

gcm_tmin <- readRDS("../Nome_Mets_aux/data/gcm_t2min_adj.Rds")
gcm_sf <- readRDS("../Nome_Mets_aux/data/gcm_sf_adj.Rds")
gcm_ws <- readRDS("../Nome_Mets_aux/data/gcm_ws_adj.Rds") 

era_tmin_adj <- readRDS("../Nome_Mets_aux/data/era5_tmin_adj.Rds") %>%
  rename(sim_adj = tmin_adj)
era_sf_adj <- readRDS("../Nome_Mets_aux/data/era5_sf_adj.Rds") %>%
  rename(sim_adj = sf_adj)
era_ws_adj <- readRDS("../Nome_Mets_aux/data/era5_ws_adj.Rds") %>%
  rename(sim_adj = ws_adj)

# daily observations
fn <- "../raw_data/GHCND/Nome_snow_tmin_19800101-20191231.csv"
obs_lst <- get_nome_daily(fn)

# hourly observations (wind)
fn <- "../raw_data/IEM/ASOS/PAOM_wind_19800101-20200101.txt"
obs_lst$ws <- get_nome_ws(fn)

tmin_df <- mk_df(gcm_tmin[[2]], gcm_tmin[[4]], era_tmin_adj, obs_lst$tmin)
sf_df <- mk_df(gcm_sf[[2]], gcm_sf[[4]], era_sf_adj, obs_lst$sf)
ws_df <- mk_df(gcm_ws[[2]], gcm_ws[[4]], era_ws_adj, obs_lst$ws)

# make plots and save
p1 <- mk_bar_df(tmin_df, sf_df, ws_df) %>%
  mk_barplot
fn1 <- "../Nome_Mets_aux/figures/manuscript/figure_1_ERA5.png"
ggsave(fn1, p1, width = 6, height = 8)

p2 <- mk_bar_df(tmin_df, sf_df, ws_df, era = FALSE) %>%
  mk_barplot
fn2 <- "../Nome_Mets_aux/figures/manuscript/figure_1_Observed.png"
ggsave(fn2, p2, width = 6, height = 8)

#------------------------------------------------------------------------------
