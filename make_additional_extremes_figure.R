# make additional extremes figure for exploration, not included in study
# THIS SCRIPT CURRENTLY UNTESTED

#-- Additional Figure ---------------------------------------------------------
# historical period broken down by year
mk_df_yr <- function(era5, obs) {
  mods <- c("ERA5", "Observed")
  obs <- obs %>%
    mutate(mod = factor("Observed", levels = mods)) %>%
    rename(sim_adj = names(.)[2])
  era5 <- era5 %>%
    mutate(mod = factor("ERA5", levels = mods)) %>%
    select(-c(names(.)[2])) %>%
    bind_rows(obs) %>%
    filter({if("ts" %in% names(.)) ts else date} >= "1980-01-01") %>%
    mutate(
      year = factor(year({if("ts" %in% names(.)) ts else date}))
    )
}

# count obs surpassing threshold (for tmin and sf)
# era arg is flag for whether to count from ERA (TRUE) or Observed (FALSE)
count_thr_yr <- function(df, 
                         thr = -30, 
                         leq = TRUE, 
                         # era = TRUE, 
                         varname) {
  # new_lvls = if(era) c("CM3/CCSM4", "ERA5") else c("CM3/CCSM4", "Observed")
  new_lvls = c("ERA5", "Observed")
  df <- df %>% 
    # group_by(mod, decade) %>%
    group_by(mod, year) %>%
    summarise(count = sum(
      {if(leq) sim_adj <= thr else sim_adj >= thr}, na.rm = TRUE)
    ) %>%
    ungroup
  # historical, either ERA or Observed
  df %>% 
    #filter(mod == {if(era) "ERA5" else "Observed"}) %>%
    filter(mod == "ERA5" | mod == "Observed") %>%
    mutate(mod = factor(mod, levels = new_lvls),
           minc = 0, maxc = 0) %>%
    rename(avc = count) %>%
    # select(decade, avc, minc, maxc, mod)
    select(year, avc, minc, maxc, mod) %>%
    mutate(varname = varname)
}

# count high wind events
# era arg is flag for whether to count from ERA (TRUE) or Observed (FALSE)
# assumes df is sorted by model and time
# count_hwe <- function(df, thr = 30, d = 10, era = TRUE, varname) {
count_hwe_yr <- function(df, thr = 30, d = 10, varname) {
  # new_lvls = if(era) c("CM3/CCSM4", "ERA5") else c("CM3/CCSM4", "Observed")
  new_lvls = c("ERA5", "Observed")
  hws <- df %>% filter(sim_adj >= thr)
  dts <- as.numeric(
    difftime(hws$ts[-1], hws$ts[-(length(hws$ts))], units = "hours")
  )
  hws$hwe <- c(0, cumsum(dts > 2)) + 1
  hws <- bind_rows(
    lapply(unique(hws$hwe), function(hwe) {
      hwe_ts <- hws$ts[hws$hwe == hwe]
      # decade <- hws$decade[hws$hwe == hwe][1]
      year <- hws$year[hws$hwe == hwe][1]
      mod <- hws$mod[hws$hwe == hwe][1]
      dur <- as.numeric(
        difftime(hwe_ts[length(hwe_ts)], hwe_ts[1], units = "hours")
      )
      # data.frame(ts = hwe_ts[1], hwe = hwe, hrs = dur, decade = decade, mod = mod)
      data.frame(ts = hwe_ts[1], hwe = hwe, hrs = dur, year = year, mod = mod)
    })
  )
  
  hws <- hws %>%
    filter(hrs > d) %>%
    # group_by(mod, decade) %>%
    group_by(mod, year) %>%
    summarise(count = n()) %>%
    ungroup
  # historical, either ERA or Observed 
  hs <- hws %>% 
    # filter(mod == {if(era) "ERA5" else "Observed"}) %>%
    filter(mod == "ERA5" | mod == "Observed") %>%
    mutate(mod = factor(mod, levels = new_lvls),
           minc = 0, maxc = 0) %>%
    rename(avc = count) %>%
    #select(decade, avc, minc, maxc, mod)
    select(year, avc, minc, maxc, mod) %>%
    tidyr::complete(year, mod, fill = list(avc = 0, minc = 0, maxc = 0)) %>%
    mutate(varname = varname)
}

# make barplot df
# mk_bar_df <- function(tmin_df, sf_df, ws_df, era = TRUE) {
mk_bar_df_yr <- function(tmin_df_yr, sf_df_yr, ws_df_yr) {
  vn1 <- "Days where $T_{minimum} \\leq -30$ °F"
  vn2 <- "Days where snowfall $\\geq 20$ cm"
  vn3 <- "High wind events (wind speed $\\geq 30$ mph for 10h)"
  vn_levels <- c(vn1, vn2, vn3)
  bind_rows(
    # count_thr(tmin_df, era = era, varname = vn1),
    count_thr_yr(tmin_df_yr, varname = vn1),
    # count_thr(
    #   sf_df, thr = 0.2, leq = FALSE, era = era, varname = vn2
    # ),
    count_thr_yr(
      sf_df_yr, thr = 0.2, leq = FALSE, varname = vn2
    ),
    # count_hwe(ws_df, era = era, varname = vn3)
    count_hwe_yr(ws_df_yr, varname = vn3)
  ) %>%
    mutate(varname = factor(varname, levels = vn_levels))
}

# make barplot
mk_barplot_yr <- function(df) {
  # cp <- if_else(levels(df$mod)[2] == "ERA5", 1, 4)
  # cols <- c("darkolivegreen3", ggsci::pal_jco("default")(4)[cp])
  cols <- c(ggsci::pal_jco("default")(4)[c(1, 4)])
  mk_tex <- function(string) latex2exp::TeX(string)
  
  # ggplot(df, aes(x = decade, y = avc)) + 
  ggplot(df, aes(x = year, y = avc)) + 
    geom_bar(aes(fill = mod), stat = "identity", position = "dodge") + 
    scale_fill_manual(values = cols) +  
    geom_errorbar(
      aes(ymin = minc, ymax = maxc, alpha = if_else(minc == maxc, 0, 1)),
      width = 0.2
    ) +
    # geom_label(
    #   aes(label = avc, group = mod),
    #   vjust = -0.25, alpha = 1,
    #   label.padding = unit(0.1, "lines"), 
    #   label.size = NA
    # ) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .2))) + 
    scale_x_discrete(breaks = as.character(seq(1990, 2020, 5))) + 
    ylab("Count") + xlab("Year") + labs(fill = "Source") +
    theme_classic() + 
    lemon::facet_rep_wrap(~as.factor(varname), nrow = 3, scales = "free_y",
                          repeat.tick.labels = TRUE,
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

tmin_df_yr <- mk_df_yr(era_tmin_adj, obs_lst$tmin)
sf_df_yr <- mk_df_yr(era_sf_adj, obs_lst$sf)
ws_df_yr <- mk_df_yr(era_ws_adj, obs_lst$ws)

# disabled for manuscript
# make plots and save
# p2 <- mk_bar_df_yr(tmin_df_yr, sf_df_yr, ws_df_yr) %>%
#   mk_barplot_yr
# fn2 <- "figs/manuscript/extr_events_by_year.png"
# ggsave(fn2, p2, width = 6, height = 8)

#------------------------------------------------------------------------------
