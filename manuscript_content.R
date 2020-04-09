# make figures for use in manuscript
# (Climactic Change figure size limits: 174mm x 234mm)

# Figure 1 - map of Alaska with Nome and seas labelled
# Figure 2 - impact frequencies by industry
# Figure 3 - impact frequencies by month
# Figure 4 - impact frequencies by weather types
# Figure 5 - impact frequencies by year
# Figure 6 - extreme event decadal frequencies for historical (ERA5/Observed)
#   future (GCMs) 

#-- Setup ---------------------------------------------------------------------
# get observed daily data, returns list of dfs
get_nome_daily <- function(fn) {
  # inch:meter
  im <- 0.0254
  df <- read.csv(fn) %>%
    mutate(date = ymd(DATE),
           SNOW = SNOW * im,
           SNWD = SNWD * im) %>%
    filter(date > "1990-01-01") %>%
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
mk_bar_df <- function(tmin_df, sf_df, ws_df) {
  vn1 <- "Days where $T_{minimum} \\leq -30$ °F"
  vn2 <- "Days where snowfall $\\geq 20$ cm"
  vn3 <- "High wind events (wind speed $\\geq 30$ mph for 10h)"
  vn_levels <- c(vn1, vn2, vn3)
  bind_rows(
    # count_thr(tmin_df, era = era, varname = vn1),
    count_thr(tmin_df, varname = vn1),
    # count_thr(
    #   sf_df, thr = 0.2, leq = FALSE, era = era, varname = vn2
    # ),
    count_thr(
      sf_df, thr = 0.2, leq = FALSE, varname = vn2
    ),
    # count_hwe(ws_df, era = era, varname = vn3)
    count_hwe(ws_df, varname = vn3)
  ) %>%
    mutate(varname = factor(varname, levels = vn_levels))
}

# make barplot
mk_barplot <- function(df, nrows = 3) {
  # cp <- if_else(levels(df$mod)[2] == "ERA5", 1, 4)
  # cols <- c("darkolivegreen3", ggsci::pal_jco("default")(4)[cp])
  cols <- c("darkolivegreen3", ggsci::pal_jco("default")(4)[c(1, 4)])
  mk_tex <- function(string) latex2exp::TeX(string)
  
  ggplot(df, aes(x = decade, y = avc)) + 
  #ggplot(df, aes(x = year, y = avc)) + 
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
    scale_x_discrete(breaks = as.character(seq(1980, 2100, 10))) + 
    ylab("Count") + xlab("Decade") + labs(fill = "Source") +
    theme_classic() + 
    facet_wrap(~as.factor(varname), nrow = nrows, scales = "free_y",
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



#-- Figure 1 ------------------------------------------------------------------
# figure 1 - map of Alaska with Nome labelled
library(tyidyverse)
library(ggmap)
library(gridExtra)

map <- get_googlemap(c(lon = -150, lat = 64.5011), zoom = 3, maptype = "satellite")

p <- ggmap(map) + 
  scale_x_continuous(
    limits = c(-190, -110),
    breaks = c(-175, -150, -125),
    labels = c("175°W", "150°W", "125°W")
  ) +
  scale_y_continuous(
    limits = c(50, 75), 
    labels = paste0(as.character(seq(50, 75, 5)), "°N")
  ) +
  geom_point(aes(x = -165.4064, y = 64.5011,  colour = "darkred"), size = 2) +
  geom_text(
    aes(-165.4064, y = 64.5011, label = "Nome"), 
    color = "white", size = 5, nudge_x = 3.5, nudge_y = 0.75, family = "serif"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(family = "serif", size = 8),
    panel.border = element_rect(colour = "grey", fill=NA, size=2),
    legend.position = "none"
  )

ggsave(
  "figs/manuscript/Fig1.png", 
  p, width = 174, height = 174 * (800/1080), units = mm
)

# code attempting to use better projection for AK
# library(raster)
# map <- get_googlemap(c(lon = -150, lat = 64.5011), zoom = 3, maptype = "satellite")
# # convert map to raster (is already "raster" object, not sure how to extract
# #   only this)
# # make a matrix of map
# bb <- unlist(attr(map, "bb"))
# y <- bb[c(1, 3)]
# x <- bb[c(2, 4)]
# g_crs <- CRS('+init=epsg:3857')
# sp_bb <- SpatialPoints(expand.grid(x, y), CRS("+init=epsg:4326"))
# sp_bb <- spTransform(sp_bb, g_crs)
# ext <- extent(sp_bb)
# mat <- as.matrix(map)
# rgb_mat <- col2rgb(mat)
# rgb_stack <- stack(
#   raster(
#     matrix(rgb_mat[1, ], nrow = 1280), crs = g_crs, 
#     xmn = ext[1], xmx = ext[2], ymn = ext[3], ymx = ext[4]
#   ),
#   raster(
#     matrix(rgb_mat[2, ], nrow = 1280), crs = g_crs, 
#     xmn = ext[1], xmx = ext[2], ymn = ext[3], ymx = ext[4]
#   ),
#   raster(
#     matrix(rgb_mat[3, ], nrow = 1280), crs = g_crs, 
#     xmn = ext[1], xmx = ext[2], ymn = ext[3], ymx = ext[4]
#   )
# )
# rgb_proj <- projectRaster(rgb_stack, crs = "+init=epsg:3413")

#------------------------------------------------------------------------------

#-- Figure 2 ------------------------------------------------------------------
# Impact frequencies by industry
# read and prep frequency by type data
# Note: these data are modified to make desired plot achieved more easily
#   within R
extrafont::font_import()

fig2_theme <- theme(
  axis.ticks = element_blank(),
  axis.text = element_text(color = "black", family = "serif", size = 11),
  axis.text.x = element_text(
    angle = 40,
    margin = margin(t = 1, b = 5),
    hjust = 0.9
  ),
  axis.title.x = element_blank(),
  axis.title.y = element_text(family = "serif", size = 14),
  plot.title = element_text(hjust = -0.1, family = "serif", size = 16),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(family = "serif", size = 12),
  legend.key.size = unit(0.7,"line"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  panel.spacing = unit(2, "lines"),
  legend.margin = margin(0, -2, 0, -2)
)

mk_fig2 <- function(fp) {
  freq_type <- read_csv(fp, col_types = "fffd", trim_ws = FALSE)
  # set ymin for annotations
  ann_ymin <- -30
  p <- ggplot(freq_type, aes(x = subtype, y = count)) + 
    geom_bar(
      aes(fill = source), color = "black", size = 0.4, width = 0.4,
      stat = "identity"
    ) + 
    scale_fill_manual(values = c("black", "white")) +
    scale_y_continuous(
      breaks = seq(0, 90, 15), 
      limits = c(0, 95),
      expand = c(0, 0)
    ) +
    ylab("Frequency") + 
    theme_bw() +
    fig2_theme +
    # add custom annotations for 
    annotation_custom(
      textGrob("Activities", gp = gpar(fontsize = 13, fontfamily = "serif")),
      xmin = 2.5, xmax = 2.5, ymin = ann_ymin, ymax = ann_ymin
    ) +
    annotation_custom(
      textGrob("Transportation", gp = gpar(fontsize = 13, fontfamily = "serif")),
      xmin = 9, xmax = 9, ymin = ann_ymin, ymax = ann_ymin
    ) +
    annotation_custom(
      textGrob("Utilities", gp = gpar(fontsize = 13, fontfamily = "serif")),
      xmin = 15, xmax = 15, ymin = ann_ymin, ymax = ann_ymin
    ) +
    annotation_custom(
      textGrob("Other", gp = gpar(fontsize = 13, fontfamily = "serif")),
      xmin = 20.5, xmax = 20.5, ymin = ann_ymin, ymax = ann_ymin
    ) +
    coord_cartesian(ylim = c(0, 90), clip = "off")
}

p2 <- mk_fig2("data/impacts_by_type.csv")
ggsave(
  "figs/manuscript/Fig2.eps", device = "eps",
  width = 174, height = 174 * (500 / 800), units = "mm"
)

fnt <- extrafont::fonts();i <- grep("serif", fnt, ignore.case = TRUE);fnt[i]

#------------------------------------------------------------------------------

#-- Figure 3 ------------------------------------------------------------------
# Impact frequencies by month

base_theme2 <- theme(
  axis.ticks = element_blank(),
  axis.text = element_text(color = "black", family = "serif", size = 12),
  axis.text.x = element_text(margin = margin(t = 5, b = -3)),
  axis.title.x = element_blank(),
  axis.title.y = element_text(family = "serif", size = 14),
  plot.title = element_text(hjust = -0.1, family = "serif", size = 16),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(family = "serif", size = 11),
  panel.grid.minor.y = element_blank(),
  panel.border = element_blank(),
  legend.margin = margin(-2, -2, -2, -2)
)
freq_mo <- read_csv("data/impacts_by_month.csv", col_types = "ffd")

#------------------------------------------------------------------------------

#-- Figure 4 ------------------------------------------------------------------
# Impact frequencies by weather type
mk_fig4 <- function(fp) {
  # read and prep weather data for barplot
  freq_wthr <- read_csv(fp, col_types = "cfd", trim_ws = FALSE)
  # being lazy and just hardcoding this one, watch out for data changes
  freq_wthr <- bind_rows(
    freq_wthr[1:8, ],
    data.frame(
      weather_type = c("", " "),
      source = factor(rep("Nome Nugget", 2), levels = levels(freq_wthr$source)),
      count = rep(NA, 2),
      stringsAsFactors = FALSE
    ),
    freq_wthr[9:12, ]
  ) %>%
  mutate(weather_type = factor(weather_type, weather_type))
  # make plot
  p <- ggplot(freq_wthr, aes(x = weather_type, y = count)) + 
    geom_bar(
      aes(fill = source), color = "black", size = 0.4, width = 0.4,
      stat = "identity"
    ) + 
    scale_fill_manual(values = c("black", "white")) +
    scale_y_continuous(
      breaks = seq(0, 50, 12.5), 
      labels = c("0", "12.5", "25", "37.5", "50"),
      limits = c(0, 50),
      expand = c(0, 0)
    ) +
    ylab("Frequency") + 
    ggtitle("B") + 
    theme_bw() +
    fig3_theme +
    theme(
      axis.text.x = element_text(margin = margin(b = -3)),
      legend.key.size = unit(0.7,"line"),
      panel.grid.major.x = element_blank()
    )
  return(p)
}

p4 <- mk_fig4("data/impacts_by_weather.csv")
ggsave("figs/manuscript/Fig5.eps", p4)

#------------------------------------------------------------------------------

#-- Figure 5 ------------------------------------------------------------------
# Impact frequencies by year
mk_fig5 <- function(fp) {
  freq_yr <- read_csv(fp, col_types = "ffd")
  p <- ggplot(freq_yr, aes(x = year, y = count, group = source)) + 
    geom_line() +
    geom_point(aes(shape = source, fill = source), size = 3) + 
    geom_smooth(
      data = subset(freq_yr, source == "Nome Nugget"),
      aes(year, count),
      method = lm, se = FALSE, color = "black", lty = 3, lwd = 0.5
    ) + 
    scale_shape_manual(values = c(21, 22)) +
    scale_fill_manual(values = fill_colors) + 
    scale_x_continuous(
      breaks = seq(1990, 2018, 4),
      minor_breaks = seq(1990, 2018),
      limits = c(1990, 2018),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, 30, 7.5), 
      labels = c("0", "7.5", "15", "22.5", "30"),
      limits = c(0, 30),
      expand = c(0, 0)
    ) +
    ylab("Frequency") + 
    ggtitle("A") +
    theme_bw() +
    fig2_theme + 
    theme(
      plot.margin = margin(3, 15, 0, 2),
      legend.spacing.x = unit(1, "pt")
    )
}

p5 <- mk_fig5("data/impacts_by_year.csv")
ggsave("figs/manuscript/Fig5.eps", p5)

#------------------------------------------------------------------------------


mk_fig2 <- function(freq_yr, freq_mo, color = FALSE) {
  fig2_theme <- theme(
    axis.ticks = element_blank(),
    axis.text = element_text(color = "black", family = "serif", size = 12),
    axis.text.x = element_text(margin = margin(t = 5, b = -3)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "serif", size = 14),
    plot.title = element_text(hjust = -0.1, family = "serif", size = 16),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = "serif", size = 11),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    legend.margin = margin(-2, -2, -2, -2)
  )
  
  fill_colors <- if(color) c("#1C45A6", "#EB6F64") else c("black", "white")
  bar_colors <- if(color) c("#399cbd", "#bd5a39") else c("black", "black")

 
  freq_mo[freq_mo == 0] <- NA
  p2 <- ggplot(freq_mo, aes(x = month, y = count, group = source)) + 
    geom_bar(
      aes(color = source, fill = source), size = 0.4, width = 0.4,
      stat = "identity", 
      position = position_dodge(0.5)
    ) + 
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = bar_colors) +
    scale_y_continuous(
      breaks = seq(0, 50, 12.5), 
      labels = c("0", "12.5", "25", "37.5", "50"),
      limits = c(0, 50),
      expand = c(0, 0)
    ) +
    ylab("Frequency") + 
    ggtitle("B") + 
    theme_bw() +
    fig2_theme +
    theme(
      axis.text.x = element_text(margin = margin(t = 1, b = -3)),
      legend.key.size = unit(0.7,"line"),
      panel.grid.major.x = element_blank(),
    )
  
  # done to avoid clipping points
  gt <- ggplot_gtable(ggplot_build(p1))
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  p <- arrangeGrob(gt, p2, nrow = 2)
  return(p)
}

freq_yr <- read_csv("data/impacts_by_year.csv", col_types = "dfd")
freq_mo <- read_csv("data/impacts_by_month.csv", col_types = "ffd")

p2 <- mk_fig2(freq_yr, freq_mo)
p2c <- mk_fig2(freq_yr, freq_mo, TRUE)

fig2_fp <- "figs/manuscript/figure_2.png"
fig2c_fp <- "figs/manuscript/figure_2_color.png"
ggsave(fig2_fp, p2, width = 6, height = 7.5)
ggsave(fig2c_fp, p2c, width = 6, height = 7.5)

#------------------------------------------------------------------------------

#-- Figure 3 ------------------------------------------------------------------
# Impact frequencies by intudstry and weather types




# make figure 3
mk_fig3 <- function(freq_type, freq_wthr, color = FALSE) {
  fig3_theme <- theme(
    axis.ticks = element_blank(),
    axis.text = element_text(color = "black", family = "serif", size = 11),
    axis.text.x = element_text(
      angle = 40,
      margin = margin(t = 1, b = 5),
      hjust = 0.9
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "serif", size = 14),
    plot.title = element_text(hjust = -0.1, family = "serif", size = 16),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = "serif", size = 12),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    legend.margin = margin(0, -2, 0, -2)
  )
  
  fill_colors <- if(color) c("#1C45A6", "#EB6F64") else c("black", "white")
  bar_colors <- if(color) c("#399cbd", "#bd5a39") else c("black", "black")
  
  
  gt <- ggplot_gtable(ggplot_build(p1))
  leg_idx <- which(sapply(gt$grobs, function(x) x$name) == "guide-box")
  legend <- gt$grobs[[leg_idx]]
  
  p <- arrangeGrob(
    arrangeGrob(
      p1 + theme(legend.position = "none"),
      p2 + theme(legend.position = "none"), 
      nrow = 2, heights = c(5, 5)
    ), legend, nrow = 2, heights = c(40, 2)
  )
  
  return(p)
}

library(grid)
library(gridExtra)

freq_type_fp <- "data/impacts_by_type.csv"
freq_wthr_fp <- "data/impacts_by_weather.csv"

freq_type <- read_freq_type(freq_type_fp)
freq_wthr <- read_freq_wthr(freq_wthr_fp)

p3 <- mk_fig3(freq_type, freq_wthr)
p3c <- mk_fig3(freq_type, freq_wthr, TRUE)

fig3_fp <- "figs/manuscript/figure_3.png"
fig3c_fp <- "figs/manuscript/figure_3_color.png"
ggsave(fig3_fp, p3, width = 6, height = 7.5)
ggsave(fig3c_fp, p3c, width = 6, height = 7.5)

#------------------------------------------------------------------------------

#-- Figure 4 ------------------------------------------------------------------
# Extreme event decadal frequencies for historical (ERA5/Observed) and
#   future (GCMs)


gcm_tmin <- readRDS("data/gcm_t2min_adj.Rds")
gcm_sf <- readRDS("data/gcm_sf_adj.Rds")
gcm_ws <- readRDS("data/gcm_ws_adj.Rds") 

# ignore data from prior to 1990
era_tmin_adj <- readRDS("data/era5_tmin_adj.Rds") %>%
  filter(date >= "1990-01-01") %>%
  rename(sim_adj = tmin_adj)
era_sf_adj <- readRDS("data/era5_sf_adj.Rds") %>%
  filter(date >= "1990-01-01") %>%
  rename(sim_adj = sf_adj)
era_ws_adj <- readRDS("data/era5_ws_adj.Rds") %>%
  filter(ts >= ymd("1990-01-01")) %>%
  rename(sim_adj = ws_adj)

# daily observations
fn <- "../data-raw/GHCND/Nome_snow_tmin_19800101-20191231.csv"
obs_lst <- get_nome_daily(fn)

# hourly observations (wind)
fn <- "../data-raw/IEM/ASOS/PAOM_wind_19800101-20200101.txt"
obs_lst$ws <- get_nome_ws(fn)

tmin_df <- mk_df(gcm_tmin[[2]], gcm_tmin[[4]], era_tmin_adj, obs_lst$tmin)
sf_df <- mk_df(gcm_sf[[2]], gcm_sf[[4]], era_sf_adj, obs_lst$sf)
ws_df <- mk_df(gcm_ws[[2]], gcm_ws[[4]], era_ws_adj, obs_lst$ws)

# make plots and save
# bar_df <- mk_bar_df(tmin_df, sf_df, ws_df)
# p1 <- mk_barplot(bar_df)
# #fn1 <- "figs/manuscript/figure_1_ERA5.png"
# fn1 <- "figs/manuscript/figure_1.png"
# ggsave(fn1, p1, width = 6, height = 8)

# make alternative figure 1, side by side
bar_df <- mk_bar_df(tmin_df, sf_df, ws_df)
p1_1 <- mk_barplot(bar_df, nrows = 1)
fn1_1 <- "figs/manuscript/figure_3.png"
ggsave(fn1_1, p1_1, width = 13.92, height = 4)

#------------------------------------------------------------------------------

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
  # hws %>%
  #   filter(mod == "CM3" | mod == "CCSM4") %>%
  #   # group_by(decade) %>%
  #   group_by(year) %>%
  #   summarise(
  #     avc = mean(count), 
  #     minc = min(count), 
  #     maxc = max(count)
  #   ) %>%
  #   mutate(mod = factor("CM3/CCSM4", levels = new_lvls)) %>%
  #   bind_rows(hs) %>%
  #   mutate(varname = varname) %>%
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

# make plots and save
p2 <- mk_bar_df_yr(tmin_df_yr, sf_df_yr, ws_df_yr) %>%
  mk_barplot_yr
#fn1 <- "figs/manuscript/figure_1_ERA5.png"
fn2 <- "figs/manuscript/extr_events_by_year.png"
ggsave(fn2, p2, width = 6, height = 8)
