# make figures for use in manuscript
# (Climactic Change figure size limits: 174mm x 234mm)

# Figure 1 - map of Alaska with Nome and seas labelled
# Figure 2 - impact frequencies by industry
# Figure 3 - impact frequencies by month
# Figure 4 - impact frequencies by weather types
# Figure 5 - impact frequencies by year
# Figure 6 - extreme event decadal frequencies for historical (ERA5/Observed)
#   future (GCMs) 

#-- Figure 1 ------------------------------------------------------------------
# figure 1 - map of Alaska with Nome labelled
make_fig1 <- function(milepost_fp, roads_fp) {
  # make df from roadsystem shapefile
  # relies on roads_sp, roads_names, status_levels, bb from parent env
  make_rs_df <- function(name, status) {
    idx <- which(
      str_detect(roads_names, regex(name, ignore_case = TRUE))
    )
    subset(roads_sp, Route_Name == roads_names[idx]) %>%
      spTransform(CRS("+init=epsg:4326")) %>%
      fortify %>%
      filter(lat > bb[2] ) %>%
      mutate(status = factor(status, status_levels))
  }
  
  # create dataframe of seasonality by milepost
  # relies on status_levels, milepost_sp, mp_names from parent env
  make_mp_df <- function(name, mp) {
    # convert to SpatialLinesDataFrame seems needed for fortify
    make_df <- function(df, id) {
      df <- df[order(df$Milepost_N),]
      coordinates(df) %>% 
        as.data.frame %>% 
        select(coords.x1, coords.x2) %>% 
        Line %>% 
        list %>% 
        Lines(id) %>%
        fortify
    }
    idx <- which(str_detect(mp_names, regex(name, ignore_case = TRUE)))
    sp_df <- subset(milepost_sp, ROUTE_NAME == mp_names[idx][1]) %>%
      spTransform(CRS("+init=epsg:4326"))
    # subset year-round and seasonal partitions, fortify, and combine
    yrnd <- subset(sp_df, Milepost_N <= mp) %>%
      make_df(paste0(name, "-yr")) %>%
      mutate(status = factor("roads", levels = status_levels))
    subset(sp_df, Milepost_N > mp) %>%
      make_df(paste0(name, "-seas")) %>%
      mutate(
        status = factor("roads (summer only)", levels = status_levels)
      ) %>%
      rbind(yrnd)
  }
  
  # get tiles
  # seward peninsula
  bb <- c(-168.9, 64.1, -162.05, 66.65)
  map <- get_stamenmap(
    bbox = bb, zoom = 8,
    maptype = "terrain-background", color = "bw", messaging = FALSE
  )
  # AK
  ak_map <- get_stamenmap(
    bbox = c(left = -173.5, bottom = 56, right = -135, top = 71.5), zoom = 5,
    maptype = "terrain-background", color = "bw", messaging = FALSE
  )
  
  # change gray ocean to white
  attrs <- attr(map, "bb")    # save attributes from original
  ak_attrs <- attr(ak_map, "bb")
  map[map == "#AEAEAE"] <- "#FFFFFF"
  ak_map[ak_map == "#AEAEAE"] <- "#FFFFFF"
  # correct class, attributes
  class(map) <- c("ggmap", "raster")
  class(ak_map) <- c("ggmap", "raster")
  attr(map, "bb") <- attrs
  attr(ak_map, "bb") <- ak_attrs
  
  # for town labels
  # point_names <- c("Nome", "Teller", "Council", "Solomon")
  # lon <- c(-165.4064, -166.3608, -163.6774, -164.4392)
  # lat <- c(64.5011, 65.2636, 64.8953, 64.5608)
  # names(lon) <- point_names
  # names(lat) <- point_names
  
  village_points <- data.frame(
    x = c(-165.4064, -166.3608, -163.6774, -164.4392),
    y = c(64.5011, 65.2636, 64.8953, 64.5608),
    name = c("Nome", "Teller", "Council", "Solomon")
  )
  
  road_points <- data.frame(
    x = c(-163.5, -166.9, -165.2),
    y = c(64.7, 65.1, 65.3),
    name = c("Nome-Council Rd", "Nome-Teller Hwy", "Kougarok Rd")
  )
  
  # DOT road system shapefile from http://dot.alaska.gov/stwdplng/mapping/transdata/DOT_RoadSystem_091318.zip
  roads_sp <- readOGR(roads_fp)
  # roads that are EITHER seasonal OR year-round
  roads_names <- as.character(roads_sp$Route_Name)
  # remove OLD GLACIER CREEK RD (regex laziness)
  roads_names <- roads_names[roads_names != "OLD GLACIER CREEK ROAD"]
  status_levels <- c("roads (summer only)", "roads")

  # Roads with partial seasonality
  # roads available in mileposts shapefile: 
  #   Kougarok, Nome-Teller, Nome-Council
  milepost_sp <- readOGR(milepost_fp)
  mp_names <- as.character(milepost_sp$ROUTE_NAME)
  
  # make df's from road system and milepost shapefiles and combine
  # code to make DFs for seasonal/nonseasonal roads
  # roads <- rbind(
  #   make_rs_df("glacier creek road", "roads"),
  #   # subsample for displaying dotted line
  #   make_rs_df("dexter bypass", "roads (summer only)") %>%
  #     filter(order %% 5 == 0),
  #   make_mp_df("kougarok", 13),
  #   make_mp_df("nome-teller", 8),
  #   make_mp_df("council", 3)
  # )
  
  # classify all roads as "road"
  roads <- rbind(
    make_rs_df("glacier creek road", "roads"),
    make_rs_df("dexter bypass", "roads"),
    make_rs_df("kougarok", "roads"),
    make_rs_df("nome-teller", "roads"),
    make_rs_df("council road", "roads")
  )
  
  # Keep labels: Bering Sea, Chukchi Sea, Beaufort Sea
  # labs_df <- data.frame(
  #   lon = c(-172, -171, -140),
  #   lat = c(57, 71, 72),
  #   text = c("Bering Sea", "Chukchi Sea", "Beaufort Sea")
  # )
  # 
  p_ak <- ggmap(ak_map) + 
    geom_path(
      aes(x = x, y = y),
      data.frame(x = bb[c(1, 1, 3, 3, 1)], y = bb[c(2, 4, 4, 2, 2)])
    ) + 
    theme(
      axis.title = element_blank(), 
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(1,1,0.3,0.3), "mm"),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )
  
  xbreaks <- seq(-168, -162, 2)
  ybreaks <- seq(64, 66, 1)
  p <- ggmap(map) + 
    geom_path(
      aes(x = long, y = lat, group = id, color = status, linetype = status),
      data = roads, size = .5
    ) +
    # geom_path(
    #   aes(x = long, y = lat, group = id, color = status, linetype = status),
    #   data = roads, size = .5, color = "gray20"
    # ) +
    scale_linetype_manual(
      values = c("roads" = "solid", "roads (summer only)" = "dashed")
    ) +
    scale_color_manual(
      values = c("roads" = "black", "roads (summer only)" = "gray20")
    ) +
    inset(
      ggplotGrob(p_ak), xmin = bb[1] + 0.05, xmax = -166.8, ymin = bb[2] + 0.04, ymax = 64.95
    ) +
    scale_x_continuous(
      breaks = xbreaks,
      labels = paste0(as.character(abs(xbreaks)), "°W"), 
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = ybreaks,
      labels = paste0(as.character(ybreaks), "°N"),
      expand = c(0, 0)
    ) +
    geom_point(
      aes(x = x, y = y),
      data = village_points
    ) +
    geom_text(
      aes(x = x, y = y, label = name),
      village_points,
      color = "black", 
      nudge_x = c(-0.2, -0.25, -0.2, 0.25), 
      nudge_y = c(-0.05, 0.02, 0.06, -0.06), 
      family = "serif"
    ) + 
    geom_text(
      aes(x = x, y = y, label = name),
      road_points,
      color = "black",
      nudge_x = c(-0.1, 0.15, -0.05), 
      nudge_y = c(-0.05, -0.06, 0), 
      family = "serif", size = 3,
    ) + 
    geom_text(
      aes(x = -168.5, y = 65.7, label = "Bering\nStrait"),
      family = "serif", fontface = "italic",
      color = "gray40", size = 4
    ) + 
    # geom_text(
    #   aes(-165.4064, 64.5011, label = "Nome"), 
    #   color = "black", size = 4, nudge_x = -0.2, nudge_y = -0.05, family = "serif"
      # alpha = 0.5, label.padding = unit(0.15, "lines"), 
      # label.size = 0, label.r = unit(0.2, "lines")
    # ) +
    # geom_text(
    #   data = labs_df,
    #   aes(label = text), 
    #   family = "serif", fontface = "italic",
    #   color = "gray25", size = 4
    # ) +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(family = "serif", size = 8),
      panel.border = element_rect(colour = "black", fill=NA, size=2),
      legend.position = c(0.76, 0.04),
      legend.title = element_blank(),
      legend.background = element_rect(color = "black", fill = "white"),
      legend.key = element_blank(),
      legend.margin = margin(-5, 5, 0, 5) 
    ) 
}

suppressMessages({
  library(ggmap)
  library(rgdal)
  library(dplyr)
  library(stringr)
  library(grid)
  #library(maptools)
  #library(sp)
  
})

# attribution: Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
#mileposts fp
data_dir <- Sys.getenv("DATA_DIR")
milepost_fp <- file.path(data_dir, "shapefiles", "Mileposts_091318.shp")
roads_fp <- file.path(data_dir, "shapefiles", "DOT_RoadSystem_091318.shp")

p1 <- make_fig1(milepost_fp, roads_fp)
out_dir <- file.path(Sys.getenv("FILES_DIR"), "manuscript-figures")
dir.create(out_dir, showWarnings = FALSE)
ggsave(
  file.path(out_dir, "Fig1.tiff"),
  p, width = 174, height = 174 * 0.9, units = "mm"
)

#------------------------------------------------------------------------------

#-- Figure 2 ------------------------------------------------------------------
# Impact frequencies by industry
# read and prep frequency by type data
# Note: these data are modified to make desired plot achieved more easily
#   within R
make_fig2 <- function(fp) {
  freq_type <- read_csv(fp, col_types = "fffd", trim_ws = FALSE)
  # set ymin for annotations
  ann_ymin <- -75
  p <- ggplot(freq_type, aes(x = subtype, y = count, fill = source)) + 
    geom_col(
      color = "black", width = 1,
      position = position_dodge2(preserve = "single", padding = 0.2)
    ) +
    # geom_bar(
    #   aes(fill = source), color = "black", size = 0.4, width = 0.4,
    #   stat = "identity", position = "dodge"
    # ) + 
    scale_fill_manual(values = c("black", "white")) +
    scale_y_continuous(
      breaks = seq(0, 90, 15), 
      limits = c(0, 95),
      expand = c(0, 0)
    ) +
    #scale_x_discrete(labels = function(x) str_wrap(x, width = 100)) + 
    ylab("Count") + 
    theme_bw() +
    fig2_theme +
    # add custom annotations for "Public and Municipal Closures"
    annotation_custom(
      # using text_grob here bc found solution to adjust line height
      ggpubr::text_grob("Public and Municipal\nClosures", 
        size = 10, rot = 60, lineheight = 0.7, hjust = 0.2
      ),
      xmax = 10.5, ymax = -95
    ) +
    annotation_custom(
      # using text_grob here bc found solution to adjust line height
      ggpubr::text_grob("Fish and wildlife counting", 
                        size = 10, rot = 60, lineheight = 0.7, hjust = 0.2
      ),
      xmax = 17.25, ymax = -109
    ) +
    annotation_custom(
      # using text_grob here bc found solution to adjust line height
      ggpubr::text_grob("Commercial fish\nand fish processing", 
                        size = 10, rot = 60, lineheight = 0.7, hjust = 0.2
      ),
      xmax = 20.7, ymax = -80
    ) +
    # annotation_custom(
    #   # using text_grob here bc found solution to adjust line height
    #   ggpubr::text_grob("Damaged\nbuildings", 
    #                     size = 10, rot = 40, lineheight = 0.7, hjust = 0.2
    #   ),
    #   xmax = 36, ymax = -40
    # ) +
    # add custom annotations for types
    annotation_custom(
      textGrob("Transportation", gp = gpar(fontsize = 12)),
      xmin = 2.75, xmax = 2.75, ymin = ann_ymin, ymax = ann_ymin
    ) +
    annotation_custom(
      textGrob("Activities", gp = gpar(fontsize = 12)),
      xmin = 9.5, xmax = 9.5, ymin = ann_ymin, ymax = ann_ymin
    ) +
    annotation_custom(
      textGrob("Utilities", gp = gpar(fontsize = 12)),
      xmin = 15.75, xmax = 15.75, ymin = ann_ymin, ymax = ann_ymin
    ) +
    annotation_custom(
      ggpubr::text_grob("Damaged\nbuildings", size = 12, lineheight = 0.7),
      xmin = 18.8, xmax = 18.8, ymin = ann_ymin, ymax = ann_ymin
    ) +
    annotation_custom(
      textGrob("Other", gp = gpar(fontsize = 12)),
      xmin = 21.2, xmax = 21.2, ymin = ann_ymin, ymax = ann_ymin
    ) +
    coord_cartesian(ylim = c(0, 90), clip = "off")
}

# save figure as .eps 
saveEPS <- function(fp, p, width = 6.85, hr = 0.625) {
  postscript(
    fp, 
    width = width, height = width * hr,
    family = "Times New Roman", 
    # these args used to make true EPS output
    paper = "special", onefile = FALSE, horizontal = FALSE
  )
  suppressWarnings(print(p))
  dev.off()
  # embed fonts in saved figure
  # need to have ghostscript installed
  embed_fonts(fp, options = "-dEPSCrop")
}

suppressMessages({
  library(tidyverse)
  library(grid)
  library(gridExtra)
  library(extrafont)
})

# theme for figure 2
# serves as base theme for figs 3-5
fig2_theme <- theme(
  axis.ticks = element_blank(),
  axis.text = element_text(color = "black", size = 11),
  axis.text.x = element_text(
    size = 10,
    angle = 60,
    margin = margin(t = 1, b = 50),
    hjust = 0.99
  ),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 13, margin = margin(r = 7)),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  legend.key.size = unit(0.7,"line"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  panel.spacing = unit(2, "lines"),
  legend.margin = margin(0, -2, 0, -2)
)

{
data_dir <- Sys.getenv("DATA_DIR")
p2 <- make_fig2(file.path(data_dir, "impacts_by_type.csv"))
# load fonts once... per machine?
loadfonts(device = "postscript", quiet = TRUE)

saveEPS(file.path(out_dir, "Fig2.eps"), p2, hr = 0.55)
}

#------------------------------------------------------------------------------

#-- Figure 3 ------------------------------------------------------------------
# Impact frequencies by month
make_fig3 <- function(fp) {
  freq_mo <- read_csv(fp, col_types = "ffd")
  freq_mo[freq_mo == 0] <- NA
  p <- ggplot(freq_mo, aes(x = month, y = count, group = source)) + 
    geom_bar(
      aes(fill = source), color = "black", size = 0.4, width = 0.4,
      stat = "identity", 
      position = position_dodge(0.5)
    ) + 
    scale_fill_manual(values = c("black", "white")) +
    scale_y_continuous(
      breaks = seq(0, 50, 10), 
      limits = c(0, 50),
      expand = c(0, 0)
    ) +
    ylab("Count") + 
    theme_bw() +
    fig2_theme +
    theme(
      axis.text.x = element_text(
        angle = 0, hjust = 0.5,
        margin = margin(t = 1, b = -3)
      ),
      legend.key.size = unit(0.7,"line")
    )
}

p3 <- make_fig3(file.path(data_dir, "impacts_by_month.csv"))
saveEPS(file.path(out_dir, "Fig3.eps"), p3, hr = 0.5)

#------------------------------------------------------------------------------

#-- Figure 4 ------------------------------------------------------------------
# Impact frequencies by weather type
# Note: these data are modified to make desired plot achieved more easily
#   within R
make_fig4 <- function(fp) {
  freq_wthr <- read_csv(fp, col_types = "ffd", trim_ws = FALSE)
  freq_wthr$source <- factor(freq_wthr$source, levels = c("Nome Nugget", "Storm Data"))
  p <- ggplot(freq_wthr, aes(x = weather_type, y = count, fill = source)) + 
    geom_col(
      color = "black", width = 0.6,
      position = position_dodge2(preserve = "single", padding = 0.2)
    ) +
    # geom_bar(
    #   aes(fill = source), color = "black", size = 0.4, width = 0.4,
    #   stat = "identity"
    # ) + 
    scale_fill_manual(values = c("black", "white")) +
    scale_y_continuous(
      breaks = seq(0, 50, 10), 
      limits = c(0, 50),
      expand = c(0, 0)
    ) +
    ylab("Count") + 
    theme_bw() +
    fig2_theme +
    theme(
      axis.text.x = element_text(margin = margin(t = 4, b = -5)),
      axis.text.y = element_text(margin = margin(r = 3)),
      legend.key.size = unit(0.7,"line"),
      legend.box.margin = margin(t = -2)
    )
  return(p)
}

p4 <- make_fig4(file.path(data_dir, "impacts_by_weather.csv"))
saveEPS(file.path(out_dir, "Fig4.eps"), p4, width = 5, hr = 0.6)

#------------------------------------------------------------------------------

#-- Figure 5 ------------------------------------------------------------------
# Impact frequencies by year
make_fig5 <- function(fp) {
  freq_yr <- read_csv(fp, col_types = "dfd")
  p <- ggplot(freq_yr, aes(x = year, y = count, group = source)) + 
    geom_line() +
    geom_point(aes(shape = source, fill = source), size = 3) + 
    geom_smooth(
      data = subset(freq_yr, source == "Nome Nugget"),
      aes(year, count),
      formula = "y ~ x",
      method = lm, se = FALSE, color = "black", lty = 3, lwd = 0.5
    ) + 
    scale_shape_manual(values = c(21, 22)) +
    scale_fill_manual(values = c("black", "white")) + 
    scale_x_continuous(
      breaks = seq(1990, 2018, 4),
      minor_breaks = seq(1990, 2018),
      limits = c(1990, 2018),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, 30, 10), 
      limits = c(0, 30),
      expand = c(0, 0)
    ) +
    ylab("Count") + 
    theme_bw() +
    fig2_theme + 
    theme(
      axis.text.x = element_text(
        angle = 0, margin = margin(t = 6), hjust = 0.5),
      panel.grid.major.x = element_line(color = "gray92"),
      plot.margin = margin(5, 15, 4, 2),
      legend.text = element_text(margin = margin(r = 10, unit = "pt")),
      legend.spacing.x = unit(1, "pt"),
      legend.margin = margin(t = 0),
    ) + 
    coord_cartesian(ylim = c(0, 30), clip = "off")
  return(p)
}

p5 <- make_fig5(file.path(data_dir, "impacts_by_year.csv"))
saveEPS(file.path(out_dir, "Fig5.eps"), p5, hr = 0.5)

#------------------------------------------------------------------------------

#-- Figure 6 ------------------------------------------------------------------
suppressMessages(library(lubridate))
# Extreme event decadal frequencies for historical (ERA5/Observed) and
#   future (GCMs)
make_fig6 <- function(fp) {
  df <- readRDS(fp)
  cols <- c("darkolivegreen3", ggsci::pal_jco("default")(4)[c(1, 4)])
  cols <- c("white", "gray85", "gray40")
  make_tex <- function(string) latex2exp::TeX(string)
  
  p <- ggplot(df, aes(x = decade, y = avc)) + 
    geom_bar(aes(fill = mod), color = "black", size = 0.25, stat = "identity", position = "dodge") + 
    scale_fill_manual(values = cols) +  
    geom_errorbar(
      aes(ymin = minc, ymax = maxc, alpha = if_else(minc == maxc, 0, 1)),
      width = 0.2
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .2))) + 
    scale_x_discrete(
      breaks = as.character(seq(1980, 2100, 10)),
      labels = c(
        "", "1990", "", "2010", "", "2030", "",
        "2050", "", "2070", "", "2090", ""
      )
    ) + 
    ylab("Count") + xlab("Decade") + labs(fill = "Source") +
    theme_classic() + 
    facet_wrap(~as.factor(varname), nrow = 2, scales = "free_y",
               labeller = as_labeller(make_tex, default = label_parsed)) + 
    theme(strip.background = element_blank(),
          strip.text = element_text(color = "black", size = 7,
                                    margin = margin(b = 0, t = 0)),
          panel.border = element_rect(color = "black", fill = NA),
          axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(size = 9),
          axis.title.x = element_text(margin = margin(t = 4, b = -8)),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.key.size = unit(0.7,"line"),
          legend.margin = margin(t = 0),
          legend.position = "bottom") + 
    guides(alpha = FALSE)
}


p6 <- make_fig6(file.path(data_dir, "extremes_bar_df.Rds"))
saveEPS(file.path(out_dir, "Fig6.eps"), p6, hr = 1)

#------------------------------------------------------------------------------
