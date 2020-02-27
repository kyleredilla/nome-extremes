# Script summary
#   Render RMarkdown files in the documents directory
#
# Output Files
#   docs/find_Nome.pdf
#   docs/Nome_ERA5_distance.pdf
#   docs/Nome_ERA5_extremes.pdf
#   docs/Nome_ERA5_sf_scaled.pdf



#-- Setup ---------------------------------------------------------------------
wrap_render <- function(arg) {
  fns <- switch(arg,
    "1" = c("find_Nome.Rmd", "find_Nome"),
    "2" = c("compare_dist.Rmd", "Nome_ERA5_distance"),
    "3" = c("compare_extremes.Rmd", "Nome_ERA5_extremes"),
    "4" = c("compare_sf.Rmd", "Nome_ERA5_sf_scaled"),
    "5" = c("adj_wrf_tmin_ts.Rmd", "View_Adjusted_WRF_Tmin"),
    "6" = c("wrf_dec_extr.Rmd", "WRF_proj_decade_summary")
  )
  render(fns[1], output_dir = doc_dir, output_file = fns[2])
}

#------------------------------------------------------------------------------

#-- Main ----------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
valid_args <- as.character(1:6)

if (!(args[1] %in% valid_args)) {
  stop("Invalid argument", call. = FALSE)
} 

suppressMessages(library(rmarkdown))

doc_dir <- "docs"

wrap_render(args[1])

#------------------------------------------------------------------------------
