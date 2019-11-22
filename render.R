# Script summary
#   Render RMarkdown files in the documents directory
#
# Output Files
#   /documents/find_Nome.pdf
#   /documents/Nome_ERA5_distance.pdf
#   /documents/Nome_ERA5_extremes.pdf
#   /documents/Nome_ERA5_sf_scaled.pdf



#-- Setup ---------------------------------------------------------------------
library(rmarkdown)

workdir <- getwd()
doc_dir <- file.path(workdir, "documents")

#------------------------------------------------------------------------------

#-- Render Files --------------------------------------------------------------
# find_Nome.pdf
render("find_Nome.Rmd", output_dir = doc_dir)

# Nome_ERA5_distance.pdf
render("compare_dist.Rmd", output_dir = doc_dir,
       output_file = "Nome_ERA5_distance")

# Nome_ERA5_extremes.pdf
render("compare_extremes.Rmd", output_dir = doc_dir,
       output_file = "Nome_ERA5_extremes")

# Nome_ERA5_sf_scaled.pdf
render("compare_sf.Rmd", output_dir = doc_dir,
       output_file = "Nome_ERA5_sf_scaled")

#------------------------------------------------------------------------------
