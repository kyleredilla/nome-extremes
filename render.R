# Script summary
#   Render RMarkdown files in the documents directory
#
# Output Files
#   /documents/find_Nome.pdf



#-- Setup ---------------------------------------------------------------------
library(rmarkdown)

workdir <- getwd()
doc_dir <- file.path(workdir, "documents")

#------------------------------------------------------------------------------

#-- Render Files --------------------------------------------------------------
# find_Nome.pdf
render("find_Nome.Rmd", output_dir = doc_dir)

# compare_dist.pdf
render("compare_dist.Rmd", output_dir = doc_dir,
       output_file = "Nome_ERA5_distance")

# compare_extremes.pdf
render("compare_extremes.Rmd", output_dir = doc_dir,
       output_file = "Nome_ERA5_extremes")
