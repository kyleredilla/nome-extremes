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
fn <- "find_Nome.Rmd"; render(fn, output_dir = doc_dir)
