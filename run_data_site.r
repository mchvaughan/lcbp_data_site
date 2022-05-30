# Info ----------------------------------------------------------
## Code written by Matthew C.H. Vaughan, Chief Scientist at Lake Champlain Basin Program

# Load packages
library(rmarkdown)
library(tidyverse)

# Render the RMarkdown file
"lcbp_data_site.Rmd" %>%
  render(output_file = "index.html")


  # ftpUpload(what = "buoyPlots_v2.R",
  #           to = "ftp://02d0c8c.netsolhost.com/test_r_script_upload.r",
  #           userpwd = userpwd)
