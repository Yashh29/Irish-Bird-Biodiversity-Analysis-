
# 01_load_data.R

library(readr)
library(dplyr)

# Locate project root


project_root <- normalizePath(
  file.path(getwd(), ".."),
  winslash = "/",
  mustWork = TRUE
)



data_file <- file.path(
  project_root,
  "data",
  "BirdsOfIreland.txt"
)

# Check that file exists


if (!file.exists(data_file)) {
  
  stop(
    paste0(
      "\nBird dataset could not be found.\n\n",
      "Expected location:\n",
      data_file,
      "\n\n",
      "Please make sure BirdsOfIreland.txt is inside:\n",
      project_root,
      "/data/\n"
    )
  )
  
}

# Load data

birds <- read.delim(
  data_file,
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# Confirmation



cat(" Irish Bird Biodiversity Dataset Loaded\n")

cat(
  "Rows:",
  format(nrow(birds), big.mark = ","),
  "\n"
)

cat(
  "Columns:",
  ncol(birds),
  "\n"
)

cat(
  "File:",
  data_file,
  "\n"
)

