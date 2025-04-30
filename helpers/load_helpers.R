# helper function to source all other helper functions cleanly in the main scripts

# list all files in this directory
helper_files <- list.files(
  path = "helpers", 
  pattern = "\\.R$", 
  full.names = TRUE
)

# except itself
helper_files <- helper_files[!grepl("load_helpers\\.R$", helper_files)]

# source all of them
sapply(helper_files, source)
