# Definindo a função include_text_rmd
include_preprocessed_text <- function(file_path) {
  preprocessed_file <- tempfile(fileext = ".md")
  opts_knit$set(progress = FALSE) # Suppress the progress bar
  knit(file_path, output = preprocessed_file, quiet = TRUE) # Set quiet = TRUE to further suppress messages
  text <- readLines(preprocessed_file, warn = FALSE, encoding = "UTF-8")
  cat(paste(text, collapse = "\n"))
}