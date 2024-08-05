# include_text.R
include_text <- function(file_path) {
  text <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  cat(paste(text, collapse = "\n"))
}