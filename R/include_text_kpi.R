# include_text_kpi.R
include_text_kpi_PT <- function(file_path, desired_pH) {
  text <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  text <- gsub("pH XXX", paste("pH", desired_pH), text)
  text <- gsub("pH para XXX", paste("pH para", desired_pH), text)
  cat(paste(text, collapse = "\n"))
}

include_text_kpi <- function(file_path, desired_pH) {
  text <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  text <- gsub("pH XXX", paste("pH", desired_pH), text)
  text <- gsub("pH to XXX", paste("pH to", desired_pH), text)
  cat(paste(text, collapse = "\n"))
}