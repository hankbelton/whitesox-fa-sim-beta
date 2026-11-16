write_human_csv <- function(d, path, append = FALSE) {
  readr::write_excel_csv(d, path, na = "", append = append)
  invisible(path)
}

read_human_csv <- function(path, ...) {
  d <- data.table::fread(path, encoding = "UTF-8", ...)

  for (col in names(d)[vapply(d, is.character, logical(1))]) {
    if (any(grepl('""', d[[col]], fixed = TRUE))) data.table::set(d, j = col, value = gsub('""', '"', d[[col]], fixed = TRUE))
  }
  d
}
