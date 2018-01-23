#' Quick and dirty render pkgdown asis
#'
#' This will first run pkgdown::build_site then run rmarkdown::render to
#' build the html pages. This is only relevant if the rmarkdown header
#' contains an html output format that is not supported by pkgdown
#' (e.g., ioslides, slidy). This will not work for pdf.
#' @keywords internal
#' @rdname asis
.asis <- function(...) {
  pkgdown::build_site(...)
  a <- dir("vignettes", full.names = TRUE, pattern = "Rmd");
  lapply(a, rmarkdown::render);
  file.copy(gsub(a, pattern = "Rmd", replacement = "html"), to = "docs/articles", overwrite = TRUE)
}

