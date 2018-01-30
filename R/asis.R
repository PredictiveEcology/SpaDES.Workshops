#' Quick and dirty render pkgdown asis
#'
#' This will first run pkgdown::build_site then run rmarkdown::render to
#' build the html pages. This is only relevant if the rmarkdown header
#' contains an html output format that is not supported by pkgdown
#' (e.g., ioslides, slidy). This will not work for pdf.
#' @keywords internal
#' @rdname asis
#' @export
.asis <- function(..., replacements) {

  #pkgdown::build_site(...)

  build_articlesCached(...)
  a <- dir("vignettes", full.names = TRUE, pattern = "Rmd")
  hasSlides <- unlist(lapply(a, function(x) {
    any(grepl(readLines(x), pattern = "slidy|ioslides"))
  }))
  if (any(hasSlides)) {
    a <- a[hasSlides]
    lapply(a, rmarkdown::render)
    htmlFilenames <- gsub(a, pattern = "Rmd", replacement = "html")
    file.copy(htmlFilenames, to = "docs/articles", overwrite = TRUE)
    unlink(htmlFilenames)
  }

  if (!missing(replacements)) {
    replaceRemoteLinksInArticles(replacements)
  }
}


#' @export
replaceRemoteLinksInArticles <- function(replacements) {
  lapply(names(replacements), function(nam) {
    indexHTML <- file.path("docs", "articles", "index.html")
    cc <- readLines(indexHTML)
    cc1 <- gsub(cc, pattern = paste0(nam, ".html"),
         replacement = replacements[[nam]])
    writeLines(cc1, indexHTML)
  })
}


#' build_articles from pkgdown, but with Caching
#'
#' @export
build_articlesCached <- function (pkg = ".", path = "docs/articles", depth = 1L, encoding = "UTF-8",
                                  quiet = TRUE, notOlderThan = NULL)
{
  old <- pkgdown:::set_pkgdown_env("true")
  on.exit(pkgdown:::set_pkgdown_env(old))
  pkg <- pkgdown:::as_pkgdown(pkg)
  path <- pkgdown:::rel_path(path, pkg$path)
  if (!pkgdown:::has_vignettes(pkg$path)) {
    return(invisible())
  }
  pkgdown:::rule("Building articles")
  pkgdown:::mkdir(path)
  pkgdown:::copy_dir(file.path(pkg$path, "vignettes"), path, exclude_matching = "rsconnect")
  articles <- tibble::tibble(input = file.path(path, pkg$vignettes$file_in),
                             output_file = pkg$vignettes$file_out, depth = pkg$vignettes$vig_depth +
                               depth)
  data <- list(pagetitle = "$title$")
  end <- lapply(seq(NROW(articles)), function(r) {
    aa <- Cache(pkgdown:::render_rmd, input = asPath(articles$input[r]), output_file = asPath(articles$output_file[r]),
          digestPathContent = TRUE,
          depth = articles$depth[r],
          pkg = pkg, data = data, encoding = encoding, quiet = quiet, cacheRepo = "cache",
          notOlderThan = notOlderThan, omitArgs = "pkg")#,
          #sideEffect = dirname(articles$input)[r])
  })
  purrr::walk(articles$input, unlink)
  pkgdown:::build_articles_index(pkg, path = path, depth = depth)
  invisible()

}
