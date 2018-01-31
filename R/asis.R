#' Quick and dirty render pkgdown asis
#'
#' This will first run pkgdown::build_site then run rmarkdown::render to
#' build the html pages. This is only relevant if the rmarkdown header
#' contains an html output format that is not supported by pkgdown
#' (e.g., ioslides, slidy). This will not work for pdf.
#' @keywords internal
#' @importFrom reproducible Cache asPath
#' @rdname asis
#' @export
.asis <- function(..., replacements, notOlderThan = NULL, cacheRepo = "docs/cache") {

  build_site(...)
  args <- formals(pkgdown::build_site)
  #out <- Cache(pkgdown::init_site, pkg = args$pkg, path = args$path, notOlderThan = notOlderThan, cacheRepo = cacheRepo)
  #out <- pkgdown::init_site(pkg = args$pkg, path = args$path)

  #out <- build_articles(..., cacheRepo = cacheRepo)

  #out <- Cache(do.call, pkgdown::build_home, args[names(args) %in% names(formals(pkgdown::build_home))], notOlderThan = notOlderThan, cacheRepo = cacheRepo)
  #out <- do.call(pkgdown::build_home, args[names(args) %in% names(formals(pkgdown::build_home))])

  #out <- Cache(do.call, pkgdown::build_reference, args[names(args) %in% names(formals(pkgdown::build_reference))], notOlderThan = notOlderThan, cacheRepo = cacheRepo)
  #out <- do.call(pkgdown::build_reference, args[names(args) %in% names(formals(pkgdown::build_reference))])

  a <- dir("vignettes", full.names = TRUE, pattern = "Rmd")
  hasSlides <- unlist(lapply(a, function(x) {
    any(grepl(readLines(x), pattern = "slidy|ioslides"))
  }))
  if (any(hasSlides)) {
    a <- a[hasSlides]

    Cache(renderSlides, a, cacheRepo = cacheRepo)
  }

  # replace modules
  if (missing(replacements )) replacements <- replacementList
  replaceRemoteLinksInArticles(replacements)

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

#' @export
build_site <- function (pkg = ".", path = "docs", examples = TRUE, run_dont_run = FALSE,
          mathjax = TRUE, preview = interactive(), seed = 1014, encoding = "UTF-8")
  {
    old <- pkgdown:::set_pkgdown_env("true")
    on.exit(pkgdown:::set_pkgdown_env(old))
    pkg <- pkgdown:::as_pkgdown(pkg)
    path <- pkgdown:::rel_path(path, pkg$path)
    pkgdown:::init_site(pkg, path)
    pkgdown:::build_home(pkg, path = path, encoding = encoding)
    pkgdown:::build_reference(pkg, lazy = FALSE, examples = examples, run_dont_run = run_dont_run,
                    mathjax = mathjax, seed = seed, path = file.path(path,
                                                                     "reference"), depth = 1L)
    build_articles(pkg, path = file.path(path, "articles"), depth = 1L,
                   encoding = encoding)
    pkgdown:::build_news(pkg, path = file.path(path, "news"), depth = 1L)
    if (preview) {
      pkgdown:::preview_site(path)
    }
    invisible(TRUE)
  }

#' build_articles from pkgdown, but with Caching
#'
#' @export
build_articles <- function (pkg = ".", path = "docs/articles", depth = 1L, encoding = "UTF-8",
                                  quiet = TRUE, notOlderThan = NULL, cacheRepo = "cache")
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
          pkg = pkg, data = data, encoding = encoding, quiet = quiet, cacheRepo = cacheRepo,
          notOlderThan = notOlderThan, omitArgs = "pkg")#,
          #sideEffect = dirname(articles$input)[r])
  })
  purrr::walk(articles$input, unlink)
  pkgdown:::build_articles_index(pkg, path = path, depth = depth)
  invisible()

}

replacementList <-
  list("SpaDES4Dummies" = "https://htmlpreview.github.io/?https://github.com/CeresBarros/SpaDES4Dummies/blob/master/SpaDES4Dummies.html")

renderSlides <- function(a) {
  lapply(a, function(x) rmarkdown::render(x))
  htmlFilenames <- gsub(a, pattern = "Rmd", replacement = "html")
  file.copy(htmlFilenames, to = "docs/articles", overwrite = TRUE)
  unlink(htmlFilenames)
}
