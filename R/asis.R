replacementList <-
  list("SpaDES4Dummies.html" = "https://htmlpreview.github.io/?https://github.com/CeresBarros/SpaDES4Dummies/blob/master/SpaDES4Dummies.html",
       "GoogleDrive.html" = "https://drive.google.com/open?id=1XnfUTRk59dORiPbdN2sreGDXNmDjcUle",
       "Caching.html" = "https://cran.r-project.org/web/packages/SpaDES/vignettes/iii-cache.html",
       "Plotting.html" = "http://quickplot.predictiveecology.org/articles/iii-plotting.html",
       "Debugging.html" = "https://github.com/PredictiveEcology/SpaDES/wiki/Debugging",
       "Articles <small>version&nbsp;0.1.0</small>" = "Content",
       "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] Workshop" = "2018-02-07",
       "Past Workshops" = "Prior to 2018",
       "Released package" = NULL
       )

#library(pkgdown)
#library(reproducible)
# unlockBinding("build_articles_index", env = as.environment("package:pkgdown"))
#lockBinding("build_articles_index", env = as.environment("package:pkgdown"))
#assignInNamespace("build_articles_index", build_articles_index2, ns = "pkgdown")


#' Ttitle2
#'
#' That
#'
#' @export
replaceRemoteLinksInArticles <- function(replacements) {
  filesToUpdate <- c("", "articles", "articlesFeb2018")
  lapply(filesToUpdate, function(f) {
    lapply(names(replacements), function(nam) {
      indexHTML <- file.path("docs", f, "index.html")
      cc <- readLines(indexHTML)
      if (is.null(replacements[[nam]])) {
        cc1 <- grep(nam, cc, invert = TRUE, value = TRUE)
      } else {
        cc1 <- gsub(cc, pattern = nam,
                    replacement = replacements[[nam]])
      }
      writeLines(cc1, indexHTML)
    })

  })
}

#' Title
#'
#' Thus
#'
#' @importFrom reproducible Cache
build_articles1 <- function (pkg = ".", quiet = TRUE, lazy = TRUE, override = list(),
          preview = NA)
{
  pkg <- pkgdown:::section_init(pkg, depth = 1L, override = override)
  if (nrow(pkg$vignettes) == 0L) {
    return(invisible())
  }
  pkgdown:::rule("Building articles")
  pkgdown:::build_articles_index(pkg)
  purrr::walk(pkg$vignettes$name, Cache, FUN = build_article, pkg = pkg,
              quiet = quiet, lazy = lazy)
  preview_site(pkg, "articles", preview = preview)
}
assignInNamespace("build_articles", build_articles1, ns = "pkgdown")
#build_site(new_process = FALSE)
