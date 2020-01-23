replacementList <- list(
  "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] Workshop" = "2019-10-10",
  "Articles <small>version&nbsp;0.1.0</small>" = "Content",
  "06c-Caching.html" = "https://cran.r-project.org/web/packages/SpaDES/vignettes/iii-cache.html",
  "06d-Debugging.html" = "https://github.com/PredictiveEcology/SpaDES/wiki/Debugging",
  "Past Workshops" = "Prior to 2019",
  "06b-Plotting.html" = "http://quickplot.predictiveecology.org/articles/iii-plotting.html",
  "Released package" = NULL,
  "03c-SpaDESInAction.html" = "https://htmlpreview.github.io/?https://github.com/tati-micheletti/SpaDESinAction/blob/master/runSimulation.html",
  "02b-SpaDES4Dummies.html" = "https://htmlpreview.github.io/?https://github.com/CeresBarros/SpaDES4Dummies/blob/master/SpaDES4Dummies.html",
  "exercise1" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/01-exercises.html",
  "exercise2" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/02-exercises.html",
  "exercise3" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/03-exercises.html",
  "exercise4" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/04-exercises.html",
  "exercise4a" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/04a-exercises.html",
  "exercise5a" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/05a-exercises.html",
  "exercise6" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/06-exercises.html",
  "exercise7" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/07-exercises.html",
  "exercise8" = "https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES.Workshops/blob/master/exercises/08-exercises.html",
  "12b-caribouPopulationGrowthModule" = "http://htmlpreview.github.io/?https://github.com/tati-micheletti/caribouPopGrowthModel/blob/master/caribouPopGrowthModel.html",
  "12a-CommunityMetricsModule" = "http://htmlpreview.github.io/?https://github.com/tati-michelett/comm_metricsNWT/blob/master/comm_metricsNWT.html",
  "<h1>Articles</h1>" = "<h1>Content</h1>"
)

#' @details
#' This is a named list where name is the first line of the multi-line grep
#' The final line is indicated by the second named list \code{replacementEnds}.
#' The content of \code{replacementStarts} represents a multi-line replacement.
replacementStarts <- list(
  "a href=\"../articles/index.html" =
    "  <a href=\"../articles/index.html\">2020-Jan</a>
</li>
  <li>
  <a href=\"../articlesOct2019/index.html\">2019-Fall</a>
  </li>
  <li>
  <a href=\"../articlesSept2018/index.html\">2018-Fall</a>
  </li>
  <li>
  <a href=\"../articlesMay2018/index.html\">2018-May</a>
  </li>
  <li>
  <a href=\"../articlesFeb2018/index.html\">2018-Feb</a>
  </li>
  <li>
  <a href=\"http://rpubs.com/PredictiveEcology/SpaDES-Intro-Course-Outline\">Prior to 2018</a>",

  "<div class=\"navbar-header\">" =
  "    <div class=\"navbar-header\">
      <button type=\"button\" class=\"navbar-toggle collapsed\" data-toggle=\"collapse\" data-target=\"#navbar\" aria-expanded=\"false\">
  <span class=\"sr-only\">Toggle navigation</span>
    <span class=\"icon-bar\"></span>
    <span class=\"icon-bar\"></span>
    <span class=\"icon-bar\"></span>
    </button>
    <span class=\"navbar-brand\">
    <a class=\"navbar-link\" href=\"../index.html\">SpaDES.Workshops</a>
    <span class=\"version label label-default\" data-toggle=\"tooltip\" data-placement=\"bottom\" title=\"Released version\">0.1.0</span>
    </span>
    </div>

    <div id=\"navbar\" class=\"navbar-collapse collapse\">
    <ul class=\"nav navbar-nav\">"
)

replacementEnds <- list(
  "SpaDES-Intro-Course-Outline" = "",
  "<ul class=\"nav navbar-nav\">" = ""
)
#library(pkgdown)
#library(reproducible)
#unlockBinding("build_articles_index", env = as.environment("package:pkgdown"))
#lockBinding("build_articles_index", env = as.environment("package:pkgdown"))
#assignInNamespace("build_articles_index", build_articles_index2, ns = "pkgdown")

####################################################################################################
#' \code{replaceRemoteLinksInArticles}
#'
#' Lorem ipsum ...
#'
#' @export
replaceRemoteLinksInArticles <- function(replacements) {
  filesToUpdate <- c("", "articles", "articlesFeb2018", "articlesMay2018", "articlesSept2018", "articlesOct2019")
  lapply(filesToUpdate, function(f) {
    lapply(names(replacements), function(nam) {
      for (indexHTML in dir(file.path("docs", f), pattern = ".html", full.names = TRUE)) {
        #browser(expr = "Released package" == nam && grepl("articlesMay2018", f) && grepl("WhatIs", indexHTML))
        cc <- readLines(indexHTML)
        if (!is.null(replacements[[nam]])) {
        #   cc1 <- grep(nam, cc, invert = TRUE, value = TRUE)
        # } else {
          cc1 <- gsub(cc, pattern = nam, replacement = replacements[[nam]])
          writeLines(cc1, indexHTML)
        }
        indexHTML
      }
    })
  })
}

replaceRemoteLinksMultiline <- function(replacementsStarts, replacementsEnds) {
  filesToUpdate <- c("", "articles", "articlesFeb2018", "articlesMay2018", "articlesSept2018", "articlesOct2019")
  lapply(filesToUpdate, function(f) {
    lapply(seq_along(names(replacementsStarts)), function(namIndex) {

      nam <- names(replacementsStarts)[namIndex]
      namEnd <- names(replacementsEnds)[namIndex]
      for (indexHTML in dir(file.path("docs", f), pattern = ".html", full.names = TRUE)) {
        #browser(expr = "Released package" == nam && grepl("articlesMay2018", f) && grepl("WhatIs", indexHTML))
        cc <- readLines(indexHTML)
        # browser(expr = grepl("index\\.html", indexHTML))
        if (!is.null(replacementStarts[[nam]])) {
          if (any(grepl(cc, pattern = nam)) && any(grepl(cc, pattern = namEnd))) {
            init <- grep(nam, cc, invert = FALSE, value = FALSE)
            end <- grep(namEnd, cc, invert = FALSE, value = FALSE)
            cc1 <- c(cc[seq_len(init - 1)],
                     strsplit(replacementStarts[[nam]], split = "\n")[[1]],
                     cc[end + seq_len(length(cc) - end)])#, file = indexHTML, append = FALSE)
            writeLines(cc1, indexHTML)
          }

        }
        indexHTML
      }
    })
  })
}

####################################################################################################
#' \code{build_articles1}
#'
#' Lorem ipsum ...
#'
#' @importFrom reproducible Cache
build_articles1 <- function(pkg = ".", quiet = TRUE, lazy = TRUE, override = list(), preview = NA) {
  pkg <- pkgdown:::section_init(pkg, depth = 1L, override = override)
  if (nrow(pkg$vignettes) == 0L) {
    return(invisible())
  }
  pkgdown:::rule("Building articles")
  pkgdown:::build_articles_index(pkg)
  #browser()
  # use lapply, so can use try ... sometimes a single vignette failes, e.g., HTTP 404.
  #   don't want all to fail
  lapply(pkg$vignettes$name, function(ff)
    try(Cache(build_article, ff, pkg = pkg, quiet = quiet, lazy = lazy)))
  #try(purrr::walk(pkg$vignettes$name, Cache, FUN = build_article, pkg = pkg,
  #            quiet = quiet, lazy = lazy))
  preview_site(pkg, "articles", preview = preview)
}
assignInNamespace("build_articles", build_articles1, ns = "pkgdown")
#build_site(new_process = FALSE)
