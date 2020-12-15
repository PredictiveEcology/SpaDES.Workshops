<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/development/stickers/hexsticker.png">

# Upcoming `SpaDES` Workshops:

If you are interested in being put on the email list for future courses, please email:

- Eliot McIntire (eliot.mcintire at canada.ca)

## 25 - 29 January 2021 (5 days -- 3 hours each day)

### Location & Times

**Virtual (Google Meets Link to come), 11am - 2pm Pacific Standard Time, each day**

We have organized a set of sessions, back to back, starting from the most "general", and ending with the most "detailed".
The hope is to attract non-modelers (e.g., managers, scientists, practitioners) to the first session, people who think they might want to see more how models work (e.g., scientists, students, technicians), and those who want to build and use models for research and operational purposes (e.g., scientists, technicians, programmers).

### Workshop outline 
(_in progress -- updated Dec 15, 2020_)

**Introduction to `SpaDES` - Sections 1-3** 
* **Sections 1-2** - ~2+ hours – This is a high level intro for scientists, managers, policy makers, decision makers, coupled with examples of ongoing projects in `SpaDES` that will showcase the utility of the framework.

* **Section 3** - ~2+ hours – This section will take you through high-level examples of how to run pre-made `SpaDES` modules, run modules from other people, and change model parameters.

**Learning `SpaDES` - Sections 4-6** 
* **Section 4** - 2+ hours – we will take you through basic `SpaDES` concepts, while using the previous day's examples to get you started with understanding the packages and framework.
* **Sections 5-6** - 2+ hours – This is intended to dive a little bit into the code, learn how to create relatively simple modules and establish links between modules. WE will also touch upon essential aspects of programming with `SpaDES`, such as caching and debugging.

This is a high level intro for scientists, managers, policy makers, decision makers, coupled with examples of ongoing projects in `SpaDES` that will showcase the utility of the framework.

**My first project in `SpaDES` - Section 7** 
* **Section 7** - 4+ hours – during this section you'll be given free time to create your own project from scratch, or adapt an existing project and create new modules. 

### Installing SpaDES

**PLEASE NOTE -- R must be installed as an administrator because we are using developer tools**

Currently (Dec 14, 2020) testing with CRAN versions of all SpaDES packages...

If using Ubuntu Linux, because there are a lot of packages, it may be faster to install 
binaries from the Rstudio CRAN mirror:
```
options("repos" = c(CRAN = "https://cran.rstudio.com"))

if (Sys.info()["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
  .os.version <- strsplit(system("lsb_release -c", intern = TRUE), ":\t")[[1]][[2]]
  .user.agent <- paste0(
    "R/", getRversion(), " R (",
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]),
    ")"
  )
  options(repos = c(CRAN = paste0("https://packagemanager.rstudio.com/all/__linux__/",
                                  .os.version, "/latest")))
  options(HTTPUserAgent = .user.agent)
}
```



`SpaDES` has many R packages that it depends on. 
If there are problems, read the error messages and try to deal with the error message.
The most common one is that some package dependency is missing, usually due to some system dependency not being available. 

## The simplest way -- Install from CRAN:
```
## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
update.packages(checkBuilt = TRUE, ask = FALSE)
if (!identical("windows", .Platform$OS.type) && !require(igraph)) 
  install.packages("igraph", type = "source", repos = "https://cran.rstudio.com") # igraph needs to be installed from source
install.packages("SpaDES", dependencies = TRUE) # we want to install Suggests also, thus "TRUE"
```

Sometimes the packages on your computer may collide with the packages being installed. 
If this happens, and errors occur. Try the previous lines again. 
If the above lines showed no errors, then you can stop here; no need to proceed with installation below.

### It may be more reliable to put all packages in their own directory for the workshop

If that still doesn't work, it may be necessary to install packages in their own directory.
The function `Require::setLibPaths` allows us to do this.

```
## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
RPackageLibrary = "~/WorkshopLibrary"
if (!dir.exists(RPackageLibrary)) dir.create(RPackageLibrary)

if (!require("Require", lib = RPackageLibrary)) install.packages("Require", lib = RPackageLibrary)
library(Require, lib = RPackageLibrary)

#### Put packages in their own directory
setLibPaths(RPackageLibrary)
## For workshop -- set libPath in your .Rprofile file -- at end of workshop can delete this ######
RprofileFile = "~/.Rprofile"
if (!file.exists(RprofileFile))
  file.create(file = RprofileFile)
if (!isTRUE(grepl("Require Customized", readLines("~/.Rprofile"))))
  cat(file = "~/.Rprofile", {
    paste0("Require::setLibPaths('", RPackageLibrary, "') ## Require Customized\n")
  }, append = TRUE)

# Install all packages in a new place on your computer -- may collide with other packages if we use your personal library
# Identify all dependencies
# dependencies = pkgDep("PredictiveEcology/SpaDES", recursive = TRUE)
options(Require.buildBinaries = TRUE, Require.RPackageCache = file.path(RPackageLibrary, ".cached"))
# Require(dependencies$SpaDES, require = FALSE) ## Install them, don't 'require' them

## Restart your R session again so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"="true") # sometimes, some irrelevant warnings occur

# Install versions of packages from GitHub --> to do from CRAN: install.packages("SpaDES")
if (!identical("windows", .Platform$OS.type) && !require(igraph)) 
  install.packages("igraph", type = "source", repos = "https://cran.rstudio.com") # igraph needs to be installed from source
Require("PredictiveEcology/SpaDES@development", upgrade = FALSE, require = FALSE) # require = FALSE, means don't load them into memory

                           
# # Predictive Ecology Miscellaneous
# Require("PredictiveEcology/pemisc@Workshop", upgrade = "never", dependencies = TRUE, type = type)
# 
# # Vegetation Dynamics
# Require("PredictiveEcology/LandR@Workshop", upgrade = "never", dependencies = TRUE, type = type)
# 
# 
# # Alex's Miscellaneous Code
# Require("achubaty/amc@development", upgrade = "never", dependencies = TRUE, type = type)
# 
# # Other 'useful functions'               
# # Require("tati-micheletti/usefun@master", upgrade = "never", dependencies = TRUE, type = type)
```

It can happen that if you try downloading from `GitHub` many times, you exceed the API rate limit:
```
install_github('PredictiveEcology/SpaDES')
Downloading GitHub repo PredictiveEcology/SpaDES@master
Error: HTTP error 403.
  API rate limit exceeded for ###.###.##.###. 
  (...)
```
The error should provide the solution to fixing this problem, but if for some reason you don't find these instructions, here they are:
- Use `usethis::browse_github_pat()` to create a GitHub token
- Use `usethis::edit_r_environ()` and add the environment variable with `GITHUB_PAT = 'your_github_token'`.
Restart R (so that the GITHUB_PAT is read) and try to reinstall: `devtools::install_github(...)`

### Workshop materials

If you are comfortable with `GitHub.com`, you can clone the entire `SpaDES.Workshops` repository and thus have all the `*.Rmd` files used in this workshop:

<https://github.com/PredictiveEcology/SpaDES.Workshops>

For a direct link to the workshops, click on the top navigation bar or go [here](http://spades-workshops.predictiveecology.org/)

## Resources:

[SpaDES wiki pages](https://github.com/PredictiveEcology/SpaDES/wiki)
