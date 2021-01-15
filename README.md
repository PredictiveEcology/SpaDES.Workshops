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

# Before workshop begins

You will need to do 4 things

1. Install lots of packages -- and make sure it all worked
2. Install Rtools, if you don't have it
3. Decide on a folder for everything in the workshop
4. Install a few SpaDES modules into that folder

*If you are using Ubuntu Linux, please see section below for installing binary package files*

**[All the code below can be found in a single R file, which may be easier to use](https://raw.githubusercontent.com/PredictiveEcology/SpaDES.Workshops/master/R/README.R)**

## Install Packages

1. Get a few helper functions (`installGitHubPackage`, `getModule`)
```{r}
## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #
source("https://raw.githubusercontent.com/PredictiveEcology/SpaDES-modules/master/R/SpaDES_Helpers.R")
```

2. Install latest Require to help with step 3
```{r}
installGitHubPackage("PredictiveEcology/Require@development") # install latest version of Require
```

3. Install SpaDES and around 130 package dependencies in your Workshop folder
```{r}
installSpaDES() 
```

4. install another 50 or so packages used by modules
```{r}
Require::Require(
  c("PredictiveEcology/LandR",
    "PredictiveEcology/pemisc",
    "tati-micheletti/usefulFuns",
    "achubaty/amc@development"), 
  upgrade = "never", 
  which = c("Imports", "Depends", "Suggets"))
```


## [Install Rtools](https://cran.r-project.org/bin/windows/Rtools/) if you don't have it

Run this next line in your R console/Rstudio session. 
If it shows a "non-empty" path, then you have what you need for the workshop.
```{r}
Sys.which("make")
```
If it shows something like this: 
```
make
  ""
```
Then you will have to debug your Rtools installation using the internet as your friend.

**If you don't have it installed and functioning already, then you will have to install it. 
You can use the instructions here for [Windows](https://cran.r-project.org/bin/windows/Rtools/).**

## Decide on your folder you will use for the workshop

```{r}
workshopPath = "~/SpaDESWorkshop"
modulePath = file.path(workshopPath, "modules")
```

## Install a few modules -- see [Wiki of known modules](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development)


```{r}
# LandR Biomass modules
getModule("PredictiveEcology/Biomass_core", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_borealDataPrep", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_regeneration", modulePath = modulePath)
getModule("PredictiveEcology/Biomass_speciesData", modulePath = modulePath)

# SCFM fire modules
getModule("PredictiveEcology/scfm", modulePath = modulePath)
```


# Tips

## Github.com tells you `error 403` 

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

## Ubuntu Linux systems -- Binary R Packages

Because there are a lot of packages, it may be faster to install binaries from the Rstudio CRAN mirror.
To use this CRAN mirror, you can run this code to set up the correct CRAN repository. 
If you put this in your `.Rprofile` file, then your R sessions will always use this binary repository:

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

## Workshop materials

If you are comfortable with `GitHub.com`, you can clone the entire `SpaDES.Workshops` repository and thus have all the `*.Rmd` files used in this workshop:

<https://github.com/PredictiveEcology/SpaDES.Workshops>

For a direct link to the workshops, click on the top navigation bar or go [here](http://spades-workshops.predictiveecology.org/)

## Resources:

[SpaDES wiki pages](https://github.com/PredictiveEcology/SpaDES/wiki)
