<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/development/stickers/hexsticker.png">

# Upcoming `SpaDES` Workshops:

If you are interested in being put on the email list for future courses, please email:

- Eliot McIntire (eliot.mcintire at canada.ca)
- Alex Chubaty (achubaty at for-cast.ca).

## 22-24 October 2019

### Location & Times

**Great Lakes Forestry Centre, 9am - 4pm each day**

We have organized a set of sessions, back to back, starting from the most "general"", and ending with the most "detailed".
The hope is to attract non-modelers (e.g., managers, scientists, practitioners) to the first session, people who think they might want to see more how models work (e.g., scientists, students, technicians), and those who want to build and use models for research and operational purposes (e.g., scientists, technicians, programmers).

- **Part 1: Introducing a modular simulation platform for non-programmers** - 1.5 hours – **day 1 - 9 am - break** - This is a high level intro for scientists, managers, policy makers, decision makers, coupled with high-level exercises in `SpaDES` that will take you through how to run pre-made `SpaDES` modules, run modules from other people, and change model parameters.

- **Part 2: Getting started with `SpaDES` in R** - 4.5 hours – **day 1 - mid morning through to end of day** - This is intended to dive a little bit into the code, learn how to create relatively simple modules and establish links between modules. ***See below for instructions***

- **Part 3:  - days 2 and 3 - 9am to end of day each day** – a much deeper dive into SpaDES modules. We will cover various topics including reproducible 'data-to-decisions' workfrows, working with spatial data, caching, and simulation optimization.  ***See below for instructions***

### [Installing SpaDES](articles/index.html)

**PLEASE NOTE -- R must be installed as an administrator because we are using developer tools**

We will be using the latest version of `SpaDES`, which includes some new features and bug fixes not in the CRAN version.
You should install the development version from GitHub following these instructions:

<https://github.com/PredictiveEcology/SpaDES/wiki/Installation>

`SpaDES` has many R packages that it depends on. 
If there are problems, read the error messages and try to deal with the error message.
The most common one is that some package dependency is missing, usually due to some system dependency not being available. 

If problems persist, try narrowing down the problem and reinstalling using the following:

```
## Restart your R session so it is clear
## Ctrl-shift-F10 if you are in Rstudio #

## Make sure your existing packages are updated, and correct for the version of R
## Get latest versions of key SpaDES packages from CRAN
dependencies <- tools::package_dependencies("SpaDES", recursive = TRUE)

## Update any versions of these dependencies that are already on your machine
update.packages(oldPkgs = unlist(dependencies), ask = FALSE, checkBuilt = TRUE) 

## Install any dependencies that are missing -- 
##   install.packages is not getting correct dependencies
missingPkgs <- dependencies$SpaDES[!(dependencies$SpaDES %in% rownames(installed.packages()))]
if (length(missingPkgs))
  install.packages(missingPkgs, dependencies = FALSE)

## Install all SpaDES packages 
install.packages("SpaDES", dependencies = FALSE)

## For the workshop, there are a few minor fixes and enhancements that are not in the CRAN version
## Restart your R session so it is clear
# Ctrl-shift-F10 if you are in Rstudio #
reproducible::Require("devtools") # installs (if needed) and loads
devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development")
```

### Installing additional packages

```r
install_github("achubaty/amc@development")
install_github("PredictiveEcology/LandR@development")
install_github("PredictiveEcology/pemisc@development")
```

### Workshop materials

If you are comfortable with `GitHub.com`, you can clone the entire `SpaDES.Workshops` repository and thus have all the `*.Rmd` files used in this workshop:

`https://github.com/PredictiveEcology/SpaDES.Workshops`

For a direct link to the workshops, click on the top navigation bar or go [here](http://spades-workshops.predictiveecology.org/)

## Resources:

[SpaDES wiki pages](https://github.com/PredictiveEcology/SpaDES/wiki)
