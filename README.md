<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/development/stickers/hexsticker.png">

# Upcoming `SpaDES` Workshops:

If you are interested in being put on the email list for future courses, please email:

- Frances Stewart (frances.stewart at canada.ca)
- Eliot McIntire (eliot.mcintire at canada.ca)

## 29 - 31 January 2020

### Location & Times

**Pacific Forestry Centre (Main conference room on ground floor), 9am - 4pm each day**

We have organized a set of sessions, back to back, starting from the most "general", and ending with the most "detailed".
The hope is to attract non-modelers (e.g., managers, scientists, practitioners) to the first session, people who think they might want to see more how models work (e.g., scientists, students, technicians), and those who want to build and use models for research and operational purposes (e.g., scientists, technicians, programmers).

### Workshop outline
**Day 1: Introduction to `SpaDES` - Sections 1-3** 
* **Sections 1-2 - 9am - lunch break** - 3 hours – This is a high level intro for scientists, managers, policy makers, decision makers, coupled with examples of ongoing projects in `SpaDES` that will showcase the utility of the framework.

* **Section 3 - 1pm - 4pm** - 3 hours – This section will take you through high-level examples of how to run pre-made `SpaDES` modules, run modules from other people, and change model parameters.

**Day 2: Learning `SpaDES` - Sections 4-6** 
* **Section 4 - 9am - break** - 1.5 hours – we will take you through basic `SpaDES` concepts, while using the previous day's examples to get you started with understanding the packages and framework.
* **Sectios 5-6 - remainder of the day** - 4.5 hours – This is intended to dive a little bit into the code, learn how to create relatively simple modules and establish links between modules. WE will also touch upon essential aspects of programming with `SpaDES`, such as caching and debugging.

This is a high level intro for scientists, managers, policy makers, decision makers, coupled with examples of ongoing projects in `SpaDES` that will showcase the utility of the framework.

**Day 3: My first project in `SpaDES` - Section 7** 
* **Section 7 - 9am - 4pm** - 6 hours – during this section you'll be given free time to create your own project from scratch, or adapt an existing project and create new modules. 

### [Installing SpaDES](articles/index.html)

**PLEASE NOTE -- R must be installed as an administrator because we are using developer tools**

We will be using the latest version of `SpaDES`, which includes some new features and bug fixes not in the CRAN version.
You should install the development version from GitHub following these instructions:

<https://github.com/PredictiveEcology/SpaDES/wiki/Installation>

`SpaDES` has many R packages that it depends on. 
If there are problems, read the error messages and try to deal with the error message.
The most common one is that some package dependency is missing, usually due to some system dependency not being available. 

If problems persist, try narrowing down the problem and re-installing using the following:

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

<https://github.com/PredictiveEcology/SpaDES.Workshops>

For a direct link to the workshops, click on the top navigation bar or go [here](http://spades-workshops.predictiveecology.org/)

## Resources:

[SpaDES wiki pages](https://github.com/PredictiveEcology/SpaDES/wiki)
