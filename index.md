<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# Next SpaDES Workshop:

## Sept 24-26, 2018, Pacific Forestry Centre, 9am - 4pm each day

If you are interested in being put on the email list for this or future courses, please email to eliot.mcintire at canada.ca

We have organized a set of sessions, back to back, starting from the most “general”, and ending with the most “detailed”. The hope is to attract non-modelers (e.g., managers, scientists, practitioners) to the first session, people who think they might want to see more how models work (e.g., scientists, students, technicians), and those who want to build and use models for research and operational purposes (e.g., scientists, technicians, programmers).

- **Part 1: Introducing a modular simulation platform for non-programmers** - 1.5 hours – **day 1 - 9 am - break** - This is a high level intro for scientists, managers, policy makers, decision makers, coupled with high-level exercises in SpaDES that will take you through how to run pre-made SpaDES modules, run modules from other people, and change model parameters.

- **Part 2: Getting started with SpaDES in R** - 4.5 hours – **day 1 - mid morning through to end of day** - This is intended to dive a little bit into the code, learn how to create relatively simple modules and establish links between modules. ***See below for instructions***

- **Part 3:  - days 2 and 3 - 9am to end of day each day** – a much deeper dive for developers of models. That means that if somebody who is interested in developing, but has already a good grasp on what SpaDES does and the higher level concepts, they can join this workshop.  ***See below for instructions***


### Location:

**Pacific Forestry Centre - Dilbert Hall**

### Remote Connecting:

Password every day: spades

**Monday Sept 24**

https://pwgsc-nh.webex.com/pwgsc-nh/j.php?MTID=m563024ba07f52ff94e8e84938a8170c8  
Meeting Number: 557 242 772


**Tuesday Sept 25**

https://pwgsc-nh.webex.com/pwgsc-nh/j.php?MTID=m4a37b92fd12b467686442e126cf36d16  
Meeting Number: 558 017 771 


**Wednesday Sept 26**

https://pwgsc-nh.webex.com/pwgsc-nh/j.php?MTID=mb43e1b9e5691e45a7c04af6d9939d1f3  
Meeting Number: 554 888 016 



### [Part 2 and 3](articles/index.html)

When following the Developer's workshop, please pre-install the `SpaDES` packages and dependencies:


```
# Restart your R session so it is clear
# Ctrl-shift-F10 if you are in Rstudio #

# If you have any of our packages or their dependencies, please update them first
# Get latest versions of key SpaDES packages from CRAN
dependencies <- tools::package_dependencies("SpaDES", recursive = TRUE)

# Update any versions of these dependencies that are already on your machine
update.packages(oldPkgs = unlist(dependencies), ask = FALSE) 

# install the latest version of the SpaDES packages and any dependencies not yet installed
install.packages("SpaDES", dependencies = TRUE) # install "suggested" packages too with TRUE

# For the workshop, there are a few minor bug fixes that are not in the CRAN version
# Restart your R session so it is clear
# Ctrl-shift-F10 if you are in Rstudio #
reproducible::Require("devtools") # installs (if needed) and loads
devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development")

```

For a direct link to the workshops, click on the top navigation bar or go [here](http://spades-workshops.predictiveecology.org/)

## Resources:

[SpaDES wiki pages](https://github.com/PredictiveEcology/SpaDES/wiki)
