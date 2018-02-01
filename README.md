<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES Workshops

## Feb 7 - 9, 2018, Pacific Forestry Centre

If you are interested in being put on the email list for this or future courses, please email to eliot.mcintire at canada.ca

We have organized a set of sessions, back to back, starting from the most “general”, and ending with the most “detailed”. The hope is to attract non-modelers (e.g., managers, scientists, practitioners) to the first session, people who think they might want to see more how models work (e.g., scientists, students, technicians), and those who want to build and use models for research and operational purposes (e.g., scientists, technicians, programmers).

- **Intro** - 1.5 hours – **9 am Feb 7** - This is a high level intro for scientists, managers, policy makers, decision makers

- **Advanced Intro** - 4.5 hours – **11 am Feb 7** - This is intended to dive a little bit into the code, to allow people to run other models from other people, and allow them to change parameters.

- **Developers** - 3 days – **Feb 7 to Feb 9** - This 3 days workshop starts with the **Intro** and **Advanced Intro** above in the first day, and continues much deeper for developers of models. That means that if somebody who is interested in developing, but has already a good grasp on what SpaDES does and the higher level concepts, they can join in day 2.  ***See below for instructions***


## Location

**Pacific Forestry Centre  -- Dilbert Hall**

## WebEx

For those who can’t be here in person, we will use WebEx with phone (The best for this is connect to the web page first, then let the web page dial you by clicking on Audio):
https://pwgsc-nh.webex.com/join/eliot.mcintirecanada.ca

If you are interested in the Advanced Intro or Developer Workshop, please email me.
If you would like to participate in the Developer workshop, you will need your own laptop.


## Developers workshops

For those of you following the Developer's workshop, you can pre-install the `SpaDES` packages:


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

# For the workshop, there is one piece that is not in the CRAN version
# Restart your R session so it is clear
# Ctrl-shift-F10 if you are in Rstudio #
reproducible::Require("devtools") # installs if needed, and loads
devtools::install_github("PredictiveEcology/SpaDES", ref = "development")

```

For a direct link to the workshops, click on the top navigation bar.

# Resources:

[SpaDES wiki pages](https://github.com/PredictiveEcology/SpaDES/wiki)
