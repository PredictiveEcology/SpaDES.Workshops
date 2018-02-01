<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES Workshops

This web site is *very much* in development. You can follow progress with the links above.

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
