<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES Workshops

This web site is *very much* in development. You can follow progress with the links above.

For those of you following the Developer's workshop, you can pre-install the `SpaDES` packages:


```
# If you have any of our packages or their dependencies, please update them first
# Get latest versions of key SpaDES packages from CRAN
dependencies <- tools::package_dependencies("SpaDES", recursive = TRUE)

# Update any versions of the dependencies of those packages
update.packages(oldPkgs = unlist(dependencies), ask = FALSE) 

# install the latest version of the SpaDES packages
install.packages("SpaDES") 
```

For a direct link to the workshops, click on the top navigation bar.

# Resources:

[SpaDES wiki pages](https://github.com/PredictiveEcology/SpaDES/wiki)
