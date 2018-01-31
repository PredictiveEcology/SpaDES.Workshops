<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES Workshops

This web site is *very much* in development. You can follow progress with the links above.

For those of you following the Developer's workshop, you can pre-install the `SpaDES` packages:


```
# If you have any of our packages or their dependencies, please update them first
# Get latest versions of our packages from CRAN
update.packages(oldPkgs = c("quickPlot", "reproducible", "SpaDES.core"), ask = FALSE) 

# Update any versions of the dependencies of those packages
dependencies = reproducible::pkgDep(c("quickPlot", "reproducible", "SpaDES.core"))
update.packages(oldPkgs = unique(unlist(dependencies)), ask = FALSE) 

# install all SpaDES packages
install.packages("SpaDES") 

```

For a direct link to the workshops, click on the top navigation bar.
