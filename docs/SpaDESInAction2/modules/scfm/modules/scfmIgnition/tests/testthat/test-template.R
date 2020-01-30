
# please do three things when this template is corrected modified.
# 1. rename this file based on the content you are testing, e.g., test-treeGrowthFunction.R
# 2. copy this file to tests folder, i.e., `/Users/stevec/Dropbox/Courses/7043H16/Lab/scfmModules/scfmIgnition/tests`.

# 3. modify the test description, i.e., test tree growth function, based on the content you are testing:,
test_that("test tree growth function", {
module <- list("scfmIgnition")
path <- list(modulePath = "/Users/stevec/Dropbox/Courses/7043H16/Lab/scfmModules", outputPath = file.path(tempdir(), "outputs"))
parameters <- list(
  #.progress = list(type = "graphical", interval = 1),
  .globals = list(verbose = FALSE),
  scfmIgnition = list(.saveInitialTime = NA)
)
times <- list(start = 0, end = 1)

# If your test function contains `time(sim)`, you can test the function at a particular simulation time by define start time above.
object1 <- "object1" # please specify
object2 <- "object2" # please specify
objects <- list("object1" = object1, "object2" = object2)

mySim <- simInit(times = times,
                 params = parameters,
                 modules = module,
                 objects = objects,
                 paths = path)

# You may need to set seed if your module or the function has the random number generator.
set.seed(1234)

# You have two strategies to test your module:
# 1. test the overall simulation results for the given objects, then, use the code below:

output <- spades(mySim, debug = FALSE)

# is output a simList?
expect_is(output, "simList")

# does output have your module in it
expect_true(any(unlist(modules(output)) %in% c(unlist(module))))

# did it simulate to the end?
expect_true(time(output) == 1)

# 2. test the functions inside of the module, then, use the line below:
# To allow the moduleCoverage function to calculate unit test coverage
# level, it needs access to all functions directly. Use this approach
# to when using any function within the simList object,
# i.e., one version as a direct call, and one with simList prepended.

output <- try(treeGrowthFunction(mySim, otherArguments))
if (is(output, "try-error")) {
  output <- mySim$treeGrowthFunction(mySim, otherArguments)
}

# treeGrowthFunction is the function you would like to test, please specify your function name
# otherArguments is the arguments needed for running the function.

# output_expected <- # please define your expection of your output
# expect_equal(output, output_expected) # or other expect function in testthat package.
})