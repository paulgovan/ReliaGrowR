pkgname <- "WeibullR.shiny"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('WeibullR.shiny')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("WeibullR.shiny")
### * WeibullR.shiny

flush(stderr()); flush(stdout())

### Name: WeibullR.shiny
### Title: A Shiny Weibull Analysis App.
### Aliases: WeibullR.shiny

### ** Examples

if (interactive()) {
  WeibullR.shiny()
}



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
