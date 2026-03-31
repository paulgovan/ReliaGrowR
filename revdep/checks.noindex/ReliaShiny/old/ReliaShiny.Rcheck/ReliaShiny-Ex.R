pkgname <- "ReliaShiny"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ReliaShiny')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ReliaShiny")
### * ReliaShiny

flush(stderr()); flush(stdout())

### Name: ReliaShiny
### Title: A Shiny Reliability Analysis App.
### Aliases: ReliaShiny

### ** Examples

if (interactive()) {
  ReliaShiny()
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
