pkgname <- "WeibullR.learnr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('WeibullR.learnr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("RAMR.learnr")
### * RAMR.learnr

flush(stderr()); flush(stdout())

### Name: RAMR.learnr
### Title: Reliability, Availability, and Maintainability
### Aliases: RAMR.learnr

### ** Examples

if (interactive()) {
  RAMR.learnr()
}



cleanEx()
nameEx("TestR.learnr")
### * TestR.learnr

flush(stderr()); flush(stdout())

### Name: TestR.learnr
### Title: An Interactive Introduction to Reliability Testing
### Aliases: TestR.learnr

### ** Examples

if (interactive()) {
  TestR.learnr()
}



cleanEx()
nameEx("avail")
### * avail

flush(stderr()); flush(stdout())

### Name: avail
### Title: Availability.
### Aliases: avail

### ** Examples

unavail <- 100
total <- 1000
avail(unavail, total)



cleanEx()
nameEx("fr")
### * fr

flush(stderr()); flush(stdout())

### Name: fr
### Title: Failure Rate (lambda).
### Aliases: fr

### ** Examples

fail <- 75
total <- 5000
fr(fail, total)



cleanEx()
nameEx("mtbf")
### * mtbf

flush(stderr()); flush(stdout())

### Name: mtbf
### Title: Mean Time Between Failures (MTBF).
### Aliases: mtbf

### ** Examples

fail <- 5
total <- 1000
mtbf(fail, total)



cleanEx()
nameEx("mttf")
### * mttf

flush(stderr()); flush(stdout())

### Name: mttf
### Title: Mean Time To Failure (MTTF).
### Aliases: mttf

### ** Examples

fail <- 5
total <- 1000
mttf(fail, total)



cleanEx()
nameEx("rel")
### * rel

flush(stderr()); flush(stdout())

### Name: rel
### Title: Reliability.
### Aliases: rel

### ** Examples

outage <- 100
total <- 1000
rel(outage, total)



cleanEx()
nameEx("serv")
### * serv

flush(stderr()); flush(stdout())

### Name: serv
### Title: Serviceability.
### Aliases: serv

### ** Examples

service <- 900
total <- 1000
serv(service, total)



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
