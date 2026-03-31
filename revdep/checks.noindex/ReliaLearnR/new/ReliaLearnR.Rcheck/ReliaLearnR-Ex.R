pkgname <- "ReliaLearnR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ReliaLearnR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("avail")
### * avail

flush(stderr()); flush(stdout())

### Name: avail
### Title: Availability (1 - unavailability / total)
### Aliases: avail

### ** Examples

avail(100, 1000)
avail(c(5,10), c(500,600))



cleanEx()
nameEx("fr")
### * fr

flush(stderr()); flush(stdout())

### Name: fr
### Title: Failure rate (lambda)
### Aliases: fr

### ** Examples

fr(75, 5000)
fr(c(10,5), c(1000,2000))



cleanEx()
nameEx("lda")
### * lda

flush(stderr()); flush(stdout())

### Name: lda
### Title: Launch the Life Data Analysis Tutorial
### Aliases: lda

### ** Examples

if (interactive()) {
  lda()
}



cleanEx()
nameEx("mtbf")
### * mtbf

flush(stderr()); flush(stdout())

### Name: mtbf
### Title: Mean Time Between Failures (MTBF) for repairable items.
### Aliases: mtbf

### ** Examples

mtbf(5, 1000)
mtbf(c(2,3), c(500,500))



cleanEx()
nameEx("mttf")
### * mttf

flush(stderr()); flush(stdout())

### Name: mttf
### Title: Mean Time To Failure (MTTF)
### Aliases: mttf

### ** Examples

mttf(5, 1000)
mttf(c(2,3), c(500,500))



cleanEx()
nameEx("ram")
### * ram

flush(stderr()); flush(stdout())

### Name: ram
### Title: Launch the RAM Analysis Tutorial
### Aliases: ram

### ** Examples

if (interactive()) {
  ram()
}



cleanEx()
nameEx("rel")
### * rel

flush(stderr()); flush(stdout())

### Name: rel
### Title: Reliability (1 - outage / total)
### Aliases: rel

### ** Examples

rel(100, 1000)
rel(c(10,20), c(500, 600))



cleanEx()
nameEx("rt")
### * rt

flush(stderr()); flush(stdout())

### Name: rt
### Title: Launch the Reliability Testing Tutorial
### Aliases: rt

### ** Examples

if (interactive()) {
  rt()
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
