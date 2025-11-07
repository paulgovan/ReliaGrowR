pkgname <- "WeibullR.plotly"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('WeibullR.plotly')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("plotly_contour")
### * plotly_contour

flush(stderr()); flush(stdout())

### Name: plotly_contour
### Title: Interactive Contour Plot
### Aliases: plotly_contour

### ** Examples

library(WeibullR)
library(WeibullR.plotly)

failures1 <- c(30, 49, 82, 90, 96)
failures2 <- c(20, 40, 60, 80, 100)
obj1 <- wblr.conf(wblr.fit(wblr(failures1), method.fit = 'mle'), method.conf = 'lrb')
obj2 <- wblr.conf(wblr.fit(wblr(failures2), method.fit = 'mle'), method.conf = 'lrb')
plotly_contour(list(obj1, obj2), main = "Overlayed Contours")




cleanEx()
nameEx("plotly_duane")
### * plotly_duane

flush(stderr()); flush(stdout())

### Name: plotly_duane
### Title: Interactive Duane Plot.
### Aliases: plotly_duane

### ** Examples

library(ReliaGrowR)
times<-c(100, 200, 300, 400, 500)
failures<-c(1, 2, 1, 3, 2)
fit<-duane(times, failures)
plotly_duane(fit)



cleanEx()
nameEx("plotly_rga")
### * plotly_rga

flush(stderr()); flush(stdout())

### Name: plotly_rga
### Title: Interactive Reliability Growth Plot.
### Aliases: plotly_rga

### ** Examples

library(ReliaGrowR)
times<-c(100, 200, 300, 400, 500)
failures<-c(1, 2, 1, 3, 2)
rga<-rga(times, failures)
plotly_rga(rga)

times <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
failures <- c(1, 2, 1, 1, 1, 2, 3, 1, 2, 4)
breakpoints <- 400
rga2 <- rga(times, failures, model_type = "Piecewise NHPP", breaks = breakpoints)
plotly_rga(rga2, fitCol = "blue", confCol = "blue", breakCol = "red")



cleanEx()
nameEx("plotly_wblr")
### * plotly_wblr

flush(stderr()); flush(stdout())

### Name: plotly_wblr
### Title: Interactive Probability Plot.
### Aliases: plotly_wblr

### ** Examples

library(WeibullR)
library(WeibullR.plotly)
failures<-c(30, 49, 82, 90, 96)
obj<-wblr.conf(wblr.fit(wblr(failures)))
plotly_wblr(obj)

suspensions<-c(100, 45, 10)
obj<-wblr.conf(wblr.fit(wblr(failures, suspensions)))
plotly_wblr(obj, suspensions, fitCol = 'blue', confCol
= 'blue')
inspection_data <- data.frame(left=c(0, 6.12, 19.92, 29.64, 35.4, 39.72, 45.32, 52.32),
                           right=c(6.12, 19.92, 29.64, 35.4, 39.72, 45.32, 52.32, 63.48),
                           qty=c(5, 16, 12, 18, 18, 2, 6, 17))
suspensions <- data.frame(time = 63.48, event = 0, qty = 73)
obj <- wblr(suspensions, interval = inspection_data)
obj <- wblr.fit(obj, method.fit = "mle")
obj <- wblr.conf(obj, method.conf = "fm", lty = 2)
suspensions <- as.vector(suspensions$time)
plotly_wblr(obj, susp = suspensions, fitCol = 'red', confCol = 'red', intCol = 'blue',
        main = 'Parts Cracking Inspection Interval Analysis',
        ylab =  'Cumulative % Cracked', xlab='Inspection Time')
failures <- c(25, 30, 42, 49, 55, 67, 73, 82, 90, 96, 101, 110, 120, 132, 145)
fit <- wblr.conf(wblr.fit(wblr(failures), dist = "weibull3p"))
plotly_wblr(fit, fitCol='darkgreen', confCol = 'darkgreen')




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
