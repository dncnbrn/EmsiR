# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}

JobN <- paste(rep("Jobs",19),seq(2003,2022,by=1),sep=".")
JobA <- paste(rep("Jobs",19),seq(2003,2022,by=1),sep="_")

metricOcc <- data.frame(name=c(JobN,"Replacements","Earnings.Median.Annual","Jobs.2003","Jobs.2016","Jobs.2022","Jobs.2016","Jobs.2022"),
                        as=c(JobA,"Replacements","Median_Earnings","LQ03","LQ16","LQ22","SS0316","SS1622"),
                        metrics=c(rep(NA,22),"LQ","LQ","LQ","SS","SS"),
                        base=c(rep(NA,25),"Jobs.2003","Jobs.2016"))
metricOcc <- metricmaker(metricOcc,"GB","Occupation")
