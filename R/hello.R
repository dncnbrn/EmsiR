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
# # #
# EpistemeLogin("Emsi-testing",
#               "33ccbbe06768f3d441c26512c01b6a5f4369da37a55f127a0dd23810d0a47ba32d990dc68f951430d32a54b3e7fc6f020ccb8d51bdcac5f0fdcbdb7c3eb28701")
#
# JobN <- paste(rep("Jobs",19),seq(2003,2022,by=1),sep=".")
# JobA <- paste(rep("Jobs",19),seq(2003,2022,by=1),sep="_")
#
# metricOcc <- data.frame(name=c(JobN,"Replacements","Earnings.Median.Annual","Jobs.2003","Jobs.2016","Jobs.2022","Jobs.2016","Jobs.2022"),
#                         as=c(JobA,"Replacements","Median_Earnings","LQ03","LQ16","LQ22","SS0316","SS1622"),
#                         metrics=c(rep(NA,22),"LQ","LQ","LQ","SS","SS"),
#                         base=c(rep(NA,25),"Jobs.2003","Jobs.2016"))
# metricOcc <- metricmaker(metricOcc,"GB","Occupation")
# #
# mapArea <- mapper(mapnames=c("Great Britain","England"),
#                   mapcodes=c("GB","ENG"))
# Area <- list(dimensionName = "Area", map = mapArea)
#
# Occupation <- list(dimensionName = "Occupation", asIdentity = TRUE)
# #
# testset <- datapull("UK","Occupation","2016.1",list(CoW("A"),Area,Occupation),metricOcc)
# #
# mgrs <- data.frame(mapnames1=c("CEOs",rep("Group of managers A",3),rep("Group of managers B",4)),
#        mapnames2=c("1115","1116","1121","1122","1123","1131","1132","1133"))
#
# mgrs2 <- mgrs %>%
#   group_by(mapnames1) %>%
#   nest()
#
# mapper(mapnames=mgrs2$mapnames1,mapcodes=mgrs2$data)

