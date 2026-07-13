install.packages("hBayesDM", dependencies=TRUE)
devtools::install_github("ccs-lab/hBayesDM", subdir = "R")

library(hBayesDM)

output1 = gng_m1(data="example", niter=2000, nwarmup=1000, nchain=4, ncore=4)

output1$rawdata %>%
  pull(subjID) %>%
  table()

output1$allIndPars


plot(output1, type = "trace")

output1$parVals$xi %>%
  t() %>%
  gbtoolbox::reliability()

output1$parVals$ep %>%
  t() %>%
  gbtoolbox::reliability()

class(output1)

output1 |> 
  gbtoolbox::reliability()

debug(reliability)
