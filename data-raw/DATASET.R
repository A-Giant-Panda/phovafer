## code to prepare datasets
load("/Users/gedongjiao/Downloads/Files/phovafer/data_raw/loadsample.RData")
load("/Users/gedongjiao/Downloads/Files/phovafer/data_raw/pvSample.RData")
usethis::use_data(loadsample, overwrite = TRUE)
usethis::use_data(pvSample, overwrite = TRUE)
