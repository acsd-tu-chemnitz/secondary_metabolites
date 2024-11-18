

Ncpus <- getOption("Ncpus")

options(Ncpus=parallel::detectCores() - 1)
install.packages("readxl")
install.packages("readr")
require(graphics)
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("svglite")
install.packages("patchwork")
install.packages("forecast")
install.packages("pracma")
#install.packages("mlmts")
install.packages("foreach")
install.packages("doParallel")
install.packages("collections")
install.packages("stringr")
install.packages("tsne")
install.packages("hexbin")
install.packages("Rtsne")
install.packages("RColorBrewer")
install.packages("mltools")
install.packages("factoextra")
install.packages("sets")
install.packages("dtw") 
install.packages("dtwclust")
install.packages("zoo")
install.packages("proxy")
install.packages("deldir")
install.packages("devtools")
install.packages("bigstatsr")
install.packages("bigmemory")
install.packages("tictoc")
devtools::install_github("mkuhn/dict")
install.packages("imputeTS")
install.packages("scatterplot3d")
install.packages("TSdist")
install.packages("Rcatch22")
install.packages("furrr")
install.packages("modelr")
install.packages("tseries")
install.packages("purrrlyr")
devtools::install_github('barkasn/fastSave')
install.packages("pROC")
install.packages("spsurvey")
install.packages("phonTools")
install.packages("smooth")
install.packages("forecast", dependencies = TRUE)
install.packages("quantreg")
install.packages("astsa")
install.packages("circlize")