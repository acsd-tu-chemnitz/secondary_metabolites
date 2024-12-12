
source("./code/R/libload.R")
source("./code/R/util_functions.R")

featdf <- readRDS("./data/processed/final_meandf.rds")


B <- length(featdf[[1]][[1]])


lEE <- list(
    "beta"= c(0, 1, 1, 1, 1, 0, 0, 0),
    "lutein"=c(0, 1, 1, 1, 1, 0, 0, 0),
    "lycopen" = c(0, 1, 1, 1, 1, 0, 0, 0),
    "caffeic" =c(0, 0, 0, 1, 1, 1, 1, 0),
    "coumaric" =c(0, 0, 0, 1, 1, 1, 1, 0),
    "ferulic" =c(0, 0, 0, 1, 1, 1, 1, 0),
    "quercetin" = c(0, 0, 0, 1, 1, 1, 1, 0),
    "naringenin" = c(0, 1, 0, 1, 1, 0, 0, 0),
    "phloretin" = c(0, 0, 0, 0, 1, 1, 1, 1),
    "coumaryl" = c(1, 0, 0, 1, 1, 1, 0, 0),
    "caffeoyl" = c(0, 0, 1, 0, 0, 1, 1, 0),
    "total_flavonoids" = c(0, 0, 1, 0, 0, 1, 1, 1),
    "total_phenolic" = c(0, 0, 1, 0, 1, 1, 1, 0)
)


#lEE <- list(
#    "total_flavonoids" = c(0, 0, 1, 0, 0, 1, 1, 1),
#    "total_phenolic" = c(0, 0, 1, 0, 1, 1, 1, 0)
#)

 lEE <- list(
         "beta"= c(0, 1, 1, 1, 1, 0, 0, 0)
 )


cap_transpiration_cl0 <- c()
cap_transpiration_cl1 <- c()
cap_radiation_cl0 <- c()
cap_radiation_cl1 <- c()

channels_lims_mean <- list("photosynthesis"=1200, "transpiration"=750, "humidity"= 600, "co2"= 400, "radiation"= 500, "temperature" = 500)
channels_lims_std <- list("photosynthesis"=1000, "transpiration"= 750 , "humidity"= 600, "co2"= 400, "radiation"= 500, "temperature" = 500)

channel_names <- c("photosynthesis", "transpiration", "humidity", "co2", "radiation","temperature")
channel_units <- c("photosynthesis"="mean photosynthesis [μmol CO2/qm s]",  "transpiration"="mean transpiration [mg H2O/qm s]",   "humidity"="mean humidity [%]", "co2"="mean CO2 [ppm]", "radiation"="mean radiation [PPFD]", "temperature"="mean temperature [°C]")



channels_lims <- channels_lims_mean
featoI <- "mean"


for (i in names(lEE)){
    print(i)
    y <- lEE[[i]]
    x0 <- featdf[y==0]
    x1 <- featdf[y==1]

    for(channel in channel_names){
        IoI = which(channel_names == channel)
        bootstat0 <- c()
        bootstat1 <- c()
        name <- paste(i,"_matrix_",IoI,sep = "")
        name <- paste(name,"_",channel,sep = "")
        nameboot <- paste(name,"boot",sep = "")
    for (j in 1:length(x0)){
        bootstat0_part <- x0[[j]][[IoI]]
        bootstat0 <- c(bootstat0,  bootstat0_part)
    }
    for (j in 1:length(x1)){
        bootstat1_part <- x1[[j]][[IoI]]
        bootstat1 <- c(bootstat1,  bootstat1_part)

if (channel == "photosynthesis"){

    cap_photosynthesis_cl0 <- bootstat0
    cap_photosynthesis_cl1 <- bootstat1
}

    if (channel == "co2"){
        cap_co2_cl0 <- bootstat0
        cap_co2_cl1 <- bootstat1
    }

    if (channel == "humidity"){
        cap_humidity_cl0 <- bootstat0
        cap_humidity_cl1 <- bootstat1
    }

    if (channel == "transpiration"){
        cap_transpiration_cl0 <- bootstat0
        cap_transpiration_cl1 <- bootstat1
    }
    if (channel == "radiation"){
        cap_radiation_cl0 <- bootstat0
        cap_radiation_cl1 <- bootstat1
    }
    if (channel == "temperature"){
        cap_temperature_cl0 <- bootstat0
        cap_temperature_cl1 <- bootstat1
    }

groups <- c(rep("0",length(bootstat0)),rep("1",length(bootstat1)))
unit = "unit"
yylim = channels_lims[[channel]]
#plot_hist_for_feature_noidx_png_wunit(featoI,bootstat0 , bootstat1,groups, name, yylim)
plot_hist_for_feature_noidx_png_wunit(featoI,bootstat0 , bootstat1,groups, name, channel_units[[channel]])
}
print(summary(c(bootstat0,bootstat1)))
}
}


## radiation x transpiration
pl <- NULL
df <- data.frame(radiation = cap_radiation_cl0, transpiration = cap_transpiration_cl0)
df2 <- data.frame(radiation = cap_radiation_cl1, transpiration = cap_transpiration_cl1)
pl <- ggplot() + geom_point( data = df, color = "#f1b0ac", aes(x = radiation, y = transpiration))+ geom_point(data = df2, color = "#75d5d7", aes(x = radiation, y = transpiration))
pl <-
pl + labs(x = "Radiation [PPFD]", y = "Transpiration [mg H2O/m2s]") + geom_point() + stat_ellipse(geom = "polygon",data = df2,aes(x = radiation, y = transpiration ), alpha = 0.1, inherit.aes = FALSE ,level = 0.90)
pl
ggsave("1__multiplot_mean.tiff",pl)

library(scales)

X <- featdf
y<- lEE$beta

acc <- binary_cross_fols_validation_with_acc(X,y,8,5)
print(paste(mean(acc$acc)*100,"±",format(std_err(acc$acc)*100, digits = 4),"[%]"))
#print(paste(mean(acc$auc)*100,"±",format(std_err(acc$auc)*100, digits = 4),"[%]"))

